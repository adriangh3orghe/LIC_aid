# IATI disbursements since 2015 from the Global Fund, World Bank (IDA) and Gavi,
# via d-portal's public query API (no API key needed)
# -----------------------------------------------------------------------------
# Replaces iati_health_multilaterals.R. Writes the same processed file, so
# build_dashboard.R and the dashboard work unchanged.
#
#   install.packages(c("httr2", "readxl", "countrycode", "dplyr", "purrr", "readr", "tidyr"))
#
# d-portal's transaction table already gives a US dollar value for every
# transaction (converted with IMF rates) and splits each transaction across
# recipient countries and sectors, so no rows are dropped for currency and
# multi-country transactions are apportioned rather than excluded.

library(httr2)
library(readxl)
library(countrycode)
library(dplyr)
library(purrr)
library(readr)

# ---- Configuration ----------------------------------------------------------

oghist_file <- "OGHIST_2026_07_15.xlsx"
since_date  <- "2015-01-01"

publishers <- c(
  global_fund = "47045",   # The Global Fund
  world_bank  = "44000",   # The World Bank (IBRD + IDA); IDA isolated below
  gavi        = "47122"    # Gavi
)

dp_url     <- "https://d-portal.org/q.csv"
page_size  <- 10000    # rows per request; results are paged with offset.
                       # d-portal honours limits up to at least 50,000, but a
                       # limit of 100,000 silently falls back to 100 rows.
batch_size <- 50       # activity ids per request (if batching works)
pause_secs <- 0.5      # be polite to a free public service

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ---- Part 1: Income groups from the latest year in OGHIST --------------------

raw <- read_excel(oghist_file, sheet = "Country Analytical History",
                  col_names = FALSE, .name_repair = "minimal")
fy_row     <- as.character(unlist(raw[5, ]))
latest_col <- max(which(grepl("^FY\\d{2}$", fy_row)))
latest_fy  <- fy_row[latest_col]

countries <- tibble(
  iso3        = as.character(unlist(raw[-(1:6), 1])),
  country     = as.character(unlist(raw[-(1:6), 2])),
  income_code = as.character(unlist(raw[-(1:6), latest_col]))
) |>
  filter(!is.na(iso3), income_code %in% c("L", "LM", "UM")) |>
  mutate(
    income_group = recode(income_code, L = "Low income",
                          LM = "Lower middle income", UM = "Upper middle income"),
    iso2 = countrycode(iso3, "iso3c", "iso2c", custom_match = c(XKX = "XK"))
  )
stopifnot(!anyNA(countries$iso2))
write_csv(countries, paste0("data/processed/lmic_countries_", tolower(latest_fy), ".csv"))
message(latest_fy, ": ", nrow(countries), " low and middle income countries")

# ---- Part 2: d-portal query helper -------------------------------------------
# Every request is a plain URL, e.g.
#   https://d-portal.org/q.csv?from=act&reporting_ref=47122&limit=5
# Paste one into a browser to see what comes back.
#
# The URL is built by hand so that "|" (d-portal's "or" separator between
# values) is sent as a literal character rather than encoded as %7C.

dp_get <- function(params) {
  Sys.sleep(pause_secs)
  qs <- imap_chr(params, \(v, k) {
    parts <- strsplit(as.character(v), "|", fixed = TRUE)[[1]]
    paste0(k, "=", paste(curl::curl_escape(parts), collapse = "|"))
  })
  url <- paste0(dp_url, "?", paste(qs, collapse = "&"))
  resp <- request(url) |>
    req_user_agent("iati-health-dashboard (R; httr2)") |>
    req_timeout(60) |>
    # Retry busy-server responses AND dropped connections / DNS failures,
    # waiting 5, 10, 20, 40, 80 seconds (about 2.5 minutes in total)
    req_retry(max_tries = 6, retry_on_failure = TRUE,
              backoff = \(i) 5 * 2^(i - 1)) |>
    req_perform()
  txt <- resp_body_string(resp)
  if (!nzchar(trimws(txt))) return(tibble())
  read_csv(I(txt), na = c("", "null", "NULL"),
           col_types = cols(.default = "c"), show_col_types = FALSE)
}

# One page, or a quick single request (used by the probe)
dp_query <- function(..., limit = page_size) dp_get(c(list(...), limit = limit))

# Every page: keep asking for the next page until one comes back short.
# Results are sorted so pages don't overlap or skip rows.
# d-portal falls back to 100 rows when it rejects a limit, so a page of exactly
# 100 rows is checked: if the next 100 rows exist, the limit was ignored and
# paging continues 100 rows at a time. Otherwise the activity simply has 100.
dp_query_all <- function(..., orderby = "aid") {
  pages <- list(); offset <- 0; size <- page_size
  repeat {
    d <- dp_get(c(list(...), orderby = orderby, limit = size, offset = offset))
    n <- if (is.null(d)) 0L else nrow(d)
    if (n > 0) pages[[length(pages) + 1]] <- d
    if (n == 100 && size != 100) {
      nxt <- dp_get(c(list(...), orderby = orderby, limit = 100, offset = offset + 100))
      if (!is.null(nxt) && nrow(nxt) > 0) {
        warning("d-portal ignored limit = ", size, "; paging 100 rows at a time")
        size <- 100
      } else break
    }
    if (n < size) break
    offset <- offset + size
  }
  list_rbind(pages)
}
trans_order <- "aid,trans_id,trans_country,trans_sector"

# ---- Part 3: Activities per publisher (ids and titles) -------------------------

acts <- imap(publishers, \(ref, name) {
  out <- paste0("data/raw/dportal_act_", name, ".csv")
  if (file.exists(out)) return(read_csv(out, col_types = cols(.default = "c")))
  message(name, ": fetching activities")
  a <- dp_query_all(from = "act", reporting_ref = ref, orderby = "aid")
  if (is.null(a) || nrow(a) == 0) stop(name, ": d-portal returned no activities for reporting_ref ", ref)
  write_csv(a, out)
  a
})

# First run: check the column names d-portal returns for activities
message("Activity columns: ", paste(names(acts[[1]]), collapse = ", "))
iwalk(acts, \(a, name) message(name, ": ", nrow(a), " activities"))
# Compare with the IATI Dashboard: about 2,700 Global Fund, 5,300 World Bank, 1,500 Gavi

# ---- Part 4: Disbursements for those activities --------------------------------
# trans_code "D" = disbursement (d-portal uses letter codes: C, D, E, IF ...)
# No date filter is sent: d-portal's trans_day filter matched nothing in testing.
# All disbursements are downloaded and the 2015 cut-off is applied in R (Part 6).
#
# A single activity id works; batches of ids joined with "|" came back empty.
# So the script first tests three ways of asking for many activities at once,
# on a handful of Gavi activities, and uses the first one that returns the
# same rows as asking one activity at a time.

nrows <- function(d) if (is.null(d)) 0L else nrow(d)

probe_ids <- head(unique(acts$gavi$aid), 5)
n_single  <- sum(map_int(probe_ids, \(a) nrows(dp_query(from = "trans", aid = a, trans_code = "D"))))
message("Probe: ", n_single, " disbursement rows from 5 Gavi activities fetched one by one")
if (n_single == 0) stop("The probe activities have no disbursements; change probe_ids")

# Strategy A: several ids in one request, separated by "|"
n_batch <- nrows(dp_query(from = "trans", aid = paste(probe_ids, collapse = "|"), trans_code = "D"))

# Strategy B: join activities to transactions and filter by publisher
probe_join <- dp_query(from = "act,trans", reporting_ref = publishers[["gavi"]],
                       aid = probe_ids[1], trans_code = "D")
n_join_one <- nrows(probe_join)
n_one      <- nrows(dp_query(from = "trans", aid = probe_ids[1], trans_code = "D"))

strategy <- if (n_batch == n_single) {
  "batch"
} else if (n_one > 0 && n_join_one == n_one) {
  "join"
} else {
  "single"
}
message("Probe results: batch = ", n_batch, ", join = ", n_join_one, "/", n_one,
        " -> using strategy '", strategy, "'")

fetch_batch <- function(aids) {
  dp_query_all(from = "trans", aid = paste(aids, collapse = "|"), trans_code = "D",
               orderby = trans_order)
}

# The join returns activity columns too; keep the transaction columns only
trans_cols <- function(d) select(d, any_of("aid"), starts_with("trans_"))

# Work is split into units (one activity, one batch of ids, or one country,
# depending on the strategy) and saved in chunks of 200 units under
# data/raw/chunks/. If the run stops, rerunning skips the chunks already saved.
# A unit that still fails after its retries is tried once more at the end of
# its chunk; if it fails again, that chunk is not saved and the next run redoes it.

chunk_size <- 200
dir.create("data/raw/chunks", recursive = TRUE, showWarnings = FALSE)

fetch_unit <- function(unit, ref) {
  switch(strategy,
    batch  = fetch_batch(unit),
    join   = {
      d <- dp_query_all(from = "act,trans", reporting_ref = ref, trans_code = "D",
                        trans_country = unit, orderby = trans_order)
      if (nrows(d) > 0) trans_cols(d) else NULL
    },
    single = dp_query_all(from = "trans", aid = unit, trans_code = "D",
                          orderby = trans_order)
  )
}

fetch_publisher <- function(ref, aids, name) {
  units <- switch(strategy,
    batch  = split(aids, ceiling(seq_along(aids) / batch_size)),
    join   = as.list(countries$iso2),
    single = as.list(aids))
  chunks <- split(seq_along(units), ceiling(seq_along(units) / chunk_size))
  message("  ", name, ": ", length(units), " ", strategy, " requests in ",
          length(chunks), " chunks")

  complete <- TRUE
  for (k in seq_along(chunks)) {
    f <- sprintf("data/raw/chunks/%s_%s_%03d.csv", name, strategy, k)
    if (file.exists(f)) next
    t0 <- Sys.time()
    idx <- chunks[[k]]
    res <- vector("list", length(idx)); failed <- integer(0)
    for (j in seq_along(idx)) {
      # res[j] <- list(...) keeps NULL results in place (res[[j]] <- NULL would drop them)
      res[j] <- list(tryCatch(fetch_unit(units[[idx[j]]], ref),
                              error = \(e) { failed <<- c(failed, j); NULL }))
    }
    for (j in failed) {                      # one more go for any failures
      res[j] <- list(tryCatch(fetch_unit(units[[idx[j]]], ref),
                              error = \(e) structure(list(), class = "failed")))
    }
    still_failed <- failed[map_lgl(res[failed], \(r) inherits(r, "failed"))]
    if (length(still_failed) > 0) {
      warning(name, " chunk ", k, ": ", length(still_failed),
              " requests failed; chunk not saved, rerun the script to retry it")
      complete <- FALSE
      next
    }
    d <- list_rbind(res)
    write_csv(if (is.null(d)) tibble(aid = character()) else d, f)
    message(sprintf("  %s chunk %d/%d saved (%.1f min)", name, k, length(chunks),
                    as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  if (!complete) return(NULL)

  files <- sprintf("data/raw/chunks/%s_%s_%03d.csv", name, strategy, seq_along(chunks))
  map(files, \(f) read_csv(f, col_types = cols(.default = "c"))) |> list_rbind()
}

trans <- imap(acts, \(a, name) {
  out <- paste0("data/raw/dportal_trans_", name, ".csv")
  if (file.exists(out)) return(read_csv(out, col_types = cols(.default = "c")))
  message(name, ": fetching disbursements")
  d <- fetch_publisher(publishers[[name]], unique(a$aid), name)
  if (is.null(d)) {
    warning(name, ": incomplete; rerun the script to fetch the missing chunks")
    return(tibble())
  }
  if (nrows(d) == 0) {
    warning(name, ": no disbursements returned; nothing cached")
    return(tibble())
  }
  write_csv(d, out)
  d
})
iwalk(trans, \(d, name) message(name, ": ", nrows(d), " disbursement rows"))
if (any(map_int(trans, nrows) == 0)) stop("Some funders are incomplete; rerun the script")

# ---- Part 5: Isolate IDA within World Bank data ---------------------------------
# d-portal has no provider-org column on transactions. IDA lending is
# concessional and reported as ODA (flow type 10); IBRD lending is reported as
# other official flows (flow type 20). Check the split before trusting it:

wb <- trans$world_bank
if (nrows(wb) == 0) stop("No World Bank disbursements were downloaded; see the probe messages above")
print(count(wb, trans_flow_code, trans_finance_code, sort = TRUE))

ida_flow_codes <- c("10")
wb_ida <- wb |> filter(trans_flow_code %in% ida_flow_codes)
message("World Bank rows kept as IDA: ", nrow(wb_ida), " of ", nrow(wb),
        if (all(is.na(wb$trans_flow_code))) " -- flow code is empty; IDA filter needs another rule" else "")

# ---- Part 6: Tidy into the format build_dashboard.R expects ---------------------

titles <- imap(acts, \(a, name) {
  title_col <- intersect(c("title", "title_narrative"), names(a))[1]
  tibble(aid = a$aid, title = if (is.na(title_col)) a$aid else a[[title_col]])
}) |> list_rbind() |> distinct(aid, .keep_all = TRUE)

to_date <- function(x) {
  n <- as.numeric(x)
  # d-portal stores dates as days since 1970-01-01; guard against seconds
  if (isTRUE(max(abs(n), na.rm = TRUE) > 1e6)) n <- n / 86400
  as.Date(n, origin = "1970-01-01")
}

tx <- bind_rows(
  trans$global_fund |> mutate(funder = "Global Fund"),
  wb_ida            |> mutate(funder = "World Bank IDA"),
  trans$gavi        |> mutate(funder = "Gavi")
) |>
  mutate(date = to_date(trans_day)) |>
  filter(date >= as.Date(since_date)) |>
  left_join(titles, by = "aid") |>
  left_join(countries |> select(iso2, iso3, country, income_group),
            by = c("trans_country" = "iso2")) |>
  transmute(
    funder,
    iati_identifier = aid,
    title_narrative = title,
    transaction_transaction_type_code = "3",
    transaction_transaction_date_iso_date = format(date),
    transaction_value = trans_value,
    transaction_value_currency = trans_currency,
    transaction_value_usd = trans_usd,
    country_iso2 = trans_country,
    iso3, country, income_group,
    sector_code = trans_sector,
    multi_country = "FALSE",          # d-portal already apportions by country
    data_source = "d-portal"
  )

message("Rows with a country outside the LIC/LMIC/UMIC list or none: ",
        sum(is.na(tx$income_group)))

write_csv(tx, "data/processed/iati_tx_gf_ida_gavi_since2015.csv")

# Quick check against published totals before building the dashboard
tx |>
  filter(!is.na(income_group)) |>
  mutate(year = substr(transaction_transaction_date_iso_date, 1, 4),
         usd = as.numeric(transaction_value_usd)) |>
  group_by(funder, year) |>
  summarise(usd_bn = round(sum(usd, na.rm = TRUE) / 1e9, 2), .groups = "drop") |>
  tidyr::pivot_wider(names_from = funder, values_from = usd_bn) |>
  print(n = 20)
