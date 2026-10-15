# Compare total health aid to low and middle income countries in three sources:
# IATI (via d-portal), the OECD Creditor Reporting System (CRS) and IHME's
# Development Assistance for Health (DAH) database
# -----------------------------------------------------------------------------
# Purpose: find the most reliable source for country- and income-group-level
# comparisons of total health aid, particularly to low-income countries.
#
#   install.packages(c("arrow", "httr2", "dplyr", "readr", "readxl", "tidyr",
#                      "purrr", "jsonlite", "WDI"))
#
# Run iati_dportal.R first (it writes the country list with income groups).
# Writes docs/sources.js, which the dashboard reads for its "Comparing data
# sources" sections. Downloads are cached in data/raw, so a rerun is quick.
#
# Inputs:
#   IATI   d-portal, all publishers, disbursements to each country, DAC sector
#          categories 120 (121, 122, 123) and 130, downloaded here
#   CRS    Downloads/CRS.parquet (OECD CRS bulk file, USD millions)
#   IHME   Downloads/IHME_DAH_DATABASE_1990_2026/ (CSV, thousands of USD in
#          constant prices of the year in the value column name, e.g. dah_23)
#   WHO    External health spending (GHED, via World Bank WDI), as an
#          independent, recipient-side benchmark rather than a candidate
#
# Currency: every source is converted to constant US dollars of `base_year`
# with the US GDP deflator (IMF WEO, NGDP_D). IATI and CRS are in current
# dollars; IHME is already in constant dollars of its price year and is rebased.

library(httr2)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(purrr)
library(jsonlite)

# ---- Configuration ----------------------------------------------------------

years      <- 2015:2026
# Downloads folder: USERPROFILE on Windows (R's "~" there is Documents)
downloads  <- file.path(Sys.getenv("USERPROFILE", unset = path.expand("~")), "Downloads")
crs_file   <- file.path(downloads, "CRS.parquet")
ihme_dir   <- file.path(downloads, "IHME_DAH_DATABASE_1990_2026")
weo_file   <- "WEOApr2026all.xlsx"     # same file as build_dashboard.R

# Health: DAC sector categories 121, 122, 123 (together "120 Health") and
# 130 (Population policies and programmes, reproductive health)
health_groups <- c("121", "122", "123", "130")

# CRS flows counted as aid: ODA grants (11), ODA grant-like (12), ODA loans (13),
# equity (19) and private development finance from foundations (30).
# Other official flows (14), such as IBRD loans, are not aid.
crs_flows <- c(11, 12, 13, 19, 30)

# IHME channels left out to match the aid definition used for CRS: IBRD lends
# on non-concessional terms (OOF in the CRS). Set to character() to keep it.
ihme_drop_channels <- c("WB_IBRD")

# IHME sources left out to match the CRS, which does not cover NGOs' own
# private fundraising or corporate giving. Gates (in the CRS), debt repayments
# (development bank lending, e.g. IDA) and government sources are kept. IHME
# records funds by the channel that disburses them, so public money passed
# through NGOs stays in (it shows under the NGO channel, not the NGO source).
# Set to character() to compare IHME's full total.
ihme_drop_sources <- c("Private_other", "Corporate_donations")

# Price year for every source. NULL = IHME's price year (no rebasing for IHME).
base_year <- NULL

dp_url     <- "https://d-portal.org/q.csv"
pause_secs <- 0.5

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# ---- Countries ----------------------------------------------------------------

country_file <- sort(list.files("data/processed", "^lmic_countries_fy\\d+\\.csv$",
                                full.names = TRUE)) |> tail(1)
if (!length(country_file)) stop("Run iati_dportal.R first: no data/processed/lmic_countries_*.csv")
countries <- read_csv(country_file, col_types = cols(.default = "c")) |>
  select(iso3, iso2, country, income_group)
fy_label <- toupper(sub(".*_(fy\\d+)\\.csv$", "\\1", country_file))

# ---- 1. IATI: health disbursements by recipient country, from d-portal --------
# d-portal adds up transactions itself: joining activities to their recipient
# countries and sectors, "sum_of_percent_of_trans_usd" splits each transaction
# by the activity's country and sector percentages. One request per year
# returns every country x sector total; the health sectors are picked out in R.
# Values are current US dollars, converted by d-portal at IMF rates.
# Not yet run against the live service: the probe and checks below stop with
# a message if d-portal answers differently than expected.

dp_get <- function(params) {
  Sys.sleep(pause_secs)
  qs <- imap_chr(params, \(v, k) {
    parts <- strsplit(as.character(v), "|", fixed = TRUE)[[1]]
    paste0(k, "=", paste(curl::curl_escape(parts), collapse = "|"))
  })
  url <- paste0(dp_url, "?", paste(qs, collapse = "&"))
  resp <- request(url) |>
    req_user_agent("iati-health-dashboard (R; httr2)") |>
    req_timeout(300) |>
    req_retry(max_tries = 6, retry_on_failure = TRUE, backoff = \(i) 5 * 2^(i - 1)) |>
    req_perform()
  txt <- resp_body_string(resp)
  if (!nzchar(trimws(txt))) return(tibble())
  read_csv(I(txt), na = c("", "null", "NULL"), col_types = cols(.default = "c"),
           show_col_types = FALSE)
}

# Aggregate query, paged (d-portal caps a page at about 50,000 rows)
dp_sum <- function(...) {
  pages <- list(); offset <- 0; size <- 20000
  repeat {
    d <- dp_get(c(list(from = "act,trans,country,sector", trans_code = "D",
                       select = "country_code,sector_code,sum_of_percent_of_trans_usd",
                       groupby = "country_code,sector_code",
                       orderby = "country_code,sector_code",
                       limit = size, offset = offset), list(...)))
    if (nrow(d)) pages[[length(pages) + 1]] <- d
    if (nrow(d) < size) break
    offset <- offset + size
  }
  d <- list_rbind(pages)
  if (!nrow(d)) return(tibble(country_code = character(), sector_code = character(), usd = numeric()))
  usd_col <- grep("sum", names(d), value = TRUE)[1]
  if (is.na(usd_col) || !all(c("country_code", "sector_code") %in% names(d)))
    stop("Unexpected d-portal columns: ", paste(names(d), collapse = ", "))
  transmute(d, country_code, sector_code, usd = as.numeric(.data[[usd_col]]))
}

# Date filters: d-portal stores days since 1970-01-01. The probe below checks
# which form of date filter it accepts, using Kenya: a one-year total must be
# above zero and below the all-years total.
year_filter <- function(y, style) {
  from <- as.Date(sprintf("%d-01-01", y)); to <- as.Date(sprintf("%d-01-01", y + 1))
  switch(style,
    iso  = list(trans_day_gteq = format(from), trans_day_lt = format(to)),
    days = list(trans_day_gteq = as.integer(from), trans_day_lt = as.integer(to)))
}

iati_cache <- function(y) sprintf("data/raw/dportal_country_health_%d.csv", y)
need <- years[!file.exists(iati_cache(years))]
if (length(need)) {
  probe_all <- sum(dp_sum(country_code = "KE")$usd, na.rm = TRUE)
  style <- NULL
  for (s in c("iso", "days")) {
    p <- sum(do.call(dp_sum, c(list(country_code = "KE"), year_filter(2019, s)))$usd, na.rm = TRUE)
    message(sprintf("d-portal probe (%s dates): Kenya 2019 $%.0fm of $%.0fm all years",
                    s, p / 1e6, probe_all / 1e6))
    if (p > 0 && p < probe_all) { style <- s; break }
  }
  if (is.null(style)) stop("d-portal ignored both date filters; see the probe messages above")

  for (y in need) {
    message("IATI ", y, ": fetching country x sector totals")
    d <- do.call(dp_sum, year_filter(y, style))
    if (!nrow(d)) { warning("IATI ", y, ": no rows"); next }
    write_csv(d, iati_cache(y))
  }
}

iati <- map(years[file.exists(iati_cache(years))], \(y)
            read_csv(iati_cache(y), col_types = "ccd") |> mutate(year = y)) |>
  list_rbind() |>
  filter(substr(sector_code, 1, 3) %in% health_groups) |>
  inner_join(countries, by = c("country_code" = "iso2")) |>
  group_by(iso3, year) |>
  summarise(iati = sum(usd, na.rm = TRUE) / 1e6, .groups = "drop")   # USD millions, current
message("IATI: ", n_distinct(iati$iso3), " countries, ", min(iati$year), "-", max(iati$year))

# Take out World Bank IBRD lending, which the CRS (other official flows) and
# IHME (channel dropped above) leave out. d-portal's totals can't separate it,
# so it comes from the World Bank transactions saved by iati_dportal.R, where
# IBRD loans carry flow type 20 (IDA is 10). Same health sectors, by the
# transaction's country and sector split. If that file is missing, IATI is
# left as is and the page says so.
wb_file <- "data/raw/dportal_trans_world_bank.csv"
iati_ibrd_adjusted <- file.exists(wb_file)
if (iati_ibrd_adjusted) {
  wb <- read_csv(wb_file, col_types = cols(.default = "c"))
  day <- as.numeric(wb$trans_day)
  if (isTRUE(max(abs(day), na.rm = TRUE) > 1e6)) day <- day / 86400   # seconds, not days
  message("World Bank flow types in IATI: ",
          paste(names(table(wb$trans_flow_code, useNA = "ifany")), collapse = ", "))
  ibrd <- wb |>
    mutate(year = as.integer(format(as.Date(day, origin = "1970-01-01"), "%Y")),
           usd = as.numeric(trans_usd)) |>
    filter(trans_flow_code == "20", year %in% years,
           substr(trans_sector, 1, 3) %in% health_groups) |>
    inner_join(countries, by = c("trans_country" = "iso2")) |>
    group_by(iso3, year) |>
    summarise(ibrd = sum(usd, na.rm = TRUE) / 1e6, .groups = "drop")
  iati <- iati |>
    left_join(ibrd, by = c("iso3", "year")) |>
    mutate(ibrd = coalesce(ibrd, 0),
           iati_with_ibrd = iati,
           iati = pmax(iati - ibrd, 0)) |>     # never below zero
    select(iso3, year, iati, iati_with_ibrd)
  message(sprintf("IATI: removed $%.1fbn of IBRD health lending (current dollars, %d-%d)",
                  sum(ibrd$ibrd) / 1e3, min(years), max(years)))
} else {
  warning(wb_file, " not found (run iati_dportal.R); IATI includes IBRD lending")
  iati <- mutate(iati, iati_with_ibrd = iati)
}

# ---- 2. CRS: health ODA disbursements by recipient -----------------------------
# Columns as in the OECD bulk file: year, de_recipientcode (ISO3; regional
# rows end in "_X"), flow_code, purpose_code, usd_disbursement (USD millions, current)

# Cached after the first read; a newer CRS.parquet is read again
crs_cache <- "data/raw/crs_country_health.csv"
if (file.exists(crs_cache) && !isTRUE(file.mtime(crs_file) > file.mtime(crs_cache))) {
  crs <- read_csv(crs_cache, col_types = "cid")
} else {
  if (!file.exists(crs_file)) stop("CRS file not found: ", crs_file)
  src <- if (grepl("\\.parquet$", crs_file)) arrow::open_dataset(crs_file) else
    read_csv(crs_file, show_col_types = FALSE, guess_max = 1e5)
  crs <- src |>
    select(year, de_recipientcode, flow_code, purpose_code, usd_disbursement) |>
    # Codes may be stored as text; compare them as numbers
    mutate(year = as.integer(year), flow_code = as.integer(flow_code),
           purpose_code = as.integer(purpose_code),
           usd_disbursement = as.numeric(usd_disbursement)) |>
    filter(year >= !!min(years), year <= !!max(years),
           flow_code %in% !!as.integer(crs_flows),
           (purpose_code >= 12100L & purpose_code < 12400L) |
           (purpose_code >= 13000L & purpose_code < 13100L)) |>
    group_by(iso3 = de_recipientcode, year) |>
    summarise(crs = sum(usd_disbursement, na.rm = TRUE), .groups = "drop") |>
    collect() |>
    mutate(year = as.integer(year))
  write_csv(crs, crs_cache)
}
crs <- semi_join(crs, countries, by = "iso3")
message("CRS: ", n_distinct(crs$iso3), " countries, ", min(crs$year), "-", max(crs$year))

# ---- 3. IHME DAH: health aid by recipient ---------------------------------------
# One row per year x source x channel x recipient. Rows with elim_ch = 1 are
# transfers between channels, dropped to avoid double counting. prelim_est = 1
# marks preliminary estimates (recent years), kept but flagged. ihme_all keeps
# every source, to report how much the source filter removes.

ihme_files <- list.files(ihme_dir, "\\.csv$", ignore.case = TRUE, full.names = TRUE)
ihme_files <- ihme_files[!grepl("codebook", ihme_files, ignore.case = TRUE)]
if (!length(ihme_files)) stop("No IHME CSV files found in ", ihme_dir)
ihme_head <- names(read_csv(ihme_files[1], n_max = 0, show_col_types = FALSE))
value_col <- grep("^dah_\\d{2}$", ihme_head, value = TRUE)
if (length(value_col) != 1) stop("Could not find the total DAH column (dah_YY) in ", basename(ihme_files[1]))
ihme_price_year <- 2000L + as.integer(sub("dah_", "", value_col))
message("IHME: total DAH column ", value_col, " -> constant ", ihme_price_year, " US dollars")

ihme <- map(ihme_files, \(f) read_csv(f, show_col_types = FALSE,
  col_select = all_of(c("year", "source", "channel", "recipient_isocode", "elim_ch", "prelim_est", value_col)),
  col_types = cols(.default = "c"))) |>
  list_rbind() |>
  mutate(year = as.integer(year), value = as.numeric(.data[[value_col]]),
         elim = coalesce(as.numeric(elim_ch), 0), prelim = coalesce(as.numeric(prelim_est), 0)) |>
  filter(year %in% years, elim != 1, !channel %in% ihme_drop_channels) |>
  semi_join(countries, by = c("recipient_isocode" = "iso3")) |>
  group_by(iso3 = recipient_isocode, year) |>
  mutate(kept = !source %in% ihme_drop_sources) |>
  summarise(ihme = sum(value[kept], na.rm = TRUE) / 1e3,             # USD millions
            ihme_all = sum(value, na.rm = TRUE) / 1e3,
            ihme_prelim = sum(value[kept & prelim == 1], na.rm = TRUE) / 1e3, .groups = "drop")
message("IHME: ", n_distinct(ihme$iso3), " countries, ", min(ihme$year), "-", max(ihme$year))

# ---- 4. WHO external health spending (benchmark) ---------------------------------

ghed_cache <- "data/raw/ghed_external.csv"
if (file.exists(ghed_cache)) {
  ghed <- read_csv(ghed_cache, col_types = "cid")
} else {
  ghed <- WDI::WDI(country = "all", indicator = c(ext = "SH.XPD.EHEX.PC.CD", pop = "SP.POP.TOTL"),
                   start = min(years), end = max(years)) |>
    transmute(iso3 = iso3c, year = as.integer(year), ghed = ext * pop / 1e6) |>
    filter(!is.na(ghed))
  write_csv(ghed, ghed_cache)
}
ghed <- semi_join(ghed, countries, by = "iso3")

# ---- 5. One currency-year: constant US dollars of base_year -----------------------

if (is.null(base_year)) base_year <- ihme_price_year
defl_cache <- "data/raw/us_gdp_deflator.csv"
if (file.exists(defl_cache)) {
  defl <- read_csv(defl_cache, col_types = "id")
} else {
  defl <- read_excel(weo_file, sheet = "Countries", col_types = "text") |>
    filter(`COUNTRY.ID` == "USA", `INDICATOR.ID` == "NGDP_D") |>
    select(matches("^\\d{4}$")) |>
    pivot_longer(everything(), names_to = "year", values_to = "deflator") |>
    mutate(year = as.integer(year), deflator = suppressWarnings(as.numeric(deflator))) |>
    filter(!is.na(deflator))
  if (!nrow(defl)) stop("No US GDP deflator (NGDP_D, USA) in ", weo_file)
  write_csv(defl, defl_cache)
}
missing_years <- setdiff(c(years, base_year, ihme_price_year), defl$year)
if (length(missing_years)) stop("US GDP deflator missing for ", paste(missing_years, collapse = ", "))
d_at <- \(y) defl$deflator[match(y, defl$year)]
to_base <- \(current_usd, y) current_usd * d_at(base_year) / d_at(y)   # current -> constant base_year

panel <- countries |>
  select(iso3, iso2, income_group) |>
  cross_join(tibble(year = years)) |>
  left_join(iati, by = c("iso3", "year")) |>
  left_join(crs,  by = c("iso3", "year")) |>
  left_join(ihme, by = c("iso3", "year")) |>
  left_join(ghed, by = c("iso3", "year")) |>
  mutate(
    iati = to_base(iati, year),
    iati_with_ibrd = to_base(iati_with_ibrd, year),
    crs  = to_base(crs, year),
    ghed = to_base(ghed, year),
    # IHME: constant ihme_price_year dollars -> constant base_year dollars
    across(c(ihme, ihme_all, ihme_prelim), \(v) v * d_at(base_year) / d_at(ihme_price_year))
  )

# A source "covers" a year once it reports for at least half the countries it
# usually reports for; later years are incomplete (reporting lags)
cover <- function(v) {
  n <- tapply(!is.na(v) & v > 0, panel$year, sum)
  as.integer(names(n)[n >= 0.5 * median(n[n > 0])])
}
coverage <- list(iati = cover(panel$iati), crs = cover(panel$crs),
                 ihme = cover(panel$ihme), ghed = cover(panel$ghed))
# IHME years that are mostly preliminary
prelim_share <- panel |> group_by(year) |>
  summarise(s = sum(ihme_prelim, na.rm = TRUE) / sum(ihme, na.rm = TRUE))
ihme_prelim_years <- prelim_share$year[!is.na(prelim_share$s) & prelim_share$s > 0.5]
for (k in names(coverage)) message(k, " covers: ", paste(range(coverage[[k]]), collapse = "-"))

# ---- 6. Agreement between sources ---------------------------------------------------
# Compared over years covered by all three candidates (and final, not
# preliminary, for IHME), on country-years where both sources report.
#   gap        median absolute gap, in %, |a/b - 1| on a log scale (symmetric)
#   within25   share of country-years where a and b differ by 25% or less
#   ratio      a's total over b's total for the group, same country-years

cand <- c("iati", "crs", "ihme")
overlap_years <- setdiff(Reduce(intersect, coverage[cand]), ihme_prelim_years)
if (!length(overlap_years)) stop("No year is covered by all three sources")
message("Comparison years: ", paste(range(overlap_years), collapse = "-"))

agree <- function(d, a, b) {
  x <- d[[a]]; y <- d[[b]]; ok <- !is.na(x) & !is.na(y) & x > 0 & y > 0
  if (sum(ok) < 3) return(list(n = sum(ok), gap = NA, within25 = NA, ratio = NA))
  lr <- abs(log(x[ok] / y[ok]))
  list(n = sum(ok), gap = round(100 * (exp(median(lr)) - 1), 1),
       within25 = round(100 * mean(lr <= log(1.25)), 1),
       ratio = round(sum(x[ok]) / sum(y[ok]), 3))
}

groups <- c(sort(unique(countries$income_group)), "All low and middle income")
in_group <- \(g) if (g == "All low and middle income") panel else filter(panel, income_group == g)

metrics <- map(set_names(groups), \(g) {
  d <- filter(in_group(g), year %in% overlap_years)
  pairs <- map(cand, \(a) {
    others <- setdiff(c(cand, "ghed"), a)
    set_names(map(others, \(b) agree(d, a, b)), others)
  }) |> set_names(cand)
  # Consistency: median gap to the other two candidates; GHED is reported apart
  score <- map_dbl(cand, \(a) mean(map_dbl(setdiff(cand, a), \(b) pairs[[a]][[b]]$gap), na.rm = TRUE))
  list(pairs = pairs, consistency = set_names(as.list(round(score, 1)), cand),
       countries = map(set_names(cand), \(a) n_distinct(d$iso3[!is.na(d[[a]]) & d[[a]] > 0])))
})

# Share of IHME's full total removed by the source filter, comparison years
# Share of IATI's total removed as IBRD lending, comparison years
iati_ibrd_share <- map(set_names(groups), \(g) {
  d <- filter(in_group(g), year %in% overlap_years)
  round(100 * (1 - sum(d$iati, na.rm = TRUE) / sum(d$iati_with_ibrd, na.rm = TRUE)), 1)
})

ihme_dropped_share <- map(set_names(groups), \(g) {
  d <- filter(in_group(g), year %in% overlap_years)
  round(100 * (1 - sum(d$ihme, na.rm = TRUE) / sum(d$ihme_all, na.rm = TRUE)), 1)
})

# ---- 7. Write docs/sources.js ----------------------------------------------------------

r2 <- \(v) ifelse(is.na(v), NA, round(v, 2))
series_rows <- \(d) unname(pmap(list(d$year, r2(d$iati), r2(d$crs), r2(d$ihme), r2(d$ghed)), c))

by_country <- panel |>
  filter(if_any(c(iati, crs, ihme, ghed), \(v) !is.na(v))) |>
  split(~iso2) |>
  map(\(d) series_rows(arrange(d, year)))

# Group totals only over the years each source covers (blank otherwise)
by_group <- map(set_names(groups), \(g) {
  d <- in_group(g) |>
    group_by(year) |>
    summarise(across(c(iati, crs, ihme, ghed), \(v) sum(v, na.rm = TRUE)), .groups = "drop")
  for (k in names(coverage)) d[[k]][!d$year %in% coverage[[k]]] <- NA
  series_rows(d)
})

out <- list(
  meta = list(
    built = format(Sys.Date()), base_year = base_year, ihme_price_year = ihme_price_year,
    # I() keeps one-element vectors as JSON arrays
    classification = fy_label, years = years, overlap_years = I(overlap_years),
    ihme_prelim_years = I(ihme_prelim_years), coverage = map(coverage, range),
    health_groups = I(health_groups), crs_flows = I(crs_flows),
    ihme_dropped_channels = I(ihme_drop_channels),
    ihme_dropped_sources = I(ihme_drop_sources), ihme_dropped_share = ihme_dropped_share,
    iati_ibrd_adjusted = iati_ibrd_adjusted, iati_ibrd_share = iati_ibrd_share,
    deflator = paste0("US GDP deflator, ", sub("\\.xlsx$", "", weo_file))
  ),
  countries = by_country,          # iso2: [[year, iati, crs, ihme, ghed], ...] USD millions
  groups = by_group,               # group: same shape, summed over the group's countries
  metrics = metrics
)

writeLines(paste0("window.SOURCES = ",
                  toJSON(out, auto_unbox = TRUE, na = "null", digits = NA, null = "null"), ";"),
           "docs/sources.js", useBytes = TRUE)
message("Wrote docs/sources.js: ", length(by_country), " countries, constant ",
        base_year, " US dollars")

# Quick look: group totals, USD billions
bind_rows(map(groups, \(g) in_group(g) |> mutate(group = g))) |>
  filter(year %in% overlap_years) |>
  group_by(group) |>
  summarise(across(c(iati, crs, ihme, ghed), \(v) round(sum(v, na.rm = TRUE) / 1e3, 1))) |>
  print()
