# Share of country-allocable health aid that went to low-income countries in
# 2023, for five channels, in IATI, IHME and the OECD CRS
# -----------------------------------------------------------------------------
# Channels: Gavi, World Bank IDA, Global Fund, Gates Foundation, US government.
# Output: a table by data source and channel with disbursements to low-income
# countries, total country-allocable disbursements and the low-income share,
# printed and saved to data/processed/lic_share_2023.csv.
#
#   install.packages(c("arrow", "httr2", "dplyr", "readr", "readxl", "tidyr",
#                      "purrr", "countrycode"))
#
# Uses the inputs of compare_sources.R: the OGHIST file, CRS.parquet and the
# IHME DAH folder in Downloads, and data/raw/dportal_trans_world_bank.csv saved
# by iati_dportal.R. For IATI it makes one extra d-portal request set for 2023
# (cached in data/raw).
#
# Definitions
#   Health aid       DAC sectors 121, 122, 123 and 130 (IATI, CRS); all DAH (IHME)
#   Country-allocable  disbursements to a named recipient country; regional,
#                    global and unallocated amounts are left out of both the
#                    numerator and the total
#   Low income       World Bank classification in `income_fy` (see below)
#   Dollars          2023 US dollars: current for IATI and CRS, IHME's constant
#                    2023 prices (the same year, so no conversion is needed)

library(httr2)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(purrr)

# ---- Configuration ----------------------------------------------------------

yr          <- 2023
# World Bank classification for 2023. FY24 is the one published in July 2023
# (based on 2022 GNI per capita) and used from July 2023; FY23 was in force
# for the first half of 2023. Change to "FY23" to use that instead.
income_fy   <- "FY24"
oghist_file <- "OGHIST_2026_07_15.xlsx"
downloads   <- file.path(Sys.getenv("USERPROFILE", unset = path.expand("~")), "Downloads")
crs_file    <- file.path(downloads, "CRS.parquet")
ihme_dir    <- file.path(downloads, "IHME_DAH_DATABASE_1990_2026")
wb_file     <- "data/raw/dportal_trans_world_bank.csv"

health_groups <- c("121", "122", "123", "130")
crs_flows     <- c(11, 12, 13, 19, 30)   # ODA and private development finance, as in compare_sources.R

channels <- c("Gavi", "World Bank IDA", "Global Fund", "Gates Foundation", "US government")

# IATI publishers (d-portal reporting_ref). The US government publishes under
# several agency identifiers starting "US-GOV-"; the ones found are listed when
# the script runs. World Bank IDA = World Bank (44000) minus IBRD lending.
iati_refs <- list(
  "Gavi"             = "^47122$",
  "World Bank IDA"   = "^44000$",
  "Global Fund"      = "^47045$",
  "Gates Foundation" = "^DAC-1601$",
  "US government"    = "^US-GOV-"
)

# CRS reporters, matched on donor_name; the names matched are listed when the
# script runs
crs_donors <- list(
  "Gavi"             = "Gavi|Vaccines and Immuni",
  "World Bank IDA"   = "International Development Association",
  "Global Fund"      = "Global Fund",
  "Gates Foundation" = "Gates Foundation",
  "US government"    = "^United States$"
)

# IHME: multilaterals are channels (the agency that disburses). Gates and the
# US are matched by source over their own channel plus NGOs they fund, which
# mirrors how the CRS records them as donors (including money they route
# through NGOs), and leaves out their contributions to Gavi, the Global Fund
# and other multilaterals (counted under those channels instead).
ihme_rules <- list(
  "Gavi"             = list(channel = "GAVI"),
  "World Bank IDA"   = list(channel = "WB_IDA"),
  "Global Fund"      = list(channel = "GFATM"),
  "Gates Foundation" = list(source = "Gates Foundation", channel = c("GATES", "NGO", "INTLNGO")),
  "US government"    = list(source = "United_States",    channel = c("BIL_USA", "NGO", "INTLNGO"))
)

dp_url     <- "https://d-portal.org/q.csv"
pause_secs <- 0.5
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ---- Income classification -----------------------------------------------------

raw <- read_excel(oghist_file, sheet = "Country Analytical History",
                  col_names = FALSE, .name_repair = "minimal")
fy_row <- as.character(unlist(raw[5, ]))
fy_col <- which(fy_row == income_fy)
if (length(fy_col) != 1) stop(income_fy, " not found in ", oghist_file)
income <- tibble(iso3 = as.character(unlist(raw[-(1:6), 1])),
                 code = as.character(unlist(raw[-(1:6), fy_col]))) |>
  filter(!is.na(iso3), grepl("^[A-Z]{3}$", iso3))
lic_iso3 <- income$iso3[income$code %in% "L"]
message(income_fy, ": ", length(lic_iso3), " low-income countries")

iso2_to_iso3 <- \(x) countrycode::countrycode(x, "iso2c", "iso3c", warn = FALSE,
                                             custom_match = c(XK = "XKX"))
# A named recipient country: a 3-letter ISO code, not IHME's "unallocated" or "global"
is_country <- \(iso3) !is.na(iso3) & grepl("^[A-Z]{3}$", iso3) & !iso3 %in% c("QZA", "WLD")

summarise_channel <- \(d) d |>
  filter(is_country(iso3)) |>
  group_by(channel) |>
  summarise(lic = sum(usd_m[iso3 %in% lic_iso3], na.rm = TRUE),
            total = sum(usd_m, na.rm = TRUE), .groups = "drop")

# ---- IATI (d-portal) --------------------------------------------------------------
# One aggregate query for 2023: every publisher x recipient country x sector,
# with each transaction split by the activity's country and sector percentages.

dp_get <- function(params) {
  Sys.sleep(pause_secs)
  qs <- imap_chr(params, \(v, k) {
    parts <- strsplit(as.character(v), "|", fixed = TRUE)[[1]]
    paste0(k, "=", paste(curl::curl_escape(parts), collapse = "|"))
  })
  resp <- request(paste0(dp_url, "?", paste(qs, collapse = "&"))) |>
    req_user_agent("iati-health-dashboard (R; httr2)") |>
    req_timeout(300) |>
    req_retry(max_tries = 6, retry_on_failure = TRUE, backoff = \(i) 5 * 2^(i - 1)) |>
    req_perform()
  txt <- resp_body_string(resp)
  if (!nzchar(trimws(txt))) return(tibble())
  read_csv(I(txt), na = c("", "null", "NULL"), col_types = cols(.default = "c"),
           show_col_types = FALSE)
}

iati_cache <- sprintf("data/raw/dportal_reporter_country_health_%d.csv", yr)
if (!file.exists(iati_cache)) {
  message("IATI ", yr, ": fetching publisher x country x sector totals")
  pages <- list(); offset <- 0; size <- 20000
  repeat {
    d <- dp_get(list(from = "act,trans,country,sector", trans_code = "D",
                     trans_day_gteq = sprintf("%d-01-01", yr),
                     trans_day_lt = sprintf("%d-01-01", yr + 1),
                     select = "reporting_ref,country_code,sector_code,sum_of_percent_of_trans_usd",
                     groupby = "reporting_ref,country_code,sector_code",
                     orderby = "reporting_ref,country_code,sector_code",
                     limit = size, offset = offset))
    if (nrow(d)) pages[[length(pages) + 1]] <- d
    if (nrow(d) < size) break
    offset <- offset + size
  }
  d <- list_rbind(pages)
  usd_col <- grep("sum", names(d), value = TRUE)[1]
  if (!nrow(d) || is.na(usd_col) || !all(c("reporting_ref", "country_code", "sector_code") %in% names(d)))
    stop("Unexpected d-portal answer; columns: ", paste(names(d), collapse = ", "))
  d <- transmute(d, reporting_ref, country_code, sector_code, usd = as.numeric(.data[[usd_col]]))
  write_csv(d, iati_cache)
}
iati_raw <- read_csv(iati_cache, col_types = "cccd") |>
  filter(substr(sector_code, 1, 3) %in% health_groups)

iati <- imap(iati_refs, \(pattern, ch) {
  d <- filter(iati_raw, grepl(pattern, reporting_ref))
  if (ch == "US government") {
    message("IATI US government publishers found: ",
            paste(sort(unique(d$reporting_ref)), collapse = ", "))
  }
  if (!nrow(d)) warning("IATI: no 2023 health disbursements for ", ch, " (", pattern, ")")
  transmute(d, channel = ch, iso3 = iso2_to_iso3(country_code), usd_m = usd / 1e6)
}) |> list_rbind()

# IDA: take IBRD lending (flow type 20) out of the World Bank total, using the
# World Bank transactions iati_dportal.R saved
if (file.exists(wb_file)) {
  wb <- read_csv(wb_file, col_types = cols(.default = "c"))
  day <- as.numeric(wb$trans_day)
  if (isTRUE(max(abs(day), na.rm = TRUE) > 1e6)) day <- day / 86400
  ibrd <- wb |>
    mutate(year = as.integer(format(as.Date(day, origin = "1970-01-01"), "%Y"))) |>
    filter(trans_flow_code == "20", year == yr, substr(trans_sector, 1, 3) %in% health_groups) |>
    transmute(channel = "World Bank IDA", iso3 = iso2_to_iso3(trans_country),
              usd_m = -as.numeric(trans_usd) / 1e6)          # subtracted
  iati <- bind_rows(iati, ibrd)
  message(sprintf("IATI: IBRD health lending removed from the World Bank: $%.0fm", -sum(ibrd$usd_m, na.rm = TRUE)))
} else {
  warning(wb_file, " not found (run iati_dportal.R); IATI World Bank figure includes IBRD")
}
iati_tab <- summarise_channel(iati)

# ---- CRS ------------------------------------------------------------------------

src <- if (grepl("\\.parquet$", crs_file)) arrow::open_dataset(crs_file) else
  read_csv(crs_file, show_col_types = FALSE, guess_max = 1e5)
crs <- src |>
  select(year, donor_name, de_recipientcode, flow_code, purpose_code, usd_disbursement) |>
  mutate(year = as.integer(year), flow_code = as.integer(flow_code),
         purpose_code = as.integer(purpose_code),
         usd_disbursement = as.numeric(usd_disbursement)) |>
  filter(year == !!as.integer(yr), flow_code %in% !!as.integer(crs_flows),
         (purpose_code >= 12100L & purpose_code < 12400L) |
         (purpose_code >= 13000L & purpose_code < 13100L)) |>
  group_by(donor_name, iso3 = de_recipientcode) |>
  summarise(usd_m = sum(usd_disbursement, na.rm = TRUE), .groups = "drop") |>
  collect()

crs <- imap(crs_donors, \(pattern, ch) {
  d <- filter(crs, grepl(pattern, donor_name))
  message("CRS ", ch, ": ", if (nrow(d)) paste(unique(d$donor_name), collapse = "; ") else "no reporter matched")
  mutate(d, channel = ch)
}) |> list_rbind()
crs_tab <- summarise_channel(crs)

# ---- IHME DAH ---------------------------------------------------------------------

ihme_files <- list.files(ihme_dir, "\\.csv$", ignore.case = TRUE, full.names = TRUE)
ihme_files <- ihme_files[!grepl("codebook", ihme_files, ignore.case = TRUE)]
if (!length(ihme_files)) stop("No IHME CSV files found in ", ihme_dir)
value_col <- grep("^dah_\\d{2}$", names(read_csv(ihme_files[1], n_max = 0, show_col_types = FALSE)),
                  value = TRUE)
if (length(value_col) != 1) stop("Could not find the total DAH column (dah_YY)")
if (value_col != sprintf("dah_%02d", yr %% 100))
  warning("IHME values are in constant ", 2000 + as.integer(sub("dah_", "", value_col)),
          " dollars, not ", yr, " dollars")

ihme_all <- map(ihme_files, \(f) read_csv(f, show_col_types = FALSE,
  col_select = all_of(c("year", "source", "channel", "recipient_isocode", "elim_ch", value_col)),
  col_types = cols(.default = "c"))) |>
  list_rbind() |>
  filter(as.integer(year) == yr, coalesce(as.numeric(elim_ch), 0) != 1) |>   # drop double counting
  mutate(usd_m = as.numeric(.data[[value_col]]) / 1e3)                        # thousands -> millions

ihme <- imap(ihme_rules, \(rule, ch) {
  d <- filter(ihme_all, channel %in% rule$channel)
  if (!is.null(rule$source)) d <- filter(d, source %in% rule$source)
  transmute(d, channel = ch, iso3 = recipient_isocode, usd_m)
}) |> list_rbind()
ihme_tab <- summarise_channel(ihme)

# ---- Table --------------------------------------------------------------------------

out <- bind_rows(IATI = iati_tab, IHME = ihme_tab, CRS = crs_tab, .id = "source") |>
  right_join(expand_grid(source = c("IATI", "IHME", "CRS"), channel = channels),
             by = c("source", "channel")) |>
  mutate(source = factor(source, c("IATI", "IHME", "CRS")),
         channel = factor(channel, channels),
         share = round(100 * lic / total, 1),
         lic = round(lic, 1), total = round(total, 1)) |>
  arrange(source, channel) |>
  transmute(source, channel,
            `Disbursements to LICs, 2023 (USD m)` = lic,
            `Total country-allocable disbursements, 2023 (USD m)` = total,
            `Share to LICs (%)` = share)

write_csv(out, "data/processed/lic_share_2023.csv")
message("Low-income classification: World Bank ", income_fy,
        ". Saved data/processed/lic_share_2023.csv")
if (requireNamespace("knitr", quietly = TRUE)) {
  print(knitr::kable(out, format = "simple", format.args = list(big.mark = ",")))
} else {
  print(out, n = Inf, width = Inf)
}
