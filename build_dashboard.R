# Build the GitHub Pages dashboard data from the IATI download
# -----------------------------------------------------------------------------
# Run iati_dportal.R (or iati_health_multilaterals.R) first. This script reads its processed output,
# keeps disbursements in USD, and writes docs/data.js, which docs/index.html reads.
#
#   install.packages(c("jsonlite", "readxl", "WDI", "tidyr", "purrr"))
#
# Folder layout (repository root):
#   iati_health_multilaterals.R
#   build_dashboard.R
#   OGHIST_2026_07_15.xlsx
#   WEOApr2026all.xlsx   <- IMF World Economic Outlook, April 2026 (all countries)
#   data/processed/iati_tx_gf_ida_gavi_since2015.csv
#   data/processed/lmic_countries_fy27.csv
#   docs/index.html      <- dashboard page (commit once)
#   docs/data.js         <- written by this script (commit after each rebuild)

library(dplyr)
library(readr)
library(jsonlite)
library(readxl)
library(WDI)
library(tidyr)
library(purrr)

start_year <- 2015

# Health sectors (OECD DAC purpose codes, matched on the first three digits):
#   121 Health, general   122 Basic health (incl. malaria, TB, COVID-19)
#   123 Non-communicable diseases   130 Population policies and reproductive
#   health (incl. STD/HIV control). Remove "130" for a narrower definition.
health_sectors        <- c("121", "122", "123", "130")
health_filter_funders <- c("World Bank IDA")   # funders limited to health sectors
weo_file   <- "WEOApr2026all.xlsx"
weo_label  <- "IMF World Economic Outlook, April 2026"
weo_to     <- 2028   # show IMF projections up to this year

# ---- Inputs -------------------------------------------------------------------

tx <- read_csv("data/processed/iati_tx_gf_ida_gavi_since2015.csv",
               col_types = cols(.default = "c"))

country_file <- sort(list.files("data/processed", "^lmic_countries_fy\\d+\\.csv$",
                                full.names = TRUE)) |> tail(1)
countries <- read_csv(country_file, col_types = cols(.default = "c"))
fy_label  <- toupper(sub(".*_(fy\\d+)\\.csv$", "\\1", country_file))

# Columns that may be absent if the API returned nothing for them
for (col in c("transaction_value_usd", "transaction_value_currency",
              "default_currency", "title_narrative", "multi_country", "data_source")) {
  if (!col %in% names(tx)) tx[[col]] <- NA_character_
}

# ---- Disbursements in USD -----------------------------------------------------

disb <- tx |>
  filter(transaction_transaction_type_code == "3") |>
  mutate(
    value     = suppressWarnings(as.numeric(transaction_value)),
    value_usd = suppressWarnings(as.numeric(transaction_value_usd)),
    currency  = coalesce(na_if(transaction_value_currency, ""), default_currency),
    usd       = case_when(!is.na(value_usd) ~ value_usd,
                          currency == "USD" ~ value,
                          TRUE ~ NA_real_),
    year          = as.integer(substr(transaction_transaction_date_iso_date, 1, 4)),
    multi_country = multi_country %in% c("TRUE", "true", "1"),
    title         = coalesce(na_if(title_narrative, ""), iati_identifier)
  ) |>
  filter(year >= start_year)

n_non_usd <- sum(is.na(disb$usd))
n_multi   <- sum(disb$multi_country & !is.na(disb$usd))
message("Disbursements: ", nrow(disb),
        " | dropped (no USD value): ", n_non_usd,
        " | excluded (multi-country): ", n_multi)

disb <- disb |>
  filter(!is.na(usd), !multi_country, !is.na(income_group))

# ---- Keep only health-sector disbursements for the funders listed above ------
# d-portal splits each transaction across its sectors by percentage, so a
# multi-sector project keeps only its health share.

if (!"sector_code" %in% names(disb)) stop("No sector_code column; rerun iati_dportal.R")

disb <- disb |> mutate(health = substr(sector_code, 1, 3) %in% health_sectors)

sector_check <- disb |>
  group_by(funder) |>
  summarise(total_bn       = round(sum(usd) / 1e9, 1),
            health_share   = round(100 * sum(usd[health]) / sum(usd), 1),
            no_sector_share = round(100 * sum(usd[is.na(sector_code)]) / sum(usd), 1),
            .groups = "drop")
print(sector_check)

ida_health <- sector_check$health_share[sector_check$funder %in% health_filter_funders]
if (length(ida_health) && any(ida_health == 0)) {
  stop("No health-sector disbursements found for ", paste(health_filter_funders, collapse = ", "),
       "; check the sector codes (sector_code column) before filtering")
}

disb <- disb |> filter(!funder %in% health_filter_funders | health)

# ---- Tab 1: funder x income group x year ---------------------------------------

income <- disb |>
  group_by(funder, income_group, year) |>
  summarise(usd = round(sum(usd)), .groups = "drop") |>
  arrange(funder, income_group, year)

# ---- Tab 2: country x funder x activity x year ---------------------------------

country_rows <- disb |>
  group_by(country_iso2, funder, iati_identifier, year) |>
  summarise(usd = round(sum(usd)), .groups = "drop") |>
  arrange(country_iso2, funder, iati_identifier, year)

activities <- disb |>
  distinct(iati_identifier, title) |>
  group_by(iati_identifier) |>
  slice(1) |>
  ungroup()

country_meta <- countries |>
  filter(iso2 %in% country_rows$country_iso2) |>
  select(iso2, country, income_group) |>
  arrange(country)


# ---- Country context: economy, population, health spending ------------------
# IMF WEO (economy, with projections) and World Bank WDI (population, health
# outcomes, and WHO Global Health Expenditure Database figures).

ctx_meta <- tribble(
  ~key,         ~label,                                   ~fmt,       ~group,            ~source,
  "population", "Population",                             "people",   "Population",      "wdi",
  "life_exp",   "Life expectancy at birth",               "years",    "Population",      "wdi",
  "u5mr",       "Under-5 mortality, per 1,000 births",    "rate",     "Population",      "wdi",
  "gdp_pc",     "GDP per capita",                         "usd",      "Economy",         "weo",
  "gdp_growth", "Real GDP growth",                        "pct",      "Economy",         "weo",
  "inflation",  "Inflation, consumer prices",             "pct",      "Economy",         "weo",
  "gov_rev",    "Government revenue",                     "pct_gdp",  "Economy",         "weo",
  "gov_exp",    "Government expenditure",                 "pct_gdp",  "Economy",         "weo",
  "gov_debt",   "Government gross debt",                  "pct_gdp",  "Economy",         "weo",
  "che_pc",     "Health spending per person",             "usd",      "Health spending", "wdi",
  "che_gdp",    "Health spending",                        "pct_gdp",  "Health spending", "wdi",
  "gghe_ge",    "Health share of government spending",    "pct",      "Health spending", "wdi",
  "ext_share",  "External share of health spending",      "pct",      "Health spending", "wdi",
  "oop_share",  "Out-of-pocket share of health spending", "pct",      "Health spending", "wdi",
  # Used for the health-financing chart rather than the indicator list
  "gghe_pc",    "Government (domestic)",                  "usd",      "chart",           "wdi",
  "pvt_pc",     "Private (domestic)",                     "usd",      "chart",           "wdi",
  "ext_pc",     "External",                               "usd",      "chart",           "wdi"
)

weo_codes <- c(gdp_pc = "NGDPDPC", gdp_growth = "NGDP_RPCH", inflation = "PCPIPCH",
               gov_rev = "GGR_NGDP", gov_exp = "GGX_NGDP", gov_debt = "GGXWDG_NGDP")

wdi_codes <- c(population = "SP.POP.TOTL",       life_exp  = "SP.DYN.LE00.IN",
               u5mr       = "SH.DYN.MORT",       che_pc    = "SH.XPD.CHEX.PC.CD",
               che_gdp    = "SH.XPD.CHEX.GD.ZS", gghe_ge   = "SH.XPD.GHED.GE.ZS",
               ext_share  = "SH.XPD.EHEX.CH.ZS", oop_share = "SH.XPD.OOPC.CH.ZS",
               gghe_pc    = "SH.XPD.GHED.PC.CD", pvt_pc    = "SH.XPD.PVTD.PC.CD",
               ext_pc     = "SH.XPD.EHEX.PC.CD")

# IMF WEO: one row per country x indicator, one column per year.
# WEO uses its own codes for two economies; Cuba and North Korea are not covered.
weo_iso <- c(KOS = "XKX", WBG = "PSE")

weo <- read_excel(weo_file, sheet = "Countries", col_types = "text") |>
  filter(`INDICATOR.ID` %in% weo_codes) |>
  mutate(
    iso3 = coalesce(unname(weo_iso[`COUNTRY.ID`]), `COUNTRY.ID`),
    key  = names(weo_codes)[match(`INDICATOR.ID`, weo_codes)],
    # "2024" or "FY2024/25": years after this are IMF estimates or projections
    last_actual = as.integer(substr(gsub("\\D", "", LATEST_ACTUAL_ANNUAL_DATA), 1, 4))
  ) |>
  filter(iso3 %in% countries$iso3) |>
  select(iso3, key, last_actual, matches("^\\d{4}$")) |>
  pivot_longer(matches("^\\d{4}$"), names_to = "year", values_to = "value") |>
  mutate(year = as.integer(year), value = suppressWarnings(as.numeric(value)),
         projected = !is.na(last_actual) & year > last_actual) |>
  filter(between(year, start_year, weo_to), !is.na(value)) |>
  select(iso3, key, year, value, projected)

# World Bank WDI, cached in data/raw so rebuilding the page needs no download
wdi_cache <- "data/raw/wdi_context.csv"
if (file.exists(wdi_cache)) {
  wdi <- read_csv(wdi_cache, col_types = cols(iso3 = "c", key = "c", year = "i", value = "d"))
} else {
  wdi <- imap(wdi_codes, \(code, key) {
    message("WDI: ", code)
    tryCatch(
      WDI(country = "all", indicator = code, start = start_year,
          end = as.integer(format(Sys.Date(), "%Y"))) |>
        transmute(iso3 = iso3c, key = key, year = as.integer(year), value = .data[[code]]),
      error = \(e) { warning("WDI ", code, " failed: ", conditionMessage(e)); NULL })
  }) |>
    list_rbind() |>
    filter(iso3 %in% countries$iso3, !is.na(value))
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
  write_csv(wdi, wdi_cache)
}

context <- bind_rows(weo, mutate(wdi, projected = FALSE)) |>
  inner_join(select(countries, iso3, iso2), by = "iso3") |>
  filter(iso2 %in% country_meta$iso2) |>
  mutate(value = signif(value, 4)) |>
  arrange(iso2, key, year)

# Compact form for the page: {iso2: {key: [[year, value, projected], ...]}}
ctx_values <- context |>
  split(~iso2) |>
  map(\(d) split(d, d$key) |>
        map(\(x) unname(pmap(list(x$year, x$value, as.integer(x$projected)), c))))

message("Context: ", n_distinct(context$iso2), " countries; latest health spending year ",
        max(context$year[context$key == "che_pc"]), "; WEO coverage ",
        n_distinct(weo$iso3), " countries")

# ---- Write docs/data.js ---------------------------------------------------------

dash <- list(
  meta = list(
    built          = format(Sys.Date()),
    source         = if (any(tx$data_source == "d-portal", na.rm = TRUE)) "d-portal" else "datastore",
    classification = fy_label,
    start_year     = start_year,
    end_year       = max(disb$year),
    dropped_non_usd = n_non_usd,
    excluded_multi  = n_multi,
    health_sectors  = health_sectors,
    # Display names: funders filtered to health sectors are labelled as such
    funder_labels   = as.list(setNames(
      ifelse(c("Global Fund", "World Bank IDA", "Gavi") %in% health_filter_funders,
             paste(c("Global Fund", "World Bank IDA", "Gavi"), "(health)"),
             c("Global Fund", "World Bank IDA", "Gavi")),
      c("Global Fund", "World Bank IDA", "Gavi")))
  ),
  funders      = c("Global Fund", "World Bank IDA", "Gavi"),
  income_groups = c("Low income", "Lower middle income", "Upper middle income"),
  income       = income,                     # [funder, income_group, year, usd]
  countries    = country_meta,               # [iso2, country, income_group]
  rows         = country_rows,               # [iso2, funder, activity_id, year, usd]
  activities   = as.list(setNames(activities$title, activities$iati_identifier)),
  context      = list(
    sources = list(weo = weo_label,
                   wdi = "World Bank World Development Indicators (health spending: WHO Global Health Expenditure Database)"),
    indicators = ctx_meta,                   # [key, label, fmt, group, source]
    values     = ctx_values
  )
)

dir.create("docs", showWarnings = FALSE)
writeLines(
  paste0("window.DASH = ",
         toJSON(dash, dataframe = "values", auto_unbox = TRUE,
                na = "null", digits = NA),
         ";"),
  "docs/data.js", useBytes = TRUE
)

message("Wrote docs/data.js (", round(file.size("docs/data.js") / 1e6, 1), " MB): ",
        nrow(country_meta), " countries, ", nrow(activities), " activities")
