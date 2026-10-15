# Health multilateral disbursements dashboard

Interactive dashboard of disbursements from the **Global Fund**, **World Bank IDA (health sectors)** and **Gavi** to low and middle income countries since 2015, built from IATI data.

**Live dashboard:** [https://adriangh3orghe.github.io/LIC_aid/#income]

## What it shows

- **Health aid by data source (top of both tabs):** total health aid disbursements to each income group and country as recorded by IATI, the OECD Creditor Reporting System (CRS) and IHME's Development Assistance for Health (DAH) database, in the same constant US dollars, with WHO external health spending as an independent reference. The income-group tab scores how closely each source agrees with the others and with WHO, to identify the most reliable source for country and income-group comparisons, particularly for low-income countries.
- **By income group:** annual disbursements from each funder to low, lower-middle and upper-middle income countries.
- **By country:** disbursements to a selected country by funder, stacked by activity, with a table of activities linked to their IATI records, plus country context: population, economic indicators and health spending by source (government, private, external).

## Data sources

| Data | Source |
|---|---|
| Disbursements | IATI data via [d-portal](https://d-portal.org), values in US dollars at IMF exchange rates |
| Income groups | World Bank historical income classifications (OGHIST), latest year |
| Economy | IMF World Economic Outlook (GDP, growth, inflation, government finances) |
| Population and health spending | World Bank World Development Indicators, health spending from the WHO Global Health Expenditure Database |
| Health aid, all funders (comparison) | IATI via d-portal (all publishers); OECD CRS bulk file (`CRS.parquet`); IHME DAH database 1990–2026 |
| Price deflator (comparison) | US GDP deflator, IMF World Economic Outlook |

## Method notes

- Disbursements only (IATI transaction type 3), calendar years, from 2015.
- World Bank flows are limited to IDA (ODA flows, flow type 10) and to health sectors (OECD DAC codes 121, 122, 123, 130). Multi-sector projects count only their health share.
- d-portal splits multi-country transactions by recipient-country percentage.
- Countries keep their latest income classification for every year, not the classification at the time of disbursement.
- The most recent year is partial, and health spending data lag by two to three years.
- Data-source comparison: health means DAC sectors 121, 122, 123 and 130 (IATI and CRS) and IHME's total DAH. CRS counts ODA and private development finance, not other official flows; IHME leaves out channel-to-channel transfers (double counting) and IBRD. All values are converted to constant US dollars of IHME's price year (2023 in the 2026 release) with the US GDP deflator. Agreement is measured over the years all three sources cover with final data.

## Repository structure

```
iati_dportal.R       Download IATI disbursements from d-portal (no API key needed)
build_dashboard.R    Aggregate data, add country indicators, write docs/data.js
compare_sources.R    Compare total health aid in IATI, CRS and IHME, write docs/sources.js
refresh.R            Full update: clear cache, download, rebuild, push
docs/index.html      Dashboard page (served by GitHub Pages)
docs/data.js         Dashboard data (generated)
docs/sources.js      Data-source comparison (generated)
```

Input files not stored in the repository: `OGHIST_*.xlsx` (World Bank) and `WEO*.xlsx` (IMF), placed in the project folder; `CRS.parquet` (OECD) and the `IHME_DAH_DATABASE_1990-2026` folder (IHME), in your Downloads folder. Downloads are cached in `data/` (git-ignored).

## Updating

Every three to six months, in R from the project folder:

```r
source("refresh.R")
```

This takes one to two hours, mostly the d-portal download. Before running it, check for a newer OGHIST file (published each July) or WEO release (April and October), and update the file names at the top of the scripts.

R packages: `arrow`, `httr2`, `readxl`, `countrycode`, `dplyr`, `purrr`, `readr`, `tidyr`, `jsonlite`, `WDI`, `gert`.
