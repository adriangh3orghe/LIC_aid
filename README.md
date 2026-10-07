# Health multilateral disbursements dashboard

Interactive dashboard of disbursements from the **Global Fund**, **World Bank IDA (health sectors)** and **Gavi** to low and middle income countries since 2015, built from IATI data.

**Live dashboard:** [https://adriangh3orghe.github.io/LIC_aid/#income]

## What it shows

- **By income group:** annual disbursements from each funder to low, lower-middle and upper-middle income countries.
- **By country:** disbursements to a selected country by funder, stacked by activity, with a table of activities linked to their IATI records, plus country context: population, economic indicators and health spending by source (government, private, external).

## Data sources

| Data | Source |
|---|---|
| Disbursements | IATI data via [d-portal](https://d-portal.org), values in US dollars at IMF exchange rates |
| Income groups | World Bank historical income classifications (OGHIST), latest year |
| Economy | IMF World Economic Outlook (GDP, growth, inflation, government finances) |
| Population and health spending | World Bank World Development Indicators, health spending from the WHO Global Health Expenditure Database |

## Method notes

- Disbursements only (IATI transaction type 3), calendar years, from 2015.
- World Bank flows are limited to IDA (ODA flows, flow type 10) and to health sectors (OECD DAC codes 121, 122, 123, 130). Multi-sector projects count only their health share.
- d-portal splits multi-country transactions by recipient-country percentage.
- Countries keep their latest income classification for every year, not the classification at the time of disbursement.
- The most recent year is partial, and health spending data lag by two to three years.

## Repository structure

```
iati_dportal.R       Download IATI disbursements from d-portal (no API key needed)
build_dashboard.R    Aggregate data, add country indicators, write docs/data.js
refresh.R            Full update: clear cache, download, rebuild, push
docs/index.html      Dashboard page (served by GitHub Pages)
docs/data.js         Dashboard data (generated)
```

Input files not stored in the repository: `OGHIST_*.xlsx` (World Bank) and `WEO*.xlsx` (IMF), placed in the project folder. Downloads are cached in `data/` (git-ignored).

## Updating

Every three to six months, in R from the project folder:

```r
source("refresh.R")
```

This takes one to two hours, mostly the d-portal download. Before running it, check for a newer OGHIST file (published each July) or WEO release (April and October), and update the file names at the top of the scripts.

R packages: `httr2`, `readxl`, `countrycode`, `dplyr`, `purrr`, `readr`, `tidyr`, `jsonlite`, `WDI`, `gert`.
