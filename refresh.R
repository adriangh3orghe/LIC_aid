# Refresh the dashboard: new IATI download, rebuild, publish to GitHub Pages
# -----------------------------------------------------------------------------
# Run every three to six months:   source("refresh.R")
# Takes one to two hours, mostly the d-portal download. Keep the computer awake.
#
# Before running, check for newer input files and update the names if needed:
#   - World Bank income classifications (OGHIST), published each July
#       -> oghist_file at the top of iati_dportal.R
#   - IMF World Economic Outlook, published each April and October
#       -> weo_file and weo_label at the top of build_dashboard.R

started <- Sys.time()

# 1. Clear cached downloads so everything is fetched fresh
if (interactive()) {
  ok <- readline("Delete data/raw and download all IATI and WDI data again? (y/n) ")
  if (!tolower(ok) %in% c("y", "yes")) stop("Refresh cancelled")
}
unlink("data/raw", recursive = TRUE)

# 2. Download IATI disbursements from d-portal
source("iati_dportal.R")

# 3. Rebuild the dashboard data (also downloads fresh WDI indicators)
source("build_dashboard.R")

# 4. Publish: commit and push the new data to GitHub
gert::git_add(c("docs/data.js", "docs/index.html"))
if (nrow(subset(gert::git_status(), staged))) {
  gert::git_commit(paste("Refresh dashboard data", format(Sys.Date())))
  gert::git_push()
  message("Pushed. The live dashboard updates within a few minutes.")
} else {
  message("No changes to publish.")
}

message("Refresh finished in ",
        round(as.numeric(difftime(Sys.time(), started, units = "mins"))), " minutes")
