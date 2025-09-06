# 0) Hard reset of workspace
rm(list = ls(all = TRUE)); gc()

# 1) Load required packages (assumes they’re installed & on library path)
library(data.table); library(rfcip)

# 2) Clean generated artifacts
unlink(c(
  "NAMESPACE",
  #list.files("./data", full.names = TRUE),
  list.files("./man",  full.names = TRUE)
))

# 3) Rebuild documentation from roxygen comments
devtools::document()

# 4) Sanity pass through R/ sources: shows any non-ASCII characters per file
for (i in list.files("R", full.names = TRUE)) {
  print(paste0("********************", i, "********************"))
  tools::showNonASCIIfile(i)
}

# 5) Check man pages only (faster than full devtools::check)
devtools::check_man()

# 6) Build PDF manual into the current working directory
devtools::build_manual(path = getwd())

# 7) Optional: run tests / full package check (uncomment when needed)
# devtools::test()
# devtools::check()

# 8) Initialize environment
study_env <- setup_environment()
Keep.List<-c("Keep.List",ls())

# 9) Clean and enrich RMA Summary of Business (SOB) data
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
clean_rma_sobtpu()

# 10) Build SCO/ECO/Area ADM table (adds SCO88/SCO90)
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
data <- data.table::rbindlist(
  lapply(
    study_env$year_beg:study_env$year_end,
    clean_rma_sco_and_eco_adm
  ),
  fill = TRUE
)
saveRDS(data,file ="data/cleaned_rma_sco_and_eco_adm.rds")

# 11) Clean agent-level data
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
lapply(
  study_env$year_beg:study_env$year_end,
  clean_agents_data
)

# 12) Build panel of supplemental insurance availability (offering) and adoption (acres)
build_supplemental_offering_and_adoption(
    cleaned_rma_sobtpu_file_path = "data/cleaned_rma_sobtpu.rds",
    output_directory = "data")

