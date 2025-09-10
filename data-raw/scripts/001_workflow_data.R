# Hard reset of workspace
rm(list = ls(all = TRUE)); gc()

# Load required packages (assumes they’re installed & on library path)
library(data.table); library(rfcip)

# Initialize environment
devtools::document()
study_env <- setup_environment()
Keep.List<-c("Keep.List",ls())

# Clean and enrich RMA Summary of Business (SOB) data
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
sob <- clean_rma_sobtpu(
  years = 2015:2024,
  insurance_plan = c(1:3, 31:33, 35:36, 87:89, 90),
  acres_only = TRUE,
  addon_only = TRUE,
  harmonize_insurance_plan_code = TRUE,
  harmonize_coverage_level_percent = FALSE,
  harmonize_unit_structure_code = FALSE)

sob <- clean_supplemental_plan_shares(sob)
saveRDS(sob,file ="data/cleaned_rma_sobtpu.rds")


# Build SCO/ECO/Area ADM table (adds SCO88/SCO90)
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
data <- data.table::rbindlist(
  lapply(
    study_env$year_beg:study_env$year_end,
    clean_rma_sco_and_eco_adm
  ),
  fill = TRUE
)
saveRDS(data,file ="data/cleaned_rma_sco_and_eco_adm.rds")

# Clean agent-level data
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
lapply(
  study_env$year_beg:study_env$year_end,
  clean_agents_data
)

# Build panel of supplemental insurance availability (offering) and adoption (acres)
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
build_supplemental_offering_and_adoption(
    cleaned_rma_sobtpu_file_path = "data/cleaned_rma_sobtpu.rds",
    output_directory = "data")

