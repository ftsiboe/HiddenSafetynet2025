#' Clean and enrich RMA Summary of Business (SOB) data
#' @description
#' Processes RMA Summary of Business (SOB) data to produce an analysis-ready
#' dataset with aggregated core insurance metrics and **shares** of Supplemental
#' Coverage Option (SCO) and Enhanced Coverage Option (ECO) by coverage level.
#'
#' @param study_env A list-like environment produced by [setup_environment()] that must
#'   include `year_beg` and `year_end` (inclusive integers). Defaults to `setup_environment()`.
#' @param output_directory Character string specifying the directory where the processed
#'   `.rds` file should be saved. Defaults to `"data-raw/data"`. The file will be
#'   named `"cleaned_rma_sobtpu.rds"`.
#'
#' @return A character message describing the processed year range and number of
#'   output rows; the main side effect is writing an `.rds` file to disk.
#'
#' @import data.table
#' @importFrom stats na.omit
#' @importFrom utils write.table
#' @importFrom rfcip get_sob_data
#'
#' @details
#' The output file will be written to `file.path(output_directory, "cleaned_rma_sobtpu.rds")`.
#' The directory is created if it does not exist.
#' @export
clean_rma_sobtpu <- function(study_env = setup_environment(), output_directory = "data-raw/data") {
  # Pull Summary of Business (SOB) data for study year
  sob <- get_sob_data(sob_version = "sobtpu", year = study_env$year_beg:study_env$year_end)

  # Keep only desired insurance plan codes (base plans 1-3 and several endorsements)
  sob <- sob[sob$insurance_plan_code %in% c(1:3, 31:33, 35:36, 87:89, 90), ]

  # Exclude CAT coverage ('C') and keep only records reported at the Acres level
  sob <- sob[!sob$coverage_type_code %in% "C", ]
  sob <- sob[sob$reporting_level_type %in% "Acres", ]

  # Switch to data.table for fast aggregations/joins
  setDT(sob)

  # Treat plan code 90 as plan 1 (re-map in place)
  sob[insurance_plan_code %in% 90, insurance_plan_code := 1]

  # ===== Base data aggregation =====
  data <- sob[
    insurance_plan_code %in% 1:3,
    .(
      insured_acres        = sum(net_reporting_level_amount,   na.rm = TRUE),
      liability_amount     = sum(liability_amount,             na.rm = TRUE),
      total_premium_amount = sum(total_premium_amount,         na.rm = TRUE),
      subsidy_amount       = sum(subsidy_amount,               na.rm = TRUE),
      indemnity_amount     = sum(indemnity_amount,             na.rm = TRUE)
    ),
    by = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
           "unit_structure_code", "insurance_plan_code", "coverage_type_code", "coverage_level_percent")
  ][
    !insured_acres %in% c(0, NA, NaN, Inf, -Inf)
  ]

  # SCO share by coverage level
  sco_data <- sob[
    insurance_plan_code %in% c(31:33),
    .(sco = sum(endorsed_commodity_reporting_level_amount, na.rm = TRUE)),
    by = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
           "insurance_plan_code", "coverage_level_percent")
  ][
    , insurance_plan_code := insurance_plan_code - 30
  ][
    data[, .(insured_acres = sum(insured_acres, na.rm = TRUE)),
         by = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
                "insurance_plan_code", "coverage_level_percent")],
    on = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
           "insurance_plan_code", "coverage_level_percent"),
    nomatch = 0
  ][
    , sco := sco / insured_acres
  ]

  sco_data <- unique(
    sco_data[, c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
                 "insurance_plan_code", "coverage_level_percent", "sco"), with = FALSE]
  )
  data <- merge(data, sco_data, by = intersect(names(data), names(sco_data)), all = TRUE)

  # ECO shares
  eco_data <- sob[
    insurance_plan_code %in% c(87:89),
    .(eco = sum(endorsed_commodity_reporting_level_amount, na.rm = TRUE)),
    by = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
           "insurance_plan_code", "coverage_level_percent")
  ][
    , insurance_plan_code := insurance_plan_code - 86
  ][
    , coverage_level_percent := paste0("eco", round(coverage_level_percent * 100))
  ][
    !eco %in% c(0, NA, NaN, Inf, -Inf)
  ] |> tidyr::spread(coverage_level_percent, eco)

  eco_data <- as.data.table(eco_data)[
    data[, .(insured_acres = sum(insured_acres, na.rm = TRUE)),
         by = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code", "insurance_plan_code")],
    on = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code", "insurance_plan_code"),
    nomatch = 0
  ][
    , eco90 := eco90 / insured_acres
  ][
    , eco95 := eco95 / insured_acres
  ]

  eco_data <- unique(
    eco_data[, c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
                 "insurance_plan_code", "eco90", "eco95"), with = FALSE]
  )
  data <- merge(data, eco_data, by = intersect(names(data), names(eco_data)), all = TRUE)

  # Clean SCO/ECO shares
  data <- as.data.frame(data)
  for (xx in c("sco", "eco90", "eco95")) {
    data[, xx] <- ifelse(data[, xx] %in% c(0, NA, NaN, Inf, -Inf), 0, data[, xx])
    data[, xx] <- ifelse(data[, xx] > 1, 1, data[, xx])
  }
  data <- data[!data$insured_acres %in% c(0, NA, NaN, Inf, -Inf), ]

  # Save to user-specified directory
  if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)
  save_path <- file.path(output_directory, "cleaned_rma_sobtpu.rds")
  saveRDS(as.data.table(data), file = save_path)

  return(sprintf(
    "Finished processing RMA SOB data for %s-%s (rows = %s). Saved to: %s",
    min(data$commodity_year), max(data$commodity_year), format(nrow(data), big.mark = ","), save_path
  ))
}




