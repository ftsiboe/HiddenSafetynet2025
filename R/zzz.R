.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 360000)

  options(future.globals.maxSize = 20 * 1024^3)  # 20 GiB

  # Register global variables used by data.table (silence R CMD check NOTES)
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      strsplit(
        ". ayp85 ayp90 base_rate base_rate_ayp base_rate_eco commodity_year
    coverage_level_percent damage_area_rate dmage_are_rate eco eco90
    eco95 endorsed_commodity_reporting_level_amount indemnity_amount
    insurance_plan_code insured_acres insured_share liability_amount
    net_reporting_level_amount observed_premium_rate
    observed_subsidy_percent patterns planted_acres price_election
    producer_id sco subsidy_amount total_premium_amount",
        "\\s+"
      )[[1]]
    )
  }
}




