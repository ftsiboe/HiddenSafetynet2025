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
    producer_id sco subsidy_amount total_premium_amount avail_aph avail_eco90 avail_eco95 avail_sco commodity_code
    county_code county_fips plan state_code ..keep Coverage_Range ENDOS_Indemnity ENDOS_Payment_Factor
     ENDOS_Producer_Premium ENDOS_Reduction_rate ENDOS_Subsidy_amount
     ENDOS_Subsidy_factor ENDOS_Total_Premium ENDOS_protection
     END_coverage_level_percent New_ENDOS_protection
     New_Expected_Crop_Value Subsidy_factor Trigger actual_farm_yield
     actual_price approved_yield combination determined_acreage
     expected_county_value expected_county_value_implied
     expected_county_yield final_county_value final_county_yield
     guaranteed_yield harvest_price indemnity insurance_guarantee
     liability new_insurance_guarantee practice_code price_election_amount
     price_risk producer_premium projected_price revenue revenue_to_count
     rma_draw_number rma_draw_price_farm rma_draw_price_pool
     rma_draw_yield_farm rma_draw_yield_pool study_env sup total_premium
     type_code unit_structure_code weighted.mean",
        "\\s+"
      )[[1]]
    )
  }
}




