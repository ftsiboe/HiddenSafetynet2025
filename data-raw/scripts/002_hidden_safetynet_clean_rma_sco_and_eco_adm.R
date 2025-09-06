
rm(list=ls(all=TRUE));gc()

devtools::document()

study_env <- setup_environment()

plan(list(tweak(multisession, workers = 4)))

data <- data.table::rbindlist(
  lapply(
    study_env$year_beg:study_env$year_end,
    function(crop_yr){
      # crop_yr <- 2022

      admfull <- data.table::rbindlist(
        lapply(
          c("Supplemental_sco","Supplemental_eco","Area"),
          function(plan){
            tryCatch({
              # plan <- "Supplemental_sco"
              adm <- tempfile(fileext = ".rds")

              download.file(
                paste0("https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/adm_compressed/",
                       crop_yr,"_",plan,"_plans_adm.rds"),
                adm, mode = "wb", quiet = TRUE)

              adm <- readRDS(adm)

              parameter_list <-  names(adm)[
                names(adm) %in% c("base_rate","price_volatility_factor","payment_factor",
                                  "expected_county_yield","final_county_yield",
                                  "projected_price","harvest_price","catastrophic_price",
                                  "expected_county_revenue","final_county_revenue" )]

              aggregation_point <-  names(adm)[
                names(adm) %in% c("commodity_year", FCIP_INSURANCE_POOL, FCIP_INSURANCE_ELECTION,
                                  "area_loss_start_percent","area_loss_end_percent")]

              adm <- adm[
                , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
                by = aggregation_point,.SDcols = parameter_list]

              return(adm)
            }, error = function(e){return(NULL)})
          }), fill = TRUE)

      ayp <- admfull[insurance_plan_code %in% c(4:6)]
      ayp <- ayp[!base_rate %in% c(0,NA,NaN,Inf,-Inf)]
      ayp <- ayp[,.(base_rate = mean(base_rate,na.rm=T)),
                 by = c("commodity_year", FCIP_INSURANCE_POOL, "insurance_plan_code", "coverage_level_percent")];gc()
      ayp <- as.data.frame(ayp)
      ayp$coverage_level_percent <- paste0("ayp",round(ayp$coverage_level_percent*100))
      ayp <- ayp |> tidyr::spread(coverage_level_percent, base_rate)
      ayp$insurance_plan_code <- ayp$insurance_plan_code + 27

      ayp <- dplyr::full_join(admfull[insurance_plan_code %in% 31:33],ayp)
      ayp <- ayp[!base_rate %in% c(0,NA,NaN,Inf,-Inf)]
      ayp <- ayp[insurance_plan_code %in% 31:33]

      # Increase SCO’s coverage level to 88%
      sco88 <- copy(ayp)[,base_rate_ayp := base_rate + (ayp90 - ayp85)*((88-86)/(90-85))]

      # Increase the coverage level of SCO to 90%.
      sco90 <- copy(ayp)[,base_rate_ayp := base_rate + (ayp90 - ayp85)*((90-86)/(90-85))]

      rm(ayp);gc()

      if(crop_yr>=2021){

        eco90 <- admfull[(insurance_plan_code %in% 87:89 & round(coverage_level_percent*100) %in% 90)
                         , c("commodity_year", FCIP_INSURANCE_POOL, "insurance_plan_code","base_rate"), with = FALSE]
        setnames(eco90,old = c("base_rate"),new = c("eco90"))
        eco90[,insurance_plan_code := insurance_plan_code - 56]

        sco85 <- admfull[(insurance_plan_code %in% 31:33 & round(coverage_level_percent*100) %in% 85)
                         , c("commodity_year", FCIP_INSURANCE_POOL, "insurance_plan_code","base_rate"), with = FALSE]
        setnames(sco85,old = c("base_rate"),new = c("sco85"))

        # Increase SCO’s coverage level to 88%
        sco88 <- dplyr::full_join(sco88,eco90)
        sco88 <- sco88[base_rate %in% c(0,NA,NaN,Inf,-Inf)]
        sco88 <- dplyr::full_join(sco88,sco85)
        sco88 <- sco88[base_rate %in% c(0,NA,NaN,Inf,-Inf)]
        sco88[,base_rate_eco := base_rate + (eco90 - sco85)*((88-86)/(90-85))]

        # Increase the coverage level of SCO to 90%.
        sco90 <- dplyr::full_join(sco90,eco90)
        sco90 <- sco90[!base_rate %in% c(0,NA,NaN,Inf,-Inf)]
        sco90 <- dplyr::full_join(sco90,sco85)
        sco90 <- sco90[!base_rate %in% c(0,NA,NaN,Inf,-Inf)]
        sco90[,base_rate_eco := base_rate + (eco90 - sco85)*((88-86)/(90-85))]

        rm(eco90,sco85);gc()
      }

      sco88[,insurance_plan_code := insurance_plan_code + 10]
      sco90[,insurance_plan_code := insurance_plan_code + 20]
      sco88[, base_rate := rowMeans(.SD, na.rm = TRUE),.SDcols = patterns("^base_rate_")]
      sco90[, base_rate := rowMeans(.SD, na.rm = TRUE),.SDcols = patterns("^base_rate_")]

      admfull <- data.table::rbindlist(list(admfull[insurance_plan_code %in% c(31:33,35:36,87:89)],sco88,sco90), fill = TRUE)

      rm(sco90,sco88);gc()

      admfull <- admfull[!base_rate %in% c(0,NA,NaN,Inf,-Inf)]

      # table(admfull$insurance_plan_code)

      return(admfull)}), fill = TRUE)

plan(sequential)

saveRDS(data,file ="data-raw/data/cleaned_rma_sco_and_eco_adm.rds")

