
rm(list=ls(all=TRUE));gc()

devtools::document()

study_env <- setup_environment()

plan(list(tweak(multisession, workers = 4)))

future_lapply(
  study_env$year_beg:study_env$year_end,
  function(crop_yr){
    tryCatch({
      # year <- 2015

      revenue_draw <- tempfile(fileext = ".rds")
      download.file(
        paste0("https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/revenue_draw/revenue_draw_",year,".rds"),
        revenue_draw, mode = "wb", quiet = TRUE)
      revenue_draw <- as.data.table(readRDS(revenue_draw))

      agentdata <- agentdata[
        unique(as.data.table(readRDS("data-raw/data/cleaned_rma_sco_and_eco_adm.rds"))[
          commodity_year %in% year, FCIP_INSURANCE_POOL, with = FALSE]),
        on = FCIP_INSURANCE_POOL, nomatch = 0];gc()

      rma_sob <- readRDS("data-raw/data/cleaned_rma_sobtpu.rds")[commodity_year %in% year]
      rma_sob[
        ,
        producer_id := do.call(paste, c(.SD, sep = "_")),
        .SDcols = c(
          "state_code", "county_code", "commodity_code", "type_code",
          "practice_code", "unit_structure_code", "insurance_plan_code",
          "coverage_type_code", "coverage_level_percent")]

      agentdata <- agentdata[rma_sob,on = intersect(names(agentdata), names(rma_sob)),nomatch = 0];rm(rma_sob);gc()

      agentdata[,observed_premium_rate := round(total_premium_amount/liability_amount,8)]

      agentdata[,observed_subsidy_percent := round(subsidy_amount/total_premium_amount,3)]

      calibrated_yield <- tempfile(fileext = ".rds")
      download.file(
        paste0("https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/calibrated_yield/calibrated_yield_",year,".rds"),
        calibrated_yield, mode = "wb", quiet = TRUE)

      calibrated_yield <- as.data.table(readRDS(calibrated_yield))[
        , c("producer_id","rate_yield","approved_yield","calibrated_yield"), with = FALSE]

      agentdata <- agentdata[calibrated_yield,on = intersect(names(agentdata), names(calibrated_yield)),nomatch = 0];rm(calibrated_yield);gc()

      price <- get_compressed_adm(year = year, insurance_plan = 1, dataset="A00810_Price")[
          , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
          by = c("commodity_year",FCIP_INSURANCE_POOL),
          .SDcols = c("projected_price")]

      agentdata <- merge(agentdata, price, by = intersect(names(agentdata), names(price)), all = TRUE);rm(price)

      agentdata <- agentdata[
        ! calibrated_yield %in% c(NA,Inf,-Inf,NaN),
        c("commodity_year",FCIP_INSURANCE_POOL,FCIP_INSURANCE_ELECTION,"producer_id",
            "rate_yield","approved_yield","calibrated_yield","projected_price",
            "observed_premium_rate","observed_subsidy_percent","insured_acres","sco", "eco90", "eco95",
            "rma_draw_lookup_rate","rma_draw_number","rma_draw_yield_farm",
            "rma_draw_price_farm","rma_draw_yield_pool","rma_draw_price_pool"), with = FALSE]

      agentdata[,planted_acres  := insured_acres]
      agentdata[,price_election := 1]
      agentdata[,insured_share  := 1]
      agentdata[,dmage_are_rate := 1]

      saveRDS(agentdata,file =paste0("data-raw/data/agentdata/agentdata",year,".rds"))
      rm(agentdata);gc();gc()
      return(year)
    }, error = function(e){return(NULL)})
  })
plan(sequential)
