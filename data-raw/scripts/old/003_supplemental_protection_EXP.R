
rm(list=ls(all=TRUE));gc();gc()
library(future.apply);library(data.table)
devtools::document()
study_environment <- setup_environment()
Keep.List<-c("Keep.List",ls())

if(Sys.getenv("SLURM_JOB_NAME") %in% "SUP_EXP01"){

  work_list <-as.data.frame(
    data.table::rbindlist(
      lapply(
        as.numeric(list.files(study_environment$wd$dir_sim)),
        function(year){
          dir.create(  file.path(study_environment$wd$dir_expected, year))
          return(data.frame(year=year,task=1:500))
        }),fill = TRUE))

  if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
    work_list <- work_list[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
  }
  # work_list <- work_list[work_list$crop_yr %in% 2015,]

  lapply(
    1:nrow(work_list),
    function(work){
      tryCatch({
        # work <- 2
        year <- work_list$year[work]
        task_id <- work_list$task[work]

        if(!paste0("expected_",year,"_",task_id,".rds") %in% list.files(file.path(study_environment$wd$dir_expected, year))){

          compute_expected_outcomes(
            year = year,
            task_id = task_id,
            agents_directory = "data/cleaned_agents_data",
            study_environment = study_environment,
            agent_identifiers = c("commodity_year","state_code","county_code","commodity_code","type_code","practice_code",
                                  "unit_structure_code","insurance_plan_code","coverage_level_percent","insured_acres"),
            disaggregate="combination")

        }

        invisible()
      }, error = function(e){invisible()})
    })

}




























if(Sys.getenv("SLURM_JOB_NAME") %in% "SUP_EXP02"){

  function(){
    lapply(
      as.numeric(list.files(dir_sim)),
      function(crop_yr){
        return(print(paste0(crop_yr,"=", length(list.files(path=paste0(dir_expected,crop_yr),recursive = T,full.names = T)))))
      })
  }

  work_list <- as.numeric(list.files(dir_expected))
  work_list <- work_list[! work_list %in% NA]
  if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
    work_list <- work_list[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))]
  }

  lapply(
    work_list,
    function(crop_yr){

      # crop_yr <- 2015

      Expected <- as.data.frame(
        data.table::rbindlist(
          lapply(
            list.files(path=paste0(dir_expected,crop_yr),recursive = T,full.names = T),
            function(expected){
              tryCatch({
                return(readRDS(expected))
              }, error = function(e){return(NULL)})
            }), fill = TRUE))

      setDT(Expected)

      upper_limits <- Expected[, lapply(.SD, function(x) quantile(unique(x[!x %in% c(-Inf,Inf,NaN,NA)]), probs = 0.95, na.rm = TRUE)),
                               by = c("crop_yr","state_cd","crop_cd","typ_cd","combination"),
                               .SDcols = c("Relmean","Relsd","Relcv","Rellapv","Rellrpv","Relnlapv","Relnlrpv","Relvar")]
      names(upper_limits) <- c("crop_yr","state_cd","crop_cd","typ_cd","combination",
                               paste0("upper_",c("Relmean","Relsd","Relcv","Rellapv","Rellrpv","Relnlapv","Relnlrpv","Relvar")))
      Expected <- merge(Expected, upper_limits, by = intersect(names(Expected),names(upper_limits)), all = TRUE);rm(upper_limits);gc()

      lower_limits <- Expected[, lapply(.SD, function(x) quantile(unique(x[!x %in% c(-Inf,Inf,NaN,NA)]), probs = 0.05, na.rm = TRUE)),
                               by = c("crop_yr","state_cd","crop_cd","typ_cd","combination"),
                               .SDcols = c("Relmean","Relsd","Relcv","Rellapv","Rellrpv","Relnlapv","Relnlrpv","Relvar")]
      names(lower_limits) <- c("crop_yr","state_cd","crop_cd","typ_cd","combination",
                               paste0("lower_",c("Relmean","Relsd","Relcv","Rellapv","Rellrpv","Relnlapv","Relnlrpv","Relvar")))
      Expected <- merge(Expected, lower_limits, by = intersect(names(Expected),names(lower_limits)), all = TRUE);rm(lower_limits);gc()

      Expected[, `:=`(Relmean  = ifelse(Relmean >upper_Relmean,upper_Relmean,Relmean),
                      Relsd    = ifelse(Relsd   >upper_Relsd,upper_Relsd,Relsd),
                      Relcv    = ifelse(Relcv   >upper_Relcv,upper_Relcv,Relcv),
                      Rellapv  = ifelse(Rellapv >upper_Rellapv,upper_Rellapv,Rellapv),
                      Rellrpv  = ifelse(Rellrpv >upper_Rellrpv,upper_Rellrpv,Rellrpv),
                      Relnlapv = ifelse(Relnlapv>upper_Relnlapv,upper_Relnlapv,Relnlapv),
                      Relnlrpv = ifelse(Relnlrpv>upper_Relnlrpv,upper_Relnlrpv,Relnlrpv),
                      Relvar   = ifelse(Relvar  >upper_Relvar,upper_Relvar,Relvar))]

      Expected[, `:=`(Relmean  = ifelse(Relmean <lower_Relmean,lower_Relmean,Relmean),
                      Relsd    = ifelse(Relsd   <lower_Relsd,lower_Relsd,Relsd),
                      Relcv    = ifelse(Relcv   <lower_Relcv,lower_Relcv,Relcv),
                      Rellapv  = ifelse(Rellapv <lower_Rellapv,lower_Rellapv,Rellapv),
                      Rellrpv  = ifelse(Rellrpv <lower_Rellrpv,lower_Rellrpv,Rellrpv),
                      Relnlapv = ifelse(Relnlapv<lower_Relnlapv,lower_Relnlapv,Relnlapv),
                      Relnlrpv = ifelse(Relnlrpv<lower_Relnlrpv,lower_Relnlrpv,Relnlrpv),
                      Relvar   = ifelse(Relvar  <lower_Relvar,lower_Relvar,Relvar))]

      Expected <- Expected[, .SD, .SDcols = !patterns("lower_")]
      Expected <- Expected[, .SD, .SDcols = !patterns("upper_")]

      saveRDS(as.data.frame(Expected),paste0(dir_expected,"expected_",crop_yr,".rds"))

      rm(Expected);gc();gc()

      return(crop_yr)
    })
}

