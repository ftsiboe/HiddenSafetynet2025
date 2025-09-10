rm(list=ls(all=TRUE));gc();gc()
library(future.apply);library(data.table)
devtools::document()
study_environment <- readRDS("data/study_environment.rds")

function(){
  lapply(
    as.numeric(list.files(study_environment$wd$dir_sim)),
    function(year){
      return(print(paste0(year,"=", length(list.files(path=file.path(study_environment$wd$dir_expected, year),recursive = T,full.names = T)))))
    })
}

work_list <- as.numeric(list.files(study_environment$wd$dir_expected))
work_list <- work_list[! work_list %in% NA]
if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  work_list <- work_list[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))]
}

lapply(
  work_list,
  aggregate_expected_outcomes,
  study_environment = study_environment,
  agent_identifiers = c("commodity_year","state_code","county_code","type_code"),
  disaggregate="combination",
  expected_directory = NULL,
  output_directory = study_environment$wd$dir_expected)


