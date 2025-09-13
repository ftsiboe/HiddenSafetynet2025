rm(list=ls(all=TRUE));gc();gc()
library(future.apply);library(data.table)
devtools::document()
study_environment <- readRDS("data/study_environment.rds")

function(){
  work_list <-as.data.frame(
    data.table::rbindlist(
      lapply(
        as.numeric(list.files(study_environment$wd$dir_sim)),
        function(year){
          dir.create(  file.path(study_environment$wd$dir_expected, year))
          return(data.frame(year=year,task=1:500))
        }),fill = TRUE))

  work_list <- work_list[!file.path(study_environment$wd$dir_expected,work_list$year,paste0("expected_",work_list$year,"_",work_list$task,".rds")) %in%
                           list.files(file.path(study_environment$wd$dir_expected),full.names = T,recursive = T),]

  saveRDS(work_list,file="data-raw/work_list_compute_expected.rds")
}

work_list <- readRDS("data-raw/work_list_compute_expected.rds")

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  work_list <- work_list[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}

lapply(
  1:nrow(work_list),
  function(work){
    tryCatch({
      # work <- 1
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
