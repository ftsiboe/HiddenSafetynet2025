rm(list=ls(all=TRUE));gc();gc()
library(future.apply);library(data.table)
devtools::document()
study_env <- setup_environment()
Keep.List<-c("Keep.List",ls())

# unlink(dir_sim,recursive = T)
dir.create(study_env$wd$dir_sim)
lapply(
  as.numeric(gsub("[^0-9]","",list.files("data/cleaned_agents_data"))),
  function(year){
    tryCatch({
      # crop_yr <- 2015
      if (!dir.exists(file.path(study_env$wd$dir_sim, year))) dir.create(file.path(study_env$wd$dir_sim, year), recursive = TRUE)
      return(year)
    }, error = function(e){return(NULL)})
  })

function(){ #
  lapply(
    as.numeric(gsub("[^0-9]","",list.files("data/cleaned_agents_data"))),
    function(year){
      return(print(paste0(year,"=", length(list.files(path=file.path(study_env$wd$dir_sim, year),recursive = T,full.names = T)))))
    })

  plan(multisession)
  future_lapply(
    study_env$year_beg:study_env$year_end,
    function(year){

      Check <- as.data.frame(file.info(list.files(path=file.path(study_env$wd$dir_sim, year),recursive = T,full.names = T)))

      lapply(
        row.names(Check),
        function(file){
          DONE <- NULL
          tryCatch({
            data <- readRDS(file)

            if (nrow(data) == 0) {
              # Delete the file if the data frame is empty
              unlink(file)
              cat("Deleted empty file:", file, "\n")
            } else {
              cat("File not empty:", file, "\n")
            }

          }, error=function(e){})
          return(DONE)
        })
      return(year)
    })
  plan(sequential)

}

work_list <-as.data.frame(
  data.table::rbindlist(
    lapply(
      as.numeric(gsub("[^0-9]","",list.files("data/cleaned_agents_data"))),
      function(year){
        data.frame(year=year,draw=1:500)
      }),fill = TRUE))

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  work_list$TASK <- rep(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_MIN")):as.numeric(Sys.getenv("SLURM_ARRAY_TASK_MAX")), length=nrow(work_list))
  work_list <- work_list[work_list$TASK %in% as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}

lapply(
  1:nrow(work_list),
  function(work){
    tryCatch({
      # work <- 1
      sim  <- work_list$draw[work]
      year <- work_list$year[work]
      if(!paste0("sim",stringr::str_pad(sim,pad="0",3),".rds") %in% list.files(file.path(study_env$wd$dir_sim, year))){

        dispatcher_supplemental_simulation(
          sim = sim,year = year,agents_dir = "data/cleaned_agents_data",
          cleaned_rma_sco_and_eco_adm_file_path ="data/cleaned_rma_sco_and_eco_adm.rds")

        }
      invisible()
    }, error = function(e){invisible()})
  })
