

rm(list=ls(all=TRUE));gc()
library(future.apply);library(data.table)
# devtools::document()

source("R/setup_environment.R")
source("R/farm_performance_metrics.R")

study_environment <- setup_environment()
Keep.List<-c("Keep.List",ls())

function(){
  unlink(study_environment$wd$dir_drawfarm,recursive = T)
  dir.create(study_environment$wd$dir_drawfarm)
  lapply(
    0:100,
    function(d){
      dir.create(file.path(study_environment$wd$dir_drawfarm,stringr::str_pad(d,pad="0",4)))
      return(d)
    })
}

function(){

  work_list <- unique(readRDS(file.path(study_environment$wd$dir_expected,"expected_2022.rds"))$combination)

  year_list <- as.numeric(list.files(study_environment$wd$dir_expected))
  year_list <- year_list[! year_list %in% NA]

  work_list <- as.data.frame(
    data.table::rbindlist(
      lapply(year_list,
             function(year){return(data.frame(year=year,combination=work_list))}), fill = TRUE))

  work_list <- work_list[!(work_list$year<= 2020 & grepl("ECO",work_list$combination)),]

  table(work_list$combination,work_list$year)

  work_list <- as.data.frame(
    data.table::rbindlist(
      lapply(0:100,
             function(draw){
               dir.create(paste0(study_environment$wd$dir_drawfarm,stringr::str_pad(draw,pad="0",4)))
               return(data.frame(draw=stringr::str_pad(draw,pad="0",4),work_list))}), fill = TRUE))

  work_list <- work_list[!gsub("__","_",paste0(work_list$draw,"/",gsub("[+]","_",work_list$combination),"_",
                                               work_list$crop_yr,"_",work_list$draw,".rds")) %in%
                           list.files(study_environment$wd$dir_drawfarm,recursive = T),]

  saveRDS(work_list,file="data-raw/work_list_compute_metrics.rds")
}

work_list <- readRDS("data-raw/work_list_compute_metrics.rds")

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  work_list <- work_list[as.numeric(work_list$draw) %in% as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
  #work_list <- work_list[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}

gc()

plan(list(tweak(multisession, workers = 8)))

future_lapply(
  1:nrow(work_list),
  function(ii){
    tryCatch({
      # ii <- 2
      combo <- work_list$combination[ii]
      draw  <- work_list$draw[ii]
      year  <- work_list$year[ii]
      output_directory <- study_environment$wd$dir_drawfarm

      output_file_path <- file.path(output_directory, draw, paste0(gsub("[+]","_", combo), "_", year, draw, ".rds"))
      output_file_path <- gsub(" ", "_", output_file_path)
      output_file_path <- gsub("__", "_", output_file_path)

      if(!basename(output_file_path) %in% list.files(dirname(output_file_path))){

        farm_performance_metrics(
          year                  = year,
          combo                 = combo,
          agent_identifiers     = c("commodity_year","state_code","county_code","commodity_code","type_code","practice_code",
                                    "unit_structure_code","insurance_plan_code","coverage_level_percent"),
          outcome_list          = c("its","Iits","rrs1","rrs2","rrs3","Irrs1","Irrs2","Irrs3","sner1","sner2","sner3",
                                    "Simrate","SimrateP","Simsuby","Simlcr","rrp1","rrp2","rrp3","itp"),
          weight_variable       = "insured_acres",
          expected_directory    = study_environment$wd$dir_expected,
          draw                  = draw,
          draw_list_file_path   = "data-raw/draw_list.rds",
          disaggregates         = c("CROP","STATE","CROP_STATE","ERSReg","CRD","COUNTY","insurance_plan_code","PLAN","RPYP","coverage_level_percent","COV","STRUCT","unit_structure_code"),
          output_file_path      = output_file_path,
          distributional_limits = c(0.05, 0.95))

      }
      invisible(output_file_path)
    }, error = function(e){invisible()})
  })

plan(sequential)





