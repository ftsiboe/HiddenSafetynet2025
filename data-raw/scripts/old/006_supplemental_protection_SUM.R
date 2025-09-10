
rm(list=ls(all=TRUE));gc()
library(future.apply);library(data.table)
devtools::document()

study_environment <- setup_environment()
Keep.List<-c("Keep.List",ls())

function(){
  work_list <- unique(readRDS(file.path(study_environment$wd$dir_expected,"expected_2022.rds"))$combination)

  work_list <- rbind(
    data.frame(year_min=2015,year_max=2023,combination=work_list),
    data.frame(year_min=2021,year_max=2023,combination=work_list),
    as.data.frame(
      data.table::rbindlist(
        lapply(c(2015:2023),
               function(year){return(data.frame(year_min=year,year_max=year,
                                                   combination=c("Basic+CURRENT","Basic only","Basic+SCO8665","Basic+SCO8665+ECO9544")))}), fill = TRUE)))

  work_list <- work_list[!paste0("summary_",gsub("__","_",gsub("[+]","_",work_list$combination)),"_",
                                 work_list$year_min,"_",work_list$year_max,".rds") %in%
                           list.files("data-raw/output/summary/"),]

  saveRDS(work_list,file="data-raw/work_summary.rds")
}

work_list <- readRDS("data-raw/work_summary.rds")

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  work_list <- work_list[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}

lapply(
  1:nrow(work_list),
  function(ii){
    tryCatch({
      # ii <- 28

      data <- data.table::rbindlist(
        lapply(
          work_list$year_min[ii]:work_list$year_max[ii],
          function(year){
            # year <- 2015
            tryCatch({
              if(work_list$combination[ii] %in% c("Basic+SCO8665+ECO9044","Basic+SCO8665+ECO9544") & year < 2021){
                data <- list.files(study_environment$wd$dir_drawfarm,pattern = paste0("Basic_SCO8665_",year),recursive = T,full.names = T)
              }else{
                output_file_pattern <- paste0(gsub("[+]","_", work_list$combination[ii]), "_", year)
                output_file_pattern <- gsub(" ", "_", output_file_pattern)
                output_file_pattern <- gsub("__", "_", output_file_pattern)
                data <- list.files(study_environment$wd$dir_drawfarm,pattern = output_file_pattern,recursive = T,full.names = T)
              }

              data <- data.table::rbindlist(
                lapply(
                  unique(data)[1:10],function(fl){
                    tryCatch({return(readRDS(fl))}, error = function(e){return(NULL)})}), fill = TRUE)

              return(data)
            }, error = function(e){return(NULL)})
          }), fill = TRUE);gc()

      data[,combination := work_list$combination[ii]]

      data <- data[variable %in% c("its","rrs1","rrs2","rrs3","Simrate","SimrateP","Simsuby","rrp1","rrp2","rrp3","itp")]

      data <- data[, .(est = mean(value, na.rm = TRUE)),by = c("variable","combination","disag","level","aggregation","draw_id")]

      rrm <- data[variable %in% c("rrp1","rrp2","rrp3","itp")]

      SimrateP <- data[variable %in% c("SimrateP"), c("aggregation","combination","disag","level","draw_id","est"), with = FALSE]

      setnames(SimrateP,old = c("est"),new = c("SimrateP"))

      rrm <- rrm[SimrateP,on = c("aggregation","combination","disag","level","draw_id"),nomatch = 0];rm(SimrateP);gc();gc()

      rrm[,est := est/SimrateP]
      rrm[,variable := gsub("p","m",variable)]

      rrm <- rrm[, .(est = mean(est, na.rm = TRUE)),by = c("variable","combination","disag","level","aggregation","draw_id")]

      data <- rbind(rrm,data);rm(rrm);gc();gc()

      data <- data[draw_id %in% "0000",.(est = mean(est,na.rm=T)), by = c("variable","combination","disag","level","aggregation")][
        data[!draw_id %in% "0000",.(est_mean = mean(est,na.rm=T),est_se = sd(est,na.rm=T),
                                    est_n  = sum(as.numeric(!est %in% c(NA,Inf,NaN,-Inf)),na.rm=T)),
             by = c("variable","combination","disag","level","aggregation")],
        on = c("variable","combination","disag","level","aggregation"),nomatch = 0];gc()

      data[,est_zv := est/est_se]
      data[,est_pv := round(2 * (1 - pt(abs(est_zv), df=est_n)),5)]
      data[,baseline := paste0(work_list$year_min[ii],"-",work_list$year_max[ii])]

      saveRDS(data,paste0("data-raw/output/summary/summary_",gsub("__","_",gsub("[+]","_",work_list$combination[ii])),"_",
                          work_list$year_min[ii],"_",work_list$year_max[ii],".rds"))
      rm(data);gc();gc()
      #--------------------------------------------------
    return(ii)
    }, error = function(e){return(NULL)})
  })

function(){
  #--------------------------------------------------------------------------------------------------------------
  # Impact - main                                                                                             ####
  rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()

  data <- list.files("data-raw/output/summary",full.names = T)
  data <- unique(data[!grepl("ALT",data)])
  #data <- c(unique(data[grepl("_2015_2023.rds",data)]),unique(data[grepl("_2021_2023.rds",data)]))

  data <- as.data.frame(
    data.table::rbindlist(
      lapply(
        data,
        function(fl){
          data <- readRDS(fl)
          data <- data[data$disag %in% "FCIP",]
          data <- data[data$variable %in% c("its","rrs1","rrs2","rrs3","sner1","sner2","sner3","rrp1","rrp2","rrp3","itp","Simrate","Simsuby","SimrateP",
                                            "liability","costA","costB","costD","costG","costE","costF","costJ","lr",
                                            "rrm1","rrm2","rrm3","rrc1","rrc2","rrc3","rrx1","rrx2","rrx3"),]
          data <- data[data$aggregation %in% c("avg_valueT","value","avg_chglvl00T","chglvl","avg_chgpct00T","avg_chglvl01T","avg_chgpct01T","chgpct"),]
          return(data)
        }), fill = TRUE))
  saveRDS(data,file="data-raw/output/figure_data/impact_main.rds")
  #--------------------------------------------------------------------------------------------------------------
  # Impact Heterogeneity 2021-Date                                                                            ####
  rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()

  data <- list.files("output/summary",full.names = T)
  data <- unique(data[!grepl("ALT",data)])
  data <- c(unique(data[grepl("_2015_2023.rds",data)]),unique(data[grepl("_2021_2023.rds",data)]))
  data <- data[!grepl("Basic only",data)]
  data <- data[!grepl("Basic only",data)]

  data <- as.data.frame(
    data.table::rbindlist(
      lapply(
        data,
        function(fl){
          data <- readRDS(fl)
          data <- data[data$variable %in% c("rrm2","itp","rrp2","SimrateP"),]
          data <- data[data$aggregation %in% c("avg_chgpct00T","avg_valueT","avg_chglvl01T","avg_chgpct01T"),]
          return(data)
        }), fill = TRUE))

  saveRDS(data,file="data-raw/output/figure_data/impact_heterogeneity.rds")
  #--------------------------------------------------------------------------------------------------------------
  # Impact incremental 2021-Date                                                                              ####
  rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
  data <- c(list.files("data-raw/output/summary",full.names = T,pattern = "2015_2023.rds"))
  data <- data[ grepl("ALT",data)]

  data <- as.data.frame(
    data.table::rbindlist(
      lapply(
        data,
        function(fl){
          data <- readRDS(fl)
          data <- data[data$variable %in% c("its","rrs1","rrs2","rrs3","sner1","sner2","sner3","rrp1","rrp2","rrp3","itp","Simrate","Simsuby","SimrateP",
                                            "liability","costA","costB","costD","costG","costE","costF","costJ","lr",
                                            "rrm1","rrm2","rrm3","rrc1","rrc2","rrc3","rrx1","rrx2","rrx3"),]
          data <- data[data$aggregation %in% c("avg_valueT","value","avg_chglvl00T","chglvl","avg_chgpct00T","chgpct"),]
          data <- data[data$disag %in% "FCIP",]
          return(data)
        }), fill = TRUE))

  data$combination <- as.numeric(gsub("[^0-9]","",data$combination))

  saveRDS(data,file="data-raw/output/figure_data/impact_incremental.rds")
  #--------------------------------------------------------------------------------------------------------------
}

