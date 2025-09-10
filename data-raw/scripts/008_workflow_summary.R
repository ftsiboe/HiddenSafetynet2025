rm(list=ls(all=TRUE));gc()
library(future.apply);library(data.table)
devtools::document()
study_environment <- readRDS("data/study_environment.rds")
Keep.List<-c("Keep.List",ls())


# Impact - main
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
data <- list.files("data-raw/output/aggregate_metrics",full.names = T)
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
saveRDS(data,file="data-raw/output/summary/impact_main.rds")


# Impact Heterogeneity 2021-Date
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
data <- list.files("data-raw/output/aggregate_metrics",full.names = T)
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
saveRDS(data,file="data-raw/output/summary/impact_heterogeneity.rds")


# Impact incremental 2021-Date
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
data <- c(list.files("data-raw/output/aggregate_metrics",full.names = T,pattern = "2015_2023.rds"))
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
saveRDS(data,file="data-raw/output/summary/impact_incremental.rds")



