
rm(list=ls(all=TRUE));gc();gc()
library(data.table)
devtools::document()
study_environment <- setup_environment()
Keep.List<-c("Keep.List",ls())


expected_directory = study_environment$wd$dir_expected
output_directory = NULL
draw_identifiers = c("state_code","county_code","commodity_code","type_code","practice_code")
seed = NULL


if (is.null(seed)) {
  seed <- study_environment$seed
}
if (is.null(output_directory)) {
  output_directory <- "data-raw"
}

if (is.null(expected_directory)) {
  stop("Provide pathe to Expected files")
}

set.seed(seed)

data <- data.table::rbindlist(
  lapply(
    list.files(expected_directory,full.names = T,pattern = "expected_"),
    function(expected){
      tryCatch({
        return(unique(as.data.frame(readRDS(expected))[c("commodity_year",draw_identifiers)]))
      }, error = function(e){return(NULL)})
    }), fill = TRUE)

data[,combination := 1]

pract <- tempfile(fileext = ".rds")
download.file(
  paste0("https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/adm_extracts/fcip_recodes_practice.rds"),
  pract, mode = "wb", quiet = TRUE)
pract <- as.data.frame(readRDS(pract))

pract$NONIRR   <- as.numeric(pract$irrigation_recode %in% "NON-IRR")
pract$IRR      <- as.numeric(pract$irrigation_recode %in% "IRR" )
pract$IRROther <- as.numeric(pract$irrigation_recode %in% "Other" )
pract$CERTIFIED    <- as.numeric(pract$organic_recode %in% "ORGANIC (CERTIFIED)" )
pract$TRANSITIONAL <- as.numeric(pract$organic_recode %in% "ORGANIC (TRANSITIONAL)")
pract$ORGANICOther <- as.numeric(pract$organic_recode %in% "NO PRACTICE SPECIFED" )
pract <- doBy::summaryBy(list(c("NONIRR","IRR","IRROther","CERTIFIED","TRANSITIONAL","ORGANICOther"),
                              c("commodity_year","commodity_code","practice_code")),data=pract,FUN=max,keep.names = T,na.rm=T)

data <- dplyr::full_join(pract,data,by=c("commodity_year","commodity_code","practice_code"))
data$irrigation_recode <- ifelse(data$IRR %in% 1,"Irrigated","Unspecified")
data$irrigation_recode <- ifelse(data$irrigation_recode %in% "Unspecified" & data$NONIRR %in% 1,"Nonirrigated",data$irrigation_recode)
data$organic_recode <- ifelse(data$CERTIFIED  %in% 1,"Organic (Certified)","Unspecified")
data$organic_recode <- ifelse(data$organic_recode %in% "Unspecified" & data$TRANSITIONAL %in% 1,"Organic (Transitional)",data$organic_recode)
data$IRRG <- as.character(data$irrigation_recode)
data$ORGC <- as.character(data$organic_recode)
data <- data[!data$combination %in% c(NaN,Inf,-Inf,NA),]
rm(pract);gc();gc()

data <- unique(data[c("state_code","county_code","commodity_code","type_code","practice_code","IRR","ORGC","combination")]);gc();gc()


data$COUNTY <- as.numeric(paste0(stringi::stri_pad(data$state_code,pad="0",2),stringi::stri_pad(data$county_code,pad="0",3)))
ERSReg <- tempfile(fileext = ".xls")
download.file(
  paste0("https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/spatial_features/County-to-ERS.Resource.Region.aggregation.xls"),
  ERSReg, mode = "wb", quiet = TRUE)
ERSReg <- as.data.frame(readxl::read_excel(ERSReg))
names(ERSReg)[names(ERSReg) %in% "name"] <- "ERSReg"
names(ERSReg)[names(ERSReg) %in% "id"]   <- "ERSReg_cd"
names(ERSReg)[names(ERSReg) %in% "county_fips"]   <- "COUNTY"
data <- dplyr::full_join(data,ERSReg[c("COUNTY","ERSReg","ERSReg_cd")],by="COUNTY")
data <- data[!data$combination %in% c(NaN,Inf,-Inf,NA),]
rm(ERSReg);gc();gc()

data$COUNTY <- paste0(stringi::stri_pad(data$state_code,pad="0",2),stringi::stri_pad(data$county_code,pad="0",3))

crd <- tempfile(fileext = ".xls")
download.file(
  paste0("https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/spatial_features/NASS.County.and.District.Codes.csv"),
  crd, mode = "wb", quiet = TRUE)
crd <- as.data.frame(readr::read_csv(crd,skip = 2))
crd <- crd[3:nrow(crd),]
names(crd) <- c("state_code","CRD","county_code","county","flag")
crd <- crd[as.numeric(as.character(crd$flag)) %in% 1,]
crd$county_code <- as.numeric(as.character(crd$county_code))
crd$state_code <- as.numeric(as.character(crd$state_code))
crd$CRD <- paste0(stringr::str_pad(crd$state_code,pad="0",2),stringr::str_pad(as.numeric(as.character(crd$CRD)),pad="0",3))
data <- dplyr::full_join(data,crd[c("state_code","county_code","CRD")],by=c("state_code","county_code"))
data <- data[!data$combination %in% c(NaN,Inf,-Inf,NA),]
rm(crd);gc();gc()

data <- unique(data[c("state_code","county_code","commodity_code","type_code","practice_code",
                      "IRR","ORGC","ERSReg","CRD","COUNTY")]);gc();gc()

data$STATE  <- data$state_code
data$FCIP  <- 0
data$CROP <- data$commodity_code
data$MCROP <- ifelse(data$commodity_code %in% c(11,18,21,41,81),data$commodity_code,0)

data$CROP_STATE  <- paste0(data$commodity_code,"-",data$STATE)
data$MCROP_STATE <- paste0(data$MCROP,"-",data$STATE)

data$CROP_ERSReg  <- paste0(data$commodity_code,"-",data$ERSReg)
data$MCROP_ERSReg <- paste0(data$MCROP,"-",data$ERSReg)

data$CROP_CRD  <- paste0(data$commodity_code,"-",data$CRD)
data$MCROP_CRD <- paste0(data$MCROP,"-",data$CRD)

data <- lapply(
  0:100,
  function(d){
    draw <- data
    if(!d %in% 0){ draw <- data[sample(1:nrow(data), nrow(data), replace=TRUE), ]}
    draw$drawID <- 1:nrow(draw)
    return(draw)
  })
names(data) <- stringr::str_pad(0:100,pad="0",4)

if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)
save_path <- file.path(output_directory, "draw_list.rds")
saveRDS(data, save_path);rm(data)
