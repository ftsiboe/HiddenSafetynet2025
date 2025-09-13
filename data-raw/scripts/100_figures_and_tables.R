#--------------------------------------------------------------------------------------------------------------
# Preliminaries                                                                                             ####
rm(list=ls(all=TRUE));gc()
library(data.table);library('magrittr');library(ggplot2);library(gridExtra)
library(dplyr);library(gtable);library(ggridges)
devtools::document()
study_environment <- readRDS("data/study_environment.rds")
myTheme <- ers_theme() +
  theme(plot.title= element_text(size=10.5),
        axis.title= element_text(size=9,color="black"),
        axis.text = element_text(size=10,color="black"),
        axis.title.y= element_text(size=9,color="black"),
        legend.title=element_blank(),
        legend.text=element_text(size=9),
        plot.caption = element_text(size=8),
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))
clo <- c("blue","firebrick","orange","darkgreen","purple","saddlebrown","darkgray")

fcip_recodes_commodity_groupings <- tempfile(fileext = ".rds")
download.file(
  "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/adm_extracts/fcip_recodes_commodity_groupings.rds",
  fcip_recodes_commodity_groupings, mode = "wb", quiet = TRUE)
fcip_recodes_commodity_groupings <- as.data.frame(readRDS(fcip_recodes_commodity_groupings))
fcip_recodes_commodity_groupings$commodity_name <- tools::toTitleCase(tolower(fcip_recodes_commodity_groupings$commodity_name))
fcip_recodes_commodity_groupings$commodity_name <- ifelse(fcip_recodes_commodity_groupings$commodity_name %in% "Grain Sorghum","Sorghum",fcip_recodes_commodity_groupings$commodity_name)

sobtpu_all <- tempfile(fileext = ".rds")
download.file(
  "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/sob/sobtpu_all.rds",
  sobtpu_all, mode = "wb", quiet = TRUE)
sobtpu_all <- as.data.frame(readRDS(sobtpu_all))

sobcov_all <- tempfile(fileext = ".rds")
download.file(
  "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/sob/sobcov_all.rds",
  sobcov_all, mode = "wb", quiet = TRUE)
sobcov_all <- as.data.frame(readRDS(sobcov_all))

Keep.List<-c("Keep.List",ls())
#--------------------------------------------------------------------------------------------------------------
# state major crop                                                                                          ####
sob <- readRDS("data/cleaned_rma_sobtpu.rds")
setDT(sob)
sob <- as.data.frame(sob[commodity_year %in% 2002:2023, .(insured_acres = sum(insured_acres, na.rm = TRUE)),by = c("commodity_code","state_code")])
sob <- sob |> group_by(state_code) |> mutate(insured_acres_max = max(insured_acres,na.rm=T)) |> as.data.frame(.)
sob <- sob[sob$insured_acres == sob$insured_acres_max,]
sob$major_commodity <- sob$commodity_code
saveRDS(sob[c("state_code","major_commodity")],file="data-raw/output/figure_data/state_major_crop.rds")
#--------------------------------------------------------------------------------------------------------------
# Figure: Supplemental coverage offering and adoption in the U.S. Federal Crop Insurance Program            ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
us_sf <- urbnmapr::get_urbn_map(map = "states", sf = TRUE)
cty_sf <- urbnmapr::get_urbn_map(map = "counties", sf = TRUE)
cty_sf$county_fips <- as.character(cty_sf$county_fips)

data <- as.data.frame(readRDS("data/supplemental_offering_and_adoption.rds"))

data$commodity_code <- ifelse(data$commodity_code %in% 22,21,data$commodity_code)
data$commodity_code <- ifelse(data$commodity_code %in% 59,51,data$commodity_code)
data$commodity_code <- ifelse(data$commodity_code %in% c(229,230,231,232,233,234,235,236),229,data$commodity_code)
data$commodity_code <- ifelse(data$commodity_code %in% c(88,332,33,32,102,107),88,data$commodity_code)

setDT(data)

data_fig <- as.data.frame(
  data[commodity_year %in% c(2015,2021,study_environment$year_end),.(
    insured_acres = sum(insured_acres,na.rm=T),
    avail_aph = max(avail_aph,na.rm=T),
    avail_sco = max(avail_sco,na.rm=T),
    avail_eco90 = max(avail_eco90,na.rm=T),
    avail_eco95 = max(avail_eco95,na.rm=T),
    sco = sum(sco,na.rm=T),
    eco90 = sum(eco90,na.rm=T),eco95 = sum(eco95,na.rm=T)),
    by = c("commodity_year","county_fips")])

data_fig <- data_fig |> tidyr::gather(plan, value, 5:ncol(data_fig))

data_fig$stat <- ifelse(grepl("avail",data_fig$plan),"available","adoption")
data_fig$plan <- gsub("avail_","",data_fig$plan)
data_fig <- data_fig |> tidyr::spread(stat, value)

data_fig <- rbind(data_fig[(data_fig$plan %in% "sco" & data_fig$commodity_year %in% c(2015,study_environment$year_end)),],
                  data_fig[(data_fig$plan %in% c("eco90","eco95") & data_fig$commodity_year %in% c(2021,study_environment$year_end)),])

data_fig$commodity_year <- ifelse(data_fig$commodity_year %in% study_environment$year_end,paste0("(b) ",study_environment$year_end," crop year"),"(a) Inception crop year")
data_fig$adoption   <- (data_fig$adoption/data_fig$insured_acres)*100
data_fig$adoption <- ifelse(data_fig$adoption >100,100,data_fig$adoption)

data_fig$adoption <- ifelse(data_fig$adoption %in% c(0,NA,NaN,Inf,-Inf),0,data_fig$adoption)
data_fig$available <- ifelse(data_fig$available %in% c(0,NA,NaN,Inf,-Inf),0,data_fig$available)

data_fig$value <- ifelse(data_fig$avail_aph %in% 0,-3,NA)
data_fig$value <- ifelse(data_fig$avail_aph %in% 1 & data_fig$available %in% 0, -2,data_fig$value)
data_fig$value <- ifelse(data_fig$avail_aph %in% 1 & data_fig$available %in% 1 & data_fig$insured_acres %in% 0, -1,data_fig$value)
data_fig$value <- ifelse(data_fig$avail_aph %in% 1 & data_fig$available %in% 1,data_fig$adoption,data_fig$value)
data_fig$cat <- cut(data_fig$value,unique(c(-Inf,-3,-2,-1,0,1,2,3,4,5,max(data_fig$value,na.rm=T))))
table(data_fig$cat)

data_fig$catIT <- as.numeric(as.integer(data_fig$cat))
keyled <- unique(data_fig[c("cat","catIT")])
keyled <- keyled[order(keyled$catIT),]
keyled$cat <- as.character(keyled$cat)
keyled$cat <- ifelse(keyled$cat %in% "(-Inf,-3]","Not eligible (APH, YP, RP, or RP-HPE not offered)",keyled$cat)
keyled$cat <- ifelse(keyled$cat %in% "(-3,-2]","Eligible but not offered",keyled$cat)
keyled$cat <- ifelse(keyled$cat %in% "(-2,-1]","No APH, YP, RP, or RP-HPE enrolment",keyled$cat)
keyled$cat <- ifelse(keyled$cat %in% "(-1,0]","0.00% adopted",keyled$cat)
keyled$cat <- ifelse(keyled$cat %in% "(0,1]","0.01 to 1.00% adopted",keyled$cat)
keyled$cat <- ifelse(keyled$cat %in% "(1,2]","1.01 to 2.00% adopted",keyled$cat)
keyled$cat <- ifelse(keyled$cat %in% "(2,3]","2.01 to 3.00% adopted",keyled$cat)
keyled$cat <- ifelse(keyled$cat %in% "(3,4]","3.01 to 4.00% adopted",keyled$cat)
keyled$cat <- ifelse(keyled$cat %in% "(4,5]","4.01 to 5.00% adopted",keyled$cat)
keyled$cat <- ifelse(keyled$cat %in% "(5,82.2]","More than 5% adopted",keyled$cat)
data_fig$cat <- factor(data_fig$catIT,levels = keyled$catIT,labels = keyled$cat)

data_fig$plan <- as.numeric(as.character(factor(data_fig$plan,levels = c("sco","eco90","eco95","hipwi"),labels = 1:4)))
data_fig$plan <- factor(data_fig$plan,levels = 1:4,labels =
                          c("Supplemental Coverage Option (SCO)",
                            "Enhanced Coverage Option (ECO)\n[90%]",
                            "Enhanced Coverage Option (ECO)\n[95%]",
                            "Hurricane Insurance Protection\n- Wind Index (HIP-WI)"))

sf_object <- cty_sf |> left_join(data_fig, by = "county_fips")
sf_object <- sf_object[!sf_object$cat %in% c(NA,NaN,Inf,-Inf),]
sf_object <- sf_object[!sf_object$plan %in% c(NA,NaN,Inf,-Inf),]

fig <- ggplot() +
  geom_sf(data = cty_sf, colour = "dimgray", fill = "darkblue",size =0.01) +
  geom_sf(data = sf_object,aes(fill = cat), colour = NA,size = 0.2) +
  geom_sf(data = cty_sf, colour = "dimgray", fill = NA,size =0.1) +
  geom_sf(data = us_sf, colour = "black", fill = NA,size =0.3) +
  scale_fill_manual(values=c("darkblue","red","orange","yellow",
                             colorRampPalette(c("yellowgreen","darkgreen"))(6)),
                    na.value="gainsboro", name="Payouts per operation") +
  guides(fill = guide_legend(nrow=3)) +
  labs(x = "", y = "", fill='',caption = "") +
  facet_grid(plan~commodity_year)+
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.text= element_text(size=6),
        legend.title=element_blank(),
        legend.key.size = unit(0.2, 'cm'),
        legend.background = element_blank(),
        plot.title = element_text(size = 8),
        axis.title.y = element_blank(), #
        axis.title.x = element_blank(), #
        axis.text.x  = element_blank(), #
        axis.text.y  = element_blank(),
        plot.caption = element_text(size=5,hjust = 0 ,vjust = 0, face = "italic"),
        strip.text = element_text(size = 6),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))

ggsave("data-raw/output/figure/supplemental_coverage_spat.png", fig,dpi = 1200,width = 6, height = 6.2)
saveRDS(sf_object,"data-raw/output/figure_data/supplemental_coverage_spat.rds")

data_fig <- as.data.frame(sf_object)
data_fig$plan <- as.character(factor(as.character(data_fig$plan),
                                     levels = c("Supplemental Coverage Option (SCO)",
                                                "Enhanced Coverage Option (ECO)\n[90%]",
                                                "Enhanced Coverage Option (ECO)\n[95%]",
                                                "Hurricane Insurance Protection\n- Wind Index (HIP-WI)"),
                                     labels = c("sco","eco90","eco95","hipwi")))

write.csv(data_fig[c("commodity_year","county_fips","state_name","insured_acres","avail_aph","plan","cat")] |> tidyr::spread(plan, cat),
          "data-raw/output/figure_data/supplemental_adoption_spat.csv")

write.csv(data_fig[c("commodity_year","county_fips","state_name","insured_acres","avail_aph","plan","available")] |> tidyr::spread(plan, available),
          "data-raw/output/figure_data/supplemental_available_spat.csv")

study_crops <- as.data.frame(data)
setDT(study_crops)
study_crops <- study_crops[avail_aph %in% 1,.(
  avail_sco = mean(avail_sco,na.rm=T)*100), by = c("commodity_year","commodity_code")]
study_crops <- as.data.frame(study_crops[,.(avail_sco = mean(avail_sco,na.rm=T)), by = c("commodity_code")])
#study_crops <- study_crops[study_crops$avail_sco >=90,]

SOBSCCTPU <- sobtpu_all

SOBSCCTPU$commodity_code <- ifelse(SOBSCCTPU$commodity_code %in% 22,21,SOBSCCTPU$commodity_code)
SOBSCCTPU$commodity_code <- ifelse(SOBSCCTPU$commodity_code %in% 59,51,SOBSCCTPU$commodity_code)
SOBSCCTPU$commodity_code <- ifelse(SOBSCCTPU$commodity_code %in% c(229,230,231,232,233,234,235,236),229,SOBSCCTPU$commodity_code)
SOBSCCTPU$commodity_code <- ifelse(SOBSCCTPU$commodity_code %in% c(88,332,33,32,102,107),88,SOBSCCTPU$commodity_code)
SOBSCCTPU <- SOBSCCTPU[SOBSCCTPU$reporting_level_type %in% "Acres",]
SOBSCCTPU <- SOBSCCTPU[SOBSCCTPU$commodity_year %in% as.numeric(gsub("[^0-9]","",list.files("data/cleaned_agents_data"))),]
SOBSCCTPU <- SOBSCCTPU[SOBSCCTPU$commodity_code %in% unique(readRDS("data-raw/draw_list.rds")[[1]]$commodity_code),]
SOBSCCTPU <- SOBSCCTPU[SOBSCCTPU$insurance_plan_code %in% c(1:3,90),]
setDT(SOBSCCTPU)
SOBSCCTPU <- as.data.frame(SOBSCCTPU[,.(insured_acres = sum(net_reporting_level_amount,na.rm=T)), by = c("commodity_code")])
SOBSCCTPU$insured_acres <- SOBSCCTPU$insured_acres/sum(SOBSCCTPU$insured_acres)
SOBSCCTPU <- SOBSCCTPU[order(-SOBSCCTPU$insured_acres),]
SOBSCCTPU <- dplyr::inner_join(SOBSCCTPU,study_crops)
SOBSCCTPU <- SOBSCCTPU[SOBSCCTPU$avail_sco >90,]
SOBSCCTPU

data_fig <- as.data.frame(data)
data_fig$crop <- ifelse(data_fig$commodity_code %in% unique(readRDS("data-raw/draw_list.rds")[[1]]$commodity_code),data_fig$commodity_code,0)
data_fig$avail_eco <- as.numeric(rowSums(data_fig[c("avail_eco90","avail_eco95")],na.rm=T) > 0)
data_fig$eco <- rowSums(data_fig[c("eco90","eco95")],na.rm=T)
setDT(data_fig)

data_fig <- as.data.frame(
  data_fig[avail_aph %in% 1,.(
    insured_acres = sum(insured_acres,na.rm=T),
    avail_sco = mean(avail_sco,na.rm=T)*100,
    avail_eco = mean(avail_eco,na.rm=T)*100,
    sco = sum(sco,na.rm=T),
    eco = sum(eco,na.rm=T)),
    by = c("commodity_year","commodity_code","crop")])

data_fig <- data_fig |> tidyr::gather(plan, value, 5:ncol(data_fig))
data_fig$stat <- ifelse(grepl("avail",data_fig$plan),"available","adoption")
data_fig$plan <- gsub("avail_","",data_fig$plan)
data_fig <- data_fig |> tidyr::spread(stat, value)

data_fig <- rbind(data_fig[(data_fig$plan %in% "sco" & data_fig$commodity_year %in% c(2015:study_environment$year_end)),],
                  data_fig[(data_fig$plan %in% c("eco") & data_fig$commodity_year %in% c(2021:study_environment$year_end)),])

data_fig$adoption  <- (data_fig$adoption/data_fig$insured_acres)*100
data_fig$adoption  <- ifelse(data_fig$adoption >100,100,data_fig$adoption)
data_fig$adoption  <- ifelse(data_fig$adoption %in% c(0,NA,NaN,Inf,-Inf),0,data_fig$adoption)
data_fig$available <- ifelse(data_fig$available %in% c(0,NA,NaN,Inf,-Inf),0,data_fig$available)

data_fig$plan <- as.numeric(as.character(factor(data_fig$plan,levels = c("sco","eco"),labels = 1:2)))
data_fig$plan <- factor(data_fig$plan,levels = 1:2,labels =
                          c("Supplemental Coverage\nOption (SCO)","Enhanced Coverage\nOption (ECO)"))

comname <- fcip_recodes_commodity_groupings
data_fig$cropname <- factor(as.numeric(data_fig$crop),levels = c(0,as.numeric(comname$commodity_code)),
                            labels = c("Other crops",comname$commodity_name))
data_fig$cropname <- as.character(data_fig$cropname)
LAB <- doBy::summaryBy(insured_acres~cropname,FUN=sum,data=data_fig)
LAB <- LAB[order(-LAB$insured_acres.sum),]
data_fig$cropname <- factor(data_fig$cropname,levels = LAB$cropname,labels = LAB$cropname)

data_fig <- data_fig |> tidyr::gather(type, value, c("adoption","available"))

data_fig$typeID <- as.numeric(as.character(factor(data_fig$type,levels = c("available","adoption"),labels = 1:2)))
data_fig$type <- factor(data_fig$typeID,levels = 1:2,labels =
                          c("Eligibility as a percentage of APH, YP, RP, or RP-HPE available insured acres",
                            "Adoption as a percentage of eligible insured acres"))

data_figx <- data_fig[!data_fig$cropname %in% "Other crops",]

fig <- ggplot(data_figx, aes(x=commodity_year, y=value, group=type,fill=type)) +
  geom_area(data=data_figx[data_figx$typeID %in% 1,]) +
  geom_area(data=data_figx[data_figx$typeID %in% 2,]) +
  facet_grid(plan~cropname)+
  guides(colour = guide_legend(nrow=1)) +
  scale_x_continuous(breaks = seq(2015,2050,2),labels= seq(2015,2050,2)) +
  scale_colour_manual(values = c("orange","purple")) +
  scale_fill_manual(values = c("orange","purple")) +
  labs(title= "", x = "",
       y ="", caption = "") +
  theme_bw() +
  theme(legend.position="bottom",legend.key.size = unit(0.2,"cm")) +
  theme(legend.text= element_text(size=8),
        legend.title=element_blank(),
        legend.key.size = unit(0.2, 'cm'),
        legend.background = element_blank(),
        plot.title = element_text(size = 8),
        axis.title.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.text.x  = element_text(size = 6,angle=90,vjust = 0.5),
        axis.text.y  = element_text(size = 6),
        strip.text = element_text(size = 7),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))

ggsave("data-raw/output/figure/supplemental_coverage_temp.png", fig,dpi = 1200,width = 11, height = 4)
saveRDS(data_figx,"data-raw/output/figure_data/supplemental_coverage_temp.rds")

write.csv(data_fig,"data-raw/output/figure_data/supplemental_coverage_temp.csv")

#--------------------------------------------------------------------------------------------------------------
# Table: Supplemental coverage availability and usage by commodity from 2015-Date                           ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
data <- readRDS("data/supplemental_offering_and_adoption.rds")
data$commodity_code <- ifelse(data$commodity_code %in% 22,21,data$commodity_code)
data$commodity_code <- ifelse(data$commodity_code %in% 59,51,data$commodity_code)
data$commodity_code <- ifelse(data$commodity_code %in% c(229,230,231,232,233,234,235,236),229,data$commodity_code)
data$commodity_code <- ifelse(data$commodity_code %in% c(88,332,33,32,102,107),88,data$commodity_code)

setDT(data)
data[, sco_use   := sco]
data[, eco90_use := eco90]
data[, eco95_use := eco95]
data[, eco_use   := eco90 + eco95]

data[, avail_eco := insured_acres*avail_aph*as.numeric(avail_eco90 + avail_eco95 >0)]
data[, avail_sco := insured_acres*avail_aph*avail_sco]
data[, avail_eco90 := insured_acres*avail_aph*avail_eco90]
data[, avail_eco95 := insured_acres*avail_aph*avail_eco95]
data[, avail_aph := insured_acres*avail_aph]

data <- data[,.(insured_acres = sum(insured_acres,na.rm=T),
                avail_aph = sum(avail_aph,na.rm=T),
                avail_sco = sum(avail_sco,na.rm=T),
                avail_eco90 = sum(avail_eco90,na.rm=T),
                avail_eco95 = sum(avail_eco95,na.rm=T),
                avail_eco = sum(avail_eco,na.rm=T),
                sco_use = sum(sco_use,na.rm=T),
                eco_use = sum(eco_use,na.rm=T),
                eco90_use = sum(eco90_use,na.rm=T),
                eco95_use = sum(eco95_use,na.rm=T)),
             by = c("commodity_code")]
dataAll <- as.data.frame(data)
dataAll$commodity_code <- 0
setDT(dataAll)
dataAll <- dataAll[,.(insured_acres = sum(insured_acres,na.rm=T),
                      avail_aph = sum(avail_aph,na.rm=T),
                      avail_sco = sum(avail_sco,na.rm=T),
                      avail_eco90 = sum(avail_eco90,na.rm=T),
                      avail_eco95 = sum(avail_eco95,na.rm=T),
                      avail_eco = sum(avail_eco,na.rm=T),
                      sco_use = sum(sco_use,na.rm=T),
                      eco_use = sum(eco_use,na.rm=T),
                      eco90_use = sum(eco90_use,na.rm=T),
                      eco95_use = sum(eco95_use,na.rm=T)),
                   by = c("commodity_code")]

data <- rbind(dataAll,data)
data <- as.data.frame(data)

data$sco_use   <- ifelse(data$avail_sco %in% 0,NA,(data$sco_use/data$avail_sco)*100)
data$eco90_use <- ifelse(data$avail_eco90 %in% 0,NA,(data$eco90_use/data$avail_eco90)*100)
data$eco95_use <- ifelse(data$avail_eco95 %in% 0,NA,(data$eco95_use/data$avail_eco95)*100)
data$eco_use   <- ifelse(data$avail_eco %in% 0,NA,(data$eco_use/data$avail_eco)*100)

for(xx in c("avail_sco","avail_eco90","avail_eco95","avail_eco")){
  data[, xx] <- ifelse(data$avail_aph %in% 0,NA, (data[, xx]/data$avail_aph)*100)
}

data$avail_aph <- ifelse(data$insured_acres %in% 0,NA, (data$avail_aph/data$insured_acres)*100)

for(xx in c("avail_aph","avail_sco","avail_eco90","avail_eco95","avail_eco","sco_use","eco_use","eco90_use","eco95_use")){
  data[, xx] <- ifelse(data[, xx] >100,100,data[, xx])
}

data <- data[!data$insured_acres %in% c(NA,0,NaN,Inf,-Inf),]
data <- data[order(-data$sco_use),]

SOBSCCTPU <- sobtpu_all
SOBSCCTPU$commodity_code <- ifelse(SOBSCCTPU$commodity_code %in% 22,21,SOBSCCTPU$commodity_code)
SOBSCCTPU$commodity_code <- ifelse(SOBSCCTPU$commodity_code %in% 59,51,SOBSCCTPU$commodity_code)
SOBSCCTPU$commodity_code <- ifelse(SOBSCCTPU$commodity_code %in% c(229,230,231,232,233,234,235,236),229,SOBSCCTPU$commodity_code)
SOBSCCTPU$commodity_code <- ifelse(SOBSCCTPU$commodity_code %in% c(88,332,33,32,102,107),88,SOBSCCTPU$commodity_code)

SOBSCCTPU <- SOBSCCTPU[SOBSCCTPU$reporting_level_type %in% "Acres",]
SOBSCCTPU <- SOBSCCTPU[SOBSCCTPU$commodity_year %in% 2015:study_environment$year_end,]
setDT(SOBSCCTPU)
SOBSCCTPU <- as.data.frame(SOBSCCTPU[,.(insured_acres_sob = sum(net_reporting_level_amount,na.rm=T)), by = c("commodity_code")])

data <- dplyr::full_join(SOBSCCTPU,data,by = c("commodity_code"))
data$insured_acres_sob <- ifelse(data$commodity_code %in% 0,sum(SOBSCCTPU$insured_acres_sob,na.rm=T),data$insured_acres_sob)

data <- data[order(-data$sco_use),]

comname <- fcip_recodes_commodity_groupings[!fcip_recodes_commodity_groupings$commodity_code %in% c(22,59,230,231,232,233,234,235,236,184,212,265,266,804,9110,1020,116,803,1191,
                                           847,802,801,208,209,210,24,308,830,73,1010,207,115,267,284,192,76,805,9999,332,33,32,102,107),]
data <- dplyr::full_join(comname[c("commodity_code","commodity_name")],data,by = c("commodity_code"))
data <- data[!data$commodity_code %in% 9999,]
data$commodity_name <- ifelse(data$commodity_code %in% 0,"All Commodities",data$commodity_name)

data <- data[order(-data$sco_use),]
data <- data[order(-data$insured_acres),]

data <- data[!data$insured_acres %in% c(NA,0,NaN,Inf,-Inf),]
data <- data[!data$avail_aph %in% c(NA,0,NaN,Inf,-Inf),]
data <- data[!data$avail_sco %in% c(NA,0,NaN,Inf,-Inf),]
data <- data[!data$sco_use %in% c(NA,0,NaN,Inf,-Inf),]

data$commodity_name <- ifelse(data$commodity_name %in% "Grain Sorghum","Sorghum",data$commodity_name)
data$commodity_name <- ifelse(data$commodity_name %in% "Pasture,Rangeland,Forage","All Pasture, Rangeland, and Forage (Including seeding)",data$commodity_name)
data$commodity_name <- ifelse(data$commodity_name %in% "Flue Cured Tobacco","Tobacco",data$commodity_name)
saveRDS(data,paste0("data-raw/output/figure_data/crop_availability.rds"))

function(){
  wb <- openxlsx::loadWorkbook(paste0("data-raw/output/supplemental_protection_RESULTS.xlsx"))
  openxlsx::writeData(wb, sheet = "crop_avail",
                      readRDS(paste0("data-raw/output/figure_data/crop_availability.rds"))[c("commodity_code","commodity_name","insured_acres","avail_aph","avail_sco","avail_eco","sco_use","eco_use")]
                      , colNames = T)
  openxlsx::saveWorkbook(wb,paste0("data-raw/output/supplemental_protection_RESULTS.xlsx"),overwrite = T)
}

#--------------------------------------------------------------------------------------------------------------
# Table: Means and Standard Deviations of US Federal Crop Insurance Pools (2015-Date)                       ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
sob_book <- readRDS("data/supplemental_offering_and_adoption.rds")
sob_book <- sob_book[sob_book$avail_aph %in% 1,]
sob_book <- sob_book[!(sob_book$avail_sco %in% 0 & sob_book$avail_eco90 %in% 0 & sob_book$avail_eco95 %in% 0),]
sob_book <- sob_book[sob_book$commodity_year %in% 2015:study_environment$year_end,]

sob_book <- dplyr::inner_join(unique(as.data.frame(sob_book)[c("commodity_year", "state_code", "county_code", "commodity_code")]),sobcov_all)
sob_book$net_acre_qty <- ifelse(grepl("Acres",sob_book$quantity_type),sob_book$net_reported_quantity,0)
sob_book$edr_acre_qty <- ifelse(grepl("Acres",sob_book$quantity_type),sob_book$endorsed_companion_acres,0)

sob_book$commodity_code <- ifelse(sob_book$commodity_code %in% 22,21,sob_book$commodity_code)
sob_book$commodity_code <- ifelse(sob_book$commodity_code %in% 59,51,sob_book$commodity_code)
sob_book$commodity_code <- ifelse(sob_book$commodity_code %in% c(229,230,231,232,233,234,235,236),229,sob_book$commodity_code)
sob_book$commodity_code <- ifelse(sob_book$commodity_code %in% c(88,332,33,32,102,107),88,sob_book$commodity_code)
sob_book <- sob_book[sob_book$insurance_plan_code %in% c(31:33,87:89,1:3,90),]

sob_book <- sob_book[!sob_book$liability_amount %in% c(0,NA,Inf,-Inf,NaN),]
sob_book_crop <- sob_book[sob_book$commodity_code %in% unique(readRDS("data-raw/draw_list.rds")[[1]]$commodity_code),]
sob_book00 <- sob_book
sob_book00$commodity_code <- 0
#sob_book <- rbind(sob_book00,sob_book,sob_book_crop)
sob_book <- rbind(sob_book00,sob_book_crop)

county_crops <- dplyr::inner_join(unique(sob_book[c("state_code", "county_code","commodity_code")]),readRDS("data-raw/output/figure_data/crop_availability.rds"))
county_crops <- unique(county_crops[c("state_code", "county_code","commodity_name","commodity_code")])
saveRDS(county_crops,file="data-raw/output/figure_data/county_crops.rds")

setDT(sob_book)

sob_book <- sob_book[,.(
  pol_prem_cnt = sum(policies_earning_premium_count,na.rm=T)/1000,
  unit_prem_cnt = sum(units_earning_premium_count,na.rm=T)/1000,
  net_acre_qty = sum(net_reported_quantity,na.rm=T)/1000000,
  edr_acre_qty = sum(endorsed_companion_acres,na.rm=T)/1000000,
  liability_amt = sum(liability_amount,na.rm=T)/1000000000,
  total_prem = sum(total_premium_amount,na.rm=T)/1000000000,
  subsidy = sum(subsidy_amount,na.rm=T)/1000000000,
  indem_amt = sum(indemnity_amount,na.rm=T)/1000000000),
  by = c("commodity_year","commodity_code")]

fcip_book <- as.data.frame(rfcip::get_sob_data(year = unique(sob_book$commodity_year),comm_cat = "S",group_by = c("crop")))
fcip_book$commodity_code <- as.numeric(fcip_book$commodity_code)
fcip_book$commodity_year <- as.numeric(fcip_book$commodity_year)
fcip_book$quantity <- ifelse(!fcip_book$quantity_type %in% "Acres",0,fcip_book$quantity)
fcip_book_crop <- fcip_book[fcip_book$commodity_code %in% unique(readRDS("data-raw/draw_list.rds")[[1]]$commodity_code),]
fcip_book00 <- fcip_book
fcip_book00$commodity_code <- 0
fcip_book <- rbind(fcip_book,fcip_book_crop,fcip_book00)

setDT(fcip_book)
fcip_book <- fcip_book[,.(
  fcip_liab = sum(liabilities,na.rm=T)/1000000000),
  by = c("commodity_year","commodity_code")]

sob_book <- dplyr::inner_join(fcip_book,sob_book)
setDT(sob_book)

sob_rep <- as.data.frame(
  sob_book[,.(
    sob_liab = sum(liability_amt,na.rm=T),
    fcip_liab = sum(fcip_liab,na.rm=T)),
    by = c("commodity_code")])

sob_book_mean <- as.data.frame(
  sob_book[,.(
    fcip_liab = mean((liability_amt/fcip_liab)*100,na.rm=T),
    pol_prem_cnt = mean(pol_prem_cnt,na.rm=T),
    unit_prem_cnt = mean(unit_prem_cnt,na.rm=T),
    net_acre_qty = mean(net_acre_qty,na.rm=T),
    edr_acre_qty = mean(edr_acre_qty,na.rm=T),
    liability_amt = mean(liability_amt,na.rm=T),
    total_prem = mean(total_prem,na.rm=T),
    subsidy = mean(subsidy,na.rm=T),
    indem_amt = mean(indem_amt,na.rm=T),
    lr = mean(indem_amt/total_prem,na.rm=T)),
    by = c("commodity_code")])
sob_book_mean <- sob_book_mean |> tidyr::gather(Variable, mean, 2:ncol(sob_book_mean))

sob_book_sd <- as.data.frame(
  sob_book[,.(
    fcip_liab = sd((liability_amt/fcip_liab)*100,na.rm=T),
    pol_prem_cnt = sd(pol_prem_cnt,na.rm=T),
    unit_prem_cnt = sd(unit_prem_cnt,na.rm=T),
    net_acre_qty = sd(net_acre_qty,na.rm=T),
    edr_acre_qty = sd(edr_acre_qty,na.rm=T),
    liability_amt = sd(liability_amt,na.rm=T),
    total_prem = sd(total_prem,na.rm=T),
    subsidy = sd(subsidy,na.rm=T),
    indem_amt = sd(indem_amt,na.rm=T),
    lr = sd(indem_amt/total_prem,na.rm=T)),
    by = c("commodity_code")])
sob_book_sd <- sob_book_sd |> tidyr::gather(Variable, sd, 2:ncol(sob_book_sd))


sob_book_lr <- as.data.frame(
  sob_book[,.(mean = sum(indem_amt,na.rm=T)/sum(total_prem,na.rm=T), Variable="lr_cum"),
           by = c("commodity_code")])

sob_book_rep <- as.data.frame(
  sob_book[,.(mean = (sum(liability_amt,na.rm=T)/sum(fcip_liab,na.rm=T))*100, Variable="liab_rep"),
           by = c("commodity_code")])

sob_book <- dplyr::full_join(sob_book_mean,sob_book_sd)
sob_book <- dplyr::full_join(sob_book,sob_book_lr)
sob_book <- dplyr::full_join(sob_book,sob_book_rep)

comname <- fcip_recodes_commodity_groupings

sob_book$commodity_code <- as.character(factor(sob_book$commodity_code,levels = c(0,as.numeric(comname$commodity_code)),
                                        labels = c("All",comname$commodity_name)))

sob_book$commodity_code <- ifelse(sob_book$commodity_code %in% "Grain Sorghum","Sorghum",sob_book$commodity_code)
sob_book$commodity_code <- ifelse(sob_book$commodity_code %in% "Pasture,Rangeland,Forage","All Pasture, Rangeland, and Forage (Including seeding)",sob_book$commodity_code)
sob_book$commodity_code <- ifelse(sob_book$commodity_code %in% "Flue Cured Tobacco","Tobacco",sob_book$commodity_code)

saveRDS(sob_book,file="data-raw/output/figure_data/book_of_business.rds")

rm(sob_book,sob_book_sd,sob_book_mean,fcip_book_crop,fcip_book,sob_book_crop,sob_book00);gc()

agentdata <- as.data.frame(
  data.table::rbindlist(
    lapply(
      c(2015:study_environment$year_end),
      function(commodity_year){
        return(readRDS(paste0("data/cleaned_agents_data/cleaned_agents_data_",commodity_year,".rds")))}), fill = TRUE));gc()

agentdata <- agentdata[c("commodity_year","state_code","county_code","commodity_code","type_code","practice_code",
                         "unit_structure_code","insurance_plan_code","coverage_level_percent",
                         "sco","eco90","eco95","rate_yield", "approved_yield", "calibrated_yield", "projected_price","insured_acres")]

expected <- as.data.frame(
  data.table::rbindlist(
    lapply(
      c(2015:study_environment$year_end),
      function(commodity_year){
        expected <- readRDS(paste0("data-raw/expected/expected_",commodity_year,".rds"))
        expected <- unique(as.data.frame(expected)[c("commodity_year","state_code","county_code","commodity_code","type_code","practice_code",
                                      "unit_structure_code","insurance_plan_code","coverage_level_percent")])
        return(expected)}), fill = TRUE));gc()

agentdata <- dplyr::inner_join(expected,agentdata,by=c("commodity_year","state_code","county_code","commodity_code","type_code",
                                                       "practice_code","unit_structure_code","insurance_plan_code","coverage_level_percent"))

sob_sum <- sobtpu_all
sob_sum$commodity_code <- ifelse(sob_sum$commodity_code %in% 22,21,sob_sum$commodity_code)
sob_sum$commodity_code <- ifelse(sob_sum$commodity_code %in% 59,51,sob_sum$commodity_code)
sob_sum$commodity_code <- ifelse(sob_sum$commodity_code %in% c(229,230,231,232,233,234,235,236),229,sob_sum$commodity_code)
sob_sum$commodity_code <- ifelse(sob_sum$commodity_code %in% c(88,332,33,32,102,107),88,sob_sum$commodity_code)
sob_sum <- sob_sum[sob_sum$insurance_plan_code %in% c(1:3,90),]
sob_sum$insured_acres <- ifelse(grepl("Acres",sob_sum$reporting_level_type),sob_sum$net_reporting_level_amount,0)
sob_sum$insurance_plan_code <- ifelse(sob_sum$insurance_plan_code %in% 90,1,sob_sum$insurance_plan_code);gc()

setDT(sob_sum)
sob_sum <- as.data.frame(
  sob_sum[,.(
    net_acre_qty = sum(insured_acres,na.rm=T),
    liability_amt = sum(liability_amount,na.rm=T),
    total_prem = sum(total_premium_amount,na.rm=T),
    subsidy = sum(subsidy_amount,na.rm=T),
    indem_amt = sum(indemnity_amount,na.rm=T)),
    by = c("commodity_year","state_code","county_code","commodity_code","type_code","practice_code","unit_structure_code","insurance_plan_code","coverage_level_percent")]);gc()

agentdata <- dplyr::inner_join(agentdata,sob_sum,by=c("commodity_year","state_code","county_code","commodity_code","type_code","practice_code",
                                                      "unit_structure_code","insurance_plan_code","coverage_level_percent"))

agentdata$liability_amt <- agentdata$liability_amt/agentdata$net_acre_qty
agentdata$total_prem    <- agentdata$total_prem/agentdata$net_acre_qty
agentdata$subsidy       <- agentdata$subsidy/agentdata$net_acre_qty
agentdata$indem_amt     <- agentdata$indem_amt/agentdata$net_acre_qty
agentdata$area_insured  <- agentdata$net_acre_qty
agentdata <- agentdata[agentdata$commodity_code %in% unique(readRDS("data-raw/draw_list.rds")[[1]]$commodity_code),]
agentdata00 <- agentdata
agentdata00$commodity_code <- 0
agentdata <- rbind(agentdata,agentdata00)

rm(agentdata00)

setDT(agentdata)

sob_rep <- dplyr::full_join(sob_rep,as.data.frame(agentdata[, .(agent_liab = sum((liability_amt*net_acre_qty)/1000000000, na.rm = TRUE)), by = c("commodity_code")]))
sob_rep <- sob_rep |> tidyr::gather(Variable, mean, 2:ncol(sob_rep))

data_mean <- as.data.frame(
  agentdata[, .(insured_acres = mean(insured_acres/1000, na.rm = TRUE),
                liability_amt = mean(liability_amt, na.rm = TRUE),
                premium_rate = mean(total_prem/liability_amt, na.rm = TRUE),
                Subsidy_Percent = mean(subsidy/total_prem, na.rm = TRUE),
                sco = mean(sco*100, na.rm = TRUE),
                eco95 = mean(eco95*100, na.rm = TRUE),
                eco90 = mean(eco90*100, na.rm = TRUE),
                projected_price = mean(projected_price, na.rm = TRUE),
                approved_yield = mean(approved_yield, na.rm = TRUE),
                rate_yield = mean(rate_yield, na.rm = TRUE),
                calibrated_yield = mean(calibrated_yield, na.rm = TRUE),
                obs_n = length(commodity_year)), by = c("commodity_code")])
data_mean <- data_mean |> tidyr::gather(Variable, mean, 2:ncol(data_mean))

data_sd <- as.data.frame(
  agentdata[, .(insured_acres = sd(insured_acres/1000, na.rm = TRUE),
                liability_amt = sd(liability_amt, na.rm = TRUE),
                premium_rate = sd(total_prem/liability_amt, na.rm = TRUE),
                Subsidy_Percent = sd(subsidy/total_prem, na.rm = TRUE),
                sco = sd(sco*100, na.rm = TRUE),
                eco95 = sd(eco95*100, na.rm = TRUE),
                eco90 = sd(eco90*100, na.rm = TRUE),
                projected_price = sd(projected_price, na.rm = TRUE),
                approved_yield = sd(approved_yield, na.rm = TRUE),
                rate_yield = sd(rate_yield, na.rm = TRUE),
                calibrated_yield = sd(calibrated_yield, na.rm = TRUE)), by = c("commodity_code")])
data_sd <- data_sd |> tidyr::gather(Variable, sd, 2:ncol(data_sd))

weighted_sd <- function(x, w) {
  sqrt(sum(w * (x - weighted.mean(x, w))^2) / sum(w))
}

Mode <- function(x,w) {
  ux <- rep(x, times = w)
  ux <- unique(ux)
  ux[which.max(tabulate(match(x, ux)))]
}

data_coverage_level_percent <- as.data.frame(
  agentdata[, .(mean = weighted.mean(x=coverage_level_percent,w=insured_acres, na.rm = TRUE),
                sd = weighted_sd(x=coverage_level_percent,w=insured_acres),
                Variable="coverage_level_percent"), by = c("commodity_code")]);gc()

data_plan <- as.data.frame(
  agentdata[, .(insured_acres = sum(x=insured_acres)), by = c("commodity_code","insurance_plan_code")]);gc()
data_plan <- data_plan |> group_by(commodity_code) |>
  mutate(insured_acresR = rank(-insured_acres)) |> as.data.frame(.)
data_plan <- data_plan[data_plan$insured_acresR %in% 1,]
setDT(data_plan)
data_plan <- as.data.frame(
  data_plan[, .(mean = mean(x=insurance_plan_code),
                Variable="insurance_plan_code"), by = c("commodity_code")]);gc()

data_unit <- as.data.frame(
  agentdata[, .(insured_acres = sum(x=insured_acres)), by = c("commodity_code","unit_structure_code")]);gc()
data_unit$unit_structure_code <- as.character(factor(data_unit$unit_structure_code,
                                           levels = c("EU","EP", "OU", "BU", "UA", "UD", "WU", "EC"),
                                           labels = c("EU","EU", "OU", "BU", "OU", "OU", "EU", "EU")))
setDT(data_unit)
data_unit <- as.data.frame(
  data_unit[, .(insured_acres = sum(x=insured_acres)), by = c("commodity_code","unit_structure_code")]);gc()

data_unit <- data_unit |> group_by(commodity_code) |>
  mutate(insured_acresR = rank(-insured_acres)) |> as.data.frame(.)
data_unit <- data_unit[data_unit$insured_acresR %in% 1,]
data_unit$Variable <- "unit_structure_code"
data_unit$mean <- as.numeric(as.character(factor(data_unit$unit_structure_code,levels = c("OU","BU","EU"),labels = 1:3)))
data_unit <- data_unit[c("commodity_code","mean","Variable")];gc()

data <- dplyr::full_join(data_sd,data_mean)
data <- dplyr::full_join(data,data_plan)
data <- dplyr::full_join(data,data_coverage_level_percent)
data <- dplyr::full_join(data,sob_rep)
data <- dplyr::full_join(data,data_unit)
comname <- fcip_recodes_commodity_groupings
data$commodity_code <- as.character(factor(data$commodity_code,levels = c(0,as.numeric(comname$commodity_code)),
                                    labels = c("All",comname$commodity_name)))

saveRDS(data,file="data-raw/output/figure_data/agent_summary.rds")

function(){

  sob_book <- readRDS("data-raw/output/figure_data/book_of_business.rds")
  wb <- openxlsx::loadWorkbook("data-raw/output/supplemental_protection_RESULTS.xlsx")
  openxlsx::writeData(wb, sheet = "book_of_business",sob_book[c("commodity_code" ,"Variable","mean","sd")] , colNames = T)
  openxlsx::saveWorkbook(wb,"data-raw/output/supplemental_protection_RESULTS.xlsx",overwrite = T)

  data <- readRDS("data-raw/output/figure_data/agent_summary.rds")
  wb <- openxlsx::loadWorkbook("data-raw/output/supplemental_protection_RESULTS.xlsx")
  openxlsx::writeData(wb, sheet = "agent_summary",data[c("commodity_code","Variable","mean","sd")] , colNames = T)
  openxlsx::saveWorkbook(wb,"data-raw/output/supplemental_protection_RESULTS.xlsx",overwrite = T)
}

#--------------------------------------------------------------------------------------------------------------
# Table 2 : main impacts                                                                                    ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
function(){
  data <- readRDS("data-raw/output/summary/impact_main.rds")
  data <- data[data$aggregation %in% c("avg_valueT","avg_chglvl00T","avg_chgpct00T"),]
  data <- data[data$combination %in% c("Basic+CURRENT","Basic only","Basic+SCO8665","Basic+ECO9044",
                                       "Basic+ECO9544","Basic+SCO8665+ECO9044","Basic+SCO8665+ECO9544"),]

  table(data$aggregation)

  data <- data[data$baseline %in% c("2021-2023","2015-2023"),
               c("aggregation","baseline","combination","variable","est","est_se","est_pv")]
  wb <- openxlsx::loadWorkbook(paste0("data-raw/output/supplemental_protection_RESULTS.xlsx"))
  openxlsx::writeData(wb, sheet = "impact_main",data , colNames = T)
  openxlsx::saveWorkbook(wb,paste0("data-raw/output/supplemental_protection_RESULTS.xlsx"),overwrite = T)
}
#--------------------------------------------------------------------------------------------------------------
# Figure : main impacts                                                                                     ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()

dodge <- position_dodge(width = 0)

data <- readRDS("data-raw/output/summary/impact_main.rds")
data <- data[data$aggregation %in% c("avg_valueT","avg_chglvl00T","avg_chgpct00T"),]
data <- data[data$combination %in% c("Basic+CURRENT","Basic only","Basic+SCO8665","Basic+ECO9044",
                                     "Basic+ECO9544","Basic+SCO8665+ECO9044","Basic+SCO8665+ECO9544"),]

table(data$aggregation)

data <- data[data$baseline %in% c("2015-2023"),
             c("aggregation","baseline","combination","variable","est","est_se","est_pv")]

data <- data[data$aggregation %in% "avg_chgpct00T",]
data <- data[data$variable %in% c("itp","rrp1","rrp3","SimrateP","rrm1","rrm2"),]
data <- data[!data$combination %in% c("Basic+CURRENT"),]

data$variable <- factor(
  data$variable,
  levels = c("itp","rrp1","rrp3","SimrateP","rrm1","rrm2"),
  labels = c("(a) Revenue transfer potential\nrelative to no insurance (%)",
             "(b) Overall risk reduction potential\nrelative to no insurance (%)",
             "(c) Relative downside risk reduction potential\nrelative to no insurance (%)",
             "(d) Premium paid per dollar\nof liability (cents)",
             "(e) Overall risk reduction potential change\nper change in in premium paid (%)",
             "(f) Relative downside reduction potential change\nper change in in premium paid (%)"))

data$combination <- factor(
  data$combination,
  levels = c("Basic only",	"Basic+SCO8665",	"Basic+ECO9044",	"Basic+ECO9544",	"Basic+SCO8665+ECO9044",	"Basic+SCO8665+ECO9544"),
  labels = c("Basic plan without stacking",
             "Basic plan stacked with SCO",
             "Basic plan stacked with ECO90",
             "Basic plan stacked with ECO95",
             "Basic plan stacked with SCO and ECO95",
             "Basic plan stacked with SCO and ECO90"
  ))

data$est_se <- ifelse(data$est_pv > 0.1,NA, data$est_se)
data$Significance <- ifelse(data$est_pv < 0.10,1, 2)
data$Significance <- ifelse(data$est_se %in% NA,2, data$Significance)
data$Significance <- factor(data$Significance,levels = 1:2,labels = c("p < 0.10","p >= 0.10"))

fig <- ggplot(data=data,aes(x=combination,y=est,group=combination,shape=combination)) +
  geom_hline(yintercept = 0,size = 0.3,color = "black",lty="solid") +
  geom_errorbar(aes(ymin = est - est_se*1.96, ymax = est + est_se*1.96,width = 0.30),size=1,position = dodge,color="#FFC425") +
  geom_point(size=2.5,position = dodge,fill="#00583D",color="#00583D") +
  labs(subtitle = "",
       x = "Scenario:100% adoption of specified combination on insured acres at current subsidy and coverage levels\n",
       y = "\nExpressed changes over baseline", fill='',caption = "") +
  scale_shape_manual(name="Significance",values = c(21:25,8)) +
  facet_wrap(~variable,ncol = 2,scales = "free_x") +
  myTheme +
  theme(panel.grid.major.x = element_line(color = "gray",size = 0.2,linetype = 5)) +
  theme(axis.text.x = element_text(size=8,color="black"),
        axis.text.y = element_text(size=8,color="black"),
        #axis.title.y= element_blank(),
        #axis.title.x= element_blank(),
        strip.text = element_text(size = 8),
        legend.text= element_text(size=8),
        legend.title= element_text(size=8),
        legend.background = element_rect(color="black"),
        legend.position="none",legend.key.size = unit(0.43,"cm"))+ coord_flip()

ggsave("data-raw/output/figure/study1_impact_main.png", fig,dpi = 1200,width = 7.5, height = 7.5)

#--------------------------------------------------------------------------------------------------------------
# Figure : Impact Annual                                                                                    ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
data <- readRDS("data-raw/output/summary/impact_main.rds")
data <- data[data$aggregation %in% c("avg_valueT","value"),]
data <- data[data$combination %in% c("Basic+CURRENT","Basic+SCO8665+ECO9544"),]
data <- data[data$variable %in% c("itp","rrp1","rrp2","Simrate","Simsuby","SimrateP"),]
# data <- data[data$variable %in% "its",]
data$grp <- factor(data$variable,
                   levels = c("itp","rrp1","rrp2","sner1","sner2","Simrate","Simsuby","SimrateP","costJ"),
                   labels = c("Revenue transfer potential\nrelative to no insurance [%]",
                              "Overall revenue variability reduction\npotential relative to no insurance [%]",
                              "Downside revenue variability reduction\npotential relative to no insurance [%]",
                              "Risk Reduction Efficiency Ratio\n(Overall risk) [%]",
                              "Risk Reduction Efficiency Ratio\n(Downside risk) [%]",
                              "Actuarially fair premium per\ndollar of liability [cents]",
                              "Subsidy per dollar of actuarially\nfair premium [cents]",
                              "Premium paid per dollar of\nliability [cents]",
                              "Total program cost (billion $)"))

data$combination <- factor(data$combination,
                           levels = c("Basic+CURRENT","Basic+SCO8665+ECO9544"),
                           labels = c("Basline at 2015-23 observed participation","Basic + SCO + ECO95 at full participation"))

data <- tidyr::separate(data,"baseline",into=c("year"),sep="-",remove = F)

lnes <- c("95% CI for 2015-23 average" = "longdash")

fig <- ggplot(data=data[!data$baseline %in% c("2015-2023","2021-2023"),],
              aes(x=year,y=est,group=combination,fill=combination,color=combination)) +
  geom_errorbar(aes(ymin = est - est_se*1.96, ymax = est + est_se*1.96,width = 0.30)) +
  geom_point(size=2) +
  geom_line() +
  geom_hline(data=data[data$baseline %in% c("2015-2023"),],
             aes(yintercept=est+ est_se*1.96,linetype="95% CI for 2015-23 average",
                 group=combination,fill=combination,color=combination)) +
  geom_hline(data=data[data$baseline %in% c("2015-2023"),],
             aes(yintercept=est- est_se*1.96,linetype="95% CI for 2015-23 average",
                 group=combination,fill=combination,color=combination)) +
  facet_wrap(~grp,ncol = 3,scales = "free_y") +
  scale_linetype_manual(name="",values = lnes) +
  scale_colour_manual(name="","Index [1=no insurance]",values = c("orange","blue")) +
  scale_fill_manual(name="","Index [1=no insurance]",values = c("orange","blue")) +
  myTheme +
  theme(axis.text.x = element_text(size=7,color="black",angle=90,vjust = 0.3),
        axis.text.y = element_text(size=7,color="black"),
        axis.title.y= element_blank(),
        axis.title.x= element_blank(),
        strip.text = element_text(size = 7),
        legend.text= element_text(size=7),
        legend.title= element_text(size=7),
        legend.background = element_rect(color="black"),
        legend.position="bottom",legend.key.size = unit(0.43,"cm"))
ggsave("data-raw/output/figure/study1_impact_annual.png", fig,dpi = 1200,width = 7, height =6)

#--------------------------------------------------------------------------------------------------------------
# Figure : Impact incremental 2021-Date                                                                     ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
data <- readRDS("data-raw/output/summary/impact_incremental.rds")
data <- data[data$variable %in% c("rrm2"),]
data <- data[data$aggregation %in% c("avg_chgpct00T"),]

fig <- ggplot(data=data,aes(x=combination,y=est)) +
  geom_vline(xintercept = seq(0,100,10),linewidth = 0.2,color = "gray",lty=5) +
  geom_errorbar(aes(ymin = est - est_se*1.96, ymax = est + est_se*1.96,width = 2),color="blue") +
  geom_line(color="blue") +
  geom_point(size=3,color="firebrick") +
  labs(subtitle = "",x = "\nSupplemental Coverage Option (SCO) adoption rate (%)",
       y = "", fill='',caption = "") +
  scale_x_continuous(breaks = seq(0,100,5),labels = seq(0,100,5)) +
  ylim(0,1.5)+
  myTheme +
  theme(axis.text.x = element_text(size=7,color="black"),
        axis.text.y = element_text(size=7,color="black"),
        axis.title.y= element_blank(),
        #axis.title.x= element_blank(),
        strip.text = element_text(size = 7),
        legend.text= element_text(size=7),
        legend.title= element_text(size=7),
        legend.background = element_rect(color="black"),
        legend.position="bottom",legend.key.size = unit(0.43,"cm"))

ggsave("data-raw/output/figure/study1_impact_incremental.png", fig,dpi = 1200,width = 6, height = 4)
#--------------------------------------------------------------------------------------------------------------
# Figure : Impact heterogeneity                                                                             ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
data <- readRDS("data-raw/output/summary/impact_heterogeneity.rds")
data <- data[data$aggregation %in% c("avg_chgpct00T"),]
data <- data[data$disag %in% c("CROP","STRUCT","COV","RPYP","FCIP"),]
data <- data[data$baseline %in% c("2015-2023"),]
data <- data[data$combination %in% c("Basic+SCO8665+ECO9544"),]
data <- data[data$variable %in% c("rrm2","rrp2","SimrateP"),]

LAB <- data[data$variable %in% "rrm2",]
LAB$disagx <- as.numeric(as.character(factor(LAB$disag,levels = c("FCIP","CROP","STRUCT","RPYP","COV"),
                                             labels = 1:5)))
LAB <- LAB[order(-LAB$disagx,-LAB$est),]
LABCOV <- LAB[LAB$disag %in% "COV",]
LABCOV <- LABCOV[order(LABCOV$level),]
LAB <- rbind(LABCOV,LAB[LAB$disag %in% "RPYP",],LAB[LAB$disag %in% "STRUCT",],LAB[LAB$disag %in% "CROP",],LAB[LAB$disag %in% "FCIP",])

comname <- unique(readRDS("data-raw/output/figure_data/crop_availability.rds")[c("commodity_name","commodity_code")])
comname <- comname[!as.numeric(comname$commodity_code) %in% NA,]
comname$commodity_name  <- ifelse(comname$commodity_code  %in% 0,"All listed crops",comname$commodity_name )

#LAB_CROP <- LAB[LAB$disag %in% "CROP",]

LAB$Xname <- as.character(factor(as.numeric(LAB$level),levels = as.numeric(comname$commodity_code),
                                 labels = comname$commodity_name))

LAB$Xname <- ifelse(LAB$Xname %in% "Grain Sorghum","Sorghum",LAB$Xname)
LAB$Xname <- ifelse(LAB$disag %in% "CROP",LAB$Xname,LAB$level)
LAB$Xname <- ifelse(LAB$disag %in% "FCIP","Full sample",LAB$Xname)
LAB$XID <- 1:nrow(LAB)
LAB <- LAB[c("Xname","XID","disag","level")]

LAB_all <- list()
for(xx in c("COV","RPYP","STRUCT","CROP","FCIP")){
  LAB_all[[xx]] <- LAB[LAB$disag %in% xx,]
}

LAB_all[[1]]$x <- c(1:nrow(LAB_all[[1]])) + 3

for(xx in 2:length(LAB_all)){
  LAB_all[[xx]]$x <- max(LAB_all[[xx-1]]$x) + c(1:nrow(LAB_all[[xx]])) + 3
}

LAB <- rbindlist(LAB_all, fill = TRUE)

data <- dplyr::inner_join(LAB,data)

mlab <- doBy::summaryBy(x~disag,data=data[data$variable %in% "rrm2",],FUN=max,keep.names = T)
mlab <- mlab[!mlab$disag %in% "FCIP",]
mlab$variable <- "rrm2"
mlab$est <- NA
mlabU <- mlab;mlabU$disag_level <- "-------------------"
mlabl <- mlab;mlabl$disag_level <- "-------------------"
mlab$disag_level <- mlab$disag
mlabU$x <- mlabU$x+3
mlab$x  <- mlab$x+2
mlabl$x <- mlabl$x+1
mlab <- rbind(mlab,mlabl,mlabU)
mlab$Xname <- mlab$disag_level
data <- as.data.frame(rbindlist(list(data,mlab), fill = TRUE))

#data$x <- factor(data$XID,levels = LAB$XID, labels = as.character(LAB$Xname))

data$grp <- factor(data$variable,
                   levels = c("rrm2","rrp2","SimrateP"),
                   labels = c("Downside revenue risk reduction\npotential change per change\nin premium paid (%)",
                              "Change in downside revenue\nrisk reduction potential (%)",
                              "Change in producer premium\npaid per dollar of liability (%)"))
#data <- data[!data$grp %in% NA,]
dodge <- position_dodge(width = 0)

xlab <- unique(data[c("x","Xname")])
xlab$Xname <- as.character(xlab$Xname)

xlab$Xname <- ifelse(xlab$Xname %in% "RP","RP",xlab$Xname)
xlab$Xname <- ifelse(xlab$Xname %in% "RPHPE","RPHPE",xlab$Xname)
xlab$Xname <- ifelse(xlab$Xname %in% "YP","YP/APH",xlab$Xname)
xlab$Xname <- ifelse(xlab$Xname %in% "OU","Optional",xlab$Xname)
xlab$Xname <- ifelse(xlab$Xname %in% "BU","Basic",xlab$Xname)
xlab$Xname <- ifelse(xlab$Xname %in% "EU","Enterprise",xlab$Xname)
xlab$Xname <- ifelse(xlab$Xname %in% "COV","Coverage level",xlab$Xname)
xlab$Xname <- ifelse(xlab$Xname %in% "CROP","Crop",xlab$Xname)
xlab$Xname <- ifelse(xlab$Xname %in% "RPYP","Insurance plan",xlab$Xname)
xlab$Xname <- ifelse(xlab$Xname %in% "STRUCT","Unit structure",xlab$Xname)

data$est_se <- ifelse(data$est_pv > 0.1,NA, data$est_se)
data$Significance <- ifelse(data$est_pv < 0.10,1, 2)
data$Significance <- ifelse(data$est_se %in% NA,2, data$Significance)
data$Significance <- factor(data$Significance,levels = 1:2,labels = c("p < 0.10","p >= 0.10"))

fig <- ggplot(data=data,aes(x=x,y=est,group=Significance,fill=Significance,color=Significance,shape=Significance)) +
  geom_hline(yintercept = 0,size = 0.3,color = "black",lty="solid") +
  geom_errorbar(aes(ymin = est - est_se*1.96, ymax = est + est_se*1.96,width = 0.30),position = dodge) +
  geom_point(size=2,position = dodge) +
  labs(subtitle = "",x = "", y = "", fill='',caption = "") +
  scale_x_continuous(breaks = xlab$x,labels = xlab$Xname) +
  scale_shape_manual(name="Significance",values = c(21,22,8)) +
  scale_colour_manual(name="Significance",values =  c("blue","gray")) +
  scale_fill_manual(name="Significance",values =  c("blue","gray")) +
  facet_wrap(~grp,ncol = 3,scales = "free_x",) +
  myTheme +
  theme(panel.grid.major.x = element_line(color = "gray",size = 0.2,linetype = 5)) +
  theme(axis.text.x = element_text(size=8,color="black"),
        axis.text.y = element_text(size=8,color="black"),
        #axis.title.y= element_blank(),
        #axis.title.x= element_blank(),
        strip.text = element_text(size = 8),
        legend.text= element_text(size=8),
        legend.title= element_text(size=8),
        legend.background = element_rect(color="black"),
        legend.position="bottom",legend.key.size = unit(0.43,"cm"))+ coord_flip()

ggsave("data-raw/output/figure/study1_impact_heterogeneity.png", fig,dpi = 1200,width = 6.3, height = 7.7)

#--------------------------------------------------------------------------------------------------------------
# Figure : Impact state                                                                                     ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()

us_sf <- urbnmapr::get_urbn_map(map = "states", sf = TRUE)
us_sf$state_fips <- as.numeric(us_sf$state_fips)

LAB <- readRDS("data-raw/output/figure_data/crop_availability.rds")
LAB <- doBy::summaryBy(insured_acres_sob~Commodity.Name+commodity_code,FUN=c(sum,length),data=LAB)
LAB <- LAB[order(-LAB$insured_acres_sob.length,-LAB$insured_acres_sob.sum),]

data <- readRDS("data-raw/output/summary/impact_heterogeneity.rds")
data$level <- as.character(data$level)
data <- data[data$disag %in% c("CROP_STATE","STATE"),]
data <- data[data$baseline %in% c("2015-2023"),]
data <- data[data$combination %in% c("Basic+SCO8665+ECO9544"),]
data <- data[data$variable %in% c("itp","rrm2","rrp2","SimrateP"),]
data <- data[data$aggregation %in% c("avg_chgpct00T"),]
STATE <- data[data$disag %in% c("STATE"),]
data  <- data[data$disag %in% c("CROP_STATE"),]
data  <- tidyr::separate(data,"level",into=c("crop","state_fips"),sep="-")
data$state_fips <- as.numeric(data$state_fips)
data$crop <- as.numeric(data$crop)
STATE$state_fips <- as.numeric(STATE$level)
STATE$crop <- 0
data <- rbind(data[c("crop","state_fips","variable","est")],STATE[c("crop","state_fips","variable","est")])

CROP <- readRDS("data-raw/output/summary/impact_heterogeneity.rds")
CROP$level <- as.character(CROP$level)
CROP <- CROP[CROP$disag %in% c("CROP","FCIP"),]
CROP <- CROP[CROP$aggregation %in% c("avg_chgpct00T"),]
CROP <- CROP[CROP$baseline %in% c("2015-2023"),]
CROP <- CROP[CROP$combination %in% c("Basic+SCO8665+ECO9544"),]
CROP <- CROP[CROP$variable %in% c("itp","rrp2","rrm2","SimrateP"),]
CROP$txt <- paste0(round(CROP$est,2)," (",round(CROP$est_se,2),")")
CROP$commodity_code <-  as.numeric(CROP$level)
CROP$commodity_code <- ifelse(CROP$disag %in% "FCIP",0,CROP$commodity_code)
LAB <- dplyr::inner_join(LAB[LAB$commodity_code %in% unique(data$crop),],CROP[c("commodity_code","variable","txt")])

comname <- unique(readRDS("data-raw/output/figure_data/crop_availability.rds")[c("commodity_name","commodity_code")])
comname <- comname[!as.numeric(comname$commodity_code) %in% NA,]

LAB$commodity_name <- as.character(factor(as.numeric(LAB$commodity_code),levels = c(0,as.numeric(comname$commodity_code)),
                                 labels = c("All listed crops",comname$commodity_name)))
LAB$commodity_name <- paste0(LAB$commodity_name," ",LAB$txt)

lapply(
  c("itp","rrp2","SimrateP","rrm2"),
  function(variable){
    # variable <- "itp"
    dataX <- data[data$variable %in% variable,]
    LABX <- LAB[LAB$variable %in% variable,]
    LABX <- LABX[order(-LABX$insured_acres_sob.length,-LABX$insured_acres_sob.sum),]

    dataX$crop <- factor(dataX$crop, levels = as.numeric(LABX$commodity_code), labels = 1:nrow(LABX))
    dataX$crop <- factor(dataX$crop,levels = 1:nrow(LABX), labels = as.character(LABX$commodity_name))

    table(cut(dataX$est,unique(round(c(min(dataX$est,na.rm=T),quantile(unique(dataX$est),seq(0,1,0.2)),max(dataX$est,na.rm=T)),2))))

    if(variable %in% "rrm2"){

      dataX$cat <- cut(dataX$est,c(0,0.10,0.15,0.20,0.50,1,Inf))
      clo <- c("orange","yellow","greenyellow","green","olivedrab","darkgreen")
      table(dataX$cat)
      lbli <- "Percentage"
    }
    if(variable %in% "rrp2"){
      # table(cut(dataX$est,unique(round(c(min(dataX$est,na.rm=T),quantile(unique(dataX$est),seq(0,1,0.2)),max(dataX$est,na.rm=T)),2))))
      dataX$cat <- cut(dataX$est,c(0,5,7.5,10,20,Inf))
      clo <- c("orange","yellow","greenyellow","green","olivedrab","darkgreen")
      table(dataX$cat)
      lbli <- "Percentage"
    }
    if(variable %in% "itp"){
      dataX$cat <- cut(dataX$est,c(0,40,60,80,100,Inf))
      clo <- c("thistle","violet","purple","#4B0082","blue")
      table(dataX$cat)
      lbli <- "Percentage"
    }
    if(variable %in% "SimrateP"){
      dataX$cat <- cut(dataX$est,c(0,20,40,60,80,100,Inf))
      table(dataX$cat)
      clo <- c("greenyellow","yellow","gold","orange","red","darkred")
      lbli <- "Percentage"
    }

    sf_object <- sf::st_as_sf(us_sf)
    sf_object <- sf_object |> left_join(dataX, by = "state_fips")
    sf_object <- sf_object[!sf_object$cat %in% c(NA,NaN,Inf,-Inf),]

    fig <- ggplot() +
      geom_sf(data = sf_object,aes(fill = cat), colour = NA,size = 0.2) +
      geom_sf(data = us_sf, colour = "black", fill = NA,size =0.1) +
      scale_fill_manual(values=clo,
                        na.value="white", name= lbli) +
      guides(fill = guide_legend(nrow=1)) +
      labs(subtitle = "",x = "", y = "", fill='',caption = "") +
      theme_bw() +
      facet_wrap(~crop,ncol = 3) +
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
      theme(legend.text= element_text(size=7),
            legend.title= element_text(size=7),
            legend.key.size = unit(0.2, 'cm'),
            legend.position="bottom",
            legend.background = element_blank(),
            plot.title = element_text(size = 8),
            axis.title.y = element_blank(), #
            axis.title.x = element_blank(), #
            axis.text.x  = element_blank(), #
            axis.text.y  = element_blank(),
            plot.caption = element_text(size=5,hjust = 0 ,vjust = 0, face = "italic"),
            strip.text = element_text(size = 7),
            strip.background = element_rect(fill = "white", colour = "black", size = 1))

    ggsave(paste0("data-raw/output/figure/study1_impact_spatial_",variable,".png"), fig,dpi = 1200,width = 6, height = 8)
    saveRDS(as.data.frame(sf_object)[c("state_fips","state_abbv","state_name","crop","variable","est","cat")],
            paste0("data-raw/output/figure_data/study1_impact_spatial_",variable,".rds"))
    return(variable)})

#--------------------------------------------------------------------------------------------------------------
# Figure : Baseline state                                                                                   ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()

us_sf <- urbnmapr::get_urbn_map(map = "states", sf = TRUE)
us_sf$state_fips <- as.numeric(us_sf$state_fips)

LAB <- readRDS("data-raw/output/figure_data/crop_availability.rds")
LAB <- doBy::summaryBy(insured_acres_sob~commodity_name+commodity_code,FUN=c(sum,length),data=LAB)
LAB <- LAB[order(-LAB$insured_acres_sob.length,-LAB$insured_acres_sob.sum),]

data <- readRDS("data-raw/output/summary/impact_heterogeneity.rds")
data$level <- as.character(data$level)
data <- data[data$disag %in% c("CROP_STATE","STATE"),]
data <- data[data$baseline %in% c("2015-2023"),]
data <- data[data$combination %in% c("Basic+CURRENT"),]
data <- data[data$variable %in% c("itp","rrp2","SimrateP"),]
data <- data[data$aggregation %in% c("avg_valueT"),]
STATE <- data[data$disag %in% c("STATE"),]
data  <- data[data$disag %in% c("CROP_STATE"),]
data  <- tidyr::separate(data,"level",into=c("crop","state_fips"),sep="-")
data$state_fips <- as.numeric(data$state_fips)
data$crop <- as.numeric(data$crop)
STATE$state_fips <- as.numeric(STATE$level)
STATE$crop <- 0
data <- rbind(data[c("crop","state_fips","variable","est")],STATE[c("crop","state_fips","variable","est")])
CROP <- readRDS("data-raw/output/summary/impact_heterogeneity.rds")
CROP$level <- as.character(CROP$level)
CROP <- CROP[CROP$disag %in% c("CROP","FCIP"),]
CROP <- CROP[CROP$baseline %in% c("2015-2023"),]
CROP <- CROP[CROP$combination %in% c("Basic+CURRENT"),]
CROP <- CROP[CROP$variable %in% c("itp","rrp2","SimrateP"),]
CROP$txt <- paste0(round(CROP$est,2)," (",round(CROP$est_se,2),")")
CROP$commodity_code <-  as.numeric(CROP$level)

CROP$commodity_code <- ifelse(CROP$disag %in% "FCIP",0,CROP$commodity_code)
LAB <- dplyr::inner_join(LAB[LAB$commodity_code %in% unique(data$crop),],CROP[c("commodity_code","variable","txt")])

comname <- unique(readRDS("data-raw/output/figure_data/crop_availability.rds")[c("commodity_name","commodity_code")])
comname <- comname[!as.numeric(comname$commodity_code) %in% NA,]

LAB$commodity_name <- as.character(factor(as.numeric(LAB$commodity_code),levels = c(0,as.numeric(comname$commodity_code)),
                                          labels = c("All listed crops",comname$commodity_name)))
LAB$commodity_name <- paste0(LAB$commodity_name," ",LAB$txt)


lapply(
  c("itp","rrp2","SimrateP"),
  function(variable){
    # variable <- "SimrateP"
    dataX <- data[data$variable %in% variable,]
    LABX <- LAB[LAB$variable %in% variable,]
    LABX <- LABX[order(-LABX$insured_acres_sob.length,-LABX$insured_acres_sob.sum),]

    dataX$crop <- factor(dataX$crop, levels = as.numeric(LABX$commodity_code), labels = 1:nrow(LABX))
    dataX$crop <- factor(dataX$crop,levels = 1:nrow(LABX), labels = as.character(LABX$commodity_name))

    if(variable %in% "rrp2"){
      # dataX$cat <- cut(dataX$est,unique(round(c(min(dataX$est,na.rm=T),quantile(unique(dataX$est),seq(0,1,0.2)),max(dataX$est,na.rm=T)))))
      # table(dataX$cat)

      dataX$cat <- cut(dataX$est,round(unique(c(min(dataX$est,na.rm=T),70,75,80,75,90,max(dataX$est,na.rm=T)))))
      clo <- c("yellow","greenyellow","green","olivedrab","darkgreen")
      table(dataX$cat)
      lbli <- "Percentage reduction in downside\nrisk relative to no insurance"
    }
    if(variable %in% "itp"){
      # dataX$cat <- cut(dataX$est,unique(round(c(min(dataX$est,na.rm=T),quantile(unique(dataX$est),seq(0,1,0.2)),max(dataX$est,na.rm=T)))))
      # table(dataX$cat)

      dataX$cat <- cut(dataX$est,round(unique(c(min(dataX$est,na.rm=T),10,20,30,40,max(dataX$est,na.rm=T)))))
      clo <- c("thistle","violet","purple","#4B0082","blue")
      table(dataX$cat)
      lbli <- "Percentage increase in revenue\n relative to no insurance"
    }
    if(variable %in% "SimrateP"){
      dataX$cat <- cut(dataX$est,round(unique(c(min(dataX$est,na.rm=T),3,4,5,10,max(dataX$est,na.rm=T)))))
      table(dataX$cat)
      clo <- c("yellow","gold","orange","red","darkred")
      lbli <- "Aggregate premium rate paid in cents"
    }

    sf_object <- sf::st_as_sf(us_sf)
    sf_object <- sf_object |> left_join(dataX, by = "state_fips")
    sf_object <- sf_object[!sf_object$cat %in% c(NA,NaN,Inf,-Inf),]

    fig <- ggplot() +
      geom_sf(data = sf_object,aes(fill = cat), colour = NA,size = 0.2) +
      geom_sf(data = us_sf, colour = "black", fill = NA,size =0.1) +
      scale_fill_manual(values=clo,na.value="white", name=lbli) +
      guides(fill = guide_legend(nrow=1)) +
      labs(subtitle = "",x = "", y = "", fill='',caption = "") +
      theme_bw() +
      facet_wrap(~crop,ncol = 3) +
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
      theme(legend.text= element_text(size=7),
            legend.title= element_text(size=7),
            legend.key.size = unit(0.2, 'cm'),
            legend.position="bottom",
            legend.background = element_blank(),
            plot.title = element_text(size = 8),
            axis.title.y = element_blank(), #
            axis.title.x = element_blank(), #
            axis.text.x  = element_blank(), #
            axis.text.y  = element_blank(),
            plot.caption = element_text(size=5,hjust = 0 ,vjust = 0, face = "italic"),
            strip.text = element_text(size = 7),
            strip.background = element_rect(fill = "white", colour = "black", size = 1))

    ggsave(paste0("data-raw/output/figure/study1_basline_spatial_",variable,".png"), fig,dpi = 1200,width = 6, height = 8)
    saveRDS(as.data.frame(sf_object)[c("state_fips","state_abbv","state_name","crop","variable","est","cat")],
            paste0("data-raw/output/figure_data/study1_basline_spatial_",variable,".rds"))
    return(variable)})

#--------------------------------------------------------------------------------------------------------------

