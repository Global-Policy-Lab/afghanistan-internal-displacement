#!/usr/bin/Rscript
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# 4_singleEvent.R uses this script
### this script: single violent days --- uses lfe package
args = commandArgs(trailingOnly = TRUE)
ddOutcomes <- readRDS(paste0("/data/afg_anon/displacement_analysis/districtDay_8-12_outcomeOnly.rds"))

# if (identical(ddEvents[, c("district_id", "date")], ddOutcomes[, c("district_id", "date")]) == FALSE) stop("Events and outcome data in different order")

suppressMessages(library(magrittr))
suppressMessages(library(dplyr))

# directory to use: /data/afg_anon/displacement_analysis/10-7singleViolence/
# check if it works using an event that throws an error

############################# run this section beforehand ###############################
# make a data set to store results: 
# each row should be a violenceDistrict and violenceDate 
# load("/data/afg_anon/displacement_analysis/allMyEvents_7-29.Rdata")
# 
# summarizeEvents <- myEvents %>%
#   group_by(distid, date_start) %>%
#   summarize(numEvents = dplyr::n(),
#             numCasualties = sum(best))
# 
# summarizeEvents$date_start <- as.Date(summarizeEvents$date_start) # 5573 obs
# 
# rViolentDays <- summarizeEvents %>%
#   filter(date_start > as.Date("2013-09-28") & date_start <= as.Date("2017-03-01")) %>%
#   data.frame()
# # 3724 events
# 
# ddEvents <- readRDS("/data/afg_anon/displacement_analysis/districtDay_eventsOnly_8-11.rds")
# tmpNames <- paste0(names(ddEvents[3:213]), "1")
# outDTF <- data.frame(distid = rViolentDays$distid, date_start = rViolentDays$date_start, stringsAsFactors = FALSE)
# outDTF[, tmpNames] <- NA # double-check the order before putting it in # see 10-20singleEvents.R
# 
# saveRDS(outDTF, file = "/data/afg_anon/displacement_analysis/10-7singleViolence/outDTF.rds")
############################# end #############################

######################## start here
load("/data/afg_anon/displacement_analysis/allMyEvents_7-29.Rdata")

summarizeEvents <- myEvents %>%
  group_by(distid, date_start) %>%
  summarize(numEvents = dplyr::n(),
            numCasualties = sum(best))

summarizeEvents$date_start <- as.Date(summarizeEvents$date_start) # 5573 obs

rViolentDays <- summarizeEvents %>%
  filter(date_start > as.Date("2013-09-28") & date_start <= as.Date("2017-03-01")) %>%
  data.frame()

outDTF <- readRDS("/data/afg_anon/displacement_analysis/10-7singleViolence/outDTF.rds")

# make Tm180 to T030 (211 cols)
districtDayTemplate <- ddOutcomes %>% 
  select(district_id, date)

ddEventsZero <- data.frame(matrix(0, nrow = nrow(ddOutcomes), ncol = length(-180:30)))
names(ddEventsZero) <- c(paste0("Tm", sprintf("%03d", 180:1)), paste0("T", sprintf("%03d", 0:30)))

ddEventsZero <- cbind(districtDayTemplate, ddEventsZero)

makeDataset2 <- function(violenceDate, violenceDistrict, districtDay) {
  for (l in -180:30) {
    tmpDate <- violenceDate - lubridate::days(l)
    
    if (l >= 0) {
      districtDay[districtDay$district_id == violenceDistrict & districtDay$date == tmpDate, paste0("T", sprintf("%03d", l))] <- 1
    } else {
      districtDay[districtDay$district_id == violenceDistrict & districtDay$date == tmpDate, paste0("Tm", sprintf("%03d", abs(l)))] <- 1
    } # m is in the past -- goes till 180
  }
  return(districtDay)
}

myFun <- function(k) {
  forPercent <- ddEvents[keepThese, ] %>%
    # select(district_id, date, dplyr::starts_with("T", ignore.case = FALSE)) %>%
    mutate_all(~as.factor(.)) %>%
    bind_cols(percentage_migrated = tmp[, "percentage_migrated"])
  
  ###### try high dimensional FE
  xnam <- names(forPercent)[3:213] # to get the Ts
  fmla <- as.formula(paste("log(percentage_migrated/(1 - percentage_migrated)) ~ ", paste(xnam, collapse= "+"), " | district_id + date") )
  
  fitlfe <- try(lfe::felm(fmla, data = forPercent))
  if (class(fitlfe) != "try-error") { # error if only one adm1 # try 1585
    tmplfe <- data.frame(term = rownames(fitlfe$coefficients), estimatelfe = c(fitlfe$coefficients), stringsAsFactors = FALSE)
    return(tmplfe)
  }
}

ddOutcomes$percentage_migrated <- ddOutcomes[, paste0("lagPercentMigratedk", sprintf("%03d", 30))]
keepThese <- which(!is.na(ddOutcomes$percentage_migrated) & ddOutcomes$percentage_migrated != 0 & ddOutcomes$percentage_migrated != 1)
tmp <- ddOutcomes[keepThese, ] 

for (i in as.numeric(args[1]):as.numeric(args[2])) {
  violenceDate <- rViolentDays[i, "date_start"]
  violenceDistrict <- rViolentDays[i, "distid"]
  ddEvents <- makeDataset2(violenceDate, violenceDistrict, ddEventsZero)
  
  tmplfe <- try(myFun(30))
  if (!is.null(tmplfe)) {
    outDTF[i, tmplfe$term] <- tmplfe$estimatelfe
    tmplfe <- NULL
  }
  
}
saveRDS(outDTF, file = paste0("/data/afg_anon/displacement_analysis/10-7singleViolence/outDTF_", args[1], "_", args[2], "lfe.rds"))

# for (i in seq(1, 3501, 100)) {
#   cat(paste0("nohup ./myCode/runPanelReg_singlelfe.R ", i, " ", i+99, " &\n"))
# }

# nohup ./myCode/runPanelReg_singlelfe.R 1 100 &
# nohup ./myCode/runPanelReg_singlelfe.R 101 200 &
# nohup ./myCode/runPanelReg_singlelfe.R 201 300 &
# nohup ./myCode/runPanelReg_singlelfe.R 301 400 &
# nohup ./myCode/runPanelReg_singlelfe.R 401 500 &
# nohup ./myCode/runPanelReg_singlelfe.R 501 600 &
# nohup ./myCode/runPanelReg_singlelfe.R 601 700 &
# nohup ./myCode/runPanelReg_singlelfe.R 701 800 &
# nohup ./myCode/runPanelReg_singlelfe.R 801 900 &
# nohup ./myCode/runPanelReg_singlelfe.R 901 1000 &
# nohup ./myCode/runPanelReg_singlelfe.R 1001 1100 &
# nohup ./myCode/runPanelReg_singlelfe.R 1101 1200 &
# nohup ./myCode/runPanelReg_singlelfe.R 1201 1300 &
# nohup ./myCode/runPanelReg_singlelfe.R 1301 1400 &
# nohup ./myCode/runPanelReg_singlelfe.R 1401 1500 &
# nohup ./myCode/runPanelReg_singlelfe.R 1501 1600 &
# nohup ./myCode/runPanelReg_singlelfe.R 1601 1700 &
# nohup ./myCode/runPanelReg_singlelfe.R 1701 1800 &
# nohup ./myCode/runPanelReg_singlelfe.R 1801 1900 &
# nohup ./myCode/runPanelReg_singlelfe.R 1901 2000 &
# nohup ./myCode/runPanelReg_singlelfe.R 2001 2100 &
# nohup ./myCode/runPanelReg_singlelfe.R 2101 2200 &
# nohup ./myCode/runPanelReg_singlelfe.R 2201 2300 &
# nohup ./myCode/runPanelReg_singlelfe.R 2301 2400 &
# nohup ./myCode/runPanelReg_singlelfe.R 2401 2500 &
# nohup ./myCode/runPanelReg_singlelfe.R 2501 2600 &
# nohup ./myCode/runPanelReg_singlelfe.R 2601 2700 &
# nohup ./myCode/runPanelReg_singlelfe.R 2701 2800 &
# nohup ./myCode/runPanelReg_singlelfe.R 2801 2900 &
# nohup ./myCode/runPanelReg_singlelfe.R 2901 3000 &
# nohup ./myCode/runPanelReg_singlelfe.R 3001 3100 &
# nohup ./myCode/runPanelReg_singlelfe.R 3101 3200 &
# nohup ./myCode/runPanelReg_singlelfe.R 3201 3300 &
# nohup ./myCode/runPanelReg_singlelfe.R 3301 3400 &
# nohup ./myCode/runPanelReg_singlelfe.R 3401 3500 &
# nohup ./myCode/runPanelReg_singlelfe.R 3501 3600 &
# nohup ./myCode/runPanelReg_singlelfe.R 3601 3700 &
# nohup ./myCode/runPanelReg_singlelfe.R 3701 3724 &
