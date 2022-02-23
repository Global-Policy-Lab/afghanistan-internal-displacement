#!/usr/bin/Rscript
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# 3_districtHeterogeneity.R and 6a_supplementaryFig2.R use this script
args = commandArgs(trailingOnly = TRUE)

suppressMessages(library(magrittr))
suppressMessages(library(dplyr))
source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")

ddEventsFull <- readRDS(paste0("/data/afg_anon/displacement_analysis/", args[1], ".rds"))
ddOutcomesFull <- readRDS(paste0("/data/afg_anon/displacement_analysis/", args[2], ".rds"))
if (identical(ddEventsFull[, c("district_id", "date")], ddOutcomesFull[, c("district_id", "date")]) == FALSE) stop("Events and outcome data in different order")

myFun <- function(k) {
  ddOutcomes$percentage_migrated <- ddOutcomes[, paste0("lagPercentMigratedk", sprintf("%03d", k))]
  keepThese <- which(!is.na(ddOutcomes$percentage_migrated) & ddOutcomes$percentage_migrated != 0 & ddOutcomes$percentage_migrated != 1)
  
  tmp <- ddOutcomes[keepThese, ] 
  forPercent <- ddEvents[keepThese, ] %>%
    # select(district_id, date, dplyr::starts_with("T", ignore.case = FALSE)) %>%
    # select(date, dplyr::starts_with("T", ignore.case = FALSE)) %>%
    mutate_all(~as.factor(.)) %>%
    bind_cols(percentage_migrated = tmp[, "percentage_migrated"])
  
  fitBeta <- betareg::betareg(percentage_migrated ~ ., data = forPercent)
  return(fitBeta)
  # 12/3: add this --- don't save each fit object
  # out <- betaGetCoefs(fitBeta, clusteredSE = TRUE)
  # return(out)
}

# ####### district heterogeneity: provincial capital or not
districtInfo <- readRDS("/data/afg_anon/displacement_analysis/district_ids_with_info.rds")
# toInclude <- districtInfo$distid[districtInfo$provincialCapital == 1]
# 
# ddEvents <- ddEventsFull[ddEventsFull$district_id %in% toInclude, ]
# ddOutcomes <- ddOutcomesFull[ddOutcomesFull$district_id %in% toInclude, ]
# 
# fitBeta <- myFun(as.numeric(args[3]))
# saveRDS(fitBeta, file = paste0("/data/afg_anon/displacement_analysis/9-25distHeterogeneity/beta_provincialCap", "_k", sprintf("%03d", as.numeric(args[3])), "_fit.Rdata"))

#####
toInclude <- districtInfo$distid[districtInfo$provincialCapital == 0]
ddEvents <- ddEventsFull[ddEventsFull$district_id %in% toInclude, ]
ddOutcomes <- ddOutcomesFull[ddOutcomesFull$district_id %in% toInclude, ]

fitBeta <- myFun(as.numeric(args[3]))
saveRDS(fitBeta, file = paste0("/data/afg_anon/displacement_analysis/9-25distHeterogeneity/beta_nonCapital", "_k", sprintf("%03d", as.numeric(args[3])), "_fit.Rdata"))

# counter <- 0
# # for (i in c(1:9, seq(10, 120, by = 5))) {
# for (i in 1:120) {
#   cat(paste0("nohup ./myCode/runPanelReg_split.R districtDay_eventsOnly_8-11 districtDay_8-12_outcomeOnly ", i, " &\n"))
#   counter <- counter + 1
#   if (counter %% 16 == 0) {
#     cat("wait\n")
#   }
# }


############################# Taliban control (SI Figure 2) ###################################
# ####### district heterogeneity: territorial control
talibanControl <- read.csv("/data/afg_anon/displacement_analysis/SIGARcontrol.csv")

talibanControl <- talibanControl %>%
  mutate(control = case_when(Oct.2017.Assessment %in% c("GIRoA In!uence", "GIRoA Control") ~ "Govt",
                             TRUE ~ "Taliban/Contested"))
# table(talibanControl$control)
# Govt Taliban/Contested 
# 221               177

toInclude <- talibanControl$distid[talibanControl$control == "Govt"]

ddEvents <- ddEventsFull[ddEventsFull$district_id %in% toInclude, ]
ddOutcomes <- ddOutcomesFull[ddOutcomesFull$district_id %in% toInclude, ]

# fitBeta <- myFun(as.numeric(args[3]))
# saveRDS(fitBeta, file = paste0("/data/tmp/xtai/9-25distHeterogeneity/fitObjs/beta_provincialCap", "_k", sprintf("%03d", as.numeric(args[3])), "_fit.Rdata"))

# 12/3: don't save each fit
out <- myFun(as.numeric(args[3]))
outFileName <- paste0("/data/afg_anon/displacement_analysis/reviewerComments/betaCoefList_fit_govtControl_SIGAR_govt.rds")
if (file.exists(outFileName)) {
  coefListK <- readRDS(outFileName)
} else {
  coefListK <- vector(mode = "list", length = 120)
}

coefListK[[as.numeric(args[3])]] <- out
saveRDS(coefListK, file = outFileName)

#####
toInclude <- talibanControl$distid[talibanControl$control == "Taliban/Contested"]
ddEvents <- ddEventsFull[ddEventsFull$district_id %in% toInclude, ]
ddOutcomes <- ddOutcomesFull[ddOutcomesFull$district_id %in% toInclude, ]

# fitBeta <- myFun(as.numeric(args[3]))
# saveRDS(fitBeta, file = paste0("/data/tmp/xtai/9-25distHeterogeneity/fitObjs/beta_nonCapital", "_k", sprintf("%03d", as.numeric(args[3])), "_fit.Rdata"))

# 12/3: don't save each fit
out <- myFun(as.numeric(args[3]))
outFileName <- paste0("/data/afg_anon/displacement_analysis/reviewerComments/betaCoefList_fit_govtControl_SIGAR_taliban.rds")
if (file.exists(outFileName)) {
  coefListK <- readRDS(outFileName)
} else {
  coefListK <- vector(mode = "list", length = 120)
}

coefListK[[as.numeric(args[3])]] <- out
saveRDS(coefListK, file = outFileName)

# nohup ./myCode/runPanelReg_split.R districtDay_eventsOnly_8-11 districtDay_8-12_outcomeOnly 30 &

