#!/usr/bin/Rscript
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
### this script: use separate data sets for events and outcomes
args = commandArgs(trailingOnly = TRUE)
ddEvents <- readRDS(paste0("/data/tmp/xtai/", args[1], ".rds"))
ddOutcomes <- readRDS(paste0("/data/tmp/xtai/", args[2], ".rds"))

if (identical(ddEvents[, c("district_id", "date")], ddOutcomes[, c("district_id", "date")]) == FALSE) stop("Events and outcome data in different order")

suppressMessages(library(magrittr))
suppressMessages(library(dplyr))
source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")

myFun <- function(k) {
  ddOutcomes$percentage_migrated <- ddOutcomes[, paste0("lagPercentMigratedk", sprintf("%03d", k))]
  keepThese <- which(!is.na(ddOutcomes$percentage_migrated) & ddOutcomes$percentage_migrated != 0 & ddOutcomes$percentage_migrated != 1)
  
  tmp <- ddOutcomes[keepThese, ] 
  forPercent <- ddEvents[keepThese, ] %>%
    # select(district_id, date, dplyr::starts_with("T", ignore.case = FALSE)) %>%
    mutate_all(~as.factor(.)) %>%
    bind_cols(percentage_migrated = tmp[, "percentage_migrated"])
  
  fitBeta <- betareg::betareg(percentage_migrated ~ ., data = forPercent)
  # saveRDS(fitBeta, file = paste0("/data/tmp/xtai/7-30panelPlots/allK/beta_k", sprintf("%03d", k), args[4], ".rds")) # update 11/19: don't save fit object
  
  ##### change these depending on which reg I am running
  # out <- betaGetCoefs(fitBeta, otherTreatnames = c("T_N000"), clusteredSE = TRUE)
  # out <- betaGetCoefs(fitBeta, clusteredSE = TRUE)
  out <- betaGetCoefs(fitBeta, otherTreatnames = c("T_S000")) # clusteredSE = TRUE is default 
  return(out)
}
out <- myFun(as.numeric(args[3]))

# outFileName <- paste0("/data/tmp/xtai/12-3robustnessChecks/betaCoefList_", args[4], ".rds")
outFileName <- paste0("/data/tmp/xtai/7-30panelPlots/allK/betaCoefList_", args[4], ".rds")
if (file.exists(outFileName)) {
  coefListK <- readRDS(outFileName)
} else {
  coefListK <- vector(mode = "list", length = 120)
}

coefListK[[as.numeric(args[3])]] <- out
saveRDS(coefListK, file = outFileName)

## To use this file:
# nohup ./myCode/runPanelReg.R districtDay_eventsOnly_8-11 districtDay_8-12_outcomeOnly 30 fit & 
# replace 30 by values from 1 to 120
