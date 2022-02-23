#!/usr/bin/Rscript
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# 5_wherePeopleGo.R uses this script
args = commandArgs(trailingOnly = TRUE) 
ddEvents <- readRDS(paste0("/data/afg_anon/displacement_analysis/districtDay_eventsOnly_8-11.rds"))

# if (identical(ddEvents[, c("district_id", "date")], ddOutcomes[, c("district_id", "date")]) == FALSE) stop("Events and outcome data in different order")

suppressMessages(library(magrittr))
suppressMessages(library(dplyr))
source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")

myFun <- function(outcomeName) {
  ddOutcomes$percentage_migrated <- ddOutcomes[, outcomeName] # 12/12: do all 3 at once
  # ddOutcomes$percentage_migrated <- ddOutcomes[, paste0("lagPercentMigratedk", sprintf("%03d", k))]
  keepThese <- which(!is.na(ddOutcomes$percentage_migrated) & ddOutcomes$percentage_migrated != 0 & ddOutcomes$percentage_migrated != 1)
  
  tmp <- ddOutcomes[keepThese, ] 
  forPercent <- ddEvents[keepThese, ] %>%
    select(district_id, date, dplyr::starts_with("T", ignore.case = FALSE)) %>%
    mutate_all(~as.factor(.)) %>%
    bind_cols(percentage_migrated = tmp[, "percentage_migrated"])
  
  fitBeta <- betareg::betareg(percentage_migrated ~ ., data = forPercent)
  # save(fitBeta, file = paste0("/data/tmp/xtai/9-22wherePeopleGo/fitObjs/beta_k", sprintf("%03d", k), args[4], ".Rdata"))# update 11/19: don't save fit object
  
  out <- betaGetCoefs(fitBeta, clusteredSE = TRUE)
  
  out$numObs <- nrow(forPercent) # 12/19: add this info
  out$meanNumMoved <- mean(tmp$numMoved) # 12/19: add this info
  
  return(out)
}

#### 1/7: new outcome variables (partition)
# sameProvincialCapComp --- this one is done
# top5exclhomeComp 
# remainingCapComp 
# break down nonCap: sameProvNonCapComp and otherProvNonCapComp

# prov cap
districtInfo <- readRDS("/data/afg_anon/displacement_analysis/district_ids_with_info.rds")
capitals <- districtInfo$distid[districtInfo$provincialCapital == 1]

### these are the 9 outcome vars that I need
outFileName0 <- paste0("/data/afg_anon/displacement_analysis/9-22wherePeopleGo/betaCoefList_sameProvincialCapComp.rds")
outFileName1 <- paste0("/data/afg_anon/displacement_analysis/9-22wherePeopleGo/betaCoefList_top5exclhomeComp_capitals.rds")
outFileName2 <- paste0("/data/afg_anon/displacement_analysis/9-22wherePeopleGo/betaCoefList_top5exclhomeComp_nonCap.rds")

outFileName3 <- paste0("/data/afg_anon/displacement_analysis/9-22wherePeopleGo/betaCoefList_remainingCapComp_capitals.rds")
outFileName4 <- paste0("/data/afg_anon/displacement_analysis/9-22wherePeopleGo/betaCoefList_remainingCapComp_nonCap.rds")

outFileName7 <- paste0("/data/afg_anon/displacement_analysis/9-22wherePeopleGo/betaCoefList_sameProvNonCapComp_capitals.rds")
outFileName8 <- paste0("/data/afg_anon/displacement_analysis/9-22wherePeopleGo/betaCoefList_sameProvNonCapComp_nonCap.rds")

outFileName9 <- paste0("/data/afg_anon/displacement_analysis/9-22wherePeopleGo/betaCoefList_otherProvNonCapComp_capitals.rds")
outFileName10 <- paste0("/data/afg_anon/displacement_analysis/9-22wherePeopleGo/betaCoefList_otherProvNonCapComp_nonCap.rds")


coefListK0 <- vector(mode = "list", length = 120)
coefListK1 <- vector(mode = "list", length = 120)
coefListK2 <- vector(mode = "list", length = 120)
coefListK3 <- vector(mode = "list", length = 120)
coefListK4 <- vector(mode = "list", length = 120)
coefListK7 <- vector(mode = "list", length = 120)
coefListK8 <- vector(mode = "list", length = 120)
coefListK9 <- vector(mode = "list", length = 120)
coefListK10 <- vector(mode = "list", length = 120)

for (i in c(7, 30, 90)) {
  cat(i, ", ")
  ddOutcomesFull <- readRDS(paste0("/data/afg_anon/displacement_analysis/districtDayWPG/districtDayWPG_9-22_outcomeOnly_", i, "_comp12-11.rds"))
  ddOutcomes <- ddOutcomesFull
  ddOutcomes[!(ddOutcomes$district_id %in% capitals), c("top5exclhomeComp", "remainingCapComp", "sameProvNonCapComp", "otherProvNonCapComp")] <- NA # capitals
  
  coefListK1[[i]] <- myFun("top5exclhomeComp")
  coefListK3[[i]] <- myFun("remainingCapComp")
  coefListK7[[i]] <- myFun("sameProvNonCapComp")
  coefListK9[[i]] <- myFun("otherProvNonCapComp")
  
  ddOutcomes <- ddOutcomesFull
  ddOutcomes[ddOutcomes$district_id %in% capitals, c("sameProvincialCapComp", "top5exclhomeComp", "remainingCapComp", "sameProvNonCapComp", "otherProvNonCapComp")] <- NA # non-capitals
  
  coefListK0[[i]] <- myFun("sameProvincialCapComp")
  coefListK2[[i]] <- myFun("top5exclhomeComp")
  coefListK4[[i]] <- myFun("remainingCapComp")
  coefListK8[[i]] <- myFun("sameProvNonCapComp")
  coefListK10[[i]] <- myFun("otherProvNonCapComp")
  
}
saveRDS(coefListK0, file = outFileName0)
saveRDS(coefListK1, file = outFileName1)
saveRDS(coefListK2, file = outFileName2)
saveRDS(coefListK3, file = outFileName3)
saveRDS(coefListK4, file = outFileName4)
saveRDS(coefListK7, file = outFileName7)
saveRDS(coefListK8, file = outFileName8)
saveRDS(coefListK9, file = outFileName9)
saveRDS(coefListK10, file = outFileName10)

# nohup ./myCode/cleanCode/runPanelReg_WPG.R &
