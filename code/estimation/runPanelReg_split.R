#!/usr/bin/Rscript
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
### this script: use separate data sets for events and outcomes --- for district heterogeneity
args = commandArgs(trailingOnly = TRUE)

suppressMessages(library(magrittr))
suppressMessages(library(dplyr))
source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")

ddEventsFull <- readRDS(paste0("/data/tmp/xtai/", args[1], ".rds"))
ddOutcomesFull <- readRDS(paste0("/data/tmp/xtai/", args[2], ".rds"))
if (identical(ddEventsFull[, c("district_id", "date")], ddOutcomesFull[, c("district_id", "date")]) == FALSE) stop("Events and outcome data in different order")

myFun <- function(k) {
  ddOutcomes$percentage_migrated <- ddOutcomes[, paste0("lagPercentMigratedk", sprintf("%03d", k))]
  keepThese <- which(!is.na(ddOutcomes$percentage_migrated) & ddOutcomes$percentage_migrated != 0 & ddOutcomes$percentage_migrated != 1)
  
  tmp <- ddOutcomes[keepThese, ] 
  forPercent <- ddEvents[keepThese, ] %>%
    mutate_all(~as.factor(.)) %>%
    bind_cols(percentage_migrated = tmp[, "percentage_migrated"])
  
  fitBeta <- betareg::betareg(percentage_migrated ~ ., data = forPercent)
  out <- betaGetCoefs(fitBeta, clusteredSE = TRUE)
  return(out)
}

# ####### district heterogeneity: provincial capital or not
districtInfo <- readRDS("/data/tmp/xtai/data/district_ids_with_info.rds")

### provincial capitals
toInclude <- districtInfo$distid[districtInfo$provincialCapital == 1]

ddEvents <- ddEventsFull[ddEventsFull$district_id %in% toInclude, ]
ddOutcomes <- ddOutcomesFull[ddOutcomesFull$district_id %in% toInclude, ]

out <- myFun(as.numeric(args[3]))
outFileName <- paste0("/data/tmp/xtai/9-25distHeterogeneity/betaCoefList_provCap.rds")
if (file.exists(outFileName)) {
  coefListK <- readRDS(outFileName)
} else {
  coefListK <- vector(mode = "list", length = 120)
}

coefListK[[as.numeric(args[3])]] <- out
saveRDS(coefListK, file = outFileName)

#####  non-provincial capitals
toInclude <- districtInfo$distid[districtInfo$provincialCapital == 0]
ddEvents <- ddEventsFull[ddEventsFull$district_id %in% toInclude, ]
ddOutcomes <- ddOutcomesFull[ddOutcomesFull$district_id %in% toInclude, ]

out <- myFun(as.numeric(args[3]))
outFileName <- paste0("/data/tmp/xtai/9-25distHeterogeneity/betaCoefList_nonCap.rds")
if (file.exists(outFileName)) {
  coefListK <- readRDS(outFileName)
} else {
  coefListK <- vector(mode = "list", length = 120)
}

coefListK[[as.numeric(args[3])]] <- out
saveRDS(coefListK, file = outFileName)

# nohup ./myCode/runPanelReg_split.R districtDay_eventsOnly_8-11 districtDay_8-12_outcomeOnly 1 &

