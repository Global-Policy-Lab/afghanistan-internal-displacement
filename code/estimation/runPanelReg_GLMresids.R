#!/usr/bin/Rscript
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# 6f_supplementaryTable1.R uses this script
args = commandArgs(trailingOnly = TRUE)
ddEvents <- readRDS(paste0("/data/afg_anon/displacement_analysis/", args[1], ".rds"))
ddOutcomes <- readRDS(paste0("/data/afg_anon/displacement_analysis/", args[2], ".rds"))

if (identical(ddEvents[, c("district_id", "date")], ddOutcomes[, c("district_id", "date")]) == FALSE) stop("Events and outcome data in different order")

suppressMessages(library(magrittr))
suppressMessages(library(dplyr))
source("/home/xtai/myCode/analyzeResultsFuns.R")

myFun <- function(k) {
  ddOutcomes$percentage_migrated <- ddOutcomes[, paste0("lagPercentMigratedk", sprintf("%03d", k))]
  keepThese <- which(!is.na(ddOutcomes$percentage_migrated) & ddOutcomes$percentage_migrated != 0 & ddOutcomes$percentage_migrated != 1)
  
  tmp <- ddOutcomes[keepThese, ] 
  forPercent <- ddEvents[keepThese, ] %>%
    # select(district_id, date, dplyr::starts_with("T", ignore.case = FALSE)) %>%
    mutate_all(~as.factor(.)) #%>%
  # bind_cols(percentage_migrated = tmp[, "percentage_migrated"])
  
  fitglm <- glm(T000 ~ ., data = forPercent, family = "binomial")
  # fitBeta <- betareg::betareg(percentage_migrated ~ ., data = forPercent)
  # saveRDS(fitBeta, file = paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/beta_k", sprintf("%03d", k), args[4], ".rds")) # update 11/19: don't save fit object
  saveRDS(fitglm, file = paste0("/data/afg_anon/displacement_analysis/reviewerComments/glmViolence", sprintf("%03d", k), args[4], ".rds")) # update 11/19: don't save fit object
  
}
out <- myFun(as.numeric(args[3]))

# nohup ./myCode/runPanelReg_GLMresids.R districtDay_eventsOnly_8-11 districtDay_8-12_outcomeOnly 30 fit_GLMresids & 
