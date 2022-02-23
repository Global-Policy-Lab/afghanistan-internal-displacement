#!/usr/bin/Rscript
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# 2_overallAndEventHet.R, 6c_supplementaryFig4.R and 6e_supplementaryFig7.R use this script
args = commandArgs(trailingOnly = TRUE)
ddEvents <- readRDS(paste0("/data/afg_anon/displacement_analysis/", args[1], ".rds"))
ddOutcomes <- readRDS(paste0("/data/afg_anon/displacement_analysis/", args[2], ".rds"))

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
  # saveRDS(fitBeta, file = paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/beta_k", sprintf("%03d", k), args[4], ".rds")) # update 11/19: don't save fit object
  # saveRDS(fitBeta, file = paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/beta_k", sprintf("%03d", k), args[4], ".rds")) # for heterogeneity results 
  saveRDS(fitBeta, file = paste0("/data/afg_anon/displacement_analysis/12-3robustnessChecks/beta_k", sprintf("%03d", k), args[4], ".rds")) # for robustness checks
  
  # out <- betaGetCoefs(fitBeta, otherTreatnames = c("T_N000"), clusteredSE = TRUE)
  # out <- betaGetCoefs(fitBeta, clusteredSE = TRUE)
  # out <- betaGetCoefs(fitBeta, otherTreatnames = c("T_B000", "T_C000"), clusteredSE = TRUE)
  # out <- betaGetCoefs(fitBeta, otherTreatnames = c("T_G000"), clusteredSE = TRUE)
  # out <- betaGetCoefs(fitBeta, otherTreatnames = c("T_G000", "T_T000"), clusteredSE = TRUE)
  # out <- betaGetCoefs(fitBeta, otherTreatnames = c("T_T000"), clusteredSE = TRUE)
  # return(out)
}
out <- myFun(as.numeric(args[3]))

# counter <- 0
# # for (i in c(1:9, seq(5, 120, by = 5))) {
# for (i in 1:120) {
# # for (i in seq(12, 120, by = 2)) {
#   # if (i == 30) next
#   # if ((i %% 5) == 0) next
#   cat(paste0("nohup ./myCode/cleanCode/runPanelReg_new.R districtDay_eventsOnly_8-11 districtDay_12-3_outcomeOnly_rob ", i, " all &\n"))
#   # cat(paste0("nohup ./myCode/runPanelReg_new.R districtDay_eventsOnly_8-11 districtDay_8-12_outcomeOnly ", i, " fit &\n"))
#   # cat(paste0("nohup ./myCode/runPanelReg_new.R districtDay_eventsOnly_8-11_60peace districtDay_8-12_outcomeOnly ", i, " fit_60peace &\n"))
#   # cat(paste0("nohup ./myCode/runPanelReg_new.R districtDay_eventsOnly_8-11_wherePrec1 districtDay_8-12_outcomeOnly ", i, " fit_wherePrec1 &\n"))
#   # cat(paste0("nohup ./myCode/runPanelReg_roblfe.R districtDay_eventsOnly_3-13_regM_modelMat districtDay_8-12_outcomeOnly ", i, " regM &\n"))
#   counter <- counter + 1
#   if (counter %% 12 == 0) {
#     cat("wait\n")
#   }
# }

## To use this file:
# nohup ./myCode/runPanelReg_new.R districtDay_eventsOnly_8-11 districtDay_8-12_outcomeOnly 30 fit & 
# replace 30 by values from 1 to 120
