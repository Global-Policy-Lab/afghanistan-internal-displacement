# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

########################## 8/6 SIGAR data ########################## 
# rsync -P /Users/xtai/Desktop/development/displacementProj/NatureHB/review/SIGARcontrol.csv xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis

# run using runPanelReg_split.R
######### 
rm(list = ls()); gc()
library(ggplot2); library(dplyr)
source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")

### fig 3b: on the same plot 
prefix <- c("fit_govtControl_SIGAR_govt", "fit_govtControl_SIGAR_taliban")
description <- c("Government", "Taliban/Contested")
totalDisplacement <- c()
for (p in 1:length(prefix)) {
  coefListK <- readRDS(paste0("/data/afg_anon/displacement_analysis/reviewerComments/betaCoefList_", prefix[p], ".rds"))
  tmp <- c()
  for (i in 1:120) {
    if (is.null(nrow(coefListK[[i]]))) next # if not processed, go next
    forPlot <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
    tmp <- rbind(tmp, forPlot)
  }
  tmp <- tmp %>%
    mutate(daysAfter = as.numeric(substr(gsub("T|m", "", term), 1, 3)))  
  
  totalDisplacement <- totalDisplacement %>%
    bind_rows(tmp %>% 
                mutate(key = description[p]))
}

source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")

pdf(paste0("/data/afg_anon/displacement_analysis/reviewerComments/8-7territorialControl_totalDisp.pdf"), width = 10, height = 4)
plot1 <- totalDispPlotFun(c("Government", "Taliban/Contested"))
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()
# rsync -P xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis/reviewerComments/8-7territorialControl_totalDisp.pdf /Users/xtai/Desktop/development/displacementProj/NatureHB/review/

# 1469 for govt
# 1885 for Taliban/contested


