# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
rm(list=ls()); gc()
library(ggplot2); library(dplyr)
source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")

### fig 3b: on the same plot 
prefix <- c("provincialCap", "nonCapital")
description <- c("Provincial capital", "Non-capital")
totalDisplacement <- c()
for (p in 1:length(prefix)) {
  coefListK <- readRDS(paste0("/data/tmp/xtai/9-25distHeterogeneity/betaCoefList_", prefix[p], ".rds"))
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

png(paste0("/data/tmp/xtai/7-30panelPlots/allK/fig3d.png"), width = 10, height = 4, res = 300, units = "in")
plot1 <- totalDispPlotFun(c("Provincial capital", "Non-capital"))
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()
