# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
rm(list=ls()); gc()
library(ggplot2); library(dplyr)
source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")

# run using runPanelReg_split
for (prefix in c("provincialCap", "nonCapital")) {
  cat(prefix, ", ")
  outFileName <- paste0("/data/afg_anon/displacement_analysis/9-25distHeterogeneity/betaCoefList_", prefix, ".rds")
  # outFileName <- paste0("/data/tmp/xtai/9-25distHeterogeneity/betaCoefList_", prefix, "_noclust.rds")
  if (file.exists(outFileName)) {
    coefListK <- readRDS(outFileName)
  } else {
    coefListK <- vector(mode = "list", length = 120)
  }
  for (i in 1:120) {
    if (!is.null(nrow(coefListK[[i]]))) next # if already processed, go next
    cat(i, ", ")
    fileName <- paste0("/data/afg_anon/displacement_analysis/9-25distHeterogeneity/beta_", prefix, "_k", sprintf("%03d", i), "_fit.Rdata")
    if (file.exists(fileName)) {
      fitBeta <- readRDS(fileName)
      # coefListK[[i]] <- betaGetCoefs(fitBeta)
      coefListK[[i]] <- betaGetCoefs(fitBeta, clusteredSE = TRUE)
    } else {
      # print message
      cat(paste0(prefix, ", k = ", i, " does not exist \n"))
    }
  }
  saveRDS(coefListK, file = outFileName) # make folder for distHeterogeneity
}


### fig 3b: on the same plot ######### 11/22: just use this 
prefix <- c("provincialCap", "nonCapital")
description <- c("Provincial capital", "Non-capital")
totalDisplacement <- c()
for (p in 1:length(prefix)) {
  coefListK <- readRDS(paste0("/data/afg_anon/displacement_analysis/9-25distHeterogeneity/betaCoefList_", prefix[p], ".rds"))
  # coefListK <- readRDS(paste0("/data/tmp/xtai/9-25distHeterogeneity/betaCoefList_", prefix[p], "_noclust.rds"))
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

totalDispPlotFunLims <- function(keyVec, myTitle) {
  myCols <- c("#420A68FF", "#DD513AFF") # add more later
  
  plot1 <- totalDisplacement %>%
    filter(key %in% keyVec) %>%
    ggplot(aes(daysAfter, exp(estimate), col = as.factor(key))) +
    geom_point() +
    geom_pointrange(aes(ymin = exp(conf.low), 
                        ymax = exp(conf.high), 
                        col = as.factor(key)),
                    position = position_dodge(width = .1)) +
    labs(#title = myTitle,
      x = "k days after violence",
      y = "Multiplicative change in odds",
      col = "") +
    geom_vline(xintercept = 0, color = "red3", linetype = 2) +
    geom_hline(yintercept = 1, color = "red3") +
    scale_x_continuous(breaks = seq(0, 120, 5)) +
    scale_y_continuous(limits = c(.91, 1.21)) +
    scale_color_manual(values = myCols[1:length(keyVec)]) +
    theme(legend.position = c(1, 1.02),
          legend.justification = c("right", "top"),
          legend.title = element_blank(),
          legend.text = element_text(size=14),
          axis.text=element_text(size=14),
          axis.title=element_text(size=14))
  
  return(plot1)
}
pdf(paste0("/data/afg_anon/displacement_analysis/2-16-22finalFigures/fig3d.pdf"), width = 10, height = 4)
plot1 <- totalDispPlotFunLims(c("Provincial capital", "Non-capital"))
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()

# rsync -P xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis/2-16-22finalFigures/fig3d.pdf /Users/xtai/Desktop/development/displacementProj/NatureHB/finalGuidelines/finalFigures
