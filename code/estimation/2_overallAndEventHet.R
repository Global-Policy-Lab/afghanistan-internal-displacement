# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# this code: for overall and event heterogeneity results --- get results using runPanelReg_new.R
rm(list=ls()); gc()
source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")
library(dplyr); library(ggplot2)

############# run analysis using runPanelReg_new.R
# lines to change:   
# out <- betaGetCoefs(fitBeta, otherTreatnames = c("T_B000", "T_C000"), clusteredSE = TRUE)
# outFileName <- paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/betaCoefList_", args[4], ".rds")
# nohup ./myCode/runPanelReg_new.R districtDay_eventsOnly_8-11 districtDay_8-12_outcomeOnly 10 fit & 
# nohup ./myCode/runPanelReg_new.R districtDay_eventsOnly_8-11 districtDay_8-12_outcomeOnly 120 fit & 

################# 1. extract and save all coefs
# outFileName <- "/data/afg_anon/displacement_analysis/7-30panelPlots/betaCoefList_fit.rds"
# outFileName <- "/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/betaCoefList_11cas.rds"
# outFileName <- "/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/betaCoefList_60peace.rds"
outFileName <- "/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/betaCoefList_talebanIS.rds"

if (file.exists(outFileName)) {
  coefListK <- readRDS(outFileName)
} else {
  coefListK <- vector(mode = "list", length = 120)
}

for (i in 1:120) {
  if (!is.null(nrow(coefListK[[i]]))) next
  cat(i, ", ")
  # change as appropriate
  # fileName <- paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/beta_k", sprintf("%03d", i), "fit.rds")
  # fileName <- paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/beta_k", sprintf("%03d", i), "fit_11cas.rds")
  # fileName <- paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/beta_k", sprintf("%03d", i), "fit_60peace.rds")
  fileName <- paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/beta_k", sprintf("%03d", i), "fit_talebanIS.rds")
  if (file.exists(fileName)) {
    fitBeta <- readRDS(fileName)
    # coefListK[[i]] <- betaGetCoefs(fitBeta) # overall
    # coefListK[[i]] <- betaGetCoefs(fitBeta, otherTreatnames = c("T_S000")) # 11cas, 60peace
    coefListK[[i]] <- betaGetCoefs(fitBeta, otherTreatnames = c("T_I000", "T_T000")) # Taliban
  } else {
    # print message
    cat(paste0("k = ", i, " does not exist \n"))
  }
}
saveRDS(coefListK, file = outFileName)


################ 2.  coef plots -- k = 30
## fig 2a
coefListK <- readRDS("/data/afg_anon/displacement_analysis/7-30panelPlots/betaCoefList_fit.rds")
dataForPlot <- coefListK[[30]]
forPlot <- dataForPlot[which(substr(dataForPlot$term, 1, 1) == "T"), ]
forPlot$plusMinus <- ifelse(substr(forPlot$term, 2, 2) == "m", 1, -1)
tmp <- gsub("T|m", "", forPlot$term)
forPlot$leadLag <- as.numeric(substr(tmp, 1, nchar(tmp) - 1))
forPlot$leadLag <- forPlot$leadLag*forPlot$plusMinus 

pdf(paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/fig2a.pdf"), width = 9, height = 5)
forPlot %>%
  ggplot(aes(leadLag, exp(estimate)))+
  geom_point()+
  geom_pointrange(aes(ymin = exp(conf.low), ymax = exp(conf.high)))+
  labs(#x = "tau (Lead/lag: positive means lagged, i.e., violence in past)",
    # y = "exp(estimate): Multiplicative change in odds") +
    x = "Days since violence",
    y = "Increase in odds of being in a different district\n(Relative to 30 days prior)") +
  geom_vline(xintercept = 0, color = "red3", linetype = 2) + 
  geom_hline(yintercept = 1, color = "red3") + 
  scale_x_continuous(breaks = seq(-30, 180, 10)) +
  theme_grey(base_size = 14)
dev.off()

############## total displacement
rm(list=ls())
gc()
# source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")

##
coefListK <- readRDS("/data/afg_anon/displacement_analysis/7-30panelPlots/betaCoefList_fit.rds")
# coefListK <- readRDS("/data/tmp/xtai/12-3robustnessChecks/betaCoefList_all.rds")
totalDisplacement <- c()
for (i in 1:120) {
  if (is.null(nrow(coefListK[[i]]))) next # if coefs not available, go next
  forPlot <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
  totalDisplacement <- rbind(totalDisplacement, forPlot)
}
totalDisplacement <- totalDisplacement %>%
  mutate(daysAfter = as.numeric(substr(gsub("T|m", "", term), 1, 3)))  

pdf(paste0("/data/afg_anon/displacement_analysis/2-16-22finalFigures/fig2b.pdf"), width = 9, height = 5)
totalDisplacement %>%
  ggplot(aes(daysAfter, exp(estimate)))+
  geom_point()+
  geom_pointrange(aes(ymin = exp(conf.low), ymax = exp(conf.high)))+
  labs(x = expression(paste(italic(k) , " days after violence")),
       y = expression(atop("Increase in odds of being in a different district", "(Relative to"~italic(k)~"days prior)"))) +
  geom_vline(xintercept = 0, color = "red3", linetype = 2) +
  geom_hline(yintercept = 1, color = "red3")+
  scale_x_continuous(breaks = seq(0, 120, 5)) +
  theme_grey(base_size = 14) 
dev.off()

################## cas and peace: from 12-11plots.R
####### total displacement
rm(list=ls()); gc()
library(dplyr); library(ggplot2)
source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")
totalDisplacement <- c()
coefListK <- readRDS(paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/betaCoefList_11cas.rds"))
for (i in 1:120) {
  if (is.null(nrow(coefListK[[i]]))) next # if not processed, go next
  tmp1 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
  if (is.null(tmp1$numDays000)) {
    tmp1$numDays000 <- NA
  }
  tmp1$key <- ">=11 casualties"
  tmp1$daysAfter <- i
  tmp2 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm_S", sprintf("%03d", i), "1")), ]
  if (is.null(tmp2$numDays000)) {
    tmp2$numDays000 <- NA
  }
  if (nrow(tmp2) > 0) {
    tmp2$key <- "<11 casualties"
    tmp2$daysAfter <- i
  }
  
  totalDisplacement <- rbind(totalDisplacement, tmp1, tmp2)
}

# update 2/16/22: editor wants y-lims to be consistent
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

pdf(paste0("/data/afg_anon/displacement_analysis/2-16-22finalFigures/fig3b.pdf"), width = 10, height = 4)
plot1 <- totalDispPlotFunLims(c(">=11 casualties", "<11 casualties"))
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()

totalDisplacement <- c()
coefListK <- readRDS(paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/betaCoefList_60peace.rds"))
for (i in 1:120) {
  if (is.null(nrow(coefListK[[i]]))) next # if not processed, go next
  tmp1 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
  if (is.null(tmp1$numDays000)) {
    tmp1$numDays000 <- NA
  }
  tmp1$key <- ">=60 days since last violence"
  tmp1$daysAfter <- i
  tmp2 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm_S", sprintf("%03d", i), "1")), ]
  if (is.null(tmp2$numDays000)) {
    tmp2$numDays000 <- NA
  }
  if (nrow(tmp2) > 0) {
    tmp2$key <- "<60 days since last violence"
    tmp2$daysAfter <- i
  }
  
  totalDisplacement <- rbind(totalDisplacement, tmp1, tmp2)
}

pdf(paste0("/data/afg_anon/displacement_analysis/2-16-22finalFigures/fig3c.pdf"), width = 10, height = 4)
plot1 <- totalDispPlotFunLims(c(">=60 days since last violence", "<60 days since last violence"))
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()

# rsync -P xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis/2-16-22finalFigures/fig3b.pdf /Users/xtai/Desktop/development/displacementProj/NatureHB/finalGuidelines/finalFigures
# rsync -P xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis/2-16-22finalFigures/fig3c.pdf /Users/xtai/Desktop/development/displacementProj/NatureHB/finalGuidelines/finalFigures

################# 10/22: taleban
rm(list=ls()); gc()
library(dplyr); library(ggplot2)

prefix <- c(
  ""
)
description <- c( 
  ""
)
totalDisplacement <- c()
for (p in 1:length(prefix)) {
  coefListK <- readRDS(paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/betaCoefList", prefix[p], "_talebanIS.rds"))
  for (i in 1:120) {
    if (is.null(nrow(coefListK[[i]]))) next # if not processed, go next
    tmp1 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
    tmp1$daysAfter <- i
    # tmp1$key <- "Taleban"
    tmp1$key <- "Others"
    # tmp2 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm_N", sprintf("%03d", i), "1")), ]
    tmp2 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm_I", sprintf("%03d", i), "1")), ]
    if (nrow(tmp2) > 0) {
      tmp2$daysAfter <- i
      tmp2$key <- "IS"
    }
    tmp3 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm_T", sprintf("%03d", i), "1")), ]
    if (nrow(tmp3) > 0) {
      tmp3$daysAfter <- i
      tmp3$key <- "Taliban"
    }
    totalDisplacement <- rbind(totalDisplacement, tmp1, tmp2, tmp3)
    # totalDisplacement <- rbind(totalDisplacement, tmp3, tmp2)
  }
}
pdf(paste0("/data/afg_anon/displacement_analysis/2-16-22finalFigures/fig3a.pdf"), width = 10, height = 4)
plot1 <- totalDispPlotFunLims(c("Taliban", "IS"))
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()
# rsync -P xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis/2-16-22finalFigures/fig3a.pdf /Users/xtai/Desktop/development/displacementProj/NatureHB/finalGuidelines/finalFigures
