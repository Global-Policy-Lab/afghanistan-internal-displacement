# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# this code: for overall and event heterogeneity results --- get results using runPanelReg.R
rm(list=ls()); gc()
source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")
library(dplyr)

################ 2.  coef plots -- k = 30
coefListK <- readRDS("/data/tmp/xtai/7-30panelPlots/allK/betaCoefList.rds")

## fig 2a
dataForPlot <- coefListK[[30]]
forPlot <- dataForPlot[which(substr(dataForPlot$term, 1, 1) == "T"), ]
forPlot$plusMinus <- ifelse(substr(forPlot$term, 2, 2) == "m", 1, -1)
tmp <- gsub("T|m", "", forPlot$term)
forPlot$leadLag <- as.numeric(substr(tmp, 1, nchar(tmp) - 1))
forPlot$leadLag <- forPlot$leadLag*forPlot$plusMinus 

png(paste0("/data/tmp/xtai/7-30panelPlots/allK/summary/fig2a.png"), width = 9, height = 5, res = 300, units = "in")
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
rm(list=ls()); gc()

##
coefListK <- readRDS("/data/tmp/xtai/7-30panelPlots/allK/betaCoefList.rds")
# coefListK <- readRDS("/data/tmp/xtai/12-3robustnessChecks/betaCoefList_all.rds")
totalDisplacement <- c()
for (i in 1:120) {
  if (is.null(nrow(coefListK[[i]]))) next # if coefs not available, go next
  forPlot <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
  totalDisplacement <- rbind(totalDisplacement, forPlot)
}
totalDisplacement <- totalDisplacement %>%
  mutate(daysAfter = as.numeric(substr(gsub("T|m", "", term), 1, 3)))  

png(paste0("/data/tmp/xtai/7-30panelPlots/allK/summary/fig2b.png"), width = 9, height = 5, res = 300, units = "in")
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


########################## event heterogeneity ########################## 
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
  coefListK <- readRDS(paste0("/data/tmp/xtai/7-30panelPlots/allK/betaCoefList", prefix[p], "_talebanIS.rds"))
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

png(paste0("/data/tmp/xtai/7-30panelPlots/allK/fig3a.png"), width = 10, height = 4, res = 300, units = "in")
# plot1 <- 
  totalDisplacement %>% 
  filter(key != "Others") %>%
  ggplot(aes(daysAfter, exp(estimate)), col = as.factor(key))+
  # geom_point()+
  geom_pointrange(aes(ymin = exp(conf.low), 
                      ymax = exp(conf.high), 
                      col = as.factor(key)) )+#,
                  # position = position_dodge(width = .2)) +
  labs(#title = paste0("ISIS violence, ~", totalDisplacement[totalDisplacement$key == "ISIS" & totalDisplacement$daysAfter == 30, "T_I000"], " violent district-days; Taliban violence, ~", totalDisplacement[totalDisplacement$key == "Taliban" & totalDisplacement$daysAfter == 30, "T_T000"], " violent district-days\n(Not plotted: violence not involving ISIS or Taliban, ~", totalDisplacement[totalDisplacement$key == "Others" & totalDisplacement$daysAfter == 30, "numDays000"], " violent district-days)"),
       x = "k days after violence",
       y = "Multiplicative change in odds",
       col = "") +
  geom_vline(xintercept = 0, color = "red3", linetype = 2) +
  geom_hline(yintercept = 1, color = "red3")+
  scale_x_continuous(breaks = seq(0, 120, 5))+
  scale_y_continuous(limits = c(.9, 1.25)) +
  scale_color_manual(values=c("#420A68FF", "#DD513AFF")) + 
  theme(legend.position = c(.95, .95),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14))
dev.off()

################################ casualties and peace ############################
rm(list=ls()); gc()
library(dplyr); library(ggplot2)
totalDisplacement <- c()
coefListK <- readRDS(paste0("/data/tmp/xtai/7-30panelPlots/allK/betaCoefList_11cas.rds"))
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

coefListK <- readRDS(paste0("/data/tmp/xtai/7-30panelPlots/allK/betaCoefList_60peace_1-27.rds"))
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

png(paste0("/data/tmp/xtai/7-30panelPlots/allK/fig3b.png"), width = 10, height = 4, res = 300, units = "in")
plot1 <- totalDispPlotFun(c(">=11 casualties", "<11 casualties"))
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()

png(paste0("/data/tmp/xtai/7-30panelPlots/allK/fig3c.png"), width = 10, height = 4, res = 300, units = "in")
plot1 <- totalDispPlotFun(c(">=60 days since last violence", "<60 days since last violence"))
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()


