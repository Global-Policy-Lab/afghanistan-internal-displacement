# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
### code for separate where_prec 

# rsync -P /Users/xtai/Desktop/development/displacementProj/code/data/allMyEvents_7-29.Rdata xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis

############# run analysis using runPanelReg_new.R
####################### total displacement
rm(list=ls()); gc()
library(dplyr); library(ggplot2)

totalDisplacement <- c()
coefListK <- readRDS("/data/afg_anon/displacement_analysis/reviewerComments/betaCoefList_fit.rds") # this one has where_prec 1 through 3
for (i in 1:120) {
  if (is.null(nrow(coefListK[[i]]))) next # if not processed, go next
  tmp1 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
  tmp1$daysAfter <- i
  tmp1$key <- "A: Exact"
  tmp2 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm_B", sprintf("%03d", i), "1")), ]
  if (nrow(tmp2) > 0) {
    tmp2$daysAfter <- i
    tmp2$key <- "B: 25km"
  }
  tmp3 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm_C", sprintf("%03d", i), "1")), ]
  if (nrow(tmp3) > 0) {
    tmp3$daysAfter <- i
    tmp3$key <- "C: District"
  }
  totalDisplacement <- rbind(totalDisplacement, tmp1, tmp2, tmp3)
}

###### CIs on same plot **** 10/28: use this version
pdf(paste0("/data/afg_anon/displacement_analysis/reviewerComments/7-27wherePrec1-3_totalDisp.pdf"), width = 10, height = 4)
totalDisplacement %>% 
  ggplot(aes(daysAfter, exp(estimate)), col = as.factor(key))+
  # geom_point()+
  geom_pointrange(aes(ymin = exp(conf.low), 
                      ymax = exp(conf.high), 
                      col = as.factor(key)),
                  position = position_dodge(width = .5) )+#,
  # position = position_dodge(width = .2)) +
  labs(#title = paste0("ISIS violence, ~", totalDisplacement[totalDisplacement$key == "ISIS" & totalDisplacement$daysAfter == 30, "T_I000"], " violent district-days; Taliban violence, ~", totalDisplacement[totalDisplacement$key == "Taliban" & totalDisplacement$daysAfter == 30, "T_T000"], " violent district-days\n(Not plotted: violence not involving ISIS or Taliban, ~", totalDisplacement[totalDisplacement$key == "Others" & totalDisplacement$daysAfter == 30, "numDays000"], " violent district-days)"),
    x = "k days after violence",
    y = "Multiplicative change in odds",
    col = "") +
  geom_vline(xintercept = 0, color = "red3", linetype = 2) +
  geom_hline(yintercept = 1, color = "red3")+
  scale_x_continuous(breaks = seq(0, 120, 5))+
  scale_y_continuous(limits = c(.95, 1.1)) +
  scale_color_manual(values=c("#420A68FF", "#DD513AFF", "#FCA50AFF")) +
  theme(legend.position = c(.93, .9),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14))
dev.off()
# rsync -P xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis/reviewerComments/7-27wherePrec1-3_totalDisp.pdf /Users/xtai/Desktop/development/displacementProj/NatureHB/review/

