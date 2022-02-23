# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

# https://stackoverflow.com/questions/51504407/non-finite-value-supplied-by-optim-error-when-using-betareg
###############################################################################
rm(list=ls()); gc()
library(ggplot2); library(dplyr)

##### make data set 
distInfo <- readRDS("/data/afg_anon/displacement_analysis/district_ids_with_info.rds")
ddEvents <- readRDS(paste0("/data/afg_anon/displacement_analysis/districtDay_eventsOnly_8-11.rds")) # for testing
ddEvents$month <- lubridate::month(ddEvents$date)
ddEvents <- ddEvents %>%
  left_join(distInfo %>%
              select(distid, region),
            by = c("district_id" = "distid"))
ddEvents$district_id <- as.character(ddEvents$district_id)
ddEvents$date <- as.character(ddEvents$date)
ddEvents$month <- as.character(ddEvents$month)
modelMatR <- model.matrix(~ . + month:region, data = ddEvents) # 2152 cols
# dim(modelMatR) # 581478   2152

# save out
tmp <- modelMatR[, c(1:2069, 2087:ncol(modelMatR))]
saveRDS(tmp, file = "/data/afg_anon/displacement_analysis/districtDay_eventsOnly_3-13_regM_modelMat.rds")

########################################################
# use runPanelReg_roblfe.R
rm(list = ls()); gc()
coefListK <- readRDS("/data/afg_anon/displacement_analysis/12-3robustnessChecks/betaCoefList_regM.rds")
totalDisplacement <- c()
for (i in 1:120) {
  if (is.null(nrow(coefListK[[i]]))) next # if coefs not available, go next
  # forPlot <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
  forPlot <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i))), ]
  totalDisplacement <- rbind(totalDisplacement, forPlot)
}
totalDisplacement <- totalDisplacement %>%
  mutate(daysAfter = as.numeric(substr(gsub("T|m", "", term), 1, 3)))  


pdf(paste0("/data/afg_anon/displacement_analysis/2-16-22finalFigures/figSM_robModel.pdf"), width = 9, height = 5)
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
# rsync -P xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis/2-16-22finalFigures/figSM_robModel.pdf /Users/xtai/Desktop/development/displacementProj/NatureHB/finalGuidelines/finalFigures
