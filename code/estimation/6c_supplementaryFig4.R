# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
######## robustness check: y-outliers
rm(list = ls()); gc()
# 12/2: robustness check
library(dplyr)
dailyModal <- read.csv("/data/afg_anon/displacement_metrics/user_count_per_district_day.csv", stringsAsFactors = FALSE) # new location
dailyModal <- dailyModal %>%
  mutate(date = as.Date(as.character(Date), format = "%Y%m%d"))
# here only district-days with positive count are included, i.e., no zeros, missing means no users

# any district with <3 years of daily modal data: delete
tmpCount <- dailyModal %>%
  group_by(district_id) %>%
  summarize(numDays = n())
# 267 districts total 

dailyModal <- dailyModal %>%
  filter(district_id %in% tmpCount$district_id[tmpCount$numDays >= 365*3])
# 225 out of 267 districts have 3 years available 

ddOutcomes <- readRDS(paste0("/data/afg_anon/displacement_analysis/districtDay_8-12_outcomeOnly.rds")) 
ddOutcomesRob <- ddOutcomes %>%
  select(district_id, date, starts_with("lagPercent")) %>%
  filter(date >= min(ddOutcomes$date) + lubridate::days(60) & date <= max(ddOutcomes$date) - lubridate::days(90) & district_id != 101) %>%
  left_join(dailyModal %>%
              select(district_id, date, count)) %>%
  filter(!is.na(count))

# ddOutcomesRob has 290364 obs
# need to make it same order as ddEvents

ddOutcomesRob <- ddOutcomes %>%
  select(district_id, date) %>%
  left_join(ddOutcomesRob)

saveRDS(ddOutcomesRob, paste0("/data/afg_anon/displacement_analysis/districtDay_12-3_outcomeOnly_rob.rds")) 

# nohup ./myCode/runPanelReg_new.R districtDay_eventsOnly_8-11 districtDay_12-3_outcomeOnly_rob 79 all &

############################# RESULTS ###############################
# run using runPanelReg_new.R
rm(list = ls()); gc()
library(dplyr)
source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")

outFileName <- "/data/afg_anon/displacement_analysis/12-3robustnessChecks/betaCoefList.rds"

if (file.exists(outFileName)) {
  coefListK <- readRDS(outFileName)
} else {
  coefListK <- vector(mode = "list", length = 120)
}

for (i in 1:120) {
  if (!is.null(nrow(coefListK[[i]]))) next
  cat(i, ", ")
  # change as appropriate
  fileName <- paste0("/data/afg_anon/displacement_analysis/12-3robustnessChecks/beta_k", sprintf("%03d", i), "all.rds")
  if (file.exists(fileName)) {
    fitBeta <- readRDS(fileName)
    coefListK[[i]] <- betaGetCoefs(fitBeta) # overall
  } else {
    # print message
    cat(paste0("k = ", i, " does not exist \n"))
  }
}
saveRDS(coefListK, file = outFileName)

############## total displacement
rm(list=ls()); gc()
library(ggplot2); library(dplyr)
# source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")

##
coefListK <- readRDS("/data/afg_anon/displacement_analysis/12-3robustnessChecks/betaCoefList.rds")
totalDisplacement <- c()
for (i in 1:120) {
  if (is.null(nrow(coefListK[[i]]))) next # if coefs not available, go next
  forPlot <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
  totalDisplacement <- rbind(totalDisplacement, forPlot)
}
totalDisplacement <- totalDisplacement %>%
  mutate(daysAfter = as.numeric(substr(gsub("T|m", "", term), 1, 3)))  


pdf(paste0("/data/afg_anon/displacement_analysis/2-16-22finalFigures/figSM_robY.pdf"), width = 9, height = 5)
# png(paste0("/data/tmp/xtai/12-3robustnessChecks/plots/figSM_robY.png"), width = 9, height = 5, res = 300, units = "in")
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
# rsync -P xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis/2-16-22finalFigures/figSM_robY.pdf /Users/xtai/Desktop/development/displacementProj/NatureHB/finalGuidelines/finalFigures


# 2/16/2022: check N for k = 30:
source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")
k30 <- readRDS("/data/afg_anon/displacement_analysis/12-3robustnessChecks/beta_k030all.rds")
out <- betaGetCoefs(k30, clusteredSE = TRUE)

