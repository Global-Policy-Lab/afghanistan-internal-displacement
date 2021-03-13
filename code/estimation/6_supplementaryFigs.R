# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# Figures S1 and S3
##################### S1: anticipatory effects 
### all k coefs
rm(list=ls()); gc()
library(dplyr); library(ggplot2)

coefListK <- readRDS("/data/tmp/xtai/7-30panelPlots/allK/betaCoefList.rds")

nonCapList <- readRDS("/data/tmp/xtai/9-25distHeterogeneity/betaCoefList_nonCapital.rds")

capList <- readRDS("/data/tmp/xtai/9-25distHeterogeneity/betaCoefList_provincialCap.rds")

### casualties, peace, taliban (_T, _I for Taliban and ISIS)
# the easiest way is to grep _S, _T and _I from "term" column and rename it, removing _S
casList <- readRDS("/data/tmp/xtai/7-30panelPlots/allK/betaCoefList_11cas.rds")

# _S for less than 11, less than 60 days
lowCasList <- lapply(casList, FUN = function(x) x[grepl("_S", x$term, fixed = TRUE), ])
lowCasList <- lapply(lowCasList, FUN = function(x) {
  x$term <- gsub("_S", "", x$term, fixed = TRUE)
  return(x)
}
)
highCasList <- lapply(casList, FUN = function(x) x[(!grepl("_S", x$term, fixed = TRUE)), ])

### PEACE
peaceList <- readRDS("/data/tmp/xtai/7-30panelPlots/allK/betaCoefList_60peace_1-27.rds")
shortPeaceList <- lapply(peaceList, FUN = function(x) x[grepl("_S", x$term, fixed = TRUE), ])
shortPeaceList <- lapply(shortPeaceList, FUN = function(x) {
  x$term <- gsub("_S", "", x$term, fixed = TRUE)
  return(x)
}
)
longPeaceList <- lapply(peaceList, FUN = function(x) x[(!grepl("_S", x$term, fixed = TRUE)), ])

## taliban IS
typeList <- readRDS("/data/tmp/xtai/7-30panelPlots/allK/betaCoefList_talebanIS.rds")
ISList <- lapply(typeList, FUN = function(x) x[grepl("_I", x$term, fixed = TRUE), ])
ISList <- lapply(ISList, FUN = function(x) {
  x$term <- gsub("_I", "", x$term, fixed = TRUE)
  return(x)
}
)
talibanList <- lapply(typeList, FUN = function(x) x[grepl("_T", x$term, fixed = TRUE), ])
talibanList <- lapply(talibanList, FUN = function(x) {
  x$term <- gsub("_T", "", x$term, fixed = TRUE)
  return(x)
}
)

extractFun <- function(dtf) {
  out <- dtf %>%
    filter(substr(term, 1, 1) == "T") %>%
    mutate(plusMinus = ifelse(substr(term, 2, 2) == "m", 1, -1)) %>% # m are positive taus (1:180)
    # filter(substr(term, 1, 1) == "T" & substr(term, 2, 2) != "m") %>%
    mutate(tmp = gsub("T|m", "", term),
           leadLag = plusMinus*as.numeric(substr(tmp, 1, nchar(tmp) - 1))) %>%
    filter(leadLag %in% -10:10) %>%
    select(conf.low, conf.high, estimate, leadLag)
  return(out)
}
dataForPlot <- extractFun(coefListK[[30]]) %>%
  mutate(key = "1_Overall") %>%
  bind_rows(extractFun(nonCapList[[30]]) %>%
              mutate(key = "2_Non-capitals")) %>%
  bind_rows(extractFun(capList[[30]]) %>%
              mutate(key = "3_Capitals")) %>%
  bind_rows(extractFun(highCasList[[30]]) %>%
              mutate(key = "4_High casualty")) %>%
  bind_rows(extractFun(lowCasList[[30]]) %>%
              mutate(key = "5_Low casualty")) %>%
  bind_rows(extractFun(longPeaceList[[30]]) %>%
              mutate(key = "6_Long peacetime")) %>%
  bind_rows(extractFun(shortPeaceList[[30]]) %>%
              mutate(key = "7_Short peacetime")) %>%
  bind_rows(extractFun(talibanList[[30]]) %>%
              mutate(key = "8_Taliban")) %>%
  bind_rows(extractFun(ISList[[30]]) %>%
              mutate(key = "9_ISIS")) 
  

png(paste0("/data/tmp/xtai/7-30panelPlots/allK/summary/figSM_ant.png"), width = 9, height = 5.5, res = 300, units = "in")
dataForPlot %>%
  ggplot(aes(key, exp(estimate)))+
  geom_pointrange(aes(ymin = exp(conf.low), ymax = exp(conf.high), 
                      group = as.factor(leadLag)),
                  position = position_dodge(width = .6),
                  size = .4, fatten = 2) +
  # coord_flip() + 
  geom_vline(xintercept = as.factor(dataForPlot$key), color = "red3", linetype = 2) +
  geom_hline(yintercept = 1, color = "red3") + 
  scale_color_manual(#labels = c("Top 5 districts", "Home provincial capital", "Within 50km"), 
    values=c("#420A68FF", "#DD513AFF", "#FCA50AFF")) +
  guides(color = guide_legend(override.aes = list(linetype = "blank")), 
         # linetype = guide_legend(override.aes = list(shape = NA))
         linetype = FALSE # suppress line for first plot; only do color
  ) +
  labs(
    # title = "30-day displacement: coefficients for tau = -10 to 10",
    y = "Increase in odds of being in a different district\n(Relative to 30 days prior)",
    x = "")+
  theme_grey(base_size = 14) +
  scale_x_discrete(labels = c("Overall",
                              "Non-capitals",
                              "Capitals",
                              "High\ncasualty",
                              "Low\ncasualty",
                              "Long\npeacetime",
                              "Short\npeacetime",
                              "Taliban",
                              "IS"))# +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

##################### S3: robustness check
######## robustness check: remove possibly lower-quality data
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

ddOutcomes <- readRDS(paste0("/data/tmp/xtai/districtDay_8-12_outcomeOnly.rds")) 
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

saveRDS(ddOutcomesRob, paste0("/data/tmp/xtai/districtDay_12-3_outcomeOnly_rob.rds")) 

# nohup ./myCode/runPanelReg.R districtDay_eventsOnly_8-11 districtDay_12-3_outcomeOnly_rob 79 all &

############################# RESULTS ###############################
coefListK <- readRDS("/data/tmp/xtai/12-3robustnessChecks/betaCoefList_all.rds")
totalDisplacement <- c()
for (i in 1:120) {
  if (is.null(nrow(coefListK[[i]]))) next # if coefs not available, go next
  forPlot <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
  totalDisplacement <- rbind(totalDisplacement, forPlot)
}
totalDisplacement <- totalDisplacement %>%
  mutate(daysAfter = as.numeric(substr(gsub("T|m", "", term), 1, 3)))  

png(paste0("/data/tmp/xtai/12-3robustnessChecks/plots/figSM_robY.png"), width = 9, height = 5, res = 300, units = "in")
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

