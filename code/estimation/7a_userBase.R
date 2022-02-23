# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
rm(list = ls()); gc()
library(dplyr)
### this script for correlations on user base
dailyModal <- read.csv("/data/afg_anon/displacement_metrics/user_count_per_district_day.csv")
# head(dailyModal)

dailyModal <- dailyModal %>%
  mutate(date = as.Date(as.character(Date), format = "%Y%m%d"))
min(dailyModal$count) 
# [1] 1

### where to find the CSO data on the server --- it got deleted --- transfer again
# rsync -P /Users/xtai/Desktop/development/displacementProj/code/data/district_ids_with_info.rds xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis/

# districtInfo <- readRDS("/data/tmp/xtai/data/district_ids_with_info.rds")
districtInfo <- readRDS("/data/afg_anon/displacement_analysis/district_ids_with_info.rds")
# March 21, 2015-March 19, 2016

## average of days in which at least one person is recorded
userBase <- dailyModal %>%
  filter(date >= as.Date("2015-03-21") & date <= as.Date("2016-03-19")) %>%
  group_by(district_id) %>%
  summarize(meanUsers = mean(count))

userBase <- userBase %>%
  left_join(districtInfo %>%
              select(distid, TOTAL),
            by = c("district_id" = "distid"))

cor(userBase$TOTAL, userBase$meanUsers, use = "complete.obs") # 0.9368902
cor.test(userBase$TOTAL, userBase$meanUsers, use = "complete.obs") 
# Pearson's product-moment correlation
# data:  userBase$TOTAL and userBase$meanUsers
# t = 41.687, df = 242, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.9194920 0.9506254
# sample estimates:
#   cor 
# 0.9368902
cor.test(userBase$TOTAL[-1], userBase$meanUsers[-1], use = "complete.obs")  # 0.6843596


cor(log(userBase$TOTAL), log(userBase$meanUsers), use = "complete.obs") # 0.5284866
cor.test(log(userBase$TOTAL), log(userBase$meanUsers), use = "complete.obs") 
# Pearson's product-moment correlation
# data:  log(userBase$TOTAL) and log(userBase$meanUsers)
# t = 9.6842, df = 242, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4315424 0.6133633
# sample estimates:
#       cor 
# 0.5284866 
