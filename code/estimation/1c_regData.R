# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
################# redo leads and lags so only have to do once ################# 
rm(list=ls()); gc()
library(dplyr)

districtDay <- read.csv("/data/afg_anon/displacement_metrics/percentage_migrated_per_district_day/district_day_metrics_k1_to_k15.csv") # update 10/20: new location
districtDay$date <- as.Date("2013-03-31") + lubridate::days(districtDay$day_series)
districtDayTemplate <- districtDay %>% 
  select(district, date) %>%
  rename(district_id = district)

##### events
load("/data/afg_anon/displacement_analysis/allMyEvents_7-29.Rdata")
# from events data set, make
# district, day, eventYN, numEvents

summarizeEvents <- myEvents %>% 
  group_by(distid, date_start) %>%
  summarize(numEvents = dplyr::n(),
            numCasualties = sum(best))

summarizeEvents$date_start <- as.Date(summarizeEvents$date_start) # 8513 obs

source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")
ddevents <- makeDataset(summarizeEvents, districtDayTemplate, "", 180)
### 8/11 update: only use tau = -30 to 180
# m is in the past*****
ddevents <- ddevents[, 1:213]
# saveRDS(ddevents, file = "/data/tmp/xtai/districtDay_eventsOnly_8-11.rds")
saveRDS(ddevents, file = "/data/afg_anon/displacement_analysis/districtDay_eventsOnly_8-11.rds")


############################ HETEROGENEITY ###############################
# >= 11 casualties and < 11 casualties
# >= 60 days peacetime and < 60 days peacetime
# Taliban, ISIS

####### 11 casualties
summarizeEventsGE11 <- summarizeEvents[which(summarizeEvents$numCasualties >= 11), ]
summarizeEventsL11 <- summarizeEvents[which(summarizeEvents$numCasualties < 11), ]

districtDayPart1 <- makeDataset(summarizeEventsGE11, districtDayTemplate, prefix = "", 180)
districtDayPart2 <- makeDataset(summarizeEventsL11, districtDayTemplate, prefix = "_S", 180)
ddevents <- left_join(districtDayPart1,
                      districtDayPart2)
ddevents <- ddevents[, c(1:213, 364:574)]
# saveRDS(ddevents, file = "/data/tmp/xtai/districtDay_eventsOnly_8-11_11casualties.rds")
saveRDS(ddevents, file = "/data/afg_anon/displacement_analysis/districtDay_eventsOnly_8-11_11casualties.rds")

####### peacetime
# time since last event
tmpEvents <- summarizeEvents %>%
  arrange(distid, date_start)
tmpEvents$previousEventDate <- c(as.Date(NA), tmpEvents$date_start[-length(tmpEvents$date_start)])
tmpEvents$previousDistid <- c(NA, tmpEvents$distid[-length(tmpEvents$date_start)])
tmpEvents$previousEventDate[tmpEvents$distid != tmpEvents$previousDistid] <- NA
tmpEvents$daysFromLastEvent <- as.numeric(difftime(as.Date(tmpEvents$date_start), as.Date(tmpEvents$previousEventDate), units = "days"))
##### *** 6/25: we are actually missing some that have time > 60 days --- 
getNAs <- tmpEvents[is.na(tmpEvents$daysFromLastEvent), ] 
getNAs$fillIn <- NA
getNAs$fillIn[is.na(getNAs$daysFromLastEvent)] <- as.numeric(difftime(as.Date(getNAs$date_start[is.na(getNAs$daysFromLastEvent)]), as.Date("2012-01-01"), units = "days")) # 1/6: change from 2013 to 2012
# sum(getNAs$fillIn >= 60) # 1/6: 301 (previously 122) 
getNAs$fillIn[getNAs$fillIn < 60] <- NA # these will not be in regressions because y var only starts in april 2013 --- so doesn't make a difference 
# update 1/6: 14 events saved:
# sum(!is.na(getNAs$fillIn[getNAs$date_start >= as.Date("2012-10-31") & getNAs$date_start <= as.Date("2013-02-28")])) # --- previously only got those from 3/1/2013 onwards
# distid date_start numEvents numCasualties previousEventDate previousDistid daysFromLastEvent fillIn
# <int> <date>         <int>         <dbl> <date>                     <int>             <dbl>  <dbl>
#   1    105 2012-12-15         1             1 NA                           104                NA    349
# 2   1006 2012-12-30         1             7 NA                          1005                NA    364
# 3   1403 2013-02-20         1             2 NA                          1402                NA    416
# 4   1610 2013-02-07         1             4 NA                          1609                NA    403
# 5   2006 2012-11-08         1             1 NA                          2005                NA    312
# 6   2108 2012-11-10         1             1 NA                          2107                NA    314
# 7   2203 2012-12-08         1             4 NA                          2202                NA    342
# 8   2305 2012-11-20         1             1 NA                          2304                NA    324
# 9   2912 2012-11-12         1             1 NA                          2911                NA    316
# 10   3005 2012-12-01         1             3 NA                          3003                NA    335
# 11   3007 2012-12-18         1             3 NA                          3006                NA    352
# 12   3105 2012-11-12         1             2 NA                          3103                NA    316
# 13   3210 2012-12-08         1             2 NA                          3209                NA    342
# 14   3404 2013-01-15         1             1 NA                          3403                NA    380

# only 3105, 2006, 1610, 1403, 105 have CDR, so missing 5 violent district days

# single events analysis not impacted by this because of the requirement that all 30 days before and 180 days after have data available --- these won't be included anyway 

tmpEvents$daysFromLastEvent[is.na(tmpEvents$daysFromLastEvent)] <- getNAs$fillIn 
tmpEvents$date_start <- as.Date(tmpEvents$date_start) 

summarizeEventsGE60 <- tmpEvents[which(tmpEvents$daysFromLastEvent >= 60), ] 
summarizeEventsL60 <- tmpEvents[which(tmpEvents$daysFromLastEvent < 60), ]

districtDayPart1 <- makeDataset(summarizeEventsGE60, districtDayTemplate, prefix = "", 180)
districtDayPart2 <- makeDataset(summarizeEventsL60, districtDayTemplate, prefix = "_S", 180)
ddevents <- left_join(districtDayPart1,
                      districtDayPart2)
ddevents <- ddevents[, c(1:213, 364:574)]
# saveRDS(ddevents, file = "/data/tmp/xtai/districtDay_eventsOnly_8-11_60peace.rds")
saveRDS(ddevents, file = "/data/afg_anon/displacement_analysis/districtDay_eventsOnly_8-11_60peace.rds")

# note that for casualties and peace, these are done on a violent day level. e.g., if there is a day where there are two events, but for the 60 preceding days there are no events, this day is considered a day with 60 days of peacetime

##### Taliban and IS

summarizeEventsV1 <- myEvents %>% 
  filter((side_a == "IS" | side_b == "IS")) %>%
  group_by(distid, date_start) %>%
  summarize(numEvents = dplyr::n()) %>%
  mutate(date_start = as.Date(date_start))
# 908 IS

summarizeEventsV2 <- myEvents %>% 
  filter((side_a != "IS" & side_b != "IS") & (side_a == "Taleban" | side_b == "Taleban")) %>%
  group_by(distid, date_start) %>%
  summarize(numEvents = dplyr::n()) %>%
  mutate(date_start = as.Date(date_start))
# 7537 

summarizeEventsV3 <- myEvents %>% 
  filter(side_a != "IS" & side_b != "IS" & side_a != "Taleban" & side_b != "Taleban") %>%
  group_by(distid, date_start) %>%
  summarize(numEvents = dplyr::n()) %>%
  mutate(date_start = as.Date(date_start))
# 85 remaining

districtDayPart1 <- makeDataset(summarizeEventsV3, districtDayTemplate, prefix = "", 180)
districtDayPart2 <- makeDataset(summarizeEventsV1, districtDayTemplate, prefix = "_I", 180) # non-taleban
districtDayPart3 <- makeDataset(summarizeEventsV2, districtDayTemplate, prefix = "_T", 180) # taleban
ddeventsOnly <- left_join(districtDayPart1,
                          districtDayPart2)
ddeventsOnly <- left_join(ddeventsOnly,
                          districtDayPart3)
ddevents <- ddeventsOnly[, c(1:213, 364:574, 725:935)]
# saveRDS(ddevents, file = "/data/tmp/xtai/districtDay_eventsOnly_8-11_talebanIS.rds")
saveRDS(ddevents, file = "/data/afg_anon/displacement_analysis/districtDay_eventsOnly_8-11_talebanIS.rds")


##### WHERE_PREC

summarizeEventsV1 <- myEvents %>% 
  filter(where_prec == 1) %>%
  group_by(distid, date_start) %>%
  summarize(numEvents = dplyr::n()) %>%
  mutate(date_start = as.Date(date_start))
# 908 IS

summarizeEventsV2 <- myEvents %>% 
  filter(where_prec == 2) %>%
  group_by(distid, date_start) %>%
  summarize(numEvents = dplyr::n()) %>%
  mutate(date_start = as.Date(date_start))
# 7537 

summarizeEventsV3 <- myEvents %>% 
  filter(where_prec == 3) %>%
  group_by(distid, date_start) %>%
  summarize(numEvents = dplyr::n()) %>%
  mutate(date_start = as.Date(date_start))
# 85 remaining

districtDayPart1 <- makeDataset(summarizeEventsV1, districtDayTemplate, prefix = "", 180)
districtDayPart2 <- makeDataset(summarizeEventsV2, districtDayTemplate, prefix = "_B", 180) # where_prec2
districtDayPart3 <- makeDataset(summarizeEventsV3, districtDayTemplate, prefix = "_C", 180) # where_prec3
ddeventsOnly <- left_join(districtDayPart1,
                          districtDayPart2)
ddeventsOnly <- left_join(ddeventsOnly,
                          districtDayPart3)
ddevents <- ddeventsOnly[, c(1:213, 364:574, 725:935)]
saveRDS(ddevents, file = "/data/afg_anon/displacement_analysis/districtDay_eventsOnly_8-11_wherePrec.rds")


############################# OUTCOME DATA SET ###############################
library(magrittr)
rm(list=ls())

districtDaySet1 <- read.csv("/data/afg_anon/displacement_metrics/percentage_migrated_per_district_day/district_day_metrics_k1_to_k15.csv")

districtDaySet1 <- districtDaySet1 %>%
  dplyr::select(-starts_with("non_migrated")) %>%
  dplyr::mutate_at(vars(starts_with("migrated_as_per")), list(~ ./impacted)) %>%
  dplyr::mutate(date = as.Date("2013-03-31") + lubridate::days(day_series)) %>%
  rename(district_id = district)

districtDaySet2 <- read.csv("/data/afg_anon/displacement_metrics/percentage_migrated_per_district_day/district_day_metrics_k16_to_k30.csv")

districtDaySet2 <- districtDaySet2 %>%
  dplyr::select(-starts_with("non_migrated")) %>%
  dplyr::mutate_at(vars(starts_with("migrated_as_per")), list(~ ./impacted)) %>%
  dplyr::mutate(date = as.Date("2013-03-31") + lubridate::days(day_series)) %>%
  rename(district_id = district)

districtDaySet3 <- read.csv("/data/afg_anon/displacement_metrics/percentage_migrated_per_district_day/district_day_metrics_k31_to_k45.csv")

districtDaySet3 <- districtDaySet3 %>%
  dplyr::select(-starts_with("non_migrated")) %>%
  dplyr::mutate_at(vars(starts_with("migrated_as_per")), list(~ ./impacted)) %>%
  dplyr::mutate(date = as.Date("2013-03-31") + lubridate::days(day_series)) %>%
  rename(district_id = district)

districtDaySet4 <- read.csv("/data/afg_anon/displacement_metrics/percentage_migrated_per_district_day/district_day_metrics_k46_to_k60.csv") 

districtDaySet4 <- districtDaySet4 %>%
  dplyr::select(-starts_with("non_migrated")) %>%
  dplyr::mutate_at(vars(starts_with("migrated_as_per")), list(~ ./impacted)) %>%
  dplyr::mutate(date = as.Date("2013-03-31") + lubridate::days(day_series)) %>%
  rename(district_id = district)

districtDaySet5 <- read.csv("/data/afg_anon/displacement_metrics/percentage_migrated_per_district_day/district_day_metrics_k61_to_k70.csv") 

districtDaySet5 <- districtDaySet5 %>%
  dplyr::select(-starts_with("non_migrated")) %>%
  dplyr::mutate_at(vars(starts_with("migrated_as_per")), list(~ ./impacted)) %>%
  dplyr::mutate(date = as.Date("2013-03-31") + lubridate::days(day_series)) %>%
  rename(district_id = district)

districtDaySet6 <- read.csv("/data/afg_anon/displacement_metrics/percentage_migrated_per_district_day/district_day_metrics_k71_to_k80.csv") 

districtDaySet6 <- districtDaySet6 %>%
  dplyr::select(-starts_with("non_migrated")) %>%
  dplyr::mutate_at(vars(starts_with("migrated_as_per")), list(~ ./impacted)) %>%
  dplyr::mutate(date = as.Date("2013-03-31") + lubridate::days(day_series)) %>%
  rename(district_id = district)

districtDaySet7 <- read.csv("/data/afg_anon/displacement_metrics/percentage_migrated_per_district_day/district_day_metrics_k81_to_k90.csv") 

districtDaySet7 <- districtDaySet7 %>%
  dplyr::select(-starts_with("non_migrated")) %>%
  dplyr::mutate_at(vars(starts_with("migrated_as_per")), list(~ ./impacted)) %>%
  dplyr::mutate(date = as.Date("2013-03-31") + lubridate::days(day_series)) %>%
  rename(district_id = district)

districtDaySet8 <- read.csv("/data/afg_anon/displacement_metrics/percentage_migrated_per_district_day/district_day_metrics_k91_to_k100.csv") 

districtDaySet8 <- districtDaySet8 %>%
  dplyr::select(-starts_with("non_migrated")) %>%
  dplyr::mutate_at(vars(starts_with("migrated_as_per")), list(~ ./impacted)) %>%
  dplyr::mutate(date = as.Date("2013-03-31") + lubridate::days(day_series)) %>%
  rename(district_id = district)

districtDaySet9 <- read.csv("/data/afg_anon/displacement_metrics/percentage_migrated_per_district_day/district_day_metrics_k101_to_k110.csv") 

districtDaySet9 <- districtDaySet9 %>%
  dplyr::select(-starts_with("non_migrated")) %>%
  dplyr::mutate_at(vars(starts_with("migrated_as_per")), list(~ ./impacted)) %>%
  dplyr::mutate(date = as.Date("2013-03-31") + lubridate::days(day_series)) %>%
  rename(district_id = district)

districtDaySet10 <- read.csv("/data/afg_anon/displacement_metrics/percentage_migrated_per_district_day/district_day_metrics_k111_to_k120.csv") 

districtDaySet10 <- districtDaySet10 %>%
  dplyr::select(-starts_with("non_migrated")) %>%
  dplyr::mutate_at(vars(starts_with("migrated_as_per")), list(~ ./impacted)) %>%
  dplyr::mutate(date = as.Date("2013-03-31") + lubridate::days(day_series)) %>%
  rename(district_id = district)

############ do join
districtDay <- dplyr::left_join(districtDaySet1 %>% 
                                  select(-day_series), 
                                districtDaySet2 %>% 
                                  select(-day_series, -impacted))

districtDay <- dplyr::left_join(districtDay, districtDaySet3 %>%
                                  select(-day_series, -impacted))

districtDay <- dplyr::left_join(districtDay, districtDaySet4 %>%
                                  select(-day_series, -impacted))

districtDay <- dplyr::left_join(districtDay, districtDaySet5 %>%
                                  select(-day_series, -impacted))

districtDay <- dplyr::left_join(districtDay, districtDaySet6 %>%
                                  select(-day_series, -impacted))

districtDay <- dplyr::left_join(districtDay, districtDaySet7 %>%
                                  select(-day_series, -impacted))
districtDay <- dplyr::left_join(districtDay, districtDaySet8 %>%
                                  select(-day_series, -impacted))
districtDay <- dplyr::left_join(districtDay, districtDaySet9 %>%
                                  select(-day_series, -impacted))
districtDay <- dplyr::left_join(districtDay, districtDaySet10 %>%
                                  select(-day_series, -impacted))

################################################################################

for (k in 1:120) {
  cat(k, ", ")
  # lag the outcome variable by k days
  # have to do this roundabout way because of tibble structure; need to get it to play nice with magrittr and dplyr::lag
  districtDay$tmpIn <- districtDay[, paste0("migrated_as_per_day_", k)]
  
  districtDay <- districtDay %>%
    arrange(district_id, date) %>%
    group_by(district_id) 
  
  districtDay <- districtDay %>%
    mutate(tmpOut = dplyr::lag(tmpIn, n = k, order_by = date))
  
  districtDay <- districtDay %>%
    ungroup() %>%
    data.frame()
  
  districtDay[, paste0("lagPercentMigratedk", sprintf("%03d", k))] <- districtDay$tmpOut
  
  districtDay <- districtDay %>%
    select(-tmpIn, -tmpOut)
  
  ### total impacted
  districtDay$tmpIn <- districtDay$impacted
  
  districtDay <- districtDay %>%
    arrange(district_id, date) %>%
    group_by(district_id) 
  
  districtDay <- districtDay %>%
    mutate(tmpOut = dplyr::lag(tmpIn, n = k, order_by = date))
  
  districtDay <- districtDay %>%
    ungroup() %>%
    data.frame()
  
  districtDay[, paste0("lagTotalImpactedk", sprintf("%03d", k))] <- districtDay$tmpOut
  
  districtDay <- districtDay %>%
    select(-tmpIn, -tmpOut)
} 

districtDay <- districtDay %>%
  select(-starts_with("migrated_as_per_day"))


saveRDS(districtDay, file = "/data/afg_anon/displacement_analysis/districtDay_8-12_outcomeOnly.rds")

############################### WPG outcomes #################################
# /data/afg_anon/displacement_metrics/visits_per_district_day/k_30.csv

# k30data <- read.csv("/data/afg_anon/displacement_metrics/visits_per_district_day/k_30.csv")
# this has 231M rows
# origin_district,impact_day,impacted,destination_district,visit_day,visits
# 101,1,86936,101,31,74020.0
# 101,1,86936,102,31,43.0
# 101,1,86936,103,31,13.0
# 101,1,86936,104,31,2.0
# 101,1,86936,105,31,14.0
# 101,1,86936,106,31,0.0

# k30data <- read.csv("/data/afg_anon/displacement_metrics/visits_per_district_day/k_30.csv", nrows = 1000)
# impact_day 1 is 2013-04-01
# impact_day 1461 is 
# as.Date("2013-04-01") + lubridate::days(1460) #days(impacted_day - 1)

# reduce this data set to 581478 obs, like districtDay. 

##### 1/7: for k=7,30,90, make:
# these are the final outcome variables used: 
# 1. sameProvincialCapComp --- this one is done
# 2. top5exclhomeComp 
# 3. remainingCapComp 
# 4. sameProvNonCapComp 
# 5. otherProvNonCapComp

rm(list=ls());gc()
library(dplyr)

# districtInfo <- readRDS("/data/tmp/xtai/data/district_ids_with_info.rds")
districtInfo <- readRDS("/data/afg_anon/displacement_analysis/district_ids_with_info.rds")

makeMetricsK <- function(k) {
  k30data <- read.csv(paste0("/data/afg_anon/displacement_metrics/visits_per_district_day/k_", k, ".csv"))
  k30data <- k30data %>%
    filter(origin_district != destination_district) %>% # *** Add this
    left_join(districtInfo %>%
                select(distid, provid, provincialCapital), 
              by = c("origin_district" = "distid")) %>%
    rename(originProvid = provid,
           originProvCapital = provincialCapital) %>%
    left_join(districtInfo %>%
                select(distid, provid, provincialCapital), 
              by = c("destination_district" = "distid")) %>%
    rename(destProvid = provid,
           destProvCapital = provincialCapital) %>%
    mutate(toSameProvincialCap = ifelse(destProvCapital == 1 & originProvid == destProvid, 1, 0)
    )
  
  
  outData <- k30data %>%
    group_by(origin_district, impact_day) %>% # now aggregate
    summarize(
      toSameProvincialCap = sum(visits[which(toSameProvincialCap == 1)]),
      toTop5exclhome = sum(visits[which(destination_district %in% c(101, 2401, 2001, 1601, 801) & originProvid != destProvid)]),
      toRemainingCap = sum(visits[which(!(destination_district %in% c(101, 2401, 2001, 1601, 801)) & originProvid != destProvid & destProvCapital == 1)]),
      # i made nonCap in different prov earlier: destProvCapital == 0 & originProvid != destProvid
      toOtherProvNonCap = sum(visits[which(destProvCapital == 0 & originProvid != destProvid)]),
      toSameProvNonCap = sum(visits[which(destProvCapital == 0 & originProvid == destProvid)])
    )
  
  rm("k30data"); gc()
  
  outData <- outData %>%
    mutate(date = as.Date("2013-04-01") + lubridate::days(impact_day - 1))
  
  # districtDay <- readRDS("/data/tmp/xtai/districtDay_8-12_outcomeOnly.rds")
  districtDay <- readRDS("/data/afg_anon/displacement_analysis/districtDay_8-12_outcomeOnly.rds")
  
  forNumMoved <- districtDay[, c("district_id", "date", paste0("lagTotalImpactedk", sprintf("%03d", k)), paste0("lagPercentMigratedk", sprintf("%03d", k)))]
  names(forNumMoved)[3] <- "numImpacted" # lagged
  names(forNumMoved)[4] <- "percentMoved"
  forNumMoved <- forNumMoved %>%
    mutate(numMoved = round(numImpacted*percentMoved)) # small rounding errors
  
  districtDay <- districtDay %>% 
    select(district_id, date) %>%
    left_join(outData %>%
                select(-impact_day), # just drop impact_day
              by = c("district_id" = "origin_district", "date" = "date")) 
  districtDay <- districtDay %>%
    arrange(district_id, date) %>%
    group_by(district_id) 
  
  districtDay <- districtDay %>%
    mutate(
      toSameProvincialCap = dplyr::lag(toSameProvincialCap, n = k, order_by = date),
      toTop5exclhome = dplyr::lag(toTop5exclhome, n = k, order_by = date),
      toRemainingCap = dplyr::lag(toRemainingCap, n = k, order_by = date),
      toOtherProvNonCap = dplyr::lag(toOtherProvNonCap, n = k, order_by = date),
      toSameProvNonCap = dplyr::lag(toSameProvNonCap, n = k, order_by = date)
    )
  
  districtDay <- districtDay %>% 
    ungroup() %>%
    data.frame() %>%
    left_join(forNumMoved %>%
                select(district_id, date, numMoved, numImpacted),
              by = c("district_id", "date")) %>%
    dplyr::mutate(
      sameProvincialCapComp = toSameProvincialCap/numMoved,
      top5exclhomeComp = toTop5exclhome/numMoved,
      remainingCapComp = toRemainingCap/numMoved,
      otherProvNonCapComp = toOtherProvNonCap/numMoved,
      sameProvNonCapComp = toSameProvNonCap/numMoved)
  
  # convert to integer --- not sure why it's numeric now
  districtDay$district_id <- type.convert(districtDay$district_id)
  
  saveRDS(districtDay, file = paste0("/data/afg_anon/displacement_analysis/districtDayWPG/districtDayWPG_9-22_outcomeOnly_", k, "_comp12-11.rds")) # new location
  
}

for (k in c(7, 30, 90)) {
  cat(k, ", ")
  makeMetricsK(k)
}
