# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
rm(list=ls()); gc()
library(ggplot2); library(dplyr)
# load("/Users/xtai/Desktop/development/displacementProj/code/data/afghanShapeAllInfo.Rdata") # use TOTAL column --- this is CSO's total population estimate

afghanShape <- sf::st_read("/Users/xtai/Dropbox/EventAnalysis/CodingWork_Rohan/DataTask/AFG_district_398/district398.shp", quiet = TRUE) %>% 
  sf::st_transform(crs = 32642)

distInfo <- readRDS("/Users/xtai/Desktop/development/displacementProj/code/data/district_ids_with_info.rds")
afghanShape <- afghanShape %>%
  left_join(distInfo %>%
              select(distid, TOTAL),
            by = c("DISTID" = "distid"))

############# CDR processing 
districtDay <- read.csv("/data/afg_anon/displacement_metrics/percentage_migrated_per_district_day/district_day_metrics_k111_to_k120.csv")

districtDay <- districtDay %>%
  # dplyr::select(-starts_with("non_migrated")) %>%
  # dplyr::mutate_at(vars(starts_with("migrated_as_per")), list(~ ./impacted)) %>%
  dplyr::mutate(date = as.Date("2013-03-31") + lubridate::days(day_series)) %>%
  dplyr::select(district, day_series, date, impacted, migrated_as_per_day_120) %>%
  rename(district_id = district) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  filter(year == 2016 & day == 1 & month %in% c(1, 5, 9)) # try not to double-count

write.csv(districtDay %>% select(district_id, month, impacted, migrated_as_per_day_120), "/data/tmp/xtai/data/10-7compareIOM.csv", row.names = FALSE)

#### try using destination districts
districtDayWPG <- read.csv("/data/afg_anon/displacement_metrics/visits_per_district_day/k_120.csv")
districtDayWPG <- districtDayWPG %>%
  filter(origin_district != destination_district) %>% # *** Add this
  # dplyr::select(-starts_with("non_migrated")) %>%
  # dplyr::mutate_at(vars(starts_with("migrated_as_per")), list(~ ./impacted)) %>%
  dplyr::mutate(date = as.Date("2013-03-31") + lubridate::days(impact_day)) %>%
  dplyr::select(origin_district, destination_district, date, impacted, visits) %>%
  # rename(district_id = destination_district) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  filter(year == 2016 & day == 1 & month %in% c(1, 5, 9)) # try not to double-count

write.csv(districtDayWPG %>% select(origin_district, destination_district, month, impacted, visits), "/data/tmp/xtai/data/10-7compareIOM_destination.csv", row.names = FALSE)

############################## 1. OUTGOING ############################## 
CDRout <- read.csv("/Users/xtai/Desktop/development/displacementProj/code/weeklyPrep/10-13/10-7compareIOM.csv")

CDRversionOutgoing <- CDRout %>%
  filter(!is.na(migrated_as_per_day_120)) %>%
  group_by(district_id) %>%
  summarize(sumYearCDR = sum(migrated_as_per_day_120),
            meanYearImpacted = mean(impacted)) %>%
  left_join(afghanShape %>%
              data.frame() %>%
              select(DISTID, PROVID, TOTAL),
            by = c("district_id" = "DISTID")) %>%
  mutate(scaledMigrated = TOTAL/meanYearImpacted*sumYearCDR)

# alternative: scale first ---- turns out it's almost the same 
# CDRversionOutgoing <- CDRout %>%
#   filter(!is.na(migrated_as_per_day_120)) %>%
#   left_join(afghanShape %>%
#               data.frame() %>%
#               select(DISTID, PROVID, TOTAL),
#             by = c("district_id" = "DISTID")) %>%
#   mutate(scaled = TOTAL/impacted*migrated_as_per_day_120) %>%
#   group_by(PROVID, district_id) %>%
#   summarize(scaledMigrated = sum(scaled))

CDRversionOutgoing$propByDist <- CDRversionOutgoing$scaledMigrated/sum(CDRversionOutgoing$scaledMigrated)
CDRversionOutgoing$propByDistExKabul <- NA
CDRversionOutgoing$propByDistExKabul[-1] <- CDRversionOutgoing$scaledMigrated[-1]/sum(CDRversionOutgoing$scaledMigrated[-1]) # -1 to exclude Kabul

######## IOM
IOM <- openxlsx::read.xlsx("/Users/xtai/Desktop/development/displacementProj/code/weeklyPrep/10-13/dtm-afghanistan-settlements-round-9-baseline-assessment.xlsx")
# use this: FledIDPs2016

# use Latitude and Longitude instead of ADM1 and 2 codes -- these look like they don't have an easy way to match 
IOMversionOutgoing <- IOM[-1, ] %>%
  select(Latitude, Longitude, FledIDPs2016) %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>% # NAs because a few are NULL (3 obs)
  filter(!is.na(Latitude))
# warnings here --- these are fine

tmp <- sf::st_as_sf(IOMversionOutgoing, coords = c("Longitude", "Latitude"), 
                    crs = 4326) %>% 
  sf::st_transform(crs = 32642) %>%
  sf::st_intersects(afghanShape, ., sparse = FALSE) # this is 398 by 2716 --- each event is on a column
whichDistricts <- apply(tmp, MARGIN = 2, FUN = function(x) which(x == TRUE)) # this will only work with points
# table(whichDistricts) # good --- no NAs
districtID <- afghanShape$DISTID[whichDistricts]

IOMversionOutgoing <- IOMversionOutgoing %>%
  bind_cols(distid = districtID) %>%
  group_by(distid) %>%
  summarize(sumYearIOM = sum(FledIDPs2016)) 

IOMversionOutgoing$propByDist <- IOMversionOutgoing$sumYearIOM/sum(IOMversionOutgoing$sumYearIOM)
IOMversionOutgoing$propByDistExKabul <- NA
IOMversionOutgoing$propByDistExKabul[-1] <- IOMversionOutgoing$sumYearIOM[-1]/sum(IOMversionOutgoing$sumYearIOM[-1]) # -1 to exclude Kabul

############# now join and aggregate by province
outOutgoingExKabul <- CDRversionOutgoing %>%
  select(district_id, propByDistExKabul, PROVID) %>%
  rename(CDR = propByDistExKabul) %>%
  left_join(IOMversionOutgoing %>%
              select(-sumYearIOM) %>%
              rename(IOM = propByDistExKabul),
            by = c("district_id" = "distid")) %>%
  group_by(PROVID) %>%
  summarize(IOM = sum(IOM, na.rm = TRUE),
            CDR = sum(CDR, na.rm = TRUE)) %>%
  left_join(afghanShape %>%
              data.frame() %>%
              select(PROVID, PROV_34_NA) %>%
              unique()) %>%
  select(-PROVID) %>%
  filter(!is.na(IOM) & !is.na(CDR))

# png("/Users/xtai/Desktop/development/displacementProj/PNASpaper/fig6a.png", width = 10, height = 4.5, res = 300, units = "in")
# pdf("/Users/xtai/Desktop/development/displacementProj/PNASpaper/fig6a.pdf", width = 10, height = 4.5)
# pdf("/Users/xtai/Desktop/development/displacementProj/PNASpaper/fig6a_4-29.pdf", width = 10, height = 4.5) # this version: includes Kabul
pdf("/Users/xtai/Desktop/development/displacementProj/PNASpaper/fig6a_5-3.pdf", width = 10, height = 4.5) # this version: excludes Kabul
outOutgoingExKabul %>%
  # outOutgoing %>%
  tidyr::pivot_longer(!PROV_34_NA, names_to = "Source", values_to = "proportion") %>%
  ggplot(aes(x = PROV_34_NA, y = proportion, fill = Source)) +
  geom_bar(position="dodge",stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(#title = "Proportion of outgoing migrants from each province\nCDR: outgoing migrants are the sum of users seen in a different district; IOM: outgoing migrants are fled IDPs\n(Kabul district is excluded)",
    x = NULL,
    y = "Province Share") +
  theme(legend.position = c(1, 1.02),
        legend.justification = c("right", "top"),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14)) +
  scale_fill_discrete(labels = c("CDR migration", "IOM displacement")) # 4/29: new
dev.off()

############################## 2. INCOMING (DESTINATION) ############################## 
# rm(list=ls()); gc()
library(ggplot2); library(dplyr)
# load("/Users/xtai/Desktop/development/displacementProj/code/data/afghanShapeAllInfo.Rdata") # use TOTAL column --- this is CSO's total population estimate

####### CDR
CDRincoming <- read.csv("/Users/xtai/Desktop/development/displacementProj/code/weeklyPrep/10-13/10-7compareIOM_destination.csv")

# scale them first
CDRversionIncoming <- CDRincoming %>%
  filter(!is.na(visits)) %>%
  left_join(afghanShape %>%
              data.frame() %>%
              select(DISTID, TOTAL),
            by = c("origin_district" = "DISTID")) %>%
  mutate(scaledIncoming = TOTAL/impacted*visits) %>%
  select(destination_district, month, scaledIncoming) %>%
  group_by(destination_district, month) %>%
  summarize(totalIncoming = sum(scaledIncoming)) %>%
  group_by(destination_district) %>%
  summarize(sumYearCDR = sum(totalIncoming)) %>%
  left_join(afghanShape %>%
              data.frame() %>%
              select(DISTID, PROVID),
            by = c("destination_district" = "DISTID"))
CDRversionIncoming$propByDist <- CDRversionIncoming$sumYearCDR/sum(CDRversionIncoming$sumYearCDR)
CDRversionIncoming$propByDistExKabul <- NA
CDRversionIncoming$propByDistExKabul[-1] <- CDRversionIncoming$sumYearCDR[-1]/sum(CDRversionIncoming$sumYearCDR[-1]) # -1 to exclude Kabul

######## IOM
# IOM <- openxlsx::read.xlsx("/Users/xtai/Desktop/development/displacementProj/code/weeklyPrep/10-13/dtm-afghanistan-settlements-round-9-baseline-assessment.xlsx")

# use Latitude and Longitude instead of ADM1 and 2 codes -- these look like they don't have an easy way to match 
### for incoming: use both arrival and returned IDPs
IOMversionIncoming <- IOM[-1, ] %>%
  select(Latitude, Longitude, ReturneeIDPs2016, ArrivalIDPs2016) %>% # modify this line
  mutate(ArrivalIDPs2016 = ArrivalIDPs2016 + ReturneeIDPs2016) %>% #### add this line
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>% # NAs because a few are NULL (3 obs)
  filter(!is.na(Latitude))
# warnings here --- these are fine

tmp <- sf::st_as_sf(IOMversionIncoming, coords = c("Longitude", "Latitude"), 
                    crs = 4326) %>% 
  sf::st_transform(crs = 32642) %>%
  sf::st_intersects(afghanShape, ., sparse = FALSE) # this is 398 by 2716 --- each event is on a column
whichDistricts <- apply(tmp, MARGIN = 2, FUN = function(x) which(x == TRUE)) # this will only work with points
# table(whichDistricts) # good --- no NAs
districtID <- afghanShape$DISTID[whichDistricts]

IOMversionIncoming <- IOMversionIncoming %>%
  bind_cols(distid = districtID) %>%
  group_by(distid) %>%
  summarize(sumYearIOM = sum(ArrivalIDPs2016)) 

IOMversionIncoming$propByDist <- IOMversionIncoming$sumYearIOM/sum(IOMversionIncoming$sumYearIOM)
IOMversionIncoming$propByDistExKabul <- NA
IOMversionIncoming$propByDistExKabul[-1] <- IOMversionIncoming$sumYearIOM[-1]/sum(IOMversionIncoming$sumYearIOM[-1]) # -1 to exclude Kabul

############# now join and aggregate by province
outIncomingExKabul <- CDRversionIncoming %>%
  select(destination_district, propByDistExKabul, PROVID) %>%
  rename(CDR = propByDistExKabul) %>%
  left_join(IOMversionIncoming %>%
              select(-sumYearIOM) %>%
              rename(IOM = propByDistExKabul),
            by = c("destination_district" = "distid")) %>%
  group_by(PROVID) %>%
  summarize(IOM = sum(IOM, na.rm = TRUE),
            CDR = sum(CDR, na.rm = TRUE)) %>%
  left_join(afghanShape %>%
              data.frame() %>%
              select(PROVID, PROV_34_NA) %>%
              unique()) %>%
  select(-PROVID) %>%
  filter(!is.na(IOM) & !is.na(CDR))

# png("/Users/xtai/Desktop/development/displacementProj/PNASpaper/fig6b.png", width = 10, height = 4.5, res = 300, units = "in")
# pdf("/Users/xtai/Desktop/development/displacementProj/PNASpaper/fig6b.pdf", width = 10, height = 4.5)
# pdf("/Users/xtai/Desktop/development/displacementProj/PNASpaper/fig6b_4-29.pdf", width = 10, height = 4.5) # this version: includes Kabul
pdf("/Users/xtai/Desktop/development/displacementProj/PNASpaper/fig6b_5-3.pdf", width = 10, height = 4.5) # this version: excludes Kabul
outIncomingExKabul %>%
  tidyr::pivot_longer(!PROV_34_NA, names_to = "Source", values_to = "proportion") %>%
  ggplot(aes(x = PROV_34_NA, y = proportion, fill = Source)) +
  geom_bar(position="dodge",stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = NULL,
    y = "Province Share") +
  theme(legend.position = c(1, 1.02),
        legend.justification = c("right", "top"),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14)) +
  scale_fill_discrete(labels = c("CDR migration", "IOM displacement")) # 4/29: new
dev.off()


### correlations; versions including Kabul district
outIncoming <- CDRversionIncoming %>%
  select(destination_district, propByDist, PROVID) %>%
  rename(CDR = propByDist) %>%
  left_join(IOMversionIncoming %>%
              select(-sumYearIOM) %>%
              rename(IOM = propByDist),
            by = c("destination_district" = "distid")) %>%
  group_by(PROVID) %>%
  summarize(IOM = sum(IOM, na.rm = TRUE),
            CDR = sum(CDR, na.rm = TRUE)) %>%
  left_join(afghanShape %>%
              data.frame() %>%
              select(PROVID, PROV_34_NA) %>%
              unique()) %>%
  select(-PROVID) %>%
  filter(!is.na(IOM) & !is.na(CDR))

outOutgoing <- CDRversionOutgoing %>%
  select(district_id, propByDist, PROVID) %>%
  rename(CDR = propByDist) %>%
  left_join(IOMversionOutgoing %>%
              select(-sumYearIOM) %>%
              rename(IOM = propByDist),
            by = c("district_id" = "distid")) %>%
  group_by(PROVID) %>%
  summarize(IOM = sum(IOM, na.rm = TRUE),
            CDR = sum(CDR, na.rm = TRUE)) %>%
  left_join(afghanShape %>%
              data.frame() %>%
              select(PROVID, PROV_34_NA) %>%
              unique()) %>%
  select(-PROVID) %>%
  filter(!is.na(IOM) & !is.na(CDR))

cor(outIncoming$IOM, outIncoming$CDR, method = "spearman") # 0.5578304
cor(outOutgoing$IOM, outOutgoing$CDR, method = "spearman") # 0.4851031
