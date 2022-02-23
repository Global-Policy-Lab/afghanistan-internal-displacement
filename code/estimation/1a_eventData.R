# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
rm(list=ls()); gc()
library(sf)
library(tidyverse)

load("/Users/xtai/Desktop/development/displacementProj/code/data/ged191")

allViolentEvents <- ged191[ged191$country == "Afghanistan", ]

# check counts
year2013to17 <- allViolentEvents %>% filter(year >= 2013 & year <= 2017) #11213
sum(year2013to17$where_prec >= 1 & year2013to17$where_prec <= 3 & year2013to17$date_prec == 1) # 5984
## end check

allViolentEvents <- allViolentEvents %>% filter(where_prec >= 1 & where_prec <= 3 & date_prec == 1) # 7/29 update: now 17424 --- cannot use %in% --- removes sf structure that we need for eventCoords

allViolentEvents <- allViolentEvents %>% filter(year >= 2012 & year <= 2018) # update 7/30: since we are using longer window, need more events 

allViolentEvents <- allViolentEvents %>% 
  select(id, year, date_start, latitude, longitude, best, type_of_violence, where_prec, side_a, side_b) # need to make distid, prov_32_na, dist32_id 

################### first make distid
afghanShape <- sf::st_read("/Users/xtai/Dropbox/EventAnalysis/CodingWork_Rohan/DataTask/AFG_district_398/district398.shp", quiet = TRUE) %>% 
  sf::st_transform(crs = 32642)
eventsCoords <- allViolentEvents %>% 
  sf::st_transform(crs = 32642)

districts <- st_intersects(afghanShape, eventsCoords, sparse = FALSE) # this is 398 by numEvents --- each event is on a column
districtYN <- apply(districts, MARGIN = 2, FUN = function(x) sum(x == TRUE)) # this will only work with points
  
whichDistricts <- apply(districts, MARGIN = 2, FUN = function(x) which(x == TRUE)) # this will only work with points
whichDistricts[which(districtYN == 0)] <- NA # this one is empty for some reason
distIDs <- afghanShape$DISTID[unlist(whichDistricts)]

allViolentEvents$distid <- distIDs
allViolentEvents <- allViolentEvents[!is.na(allViolentEvents$distid), ] # remove stray obs

# add prov_34_na and dist_34_na
allViolentEvents$prov_34_na <- afghanShape$PROV_34_NA[match(allViolentEvents$distid, afghanShape$DISTID)]
allViolentEvents$dist_34_na <- afghanShape$DIST_34_NA[match(allViolentEvents$distid, afghanShape$DISTID)]

myEvents <- allViolentEvents %>%
  mutate(provDistName = paste0(prov_34_na, "-", dist_34_na)) %>%
  select(-prov_34_na, -dist_34_na) 

myEvents <- data.frame(myEvents, stringsAsFactors = FALSE)
myEvents <- myEvents %>% select(-geometry)
save(myEvents, file = "/Users/xtai/Desktop/development/displacementProj/code/data/allMyEvents_7-29.Rdata") # this version has where_prec 1 to 3, and type_of_violence
# update 10/20: also has side_a and side_b


################## FIGURE 1 --- run this after 1b_afgData
# use this instead for district population --- taken from 7-7distritFEs2.R
rm(list = ls())
afghanShape <- sf::st_read("/Users/xtai/Dropbox/EventAnalysis/CodingWork_Rohan/DataTask/AFG_district_398/district398.shp", quiet = TRUE) %>% 
  sf::st_transform(crs = 32642)

distInfo <- readRDS("/Users/xtai/Desktop/development/displacementProj/code/data/district_ids_with_info.rds")
afghanShape <- afghanShape %>%
  left_join(distInfo %>%
              select(distid, TOTAL),
            by = c("DISTID" = "distid"))

################### tower groups 
towerGroups <- read.csv("/Users/xtai/Desktop/development/displacementProj/code/data/Final_Aggregated_GroupIDs_UTM42N.csv", stringsAsFactors = FALSE)
towerGroups$tmp <- sub("POINT (", "", towerGroups$New_Coordinates, fixed = TRUE)
towerGroups$tmp <- sub(")", "", towerGroups$tmp, fixed = TRUE)
# towerGroups$longitude <- 

tmp <- strsplit(towerGroups$tmp, " ", fixed = TRUE)
towerGroups$longitude <- unlist(lapply(tmp, FUN = function(x) as.numeric(x[1])))
towerGroups$latitude <- unlist(lapply(tmp, FUN = function(x) as.numeric(x[2])))
sites <- sf::st_as_sf(towerGroups, coords = c("longitude", "latitude"), 
                      crs = 4326) %>% 
  sf::st_transform(crs = 32642)
###############

load("/Users/xtai/Desktop/development/displacementProj/code/data/ged191")
allViolentEvents <- ged191[ged191$country == "Afghanistan", ]
allViolentEvents <- allViolentEvents %>% filter(where_prec <= 3 & date_prec == 1) # 
allViolentEvents <- allViolentEvents %>% filter(year >= 2013 & year <= 2017) # 

load("/Users/xtai/Desktop/development/displacementProj/code/data/allMyEvents_7-29.Rdata") # this has district info
eventsCoords <- allViolentEvents %>% 
  select(id, year, date_start, latitude, longitude, best, type_of_violence, where_prec, side_a, side_b) %>%
  left_join(myEvents %>%
              select(id, distid), by = "id")
eventsCoords <- eventsCoords %>% 
  sf::st_transform(crs = 32642)

png("/Users/xtai/Desktop/development/displacementProj/PNASpaper/fig1.png", width = 6, height = 5, res = 300, units = "in")
plot1 <- ggplot() + 
  # theme_void() +
  geom_sf(data = afghanShape[-352, ], aes(fill = log(TOTAL))) + # 1/28: remove Kabul
  viridis::scale_fill_viridis(alpha = .4, name = "Population") +
  labs(title = paste0("Afghanistan Districts: ", nrow(eventsCoords), " violent events in red, \n1439 cell tower groups in black")) + 
  geom_sf(data = sites, size = .6) + 
  geom_sf(data = eventsCoords, size = .3, shape = 21, fill = "darkred", col = "darkred", alpha = .7)
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()


png("/Users/xtai/Desktop/development/displacementProj/PNASpaper/fig1_inset.png", width = 6, height = 5, res = 300, units = "in")
ggplot() + 
  theme_void() +
  geom_sf(data = afghanShape[352, ]) + # 1/28: remove Kabul
  geom_sf(data = sites[sites$distid == 101, ], size = 3.5) + 
  geom_sf(data = eventsCoords[eventsCoords$distid == 101, ], size = 3.5, fill = "red", col = "red")
dev.off()

### number of events in districts with no cell tower
distsWithTower <- unique(sites$distid) # 267 of them
sum(eventsCoords$distid %in% distsWithTower) # 5021
# 5021/nrow(eventsCoords)
# [1] 0.8390709


############# 7/25: reviewer comments
# reviewer wants panel with 3 figures 

pdf("/Users/xtai/Desktop/development/displacementProj/NatureHB/review/7-25figure1_skinny.pdf", width = 5, height = 5)
# pdf("/Users/xtai/Desktop/development/displacementProj/NatureHB/review/7-25figure1.pdf", width = 6, height = 5)
ggplot() + 
  # theme_void() +
  geom_sf(data = afghanShape[-352, ], aes(fill = TOTAL)) + # 1/28: remove Kabul
  # scale_fill_gradient(trans = "log") +
  viridis::scale_fill_viridis(trans = "log", 
                              alpha = .4, 
                              name = "Population",
                              breaks = c(3000, 10000, 30000, 100000, 250000, 500000),
                              labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(legend.position = c(1, 0.35),
        legend.justification = c("right", "top"),
        legend.title = element_text(size=9))
        # legend.text = element_text(size=14),
        # axis.text=element_text(size=14),
        # axis.title=element_text(size=14))

ggplot() + 
  # theme_void() +
  geom_sf(data = afghanShape) + # 1/28: remove Kabul
  # viridis::scale_fill_viridis(alpha = .4, name = "Population") +
  # labs(title = paste0("Afghanistan Districts: ", nrow(eventsCoords), " violent events in red, \n1439 cell tower groups in black")) + 
  geom_sf(data = sites, size = .6, aes(color = "black")) +
  # geom_point(1) +
  scale_color_manual(values = "black",
                     name = NULL,
                     labels = "Cell towers") + 
  theme(legend.position = c(0, 1.02),
        legend.justification = c("left", "top"),
        legend.background=element_blank(),
        legend.key = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=1)))

ggplot() + 
  # theme_void() +
  geom_sf(data = afghanShape) + # 1/28: remove Kabul
  # viridis::scale_fill_viridis(alpha = .4, name = "Population") +
  # labs(title = paste0("Afghanistan Districts: ", nrow(eventsCoords), " violent events in red, \n1439 cell tower groups in black")) + 
  geom_sf(data = eventsCoords, size = .3, shape = 21, fill = "darkred", alpha = .7,
          aes(color = "darkred")) +
  scale_color_manual(values = "darkred",
                     name = NULL,
                     labels = "Violence") + 
  theme(legend.position = c(0, 1.02),
        legend.justification = c("left", "top"),
        legend.background=element_blank(),
        legend.key = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=1)))

dev.off()

# Kabul excluded: population 3678034
