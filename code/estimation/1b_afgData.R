# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# Data sources:
# 1. CSO data: https://data.humdata.org/dataset/estimated-population-of-afghanistan-2015-2016
# 2. Afghanistan shape file: district398.shp, available at https://esoc.princeton.edu/data/administrative-boundaries-398-districts

rm(list=ls())
library(dplyr)
library(ggplot2)

######################################################################
# CSO population by district data (2015) --- uses either CSO codes (unknown what these are) or AGCHO codes (mapping from AGCHO->AIMS is available, and I mapped AIMS->shape file manually)
# CODE_LOOKUP sheet has mapping from AGCHO->AIMS (409 rows)
# merge to our data using AIMS_DIST_CODE 

afghanShape <- sf::st_read("/Users/xtai/Dropbox/EventAnalysis/CodingWork_Rohan/DataTask/AFG_district_398/district398.shp", quiet = TRUE) %>% 
  sf::st_transform(crs = 32642) %>%
  mutate(PROV_34_NA = as.character(PROV_34_NA),
         DIST_34_NA = as.character(DIST_34_NA))
distIDmanual <- read.csv("/Users/xtai/Desktop/development/displacementProj/code/data/district_ids_withAIMS.csv", stringsAsFactors = FALSE) # I mapped these manually to AIMS
afghanShape <- afghanShape %>%
  left_join(distIDmanual %>%
              select(distid, AIMS_DIST_CODE, provincialCapital),  # also include AIMS_DIST_CODE for later
            by = c("DISTID" = "distid"))

distInfo <- openxlsx::read.xlsx("/Users/xtai/Dropbox/EventAnalysis/CodingWork_Rohan/DataTask/CSO Population by District.xlsx", sheet = "DIST_AGCHO_CODE") # 409 rows
# this is where the outcome vars i want are --- indexed by numeric AGCHO_DIST_CODE
# distInfo uses AGCHO_DIST_CODE ---- numeric from 101 to 3405, and 9999
# distInfo$AGCHO_DIST_CODE %in% mapping$AGCHO_DIST_CODE --- all TRUE

mapping <- openxlsx::read.xlsx("/Users/xtai/Dropbox/EventAnalysis/CodingWork_Rohan/DataTask/CSO Population by District.xlsx", sheet = "CODE_LOOKUP")
# this has AGCHO_DIST_CODE's mapping to AIMS_DIST_CODE --- both in character

# sum(unique(betaDistrictFE$AIMS_DIST_CODE) %in% mapping$AIMS_DIST_CODE)
# 233 --- all mapped
# only keep rows in mapping that are in betaDistrictFE --- mapping has other codes that we don't want

mapping <- mapping %>%
  filter(AIMS_DIST_CODE %in% afghanShape$AIMS_DIST_CODE) 

# two entries with AIMS_DIST_CODE == 2102 --- remove one of them
# AGCHO_PROV_NA_ENG AGCHO_PROV_CODE AGCHO_DIST_NA_ENG AGCHO_DIST_CODE CSO_PROV_NA_ENG CSO_PROV_CODE
# 83           Daykundi              22             Gizab            2205        Daykundi            24
# 295          Daykundi              22             Gizab            2205         Urozgan            25
# CSO_DIST_NA_ENG CSO_DIST_ID CSO_DIST_CODE CSO_DIST_STATUS AIMS_PROV_NA_ENG AIMS_PROV_CODE
# 83            Patoo         09*          2409       Temporary         Daykundi             22
# 295           Gizab          06          2506        Official         Daykundi             22
# AIMS_DIST_NA_ENG AIMS_DIST_CODE
# 83             Gizab           2102
# 295            Gizab           2102

mapping <- mapping %>%
  filter(CSO_DIST_STATUS == "Official") %>%
  select(AGCHO_DIST_CODE, AIMS_DIST_CODE) %>% # keeps 233 rows
  mutate(AGCHO_DIST_CODE = as.numeric(AGCHO_DIST_CODE),
         AIMS_DIST_CODE = as.numeric(AIMS_DIST_CODE))

distInfo <- mapping %>%
  left_join(distInfo %>%
              select(AGCHO_DIST_CODE, TOTAL),
            by = "AGCHO_DIST_CODE")

# sum(unique(betaDistrictFE$AIMS_DIST_CODE) %in% distInfo$AIMS_DIST_CODE) # check --- all in

distIDmanual <- distIDmanual %>%
  left_join(distInfo %>%
              select(AIMS_DIST_CODE, TOTAL),
            by = "AIMS_DIST_CODE")


#### regions: from Musa 
NorthEasternAfghanistan <- c("Badakhshan", "Baghlan", "Kunduz", "Takhar")
NorthWesternAfghanistan <- c("Balkh", "Faryab", "Jawzjan", "Samangan", "Sari Pul")
CentralAfghanistan <- c("Kabul", "Kapisa", "Logar", "Panjsher", "Parwan", "Maydan Wardak")
EasternAfghanistan <- c("Kunar", "Laghman", "Nangarhar", "Nuristan")
WesternAfghanistan <- c("Badghis", "Bamyan", "Farah", "Ghor", "Hirat")
SouthEasternAfghanistan <- c("Ghazni", "Khost", "Paktya", "Paktika")
SouthWesternAfghanistan <- c("Daykundi", "Hilmand", "Kandahar", "Nimroz", "Uruzgan", "Zabul")

regions <- data.frame(prov_name = c(NorthEasternAfghanistan, 
                                    NorthWesternAfghanistan, 
                                    CentralAfghanistan, 
                                    EasternAfghanistan, 
                                    WesternAfghanistan, 
                                    SouthEasternAfghanistan, 
                                    SouthWesternAfghanistan), 
                      region = c(rep("NE", length(NorthEasternAfghanistan)),
                                 rep("NW", length(NorthWesternAfghanistan)),
                                 rep("C", length(CentralAfghanistan)),
                                 rep("E", length(EasternAfghanistan)),
                                 rep("W", length(WesternAfghanistan)),
                                 rep("SE", length(SouthEasternAfghanistan)),
                                 rep("SW", length(SouthWesternAfghanistan))),
                      stringsAsFactors = FALSE
)

# sum(regions$prov_name %in% districtInfo$prov_name)
# [1] 34

distIDmanual <- distIDmanual %>%
  left_join(regions, by = "prov_name")

saveRDS(distIDmanual, "/Users/xtai/Desktop/development/displacementProj/code/data/district_ids_with_info.rds")

