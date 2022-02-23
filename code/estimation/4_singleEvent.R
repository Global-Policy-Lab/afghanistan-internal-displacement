# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# first use runPanelReg_singlelfe.R
rm(list=ls()); gc()
library(ggplot2); library(dplyr)
### non-robust version
#### put rds together
outDTF <- readRDS("/data/afg_anon/displacement_analysis/10-7singleViolence/outDTF.rds")
for (i in seq(from = 1, to = 3601, by = 100)) {
  tmp <- readRDS(paste0("/data/afg_anon/displacement_analysis/10-7singleViolence/outDTF_", i, "_", i + 99, "lfe.rds"))
  outDTF[i:(i+99), 3:ncol(outDTF)] <- tmp[i:(i+99), 3:ncol(tmp)] 
}
tmp <- readRDS(paste0("/data/afg_anon/displacement_analysis/10-7singleViolence/outDTF_3701_3724lfe.rds"))
outDTF[3701:nrow(outDTF), 3:ncol(outDTF)] <- tmp[3701:nrow(outDTF), 3:ncol(tmp)] 

saveRDS(outDTF, file = "/data/afg_anon/displacement_analysis/10-7singleViolence/outDTF_lfe.rds")
# sum(!is.na(outDTF$T0001)) # 2359 ---- correct
# identical(as.numeric(!is.na(outDTF$T0001)), keep) # true
# numNA <- apply(outDTF[, 3:ncol(outDTF)], 1, function(x) sum(is.na(x)))
# table(numNA)
# numNA
# 0  211 
# 2359 1365 
##########################################################################################

rm(list=ls()); gc()
library(dplyr); library(ggplot2)
outDTF <- readRDS("/data/afg_anon/displacement_analysis/10-7singleViolence/outDTF_lfe.rds") # this one

extractedStats <- data.frame(district = outDTF$distid, 
                             date = outDTF$date_start, 
                             stringsAsFactors = FALSE) # take mean of first 30

extractedStats$t1to15 = apply(outDTF[, 168:182], MARGIN = 1, FUN = mean)
extractedStats$t16to30 = apply(outDTF[, 153:167], MARGIN = 1, FUN = mean)
extractedStats$t31to45 = apply(outDTF[, 138:152], MARGIN = 1, FUN = mean)
extractedStats$t46to60 = apply(outDTF[, 123:137], MARGIN = 1, FUN = mean)
extractedStats$t61to75 = apply(outDTF[, 108:122], MARGIN = 1, FUN = mean)
extractedStats$t76to90 = apply(outDTF[, 93:107], MARGIN = 1, FUN = mean)

### now add district characteristics 
# districtInfo <- readRDS("/data/tmp/xtai/data/district_ids_with_info.rds")
districtInfo <- readRDS("/data/afg_anon/displacement_analysis/district_ids_with_info.rds")

extractedStats <- extractedStats %>%
  left_join(districtInfo %>%
              select(distid, provincialCapital, TOTAL),
            by = c("district" = "distid")) %>%
  mutate(TOTAL = log(TOTAL))

############ now add event characteristics 
# load("/data/tmp/xtai/allMyEvents_7-29.Rdata")
load("/data/afg_anon/displacement_analysis/allMyEvents_7-29.Rdata")

####### peacetime --- this is by district-day
summarizeEvents <- myEvents %>% 
  group_by(distid, date_start) %>%
  summarize(numEvents = dplyr::n(),
            numCasualties = sum(best))
summarizeEvents$date_start <- as.Date(summarizeEvents$date_start) # 8513 obs
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
getNAs$fillIn[is.na(getNAs$daysFromLastEvent)] <- as.numeric(difftime(as.Date(getNAs$date_start[is.na(getNAs$daysFromLastEvent)]), as.Date("2013-01-01"), units = "days"))
# sum(getNAs$fillIn >= 60)
getNAs$fillIn[getNAs$fillIn < 60] <- NA # these will not be in regressions because y var only starts in april --- so doesn't make a difference 

#### NOTE 1/6: in single day analysis, earliest event considered will be April 2013 + 1 month lag (for k=30) + 6 months, i.e., Oct 2013. some events's upper bound for daysFromLastEvent are truncated to 9 months (if not using binary version)

tmpEvents$daysFromLastEvent[is.na(tmpEvents$daysFromLastEvent)] <- getNAs$fillIn 
tmpEvents$date_start <- as.Date(tmpEvents$date_start) 


myEvents <- myEvents %>%
  mutate(date_start = as.Date(date_start)) %>%
  left_join(tmpEvents %>%
              select(distid, date_start, daysFromLastEvent),
            by = c("distid", "date_start"))

myEvents <- myEvents %>%
  mutate(taliban = ifelse(side_a == "Taleban" | side_b == "Taleban", 1, 0),
         ISIS = ifelse(side_a == "IS" | side_b == "IS", 1, 0)
  )

# cannot distinguish between multiple events on the same district day, since there is a single indicator for it --- combine them
summarizeEvents <- myEvents %>%
  group_by(distid, date_start) %>%
  summarize(best = sum(best),
            taliban = max(taliban),
            ISIS = max(ISIS),
            daysFromLastEvent = max(daysFromLastEvent)) # just for fun --- daysFromLastEvent should be unique

extractedStats <- extractedStats %>%
  mutate(date = as.Date(date)) %>%
  left_join(summarizeEvents %>% 
              select(date_start, distid, best, taliban, ISIS, daysFromLastEvent),
            by = c("date" = "date_start", "district" = "distid"))
# 196 obs --- good

extractedStats <- extractedStats %>%
  mutate(logbest = log(best),
         logPeace = log(daysFromLastEvent))


saveRDS(extractedStats, file = "/data/afg_anon/displacement_analysis/10-7singleViolence/12-4fixedExtractedStats.rds")
# saveRDS(extractedStats, file = "/data/tmp/xtai/12-3robustnessChecks/12-4fixedExtractedStats.rds")


# 12/13: binary version of peace and cas
### note: for this set have to remove all references of best != 0 (not taking log so 0's are fine)
rm(list=ls()); gc()
library(dplyr); library(ggplot2)

extractedStats <- readRDS("/data/afg_anon/displacement_analysis/10-7singleViolence/12-4fixedExtractedStats.rds")
forRegs <- extractedStats %>%
  rename(t01to15 = t1to15)


# pdf("/data/tmp/xtai/10-7singleViolencePK/12-13regResults_binary.pdf", width = 6, height = 6)

################### 5. reduced model, robust errors
forPlot <- c()
outcomeVec <- c("t01to15", "t16to30", "t31to45", "t46to60", "t61to75", "t76to90")
for (i in 1:length(outcomeVec)){
  forRegs$outcomeVar <- forRegs[, outcomeVec[i]]
  fit <- lm((outcomeVar) ~ as.factor(provincialCapital) + TOTAL + as.factor(ISIS) + as.factor(best >= 11)  + as.factor(daysFromLastEvent >= 60), data = forRegs)
  
  tryThis <- lmtest::coefci(fit, vcov. = sandwich::vcovCL(fit, cluster = forRegs$district, type = "HC0")) # takes less than a minute! ---- coefci uses df.residual
  tmp <- data.frame(term = rownames(tryThis), stringsAsFactors = FALSE)
  tmp$conf.low <- tryThis[, 1]
  tmp$conf.high <- tryThis[, 2]
  tmp$estimate <- (tmp$conf.low + tmp$conf.high)/2
  tmp$Model <- outcomeVec[i]
  
  forPlot <- rbind(forPlot, tmp)
} 
plot4 <- forPlot %>%
  filter(term != "(Intercept)") %>%
  mutate(term = case_when(term == "TOTAL" ~ "1Population\n(log)",# 1 goes on bottom
                          term == "as.factor(provincialCapital)1" ~ "2Provincial\ncapital",
                          term == "as.factor(ISIS)1" ~ "5ISIS",
                          term == "as.factor(best >= 11)TRUE" ~ "4High\ncasualty",
                          term == "as.factor(daysFromLastEvent >= 60)TRUE" ~ "3Long\npeacetime",
                          TRUE ~ term)) %>%
  ggplot(aes(term, estimate), col = (Model))+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, col = Model),
                  position = position_dodge(width = .7),
                  size = 1.5, fatten = 3) + # fatten changes the circles only; size changes bars and legend circles
  coord_flip() + 
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) + # change this from vline to hline
  scale_colour_manual(labels = c("1-15", "16-30", "31-45", "46-60", "61-75", "76-90"),
                      values = scales::viridis_pal(option = "B")(7)) +
  guides(color = guide_legend(override.aes = list(linetype = "blank"))) +  # shape for the type of dot
  labs(#title = paste0("All obs, raw input values, clustered se"),
    y = "Coefficient estimate",
    x = NULL,
    col = "Days since\nviolence")  + # y here because of coord_flip
  theme(legend.position = c(1, 0),
        legend.justification = c("right", "bottom"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y = element_text(
          # angle = 90, 
          # vjust = 0,
          vjust = .5,
          hjust = 1)
  ) +
  scale_x_discrete(labels = c("Population\n(log)",
                              "Provincial\ncapital",
                              "Long\npeacetime",
                              "High\ncasualty",
                              "IS")) # bottom to top

pdf(paste0("/data/afg_anon/displacement_analysis/2-16-22finalFigures/fig3e.pdf"), width = 6, height = 6)
gridExtra::grid.arrange(plot4, nrow = 1)
dev.off()

# rsync -P xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis/2-16-22finalFigures/fig3e.pdf /Users/xtai/Desktop/development/displacementProj/NatureHB/finalGuidelines/finalFigures

################################## END ##################################
