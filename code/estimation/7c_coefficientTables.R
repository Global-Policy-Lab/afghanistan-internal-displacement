# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# this script for coefficient tables (Supplementary Tables 2-9)
rm(list = ls()); gc()
library(kable); library(dplyr)
# figure 2

## fig 2a
coefListK <- readRDS("/data/afg_anon/displacement_analysis/7-30panelPlots/betaCoefList_fit.rds")
dataForPlot <- coefListK[[30]]
forPlot <- dataForPlot[which(substr(dataForPlot$term, 1, 1) == "T"), ]
forPlot$plusMinus <- ifelse(substr(forPlot$term, 2, 2) == "m", 1, -1)
tmp <- gsub("T|m", "", forPlot$term)
forPlot$leadLag <- as.numeric(substr(tmp, 1, nchar(tmp) - 1))
forPlot$leadLag <- forPlot$leadLag*forPlot$plusMinus 

forTable <- forPlot %>%
  mutate(stdErr = (estimate - conf.low)/qnorm(.975),
         pValue = (1 - pnorm( abs(estimate / stdErr) ))*2,
         estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>%
  select(leadLag, estimate, conf.low, conf.high, pValue) %>%
  arrange(leadLag)

rownames(forTable) <- NULL

# stargazer(forTable, summary = FALSE, rownames = FALSE,
#           flip = TRUE)
#           #column.labels = )

knitr::kable(forTable, "latex", booktabs = TRUE,
             # longtable = TRUE,
             align = "ccccc",
             digits = 4,
             col.names = c("Lag", "Estimate", "CI lower", "CI upper", "p-value"))

############## total displacement (Figure 2a)
rm(list=ls()); gc()
# source("/home/xtai/myCode/cleanCode/analyzeResultsFuns.R")

##
coefListK <- readRDS("/data/afg_anon/displacement_analysis/7-30panelPlots/betaCoefList_fit.rds")
# coefListK <- readRDS("/data/tmp/xtai/12-3robustnessChecks/betaCoefList_all.rds")
totalDisplacement <- c()
for (i in 1:120) {
  if (is.null(nrow(coefListK[[i]]))) next # if coefs not available, go next
  forPlot <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
  totalDisplacement <- rbind(totalDisplacement, forPlot)
}
totalDisplacement <- totalDisplacement %>%
  mutate(daysAfter = as.numeric(substr(gsub("T|m", "", term), 1, 3)))  


### Fig 3a
################# 10/22: taleban
rm(list=ls()); gc()
library(dplyr); library(ggplot2)

prefix <- c(
  ""
)
description <- c( 
  ""
)
totalDisplacement <- c()
for (p in 1:length(prefix)) {
  coefListK <- readRDS(paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/betaCoefList", prefix[p], "_talebanIS.rds"))
  for (i in 1:120) {
    if (is.null(nrow(coefListK[[i]]))) next # if not processed, go next
    tmp1 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
    tmp1$daysAfter <- i
    # tmp1$key <- "Taleban"
    tmp1$key <- "Others"
    # tmp2 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm_N", sprintf("%03d", i), "1")), ]
    tmp2 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm_I", sprintf("%03d", i), "1")), ]
    if (nrow(tmp2) > 0) {
      tmp2$daysAfter <- i
      tmp2$key <- "IS"
    }
    tmp3 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm_T", sprintf("%03d", i), "1")), ]
    if (nrow(tmp3) > 0) {
      tmp3$daysAfter <- i
      tmp3$key <- "Taliban"
    }
    totalDisplacement <- rbind(totalDisplacement, tmp1, tmp2, tmp3)
    # totalDisplacement <- rbind(totalDisplacement, tmp3, tmp2)
  }
}

forTable <- totalDisplacement %>%
  filter(key %in% c("Taliban", "IS")) %>%
  mutate(stdErr = (estimate - conf.low)/qnorm(.975),
         pValue = (1 - pnorm( abs(estimate / stdErr) ))*2,
         estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>%
  select(daysAfter, key, estimate, conf.low, conf.high, pValue) %>%
  arrange(daysAfter)
rownames(forTable) <- NULL
knitr::kable(forTable, "latex", booktabs = TRUE,
             align = "cccccc",
             digits = 4,
             linesep = c('', '', '', '', '', '', '', '', '', '\\addlinespace'),
             col.names = c("Days after violence", "Type of violence", "Estimate", "CI lower", "CI upper", "p-value"))


################## cas and peace: from 12-11plots.R
####### total displacement
rm(list=ls()); gc()

totalDisplacement <- c()
coefListK <- readRDS(paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/betaCoefList_11cas.rds"))
for (i in 1:120) {
  if (is.null(nrow(coefListK[[i]]))) next # if not processed, go next
  tmp1 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
  if (is.null(tmp1$numDays000)) {
    tmp1$numDays000 <- NA
  }
  tmp1$key <- ">=11 casualties"
  tmp1$daysAfter <- i
  tmp2 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm_S", sprintf("%03d", i), "1")), ]
  if (is.null(tmp2$numDays000)) {
    tmp2$numDays000 <- NA
  }
  if (nrow(tmp2) > 0) {
    tmp2$key <- "<11 casualties"
    tmp2$daysAfter <- i
  }
  
  totalDisplacement <- rbind(totalDisplacement, tmp1, tmp2)
}

forTable <- totalDisplacement %>%
  mutate(stdErr = (estimate - conf.low)/qnorm(.975),
         pValue = (1 - pnorm( abs(estimate / stdErr) ))*2,
         estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>%
  select(daysAfter, key, estimate, conf.low, conf.high, pValue) %>%
  arrange(daysAfter)
rownames(forTable) <- NULL
knitr::kable(forTable, "latex", booktabs = TRUE,
             align = "cccccc",
             digits = 4,
             linesep = c('', '', '', '', '', '', '', '', '', '\\addlinespace'),
             col.names = c("Days after violence", "Type of violence", "Estimate", "CI lower", "CI upper", "p-value"))


rm(list=ls()); gc()
totalDisplacement <- c()
# coefListK <- readRDS(paste0("/data/tmp/xtai/7-30panelPlots/allK/betaCoefList_60peace.rds"))
coefListK <- readRDS(paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/betaCoefList_60peace.rds"))
for (i in 1:120) {
  if (is.null(nrow(coefListK[[i]]))) next # if not processed, go next
  tmp1 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
  if (is.null(tmp1$numDays000)) {
    tmp1$numDays000 <- NA
  }
  tmp1$key <- ">=60 days since last violence"
  tmp1$daysAfter <- i
  tmp2 <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm_S", sprintf("%03d", i), "1")), ]
  if (is.null(tmp2$numDays000)) {
    tmp2$numDays000 <- NA
  }
  if (nrow(tmp2) > 0) {
    tmp2$key <- "<60 days since last violence"
    tmp2$daysAfter <- i
  }
  
  totalDisplacement <- rbind(totalDisplacement, tmp1, tmp2)
}

forTable <- totalDisplacement %>%
  mutate(stdErr = (estimate - conf.low)/qnorm(.975),
         pValue = (1 - pnorm( abs(estimate / stdErr) ))*2,
         estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>%
  select(daysAfter, key, estimate, conf.low, conf.high, pValue) %>%
  arrange(daysAfter)
rownames(forTable) <- NULL
knitr::kable(forTable, "latex", booktabs = TRUE,
             align = "cccccc",
             digits = 4,
             linesep = c('', '', '', '', '', '', '', '', '', '\\addlinespace'),
             col.names = c("Days after violence", "Type of violence", "Estimate", "CI lower", "CI upper", "p-value"))

############ 3d
rm(list=ls()); gc()
prefix <- c("provincialCap", "nonCapital")
description <- c("Provincial capital", "Non-capital")
totalDisplacement <- c()
for (p in 1:length(prefix)) {
  coefListK <- readRDS(paste0("/data/afg_anon/displacement_analysis/9-25distHeterogeneity/betaCoefList_", prefix[p], ".rds"))
  # coefListK <- readRDS(paste0("/data/tmp/xtai/9-25distHeterogeneity/betaCoefList_", prefix[p], "_noclust.rds"))
  tmp <- c()
  for (i in 1:120) {
    if (is.null(nrow(coefListK[[i]]))) next # if not processed, go next
    forPlot <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
    tmp <- rbind(tmp, forPlot)
  }
  tmp <- tmp %>%
    mutate(daysAfter = as.numeric(substr(gsub("T|m", "", term), 1, 3)))  
  
  totalDisplacement <- totalDisplacement %>%
    bind_rows(tmp %>% 
                mutate(key = description[p]))
}

forTable <- totalDisplacement %>%
  mutate(stdErr = (estimate - conf.low)/qnorm(.975),
         pValue = (1 - pnorm( abs(estimate / stdErr) ))*2,
         estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>%
  select(daysAfter, key, estimate, conf.low, conf.high, pValue) %>%
  arrange(daysAfter)
rownames(forTable) <- NULL
knitr::kable(forTable, "latex", booktabs = TRUE,
             align = "cccccc",
             digits = 4,
             linesep = c('', '', '', '', '', '', '', '', '', '\\addlinespace'),
             col.names = c("Days after violence", "Type of violence", "Estimate", "CI lower", "CI upper", "p-value"))


####################### Figure 5
rm(list=ls()); gc()
suffix <- c("sameProvincialCapComp", 
            "top5exclhomeComp_capitals", "top5exclhomeComp_nonCap",
            "remainingCapComp_capitals", "remainingCapComp_nonCap",
            "sameProvNonCapComp_capitals", "sameProvNonCapComp_nonCap",
            "otherProvNonCapComp_capitals", "otherProvNonCapComp_nonCap")

totalDisplacement <- c()
for (p in length(suffix):1) {
  outFileName <- paste0("/data/afg_anon/displacement_analysis/9-22wherePeopleGo/betaCoefList_", suffix[p], ".rds")
  coefListK <- readRDS(outFileName)
  tmp <- c()
  for (i in 1:120) {
    if (is.null(nrow(coefListK[[i]]))) next # if coefs not available, go next
    forPlot <- coefListK[[i]][which(coefListK[[i]]$term == paste0("Tm", sprintf("%03d", i), "1")), ]
    tmp <- rbind(tmp, forPlot)
  }
  tmp <- tmp %>%
    mutate(daysAfter = as.numeric(substr(gsub("T|m", "", term), 1, 3)))  
  
  totalDisplacement <- totalDisplacement %>%
    bind_rows(tmp %>% 
                mutate(key = suffix[p]))
}

totalDisplacement$origin <- ifelse(totalDisplacement$key %in% c("top5exclhomeComp_nonCap", "sameProvincialCapComp", "remainingCapComp_nonCap", "sameProvNonCapComp_nonCap", "otherProvNonCapComp_nonCap"), "Non-capitals", "Capitals")
totalDisplacement <- totalDisplacement %>% 
  mutate(key = case_when(key == "top5exclhomeComp_nonCap" ~ "Different province: Top-5 cities",
                         key == "sameProvincialCapComp" ~ "Same province: Capital",
                         key == "remainingCapComp_nonCap" ~ "Different province: Other capitals",
                         key == "sameProvNonCapComp_nonCap" ~ "Same province: Non-capital",
                         key == "otherProvNonCapComp_nonCap" ~ "Different province: Non-capital",
                         key == "top5exclhomeComp_capitals" ~ "Different province: Top-5 cities",
                         key == "remainingCapComp_capitals" ~ "Different province: Other capitals",
                         key == "sameProvNonCapComp_capitals" ~ "Same province: Non-capital",
                         key == "otherProvNonCapComp_capitals" ~ "Different province: Non-capital",
                         key == "sameProvincialCapComp" ~ "Same province: Capital",
                         TRUE ~ key)) %>%
  arrange(desc(origin), desc(key))

forTable <- totalDisplacement %>%
  mutate(stdErr = (estimate - conf.low)/qnorm(.975),
         pValue = (1 - pnorm( abs(estimate / stdErr) ))*2,
         estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>%
  select(origin, key, daysAfter, estimate, conf.low, conf.high, pValue) 
rownames(forTable) <- NULL
knitr::kable(forTable, "latex", booktabs = TRUE,
             align = "ccccccc",
             digits = 4,
             linesep = c('', '', '\\addlinespace'),
             col.names = c("Origin district", "Destination district", "Days after violence", "Estimate", "CI lower", "CI upper", "p-value"))


########## Figure 4
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

forPlot <- forPlot %>%
  filter(term != "(Intercept)") %>%
  mutate(term = case_when(term == "TOTAL" ~ "1Population\n(log)",# 1 goes on bottom
                          term == "as.factor(provincialCapital)1" ~ "2Provincial\ncapital",
                          term == "as.factor(ISIS)1" ~ "5ISIS",
                          term == "as.factor(best >= 11)TRUE" ~ "4High\ncasualty",
                          term == "as.factor(daysFromLastEvent >= 60)TRUE" ~ "3Long\npeacetime",
                          TRUE ~ term),
         Model = case_when(Model == "t01to15" ~ "1-15",# 1 goes on bottom
                           Model == "t16to30" ~ "16-30",
                           Model == "t31to45" ~ "31-45",
                           Model == "t46to60" ~ "46-60",
                           Model == "t61to75" ~ "61-75",
                           Model == "t76to90" ~ "76-90",
                           TRUE ~ Model),
         Variable = case_when(term == "1Population\n(log)" ~ "Population (log)",# 1 goes on bottom
                              term == "2Provincial\ncapital" ~ "Provincial capital",
                              term == "5ISIS" ~ "IS",
                              term == "4High\ncasualty" ~ "High casualty",
                              term == "3Long\npeacetime" ~ "Long peacetime",
                              TRUE ~ term)) %>%
  arrange(desc(term), Model)

forTable <- forPlot %>%
  mutate(stdErr = (estimate - conf.low)/qnorm(.975),
         pValue = (1 - pnorm( abs(estimate / stdErr) ))*2,
         estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>%
  select(Variable, Model, estimate, conf.low, conf.high, pValue) 
rownames(forTable) <- NULL

knitr::kable(forTable, "latex", booktabs = TRUE,
             align = "cccccc",
             digits = 4,
             linesep = c('', '', '', '', '', '\\addlinespace'),
             col.names = c("Variable", "Days since violence", "Estimate", "CI lower", "CI upper", "p-value"))

