# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

##################### anticipatory effects 
### all k coefs
rm(list=ls()); gc()
library(dplyr); library(ggplot2)

coefListK <- readRDS("/data/afg_anon/displacement_analysis/7-30panelPlots/betaCoefList_fit.rds")

nonCapList <- readRDS("/data/afg_anon/displacement_analysis/9-25distHeterogeneity/betaCoefList_nonCapital.rds")

capList <- readRDS("/data/afg_anon/displacement_analysis/9-25distHeterogeneity/betaCoefList_provincialCap.rds")

### casualties, peace, taliban (_T, _I for Taliban and ISIS)
# the easiest way is to grep _S, _T and _I from "term" column and rename it, removing _S
casList <- readRDS(paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/betaCoefList_11cas.rds"))
# 1658 Tm0011 -1.206931e-02 5.131504e-02  1.962286e-02        875
# 1659  T0001 -1.574012e-02 3.986192e-02  1.206090e-02        875
# 2103 Tm_S0011 -8.082789e-03  3.964470e-02  1.578096e-02        329   2556
# 2104  T_S0001 -6.985750e-03  3.499449e-02  1.400437e-02        329   2556

# _S for less than 11, less than 60 days
lowCasList <- lapply(casList, FUN = function(x) x[grepl("_S", x$term, fixed = TRUE), ])
lowCasList <- lapply(lowCasList, FUN = function(x) {
  x$term <- gsub("_S", "", x$term, fixed = TRUE)
  return(x)
}
)
highCasList <- lapply(casList, FUN = function(x) x[(!grepl("_S", x$term, fixed = TRUE)), ])

### PEACE
peaceList <- readRDS(paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/betaCoefList_60peace.rds"))
shortPeaceList <- lapply(peaceList, FUN = function(x) x[grepl("_S", x$term, fixed = TRUE), ])
shortPeaceList <- lapply(shortPeaceList, FUN = function(x) {
  x$term <- gsub("_S", "", x$term, fixed = TRUE)
  return(x)
}
)
longPeaceList <- lapply(peaceList, FUN = function(x) x[(!grepl("_S", x$term, fixed = TRUE)), ])

## taliban IS
typeList <- readRDS("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/betaCoefList_talebanIS.rds")
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

pdf(paste0("/data/afg_anon/displacement_analysis/2-16-22finalFigures/figSM_ant.pdf"), width = 9, height = 5.5)
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
# rsync -P xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis/2-16-22finalFigures/figSM_ant.pdf /Users/xtai/Desktop/development/displacementProj/NatureHB/finalGuidelines/finalFigures


