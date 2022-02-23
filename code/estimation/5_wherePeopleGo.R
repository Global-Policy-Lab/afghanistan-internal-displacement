# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
rm(list=ls()); gc()
# runPanelReg_WPG.R to run 
library(dplyr); library(ggplot2)

#### 1/11 --- some rearranging 
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

# pdf(paste0("/data/tmp/xtai/9-22wherePeopleGo/plots/1-27comp_newLayout.pdf"), width = 7, height = 5)
plot1 <- totalDisplacement %>%
  filter(daysAfter %in% c(7, 30, 90) &
           key %in% c("top5exclhomeComp_nonCap", "sameProvincialCapComp", "remainingCapComp_nonCap", "sameProvNonCapComp_nonCap", "otherProvNonCapComp_nonCap")) %>%
  mutate(tmpCol = case_when(key == "top5exclhomeComp_nonCap" ~ "Different",
                            key == "sameProvincialCapComp" ~ "Same",
                            key == "remainingCapComp_nonCap" ~ "Different",
                            key == "sameProvNonCapComp_nonCap" ~ "Same",
                            key == "otherProvNonCapComp_nonCap" ~ "Different",
                            TRUE ~ "")) %>%
  mutate(key = case_when(key == "top5exclhomeComp_nonCap" ~ "Different province:\nTop-5 cities",
                         key == "sameProvincialCapComp" ~ "Same province:\nCapital",
                         key == "remainingCapComp_nonCap" ~ "Different province:\nOther capitals",
                         key == "sameProvNonCapComp_nonCap" ~ "Same province:\nNon-capital",
                         key == "otherProvNonCapComp_nonCap" ~ "Different province:\nNon-capital",
                         TRUE ~ key)) %>%
  ggplot(aes(key, exp(estimate)), col = as.factor(tmpCol))+
  geom_pointrange(aes(ymin = exp(conf.low), ymax = exp(conf.high), 
                      lty = as.factor(daysAfter), 
                      col = as.factor(tmpCol)),
                  position = position_dodge(width = .4),
                  # size = .4, fatten = 2) +
                  size = 1.4, fatten = 2) + # fatten changes the circles only; size changes bars and legend circles
  coord_flip() + 
  geom_hline(yintercept = 1, colour = "grey60", linetype = 2) + # change this from vline to hline
  scale_color_manual(#labels = c("Top 5 districts", "Home provincial capital", "Within 50km"), 
    # values=c("#420A68FF", "#DD513AFF", "#FCA50AFF")) +
    values=c("#DD513AFF", "#84cdadFF")) +
  # scale_colour_manual(values = scales::viridis_pal(option = "B")(3)) +
  guides(color = guide_legend(override.aes = list(linetype = "blank")), 
         # linetype = guide_legend(override.aes = list(shape = NA))
         linetype = FALSE # suppress line for first plot; only do color
  ) +  # shape for the type of dot
  # scale_x_discrete(labels = ) +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.position="top",
        legend.justification = c("right", "top"),
        legend.title = element_text(size=14, face = "bold"),
        legend.text = element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.box = "vertical" # this makes the two legends one on top of the other
  ) +
  labs(#title = paste0("Effect of violence on destination of movers: non-capitals"),
    title = "Non-capitals",
    y = "Multiplicative change in odds",
    x = "",
    lty = "Days after violence",
    col = "Province") + # y here because of coord_flip
  scale_y_continuous(limits = c(.93, 1.06), breaks = seq(.94, 1.06, by = .02))


plot2 <-totalDisplacement %>%
  filter(daysAfter %in% c(7, 30, 90) &
           key %in% c("top5exclhomeComp_capitals", "remainingCapComp_capitals", "sameProvNonCapComp_capitals", "otherProvNonCapComp_capitals", "sameProvincialCapComp")) %>%
  mutate(estimate = ifelse(key == "sameProvincialCapComp", NA, estimate),
         conf.low = ifelse(key == "sameProvincialCapComp", NA, conf.low),
         conf.high = ifelse(key == "sameProvincialCapComp", NA, conf.high)
  ) %>%
  mutate(tmpCol = case_when(key == "top5exclhomeComp_capitals" ~ "Different",
                            key == "remainingCapComp_capitals" ~ "Different",
                            key == "sameProvNonCapComp_capitals" ~ "Same",
                            key == "otherProvNonCapComp_capitals" ~ "Different",
                            key == "sameProvincialCapComp" ~ "Same",
                            TRUE ~ "")) %>%
  mutate(key = case_when(key == "top5exclhomeComp_capitals" ~ "Different province:\nTop-5 cities",
                         key == "remainingCapComp_capitals" ~ "Different province:\nOther capitals",
                         key == "sameProvNonCapComp_capitals" ~ "Same province:\nNon-capital",
                         key == "otherProvNonCapComp_capitals" ~ "Different province:\nNon-capital",
                         key == "sameProvincialCapComp" ~ "Same province:\nCapital",
                         TRUE ~ key)) %>%
  ggplot(aes(key, exp(estimate)), col = as.factor(tmpCol))+
  geom_pointrange(aes(ymin = exp(conf.low), 
                      ymax = exp(conf.high), 
                      lty = as.factor(daysAfter), 
                      col = as.factor(tmpCol)),
                  position = position_dodge(width = .4),
                  # size = .4, fatten = 2) +
                  size = 1.4, fatten = 2) + # fatten changes the circles only; size changes bars and legend circles
  coord_flip() + 
  geom_hline(yintercept = 1, colour = "grey60", linetype = 2) + # change this from vline to hline
  scale_color_manual(#labels = c("Top 5 districts", "Within 50km"), 
    # values=c("#420A68FF", "#DD513AFF", "#FCA50AFF")) +
    values=c("#DD513AFF", "#84cdadFF")) +
  # scale_colour_manual(values = scales::viridis_pal(option = "B")(3)) +
  guides(#color = guide_legend(override.aes = list(linetype = "blank")), 
    color = FALSE, # suppress color for second plot, only do line
    linetype = guide_legend(override.aes = list(shape = NA, size = .8, key_glyph = "draw_key_path"))) +  # shape for the type of dot
  # scale_x_discrete(labels = ) +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.position= "top",
        legend.justification = c("left", "top"),
        legend.title = element_text(size=14,face="bold"),
        legend.text = element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        # legend.box = "vertical" # this makes the two legends one on top of the other
        axis.text.y = element_blank(), # suppress y-axis labels
        axis.ticks.y = element_blank() # suppress tick marks as well 
  ) +
  labs(#title = paste0("Effect of violence on destination of movers: capitals"),
    title = "Capitals",
    y = "Multiplicative change in odds",
    x = "",
    lty = "Days after violence",
    col = "Province") +# y here because of coord_flip
  # scale_y_continuous(limits = c(.91, 1.095), breaks = seq(.92, 1.08, by = .02))
  scale_y_continuous(limits = c(.93, 1.06), breaks = seq(.94, 1.06, by = .02))


pdf(paste0("/data/afg_anon/displacement_analysis/2-16-22finalFigures/fig5_col.pdf"), width = 12, height = 7)
gridExtra::grid.arrange(plot1, plot2, nrow = 1, widths = c(5, 3.9)) # gets a warning --- it's fine
dev.off()
# rsync -P xtai@umtiti.ischool.berkeley.edu:/data/afg_anon/displacement_analysis/2-16-22finalFigures/fig5_col.pdf /Users/xtai/Desktop/development/displacementProj/NatureHB/finalGuidelines/finalFigures

