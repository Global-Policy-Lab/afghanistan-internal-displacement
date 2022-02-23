# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# see runPanelReg_GLMresids.R --- takes 4 hours
glmFit <- readRDS("/data/afg_anon/displacement_analysis/reviewerComments/glmViolence030fit_GLMresids.rds")

rstandardized <- rstandard(glmFit, type = "deviance")
rResp <- resid(glmFit, type = "response")

glmResids <- cbind(glmFit$model, devianceResids = rstandardized, responseResids = rResp)

saveRDS(glmResids, "/data/afg_anon/displacement_analysis/reviewerComments/11-5glmResids.rds")

# want figures for residuals vs. district FE, time FE, T_ coefficients
# then use ___ to predict these residuals

###############################
# 11/8: predict these anomalies using lags of displacement
glmResids <- readRDS("/data/afg_anon/displacement_analysis/reviewerComments/11-5glmResids.rds")

# get lags of displacement outcome (30-day displacement)
ddOutcomes <- readRDS("/data/afg_anon/displacement_analysis/districtDay_8-12_outcomeOnly.rds")

ddOutcomes <- ddOutcomes %>%
  select(district_id, date,
         paste0("lagTotalImpactedk", sprintf("%03d", 30:45)),
         paste0("lagPercentMigratedk", sprintf("%03d", 30))) %>%
  rename(percentage_migrated = lagPercentMigratedk030)

for (k in 1:15) {
  cat(k, ", ")
  # lag the outcome variable by k days
  # have to do this roundabout way because of tibble structure; need to get it to play nice with magrittr and dplyr::lag
  
  ddOutcomes <- ddOutcomes %>%
    arrange(district_id, date) %>%
    group_by(district_id) 
  
  ddOutcomes <- ddOutcomes %>%
    mutate(tmpOut = dplyr::lag(percentage_migrated, n = k, order_by = date))
  
  ddOutcomes <- ddOutcomes %>%
    ungroup() %>%
    data.frame()
  
  ddOutcomes[, paste0("lagOutcome", sprintf("%02d", k))] <- ddOutcomes$tmpOut
  
  ddOutcomes <- ddOutcomes %>%
    select(-tmpOut)
} 
# saveRDS(districtDay, file = "/data/tmp/xtai/districtDay_8-12_outcomeOnly.rds")

glmResidsLagOutcomes <- glmResids %>%
  select(district_id, date, responseResids) %>%
  mutate(district_id = as.integer(as.character(district_id)),
         date = as.Date(date)) %>%
  left_join(ddOutcomes, by = c("district_id", "date"))

############ trends
for (k in 1:14) {
  glmResidsLagOutcomes[, paste0("trendOutcome", sprintf("%02d", k))] <- glmResidsLagOutcomes[, paste0("lagOutcome", sprintf("%02d", k))] - glmResidsLagOutcomes[, paste0("lagOutcome", sprintf("%02d", k + 1))]
  glmResidsLagOutcomes[, paste0("trendImpacted", sprintf("%02d", k))] <- glmResidsLagOutcomes[, paste0("lagTotalImpactedk", sprintf("%03d", k + 30))] - glmResidsLagOutcomes[, paste0("lagTotalImpactedk", sprintf("%03d", k + 31))]
} 

saveRDS(glmResidsLagOutcomes, "/data/afg_anon/displacement_analysis/reviewerComments/11-8glmResidsLagOutcomes.rds")

####################################### 11/12: start here #########################################
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

#################### now do regs and ML approach
glmResidsLagOutcomes <- readRDS("/data/afg_anon/displacement_analysis/reviewerComments/11-8glmResidsLagOutcomes.rds")

fit0 <- lm(responseResids ~ ., glmResidsLagOutcomes %>% 
             select(responseResids, 
                    starts_with("lagTotalImpacted")#,
                    # starts_with("lagOutcome")
             ) %>%
             select(-lagTotalImpactedk030)
)
summary(fit0)
# Multiple R-squared:  0.000238,	Adjusted R-squared:  0.000193 

fit0 <- lm(responseResids ~ ., glmResidsLagOutcomes %>% 
             select(responseResids, 
                    # starts_with("lagTotalImpacted"),
                    starts_with("lagOutcome")
             ) #%>%
           # select(-lagTotalImpactedk030)
)
summary(fit0)
# Multiple R-squared:  3.053e-05,	Adjusted R-squared:  -1.457e-05 

fit1 <- lm(responseResids ~ ., glmResidsLagOutcomes %>% 
             select(responseResids, starts_with("trendOutcome")))
summary(fit1)
# Multiple R-squared:  2.932e-05,	Adjusted R-squared:  -1.277e-05 
fit1 <- lm(responseResids ~ ., glmResidsLagOutcomes %>% 
             select(responseResids, starts_with("trendImpacted")))
summary(fit1)
# Multiple R-squared:  0.0002165,	Adjusted R-squared:  0.0001745 


fit1 <- lm(responseResids ~ ., glmResidsLagOutcomes %>% 
             select(responseResids, 
                    starts_with("trend"),
                    starts_with("lagTotalImpacted"),
                    starts_with("lagOutcome")
             ) %>%
             select(-lagTotalImpactedk030))

summary(fit1)
# Multiple R-squared:  0.0002771,	Adjusted R-squared:  0.0001839 

### 50 tree version: run using runRF.R
# first 4 take up to 20 hours
trendImpacted <- readRDS("/data/afg_anon/displacement_analysis/reviewerComments/predsRFtrendImpacted_50trees.rds")
cor(trendImpacted$responseResids, trendImpacted$preds)^2 # 0.00002001961

trendOutcome <- readRDS("/data/afg_anon/displacement_analysis/reviewerComments/predsRFtrendOutcome_50trees.rds")
cor(trendOutcome$responseResids, trendOutcome$preds)^2 # 0.000009904278

lagOutcome <- readRDS("/data/afg_anon/displacement_analysis/reviewerComments/predsRFlagOutcome_50trees.rds")
cor(lagOutcome$responseResids, lagOutcome$preds)^2 # 0.0000001421853

lagImpacted <- readRDS("/data/afg_anon/displacement_analysis/reviewerComments/predsRFlagImpacted_50trees.rds")
cor(lagImpacted$responseResids, lagImpacted$preds)^2 # 0.00008082163

allPreds <- readRDS("/data/afg_anon/displacement_analysis/reviewerComments/predsRFall_50trees.rds")
cor(allPreds$responseResids, allPreds$preds)^2 # 0.00001870286

# 2:56-7:38 for one fold --- 4 hours 45 minutes

########## 11/14: CI and p-values ----- use bootstrap
# https://stats.stackexchange.com/questions/272417/get-p-value-of-coefficients-in-regression-models-using-bootstrap

tmpData <- glmResidsLagOutcomes %>% 
  select(responseResids, 
         starts_with("trend"),
         starts_with("lagTotalImpacted"),
         starts_with("lagOutcome")
  ) %>%
  select(-lagTotalImpactedk030)

bootTest <- sapply(1:1e4,function(x){
  rows <- sample(nrow(tmpData), nrow(tmpData), replace = TRUE)
  mdl <- lm(responseResids ~ ., data = tmpData[rows,])
  return(summary(mdl)$r.squared)
})
# started at 12:53 --- might take 6 hours 7:39 done
saveRDS(bootTest, file = "/data/afg_anon/displacement_analysis/reviewerComments/bootTest.rds")
# pvals <- sapply(1:nrow(bootTest),function(x) {
distribution <- ecdf(bootTest)
qt0 <- distribution(0)
if(qt0 < 0.5){
  return(2*qt0)
} else {
  return(2*(1-qt0))
}
# })

quantile(bootTest, probs = c(.025, .975)) # 0.0002605404 0.0010379573 
# system.time(fit1 <- lm(responseResids ~ ., data = tmpData )) # 2.2 seconds
# summary(fit1)$r.squared

