# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# this script for CIs and p-values reported in paper
rm(list = ls()); gc()
library(dplyr)
# 8/9/21: get CIs for paper

################## overall 
### k = 10:
coefListK <- readRDS("/data/afg_anon/displacement_analysis/7-30panelPlots/betaCoefList_fit.rds")
tmp <- coefListK[[10]] %>%
  filter(substr(term, 1, 1) == "T")

tmp[tmp$term == "Tm0101", ]
# term   conf.low conf.high   estimate numDays000
# 171 Tm0101 0.02559404 0.0533773 0.03948567       3371

exp(tmp[tmp$term == "Tm0101", 2:4])
# conf.low conf.high estimate
# 171 1.025924  1.054828 1.040276

fitData <- readRDS("/data/afg_anon/displacement_analysis/7-30panelPlots/beta_k010fit.rds")
testInstead <- lmtest::coeftest(fitData, vcov. = sandwich::vcovCL(fitData, cluster = fitData$model$district_id, type = "HC0"))
testInstead[rownames(testInstead) == "Tm0101", ]
# Estimate   Std. Error      z value     Pr(>|z|) 
# 3.948567e-02 7.087671e-03 5.571037e+00 2.532280e-08 

### k = 120:
tmp <- coefListK[[120]] %>%
  filter(substr(term, 1, 1) == "T")

tmp[tmp$term == "Tm1201", ]
# term    conf.low  conf.high   estimate numDays000
# 61 Tm1201 0.004542418 0.02831411 0.01642826       3144
exp(tmp[tmp$term == "Tm1201", 2:4])
# conf.low conf.high estimate
# 61 1.004553  1.028719 1.016564

fitData120 <- readRDS("/data/afg_anon/displacement_analysis/7-30panelPlots/beta_k120fit.rds")
testInstead120 <- lmtest::coeftest(fitData120, vcov. = sandwich::vcovCL(fitData120, cluster = fitData120$model$district_id, type = "HC0"))
testInstead120[rownames(testInstead120) == "Tm1201", ]
# Estimate  Std. Error     z value    Pr(>|z|) 
# 0.016428263 0.006064295 2.709014603 0.006748337 

#################### Taliban/IS
coefListK <- readRDS("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/betaCoefList_fit_talebanIS.rds")
tmp <- coefListK[[1]] %>%
  filter(substr(term, 1, 1) == "T")
tmp[tmp$term %in% c("Tm0011", "Tm_T0011", "Tm_I0011"), ]
exp(tmp[tmp$term %in% c("Tm0011", "Tm_T0011", "Tm_I0011"), 2:4])
# conf.low conf.high estimate
# 180 0.9796487  1.345945 1.148283 # don't need this
# 391 1.0574118  1.201508 1.127160
# 602 0.9956568  1.043293 1.019197

## Taliban/IS
fitDataTI <- readRDS("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/beta_k001fit_talebanIS.rds")
testInsteadTI <- lmtest::coeftest(fitDataTI, vcov. = sandwich::vcovCL(fitDataTI, cluster = fitDataTI$model$district_id, type = "HC0"))
tmpOut <- testInsteadTI[rownames(testInsteadTI) %in% c("Tm_T0011", "Tm_I0011"), ]
# Estimate Std. Error  z value     Pr(>|z|)
# Tm_I0011 0.11970080 0.03259055 3.672869 0.0002398429
# Tm_T0011 0.01901476 0.01192230 1.594890 0.1107368638

tmpVcov <- sandwich::vcovCL(fitDataTI, cluster = fitDataTI$model$district_id, type = "HC0")
# note on calculating p-value:
(1 - pnorm(0.11970080/0.03259055))*2
# [1] 0.0002398431

# difference of two normals
# CI:
# point estimate: (Taliban - ISIS)
exp(tmpOut[1, 1]) - exp(tmpOut[2, 1]) # 0.1079629
tmpOut[1, 1] - tmpOut[2, 1] # 0.100686

pointEstimate <- tmpOut[1, 1] - tmpOut[2, 1]
tmpCov <- tmpVcov[rownames(tmpVcov)  == "Tm_T0011", rownames(tmpVcov)  == "Tm_I0011"]
stdErr <- sqrt(tmpOut[1, 2]^2 + tmpOut[2, 2]^2 - 2*tmpCov)

pointEstimate + qnorm(.975)*stdErr # CI High: 0.1652212
pointEstimate - qnorm(.975)*stdErr # CI Low: 0.0361509
(1 - pnorm( pointEstimate / stdErr ))*2 # p-value: 0.002229055

###### 8/20: write loop to compare coefficients (peacetime is done)
# peacetime
library(dplyr)
diffTests <- data.frame(k = 1:120, 
                        estimate = rep(NA, 120), 
                        low = rep(NA, 120), 
                        high = rep(NA, 120), 
                        pValue = rep(NA, 120))

diffFunPeace <- function(k) {
  fitData <- readRDS(paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/beta_k", sprintf("%03d", k), "fit_60peace.rds"))
  tmpVcov <- sandwich::vcovCL(fitData, cluster = fitData$model$district_id, type = "HC0")
  testInstead <- lmtest::coeftest(fitData, vcov. = tmpVcov)
  tmpOut <- testInstead[rownames(testInstead) %in% c(paste0("Tm_S", sprintf("%03d", k), "1"), paste0("Tm", sprintf("%03d", k), "1")), ]
  
  pointEstimate <- tmpOut[paste0("Tm_S", sprintf("%03d", k), "1"), 1] - tmpOut[paste0("Tm", sprintf("%03d", k), "1"), 1]
  tmpCov <- tmpVcov[rownames(tmpVcov)  == paste0("Tm_S", sprintf("%03d", k), "1"), rownames(tmpVcov)  == paste0("Tm", sprintf("%03d", k), "1")]
  stdErr <- sqrt(tmpOut[paste0("Tm_S", sprintf("%03d", k), "1"), 2]^2 + tmpOut[paste0("Tm", sprintf("%03d", k), "1"), 2]^2 - 2*tmpCov)
  
  high <- pointEstimate + qnorm(.975)*stdErr # CI High: 0.1652212
  low <- pointEstimate - qnorm(.975)*stdErr # CI Low: 0.0361509
  pValue <- (1 - pnorm( pointEstimate / stdErr ))*2 # p-value: 0.002229055
  
  ret <- list(estimate = pointEstimate, low = low, high = high, pValue = pValue)
}

for (i in 1:120) {
  cat(i, ", ")
  diffTests[i, 2:5] <- diffFunPeace(i)
}

system.time(diffTests[k, 2:5] <- diffFunPeace(k))
# takes 86 seconds

saveRDS(diffTests, "/data/afg_anon/displacement_analysis/reviewerComments/diffTests_peace8-20.rds")

diffTestsPeace <- readRDS("/data/afg_anon/displacement_analysis/reviewerComments/diffTests_peace8-20.rds")

# paired t-test
pointEstimate <- mean(diffTestsPeace$estimate) #0.02973688
stdErr <- (sd(diffTestsPeace$estimate)/sqrt(nrow(diffTestsPeace)))
testStat <- pointEstimate/stdErr
(pValue <- (1 - pt( testStat, df = 119 ))*2 )
pointEstimate + qt(.975, df = 119)*stdErr # high 0.03081066
pointEstimate - qt(.975, df = 119)*stdErr # low 0.0286631

###### 8/21: now do provincial capitals
library(dplyr)
diffTests <- data.frame(k = 1:120, 
                        estimate = rep(NA, 120), 
                        low = rep(NA, 120), 
                        high = rep(NA, 120), 
                        pValue = rep(NA, 120))

diffFunProvCap <- function(k) {
  # nonCap
  fitData <- readRDS(paste0("/data/afg_anon/displacement_analysis/9-25distHeterogeneity/beta_nonCapital_k", sprintf("%03d", k), "_fit.Rdata"))
  tmpVcov <- sandwich::vcovCL(fitData, cluster = fitData$model$district_id, type = "HC0")
  testInstead <- lmtest::coeftest(fitData, vcov. = tmpVcov)
  
  pointEstimate1 <- testInstead[paste0("Tm", sprintf("%03d", k), "1"), "Estimate"]
  stdErr1 <- testInstead[paste0("Tm", sprintf("%03d", k), "1"), "Std. Error"]
  
  # cap
  fitData <- readRDS(paste0("/data/afg_anon/displacement_analysis/9-25distHeterogeneity/beta_provincialCap_k", sprintf("%03d", k), "_fit.Rdata"))
  tmpVcov <- sandwich::vcovCL(fitData, cluster = fitData$model$district_id, type = "HC0")
  testInstead <- lmtest::coeftest(fitData, vcov. = tmpVcov)
  
  pointEstimate2 <- testInstead[paste0("Tm", sprintf("%03d", k), "1"), "Estimate"]
  stdErr2 <- testInstead[paste0("Tm", sprintf("%03d", k), "1"), "Std. Error"]
  
  # together
  pointEstimate <- pointEstimate1 - pointEstimate2
  stdErr <- stdErr1 + stdErr2
  
  high <- pointEstimate + qnorm(.975)*stdErr 
  low <- pointEstimate - qnorm(.975)*stdErr 
  pValue <- (1 - pnorm( pointEstimate / stdErr ))*2 
  
  ret <- list(estimate = pointEstimate, low = low, high = high, pValue = pValue)
}

for (i in 1:120) {
  cat(i, ", ")
  diffTests[i, 2:5] <- diffFunProvCap(i)
}

saveRDS(diffTests, "/data/afg_anon/displacement_analysis/reviewerComments/diffTests_provCap8-20.rds")
# started at 12:12

diffTests <- readRDS("/data/afg_anon/displacement_analysis/reviewerComments/diffTests_provCap8-20.rds")

pointEstimate <- mean(diffTests$estimate) #0.02595887
stdErr <- (sd(diffTests$estimate)/sqrt(nrow(diffTests)))
testStat <- pointEstimate/stdErr
(pValue <- (1 - pt( testStat, df = 119 ))*2 )
pointEstimate + qt(.975, df = 119)*stdErr # high: 0.02758023
pointEstimate - qt(.975, df = 119)*stdErr # low:  0.02433751

# casualties
library(dplyr)
diffTests <- data.frame(k = 1:120, 
                        estimate = rep(NA, 120), 
                        low = rep(NA, 120), 
                        high = rep(NA, 120), 
                        pValue = rep(NA, 120))

diffFunCas <- function(k) {
  fitData <- readRDS(paste0("/data/afg_anon/displacement_analysis/7-30panelPlots/heterogeneity/beta_k", sprintf("%03d", k), "fit_11cas.rds"))
  tmpVcov <- sandwich::vcovCL(fitData, cluster = fitData$model$district_id, type = "HC0")
  testInstead <- lmtest::coeftest(fitData, vcov. = tmpVcov)
  tmpOut <- testInstead[rownames(testInstead) %in% c(paste0("Tm_S", sprintf("%03d", k), "1"), paste0("Tm", sprintf("%03d", k), "1")), ]
  
  pointEstimate <- tmpOut[paste0("Tm", sprintf("%03d", k), "1"), 1] - tmpOut[paste0("Tm_S", sprintf("%03d", k), "1"), 1] # switch this: high-casualty - low-casualty (prefix S)
  tmpCov <- tmpVcov[rownames(tmpVcov)  == paste0("Tm_S", sprintf("%03d", k), "1"), rownames(tmpVcov)  == paste0("Tm", sprintf("%03d", k), "1")]
  stdErr <- sqrt(tmpOut[paste0("Tm_S", sprintf("%03d", k), "1"), 2]^2 + tmpOut[paste0("Tm", sprintf("%03d", k), "1"), 2]^2 - 2*tmpCov)
  
  high <- pointEstimate + qnorm(.975)*stdErr # CI High: 0.1652212
  low <- pointEstimate - qnorm(.975)*stdErr # CI Low: 0.0361509
  pValue <- (1 - pnorm( pointEstimate / stdErr ))*2 # p-value: 0.002229055
  
  ret <- list(estimate = pointEstimate, low = low, high = high, pValue = pValue)
}

for (i in 1:120) {
  cat(i, ", ")
  diffTests[i, 2:5] <- diffFunCas(i)
}

saveRDS(diffTests, "/data/afg_anon/displacement_analysis/reviewerComments/diffTests_cas8-20.rds")

pointEstimate <- mean(diffTests$estimate)
stdErr <- (sd(diffTests$estimate)/sqrt(nrow(diffTests)))
testStat <- pointEstimate/stdErr
(pValue <- (1 - pt( testStat, df = 119 ))*2 )
pointEstimate + qt(.975, df = 119)*stdErr # high: 0.03631865
pointEstimate - qt(.975, df = 119)*stdErr # low: 0.03115508

####################### singlelfe ######################
rm(list=ls()); gc()
library(dplyr); library(ggplot2)

extractedStats <- readRDS("/data/afg_anon/displacement_analysis/10-7singleViolence/12-4fixedExtractedStats.rds")
forRegs <- extractedStats %>%
  rename(t01to15 = t1to15)

################### 5. reduced model, robust errors
diffTests <- data.frame(outcomeVec = c("t01to15", "t16to30", "t31to45", "t46to60", "t61to75", "t76to90"),
                        estimate = NA, 
                        low = NA, 
                        high = NA, 
                        pValue = NA)

outcomeVec <- c("t01to15", "t16to30", "t31to45", "t46to60", "t61to75", "t76to90")

for (i in 1:length(outcomeVec)) {
  forRegs$outcomeVar <- forRegs[, outcomeVec[i]]
  fit <- lm((outcomeVar) ~ as.factor(provincialCapital) + TOTAL + as.factor(ISIS) + as.factor(best >= 11)  + as.factor(daysFromLastEvent >= 60), data = forRegs)
  
  ### get the difference between IS and longPeacetime --- want beta_IS + beta_longPeace)
  tmpVcov <- sandwich::vcovCL(fit, cluster = forRegs$district, type = "HC0")
  testInstead <- lmtest::coeftest(fit, vcov. = tmpVcov)
  
  pointEstimate <- testInstead["as.factor(ISIS)1", 1] + testInstead["as.factor(daysFromLastEvent >= 60)TRUE", 1] 
  
  tmpCov <- tmpVcov[rownames(tmpVcov) == "as.factor(ISIS)1", rownames(tmpVcov) == "as.factor(daysFromLastEvent >= 60)TRUE"]
  
  stdErr <- sqrt(testInstead["as.factor(ISIS)1", 2]^2 + testInstead["as.factor(daysFromLastEvent >= 60)TRUE", 2]^2 + 2*tmpCov)
  
  high <- pointEstimate + qnorm(.975)*stdErr # CI High: 0.1652212
  low <- pointEstimate - qnorm(.975)*stdErr # CI Low: 0.0361509
  pValue <- (1 - pnorm( pointEstimate / stdErr ))*2 # p-value: 0.002229055
  
  diffTests[i, "estimate"] <- pointEstimate
  diffTests[i, "high"] <- high
  diffTests[i, "low"] <- low
  diffTests[i, "pValue"] <- pValue
} 
### comparing IS and short peacetime:
# outcomeVec  estimate        low      high      pValue
# 1    t01to15 0.1939879 0.03183502 0.3561408 0.019039422
# 2    t16to30 0.1696499 0.03045625 0.3088436 0.016902874
# 3    t31to45 0.1959396 0.05573253 0.3361466 0.006161645
# 4    t46to60 0.2417854 0.06950259 0.4140682 0.005947613
# 5    t61to75 0.2509245 0.08908540 0.4127636 0.002374916
# 6    t76to90 0.2095221 0.04336360 0.3756807 0.013455885

