#!/usr/bin/Rscript
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# 6d_supplementaryFig5.R uses this script
args = commandArgs(trailingOnly = TRUE)
ddEvents <- readRDS(paste0("/data/afg_anon/displacement_analysis/", args[1], ".rds")) # districtDay_eventsOnly_8-11 
#3-13_regM_modelMat
ddOutcomes <- readRDS(paste0("/data/afg_anon/displacement_analysis/", args[2], ".rds"))

# if (identical(as.character(ddEvents[, c("district_id", "date")]), ddOutcomes[, c("district_id", "date")]) == FALSE) stop("Events and outcome data in different order")

suppressMessages(library(dplyr))

betaGetCoefs2 <- function(fitData, cluster, numDays000, clusteredSE = TRUE) {
  # check if converged: # add this 10/5
  if (fitData$converged == FALSE) {
    stop("Beta reg did not converge")
  }
  # do clustered standard errors instead
  # districtDay$outcome <- districtDay[, paste0("lagPercentMigratedk", sprintf("%03d", k))]
  # tmp <- districtDay[!is.na(districtDay[, "outcome"]) & districtDay[, "outcome"] != 0 & districtDay[, "outcome"] != 1, ] # CHANGE FROM 100 TO 1
  if (clusteredSE == TRUE) {
    tryThis <- lmtest::coefci(fitData, vcov. = sandwich::vcovCL(fitData, cluster = cluster, type = "HC0")) # takes less than a minute!
    
    forPlot <- data.frame(term = rownames(tryThis), stringsAsFactors = FALSE)
    forPlot$conf.low <- tryThis[, 1]
    forPlot$conf.high <- tryThis[, 2]
    forPlot$estimate <- (forPlot$conf.low + forPlot$conf.high)/2
  } else if (clusteredSE == FALSE) {
    # forPlot <- broom::tidy(fitData, conf.int = TRUE) # broom requires too much memory
    coefs <- fitData$coefficients$mean
    variance <- diag(fitData$vcov)
    if (identical(names(coefs), names(variance)[-length(names(variance))]) == FALSE) {
      stop("Problem with coef order")
    }
    forPlot <- data.frame(term = names(coefs), 
                          estimate = as.vector(coefs), 
                          variance = as.vector(variance[-length(variance)]),
                          stringsAsFactors = FALSE)
    forPlot <- forPlot %>%
      mutate(conf.low = estimate - qnorm(.975)*sqrt(variance),
             conf.high = estimate + qnorm(.975)*sqrt(variance))
  }
  # if not clustered, can tell because coefListK will have variance column
  
  forPlot$numDays000 <- numDays000 # 9/10: add this new info
  # for event heterogeneity, this gives only numbers of one type of event
  return(forPlot)
}

myFun <- function(k) {
  y <- ddOutcomes[, paste0("lagPercentMigratedk", sprintf("%03d", k))]
  keepThese <- which(!is.na(y) & y != 0 & y != 1)
  
  y <- y[keepThese] 
  x <- ddEvents[keepThese, ] 
  
  linkfun <- function(mu) {.Call(stats:::C_logit_link, mu)}
  out <- lm.fit(x, linkfun(y))
  toDropLM <- which(is.na(out$coefficients)) 
  
  xLM <- x[, setdiff(1:ncol(x), toDropLM)]
  # fitBeta <- betareg::betareg.fit(xLM, y) # unfortunately can't get clustered SEs with this
  forPercent <- xLM[, -1] %>%
    data.frame() %>%
    bind_cols(percentage_migrated = y)
  fitBeta <- betareg::betareg(percentage_migrated ~ ., data = forPercent, model = FALSE)
  saveRDS(fitBeta, file = paste0("/data/afg_anon/displacement_analysis/12-3robustnessChecks/beta_k", sprintf("%03d", k), args[4], ".rds")) # in case getcoefs2 doesn't work
  
  out <- betaGetCoefs2(fitBeta, cluster = ddOutcomes$district_id[keepThese], numDays000 = sum(xLM[, "T000"] == 1), clusteredSE = TRUE) # check if this works 
  return(out)
}
out <- myFun(as.numeric(args[3]))

outFileName <- paste0("/data/afg_anon/displacement_analysis/12-3robustnessChecks/betaCoefList_", args[4], ".rds")
if (file.exists(outFileName)) {
  coefListK <- readRDS(outFileName)
} else {
  coefListK <- vector(mode = "list", length = 120)
}

coefListK[[as.numeric(args[3])]] <- out
saveRDS(coefListK, file = outFileName)

# nohup ./myCode/cleanCode/runPanelReg_roblfe.R districtDay_eventsOnly_3-13_regM_modelMat districtDay_8-12_outcomeOnly 30 regM &

# counter <- 0
# for (i in 1:120) {
#   cat(paste0("nohup ./myCode/cleanCode/runPanelReg_roblfe.R districtDay_eventsOnly_3-13_regM_modelMat districtDay_8-12_outcomeOnly ", i, " regM &\n"))
#   counter <- counter + 1
#   if (counter %% 16 == 0) {
#     cat("wait\n")
#   }
# }
