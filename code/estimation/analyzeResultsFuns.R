# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# some helper functions
##### 0. create leads and lags
makeDataset <- function(inputSummarizeEvents, districtDayStartDataset, prefix, numLeadLags) {
  districtDay <- districtDayStartDataset
  districtDay$distDate <- paste0(districtDay$district_id, districtDay$date)
  
  for (l in -numLeadLags:numLeadLags) {
    cat(l, ", ")
    inputSummarizeEvents$tmpDate <- inputSummarizeEvents$date_start - lubridate::days(l)
    inputSummarizeEvents$distDate <- paste0(inputSummarizeEvents$distid, inputSummarizeEvents$tmpDate)
    
    treat <- rep(0, nrow(districtDay))
    treat[which(districtDay$distDate %in% inputSummarizeEvents$distDate)] <- 1 # if there is already a 1 or district day is too far back to not be in data set --- doesn't change anything
    districtDay <- cbind(districtDay, treat)
    
    if (l >= 0) {
      names(districtDay)[which(names(districtDay) == "treat")] <- paste0("T", prefix, sprintf("%03d", l))
    } else {
      names(districtDay)[which(names(districtDay) == "treat")] <- paste0("Tm", prefix, sprintf("%03d", abs(l)))
    }
    # m is in past
    
    # districtDay <- dplyr::left_join(districtDay, 
    #                                 inputSummarizeEvents %>% 
    #                                   select(distid, tmpDate, numEvents), 
    #                                 by = c("district_id" = "distid", "date" = "tmpDate")) # tmpDate instead of date_start
    # districtDay$treat <- ifelse(!is.na(districtDay$numEvents), 1, 0)
    # 
    # if (l >= 0) {
    #   names(districtDay)[which(names(districtDay) == "treat")] <- paste0("T", prefix, sprintf("%03d", l))
    # } else {
    #   names(districtDay)[which(names(districtDay) == "treat")] <- paste0("Tm", prefix, sprintf("%03d", abs(l)))
    # }
    # districtDay <- districtDay %>%
    #   select(-numEvents)
    
  }
  districtDay <- districtDay %>%
    select(-distDate)
  
  return(districtDay)
  
}

##### 1. extracting coefficients from fit object
betaGetCoefs <- function(fitData, otherTreatnames = FALSE, clusteredSE = TRUE) {
  # check if converged: # add this 10/5
  if (fitData$converged == FALSE) {
    stop("Beta reg did not converge")
  }
  # do clustered standard errors instead
  # districtDay$outcome <- districtDay[, paste0("lagPercentMigratedk", sprintf("%03d", k))]
  # tmp <- districtDay[!is.na(districtDay[, "outcome"]) & districtDay[, "outcome"] != 0 & districtDay[, "outcome"] != 1, ] # CHANGE FROM 100 TO 1
  if (clusteredSE == TRUE) {
    tryThis <- lmtest::coefci(fitData, vcov. = sandwich::vcovCL(fitData, cluster = fitData$model$district_id, type = "HC0")) # takes less than a minute!
    
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
  
  forPlot$numDays000 <- sum(fitData$model$T000 == 1) # 9/10: add this new info
  # for event heterogeneity, this gives only numbers of one type of event
  # add 10/10:
  if (otherTreatnames[1] != FALSE) {
    for (i in 1:length(otherTreatnames)) {
      forPlot[, otherTreatnames[i]] <- sum(fitData$model[, otherTreatnames[i]] == 1)
    }
  }
  
  return(forPlot)
}

##### 2. make the coefficient plots
makeCoefPlotsOddsScale <- function(dataForPlot, k, lowerTau, upperTau, customTitle = "Coefficients for violence indicator, Beta reg on proportion in a different district (SE clustered by district)", plot = TRUE) {
  forPlot <- dataForPlot[which(substr(dataForPlot$term, 1, 1) == "T"), ]
  forPlot$plusMinus <- ifelse(substr(forPlot$term, 2, 2) == "m", 1, -1)
  tmp <- gsub("T|m", "", forPlot$term)
  forPlot$leadLag <- as.numeric(substr(tmp, 1, nchar(tmp) - 1))
  forPlot$leadLag <- forPlot$leadLag*forPlot$plusMinus 
  
  plot1 <- forPlot %>%
    ggplot(aes(leadLag, exp(estimate)))+
    geom_point()+
    geom_pointrange(aes(ymin = exp(conf.low), ymax = exp(conf.high)))+
    labs(title = paste0(customTitle, "\nOutcome: Compared to ", sprintf("%03d", k), " days ago"),
         x = "tau (Lead/lag: positive means lagged, i.e., violence in past)",
         y = "exp(estimate): Multiplicative change in odds") +
    geom_vline(xintercept = 0, color = "red3", linetype = 2) + 
    geom_hline(yintercept = 1, color = "red3") + 
    scale_x_continuous(breaks = seq(lowerTau, upperTau, 10))
  if (plot == TRUE) {
    gridExtra::grid.arrange(plot1, nrow = 1)
  } else {
    return(plot1)
  }
}

###### this function: for peace, casualty and provincal cap plots
totalDispPlotFun <- function(keyVec, myTitle) {
  # myTitle <- paste0("Impact of violent day on odds of being in a different district each day after violence\n", keyVec[1], ": ~", totalDisplacement$numDays000[totalDisplacement$daysAfter == 30 & totalDisplacement$key == keyVec[1]], " violent district-days")
  myCols <- c("#420A68FF", "#DD513AFF") # add more later
  
  plot1 <- totalDisplacement %>%
    filter(key %in% keyVec) %>%
    ggplot(aes(daysAfter, exp(estimate), col = as.factor(key))) +
    geom_point() +
    geom_pointrange(aes(ymin = exp(conf.low), 
                        ymax = exp(conf.high), 
                        col = as.factor(key)),
                    position = position_dodge(width = .1)) +
    labs(#title = myTitle,
      x = "k days after violence",
      y = "Multiplicative change in odds",
      col = "") +
    geom_vline(xintercept = 0, color = "red3", linetype = 2) +
    geom_hline(yintercept = 1, color = "red3") +
    scale_x_continuous(breaks = seq(0, 120, 5)) +
    scale_color_manual(values = myCols[1:length(keyVec)]) +
    theme(legend.position = c(1, 1.02),
          legend.justification = c("right", "top"),
          legend.title = element_blank(),
          legend.text = element_text(size=14),
          axis.text=element_text(size=14),
          axis.title=element_text(size=14))
  
  return(plot1)
}
