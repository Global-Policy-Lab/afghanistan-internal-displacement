#!/usr/bin/Rscript
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# 6f_supplementaryTable1.R uses this script
args = commandArgs(trailingOnly = TRUE)

library(dplyr)

#################### now do regs and ML approach
glmResidsLagOutcomes <- readRDS("/data/afg_anon/displacement_analysis/reviewerComments/11-8glmResidsLagOutcomes.rds")

###### rf:
tmpFun <- function(train) {
  tmp <- complete.cases(train)
  train <- train[complete.cases(train),]
  
  trainX <- train %>% select(-responseResids)
  trainY <- train$responseResids
  
  K = 10
  n <- nrow(train)
  d = ceiling(n/K)
  set.seed(0)
  i.mix = sample(1:n)
  folds = vector(mode = "list", length = K)
  
  for (i in 1:K) {
    folds[[i]] <- i.mix[((i - 1)*d + 1):(i*d)]
  }
  
  preds <- rep(NA, nrow(train))
  
  for (k in 1:K) {
    cat("Fold", k, "\n")
    i.tr <- unlist(folds[-k])
    i.tr <- i.tr[!is.na(i.tr)]
    i.val <- folds[[k]]
    i.val <- i.val[!is.na(i.val)]
    
    x.tr <- trainX[i.tr, ]
    y.tr <- trainY[i.tr]
    # xy.tr <- train[i.tr, ]
    x.val <- train[i.val, ]
    
    set.seed(1)
    tmpFit <- randomForest::randomForest(x = x.tr, y = y.tr, ntree = 50)
    # tmpFit <- rpart::rpart(responseResids ~ ., data = xy.tr, method = "anova",
    #                        control = rpart::rpart.control(minsplit = 0, cp = 0, xval = 1))
    
    preds[i.val] <- predict(tmpFit, x.val, type = "response")
    # preds[i.val] <- predict(tmpFit, x.val, type = "vector")
  }
  train$preds <- preds
  return(train)
}

if (args[1] == "trendImpacted") {
  trainTmp <- glmResidsLagOutcomes %>% 
    select(responseResids, 
           starts_with("trendImpacted")#,
           # starts_with("trendOutcome"),
           # starts_with("lagTotalImpacted"),
           # starts_with("lagOutcome")
    ) # %>%
  # select(-lagTotalImpactedk030)
} else if (args[1] == "trendOutcome") {
  trainTmp <- glmResidsLagOutcomes %>% 
    select(responseResids, 
           # starts_with("trendImpacted")#,
           starts_with("trendOutcome")#,
           # starts_with("lagTotalImpacted"),
           # starts_with("lagOutcome")
    ) # %>%
  # select(-lagTotalImpactedk030)
} else if (args[1] == "lagImpacted") {
  trainTmp <- glmResidsLagOutcomes %>% 
    select(responseResids, 
           # starts_with("trendImpacted")#,
           # starts_with("trendOutcome")#,
           starts_with("lagTotalImpacted")#,
           # starts_with("lagOutcome")
    )  %>%
    select(-lagTotalImpactedk030)
} else if (args[1] == "lagOutcome") {
  trainTmp <- glmResidsLagOutcomes %>% 
    select(responseResids, 
           # starts_with("trendImpacted")#,
           # starts_with("trendOutcome")#,
           # starts_with("lagTotalImpacted"),
           starts_with("lagOutcome")
    ) # %>%
  # select(-lagTotalImpactedk030)
} else if (args[1] == "all") {
  trainTmp <- glmResidsLagOutcomes %>% 
    select(responseResids, 
           starts_with("trendImpacted"),
           starts_with("trendOutcome"),
           starts_with("lagTotalImpacted"),
           starts_with("lagOutcome")
    ) %>%
    select(-lagTotalImpactedk030)
}

trainTmp <- tmpFun(trainTmp)
saveRDS(trainTmp, file = paste0("/data/afg_anon/displacement_analysis/reviewerComments/predsRF", args[1], "_50trees.rds")) 

# nohup ./myCode/runRF.R trendImpacted & 
# nohup ./myCode/runRF.R trendOutcome & 
# nohup ./myCode/runRF.R lagImpacted & 
# nohup ./myCode/runRF.R lagOutcome & 
# nohup ./myCode/runRF.R all & 
