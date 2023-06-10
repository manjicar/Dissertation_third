library(plyr)
library(dplyr)
library(tidyr)
library(Cairo)
library(roxygen2)
library(crqa)
library(readxl)

#' main
#' 
#' Apply All Calculations for a Specific Index
#' 
#' @description 
#' 
#' @return  

main <- function(video, index) {
  
  orangeRed    <- prepInterval('Orange', 'Red', video, index)
  blueOrange   <- prepInterval('Blue', 'Orange', video, index)
  blueRed      <- prepInterval('Blue', 'Red', video, index)
  
  #applyCRQA(blueOrange, orangeRed, blueRed, video, index)
  applyDCRP(blueOrange, orangeRed, blueRed, video, index)
}


dcrpResults <- mapply(main, 1, 1:15)
mapply(main, 2, 1:5)

#' prepInterval
#' 
#' Load the time interval and create a time series
#' 
#' @description  
#' The prepInterval function loads the analysis interval (data frame), selects 
#' the columns of interest (time and target code), and creates a continuous (at
#' the level of tenths of second) time series of target codes.
#' 
#' @return 
#' A time series of target codes


prepInterval <- function(color1,
                         color2,
                         video,
                         index) {

  fileName1 <- paste0("./inputData/", color1, "_w", color2, "_V", video, ".csv")
  read.csv(fileName1, header = TRUE) %>%
           filter(Index == index) %>%
           select(Sync.Time, Target.Code) %>%
           separate(Sync.Time, c("Min", "Sec", "Tenth"))%>%
           transmute(time = as.numeric(Min)*600 + as.numeric(Sec)* 10 + 
                     as.numeric(substr(Tenth, 1, 1)), 
                     Target = Target.Code) %>% drop_na(time) -> dataF1
  
  fileName2 <- paste0("./inputData/",color2, "_w", color1, "_V", video, ".csv")
  read.csv(fileName2, header = TRUE) %>%
           filter(Index == index) %>%
           select(Sync.Time, Target.Code) %>%
           separate(Sync.Time, c("Min", "Sec", "Tenth")) %>%
           transmute(time = as.numeric(Min)*600 + as.numeric(Sec)* 10 + 
                     as.numeric(substr(Tenth, 1, 1)), 
                     Target = Target.Code) %>% drop_na(time) -> dataF2
 
  #Create a sequence of tenths of second with no gaps from the first to the last 
  #tenth of second
  tenthSequence1 <- data.frame(time = seq(max(dataF1$time) - dataF1$time[1]) +
                               dataF1$time[1] - 1)
  tenthSequence2 <- data.frame(time = seq(max(dataF2$time) - dataF2$time[1]) +
                               dataF2$time[1] - 1)
 
  #Create a data frame by joining tenthSequence and dataF. The new dataframe 
  #contains a continuous time column (at the level of tenths of a second) from
  #the beginning to the end of the time interval and an incomplete target code
  #column 
  timeSer1 <- left_join(tenthSequence1, dataF1, by = "time")
  timeSer2 <- left_join(tenthSequence2, dataF2, by = "time")
 
  #Create the time series to use in the analysis by filling in missing target 
  #values
  for (i in 1:nrow(timeSer1)) {
    if (is.na(timeSer1$Target[i])) {
      timeSer1$Target[i] <- timeSer1$Target[i-1]
    }
  }
  for (i in 1:nrow(timeSer2)) {
    if (is.na(timeSer2$Target[i])) {
      timeSer2$Target[i] <- timeSer2$Target[i-1]
    }
  }
  
  #Join both time series
  timeSer <- inner_join(timeSer1, timeSer2, by = "time")
}

#' applyCRQA
#' 
#' Apply a Cross-Recurrence Quantification Analysis
#' 
#' @description 
#' 
#' @return  

applyCRQA <- function(blueOrange,
                      orangeRed,
                      blueRed,
                      video,
                      index) {
  
 
  crqa_blueOrange <- crqa(ts1 = blueOrange$Target.x, ts2 = blueOrange$Target.y,
                          delay = 0, embed = 0, normalize = 0, rescale = 0, 
                          radius = 0.05, mindiagline = 2, minvertline = 2, 
                          tw = 0, whiteline = FALSE, side = "both")

  crqa_orangeRed  <- crqa(ts1 = orangeRed$Target.x, ts2 = orangeRed$Target.y, 
                          delay = 0, embed = 0, normalize = 0, rescale = 0, 
                          radius = 0.05, mindiagline = 2, minvertline = 2,
                          tw = 0, whiteline = FALSE, side = "both")

  crqa_blueRed    <- crqa(ts1 = blueRed$Target.x, ts2 = blueRed$Target.y, 
                          delay = 0, embed = 0, normalize = 0, rescale = 0, 
                          radius = 0.05, mindiagline = 2, minvertline = 2, 
                          tw = 0, whiteline = FALSE, side = "both")


  #Create cross-recurrence plots
  crqaPlot_blueOrange <- paste0("./outcomes/crqa_blueOrange_V", video, "_", index, ".png")
  crqaPlot_orangeRed  <- paste0("./outcomes/crqa_orangeRed_V", video, "_", index, ".png")
  crqaPlot_blueRed    <- paste0("./outcomes/crqa_blueRed_V", video, "_", index, ".png")

  png(filename = crqaPlot_blueOrange)
  RP1 <- as.matrix(crqa_blueOrange$RP)
  z <- t(RP1[,ncol(RP1):1])
  x <- seq(1:nrow(z))
  y <- seq(1:ncol(z))
  title <- paste0("Interval V", video, "_", index, " CRQA Plot")
  image(x, y, z, main = title,
        col = gray.colors(2, start = 0, end = 1, rev = TRUE),
        xlab = "Participant 2 Time (Tenths of a Second)",
        ylab = "Participant 3 Time (Tenths of a Second)")
  dev.off()

  png(filename = crqaPlot_orangeRed)
  RP2 <- as.matrix(crqa_orangeRed$RP)
  z <- t(RP2[,ncol(RP2):1])
  x <- seq(1:nrow(z))
  y <- seq(1:ncol(z))
  title <- paste0("Interval V", video, "_", index, " CRQA Plot")
  image(x, y, z, main = title,
        col = gray.colors(2, start = 0, end = 1, rev = TRUE),
        xlab = "Participant 1 Time (Tenths of a Second)",
        ylab = "Participant 2 Time (Tenths of a Second)")
  dev.off()

  png(filename = crqaPlot_blueRed)
  RP3 <- as.matrix(crqa_blueRed$RP)
  z <- t(RP3[,ncol(RP3):1])
  x <- seq(1:nrow(z))
  y <- seq(1:ncol(z))
  title <- paste0("Interval V", video, "_", index, " CRQA Plot")
  image(x, y, z, main = title,
        col = gray.colors(2, start = 0, end = 1, rev = TRUE),
        xlab = "Participant 1 Time (Tenths of a Second)",
        ylab = "Participant 3 Time (Tenths of a Second)")
  dev.off()


  #Export cross-recurrence outcome measures
  crqaMeasures_blueOrange <- paste0("./outcomes/crqa_blueOrange_V",
                                   video, "_", index, ".txt")
  crqaMeasures_orangeRed  <- paste0("./outcomes/crqa_orangeRed_V",
                                   video, "_", index, ".txt")
  crqaMeasures_blueRed    <- paste0("./outcomes/crqa_blueRed_V",
                                   video, "_", index, ".txt")

  write.table(unlist(crqa_blueOrange[1:9]), file = crqaMeasures_blueOrange,
              col.names = FALSE)
  write.table(unlist(crqa_orangeRed[1:9]), file = crqaMeasures_orangeRed,
              col.names = FALSE)
  write.table(unlist(crqa_blueRed[1:9]), file = crqaMeasures_blueRed,
              col.names = FALSE)
  
}


#' applyDCRP
#' 
#' Apply a Diagonal Cross-Recurrence Profile Analysis
#' 
#' @description 
#' 
#' @return  

applyDCRP <- function(blueOrange,
                      orangeRed,
                      blueRed,
                      video,
                      index) {
  
  #Window size 100 tenths of second (lag around the main diagonal, 50 on each side)

  dcrp_blueOrange <- drpfromts(ts1 = blueOrange$Target.x, 
                               ts2 = blueOrange$Target.y, windowsize = 50,
                               datatype = "continuous", radius = 0.05)
  
  dcrp_orangeRed  <- drpfromts(ts1 = orangeRed$Target.x,
                               ts2 = orangeRed$Target.y, windowsize = 50,
                               datatype = "continuous", radius = 0.05)

  dcrp_blueRed    <- drpfromts(ts1 = blueRed$Target.x,
                               ts2 = blueRed$Target.y, windowsize = 50,
                               datatype = "continuous", radius = 0.05)


# #DCRP plots

  dcrpPlot_blueOrange <- paste("./outcomes/dcrp_blueOrange_V",
                               video, "_", index, ".png", sep = "")
  dcrpPlot_orangeRed  <- paste("./outcomes/dcrp_orangeRed_V",
                               video, "_", index, ".png", sep = "")
  dcrpPlot_blueRed    <- paste("./outcomes/dcrp_blueRed_V",
                               video, "_", index, ".png", sep = "")

  png(filename = dcrpPlot_blueOrange)
  plot(-50:50, dcrp_blueOrange$profile, type = "l", xlab = "Lag", ylab = "%REC")
  dev.off()

  png(filename = dcrpPlot_orangeRed)
  plot(-50:50, dcrp_orangeRed$profile, type = "l", xlab = "Lag", ylab = "%REC")
  dev.off()

  png(filename = dcrpPlot_blueRed)
  plot(-50:50, dcrp_blueRed$profile, type = "l", xlab = "Lag", ylab = "%REC")
  dev.off()


  #DCRP outcome measures
  dcrpMeasures_blueOrange <- paste0("./outcomes/dcrp_blueOrange_V",
                                    video, "_", index, ".txt")
  dcrpMeasures_orangeRed  <- paste0("./outcomes/dcrp_orangeRed_V",
                                    video, "_", index, ".txt")
  dcrpMeasures_blueRed    <- paste0("./outcomes/dcrp_blueRed_V",
                                    video, "_", index, ".txt")


  write.table(unlist(dcrp_blueOrange[2:3]), file = dcrpMeasures_blueOrange,
              col.names = FALSE)
  write.table(unlist(dcrp_orangeRed[2:3]), file = dcrpMeasures_orangeRed,
              col.names = FALSE)
  write.table(unlist(dcrp_blueRed[2:3]), file = dcrpMeasures_blueRed,
              col.names = FALSE)
  #dcrpResults <- dcrp_blueOrange[2:3]
  #write.csv(unlist(dcrp_blueOrange[2:3]), file = dcrpMeasures_blueOrange)
}

createDF <- function(){
  
 list_of_files <- list.files(path = "./outcomes/", recursive = TRUE,
                             pattern = "\\.txt$", full.names = TRUE)
 
 library(tidyverse)
  
 dfLong <- list_of_files %>% set_names(.) %>% 
       map_df(read_table2, .id = "FileName", col_names = FALSE)
 colnames(dfLong) <- c('FileName', 'Measure', 'Value')
 #dfLong %>% filter(!grepl('dcrp', FileName))  -> dfLongClean
 #dfWide <- pivot_wider(dfLongClean, names_from = Measure, values_from = Value)
 dfWide <- pivot_wider(dfLong, names_from = Measure, values_from = Value)
}

applyClustering <- function(){
  
  library(cluster)
  library(factoextra)
  df <- createDF()
  df %>% select(1:5, 7:8) -> df 
  df[,-1] <- scale(df[,-1])
  dfNoNames <- select(df, -1)
  namesOnly <- select(df, 1)
  
  dfWide[,-1] <- scale(dfWide[,-1])
  dfNoNames1 <- select(dfWide, -1)
  #dfNoNames <- select(dfNoNames1, 1, 2, 3, 4, 5, 7, 9)
  
  #K-Means Clustering
  #Optimal number of clusters
  fviz_nbclust(dfNoNames, kmeans, method = "silhouette")
  # fviz_nbclust(dfNoNames, kmeans, method = "wss")
  # gap_stat <<- clusGap(dfNoNames,
  #                      FUN = kmeans,
  #                      nstart = 25,
  #                      K.max = 10,
  #                      B = 50)
  # fviz_gap_stat(gap_stat)
  
  kmFinal <- kmeans(dfNoNames, 2)
  dfNoNames$cluster <- kmFinal$cluster
  dfNoNames$FileName <- namesOnly$FileName
  head(kmFinal, 10)
  
  #Hierarchical Clustering
  dfWide2 <- select(dfWide, 1:11)
  dfWide2Top <- slice(dfWide2, 1:60)
  dfWide2Top <- select(dfWide2Top, -11)
  dfWide2Bottom <- slice(dfWide2, 61:120)
  dfWide2Bottom <- select(dfWide2Bottom, 11)
  dfWide3 <- cbind(dfWide2Top, dfWide2Bottom) 
  dfWide3[,-1] <- scale(dfWide3[,-1])
  dfNoNames2 <- select(dfWide3, -1)
  maxlag <- read.csv("./inputData/maxlag.csv", header = TRUE)
  maxlag1 <- select(maxlag, 3)
  maxlag2 <- scale(maxlag1)
  dfNoNames1 <- cbind(dfNoNames2, maxlag2)
  #dfNoNames <- select(dfNoNames1, 1, 2, 3, 4, 5, 7, 9)
  dfNoNames <- select(dfNoNames1, 11)
  hFinal <- hclust(dist(dfNoNames), method = "ward.D")
  #hFinal <- hclust(dist(dfNoNames), method = "complete")
  #hFinal <- hclust(dist(dfNoNames), method = "ward.D2")
  plot(hFinal)
  rect.hclust(hFinal, k=2)
  groups <- cutree(hFinal, k=2)
  dfNoNames$clust <- groups
  write.csv(groups, "./outcomes/clust1.csv")
  
  #Hierarchical Clustering with pvclust
  library(pvclust)
  dfNoNames <- select(dfNoNames1, 6)
  phFinal <- pvclust(dfNoNames, method.dist = "cor",
                     method.hclust = "average", nboot = 10000,
                     parallel = TRUE)
  plot(phFinal)
  pvrect(phFinal, alpha = 0.95)
  print(phFinal, digits = 10)
  
  #Density-Based Spatial Clustering (DBSCAN)
  library(dbscan)
  dfNoNames <- select(dfNoNames1, 7, 9)
  m <- as.matrix(dfNoNames)
  
  eps_plot <- kNNdistplot(m, k=3)
  eps_plot %>% abline(h = 0.95, lty = 2)
  db <- dbscan(m, eps = 0.95, minPts = 3)
  db
}

#Feature (Variable) Selection

#Dyad Plotting
  x <- dfNoNames1$`"ENTR"`
  y <- dfNoNames1$`"rENTR"`
  plot(x, y)
  
#Correlation
  library(corrplot)
  m <- cor(dfNoNames1)
  corrplot(m, method = 'number', order= 'hclust', type = 'full')
  
  
#PCA
  dfNoNames <- select(dfNoNames1, 1, 3:7, 9)
  components <- prcomp(dfNoNames, scale = FALSE)
  components$rotation <- -1*components$rotation
  components$x <- -1*components$x
  var_explained <- components$sdev^2 / sum(components$sdev^2)
  
  dfPCA <- components$x
  dfPCA <- select(as.data.frame(dfPCA), 1:3)
  hFinalPCA <- hclust(dist(dfPCA), method = "ward.D")
  #hFinal <- hclust(dist(dfNoNames1[, 1:5, 7]), method = "ward.D")
  plot(hFinalPCA)
  rect.hclust(hFinalPCA, k=2)
  groups <- cutree(hFinalPCA, k=2)
  groups
  
#Logistic Regression
  library(car)
  library(mlogit)
  
  dv <- read.csv("DV_051423.csv", header = TRUE, stringsAsFactors = TRUE)
    iv <- dfNoNames1  
  df <- cbind(dv, iv)  
  
#Logistic Regression Checking for multicollinearity
  #*************************************************
  #************************************************
  library(dplyr)
  library(car)
  dfMult <- select(df, 3:14)
  names(dfMult) <- c('PI', 'DM', 'BL', 'RR', 'DET', 'NRLINE', 'maxL', 'L', 'ENTR', 'rENTR', 'LAM', 'TT')
  #Logistic Regression with different CRQA predictors
  modelMult1 <- glm(DM ~ RR + DET + NRLINE + maxL + L + ENTR + rENTR + LAM + TT, data = dfMult, family = binomial)
  summary(modelMult1)
    vif(modelMult1)
  modelMult2 <- glm(DM ~ RR + DET + NRLINE + maxL + L + ENTR + rENTR, data = dfMult, family = binomial)
  summary(modelMult2)
  vif(modelMult2)
  modelMult3 <- glm(DM ~ RR + DET + NRLINE + L + ENTR + rENTR, data = dfMult, family = binomial)
  summary(modelMult3)
  vif(modelMult3)  
  modelMult4 <- glm(DM ~ RR + DET + NRLINE + maxL + ENTR + rENTR, data = dfMult, family = binomial)
  summary(modelMult4)
  vif(modelMult4)  
  modelMult5 <- glm(DM ~ RR + NRLINE + L + ENTR + rENTR, data = dfMult, family = binomial)
  summary(modelMult5)
  vif(modelMult5)
  modelMult6 <- glm(DM ~ RR + NRLINE + L + ENTR, data = dfMult, family = binomial)
  summary(modelMult6)
  vif(modelMult6)
  
 
  # library(caret)
  # 
  # modelMult6.preds <- predict(modelMult6, type = "response")
  # ord <- order(dfMult$RR)
  # dfMult2 <- dfMult[ord,]  
  # modelMult6.preds <- modelMult6.preds[ord]
  # 
  # 
  # 
  # windows()
  # with(dfMult2, plot(RR, ifelse(DM=="Y", 1, 0),
  #                     ylab="predicted probability of Y"))
  # lines(dfMult2$RR, modelMult6.preds)  
  # 
  # modelClass <- ifelse(modelMult6.preds < .5, "N", "Y")
  # confusionMatrix(modelClass, dfMult2$DM)  
  # table(modelClass, dfMult2$DM)
  
#Remove unwanted columns, including those highly correlated
  dfClean <- select(df, 3:8, 12, 13, 16)
  names(dfClean) <- c('PI', 'DM', 'BL', 'RR', 'DET', 'NRLINE', 'rENTR', 'LAM', 'maxlag')
  
  modelPower <- glm(PI ~ RR + DET + NRLINE + rENTR + LAM, data = dfClean, family = binomial)
  summary(modelPower)
  modelPowerrENTR <- glm(PI ~ rENTR, data = dfClean, family = binomial)
  summary(modelPowerrENTR)
 
  modelDecision <- glm(DM ~ RR + DET + NRLINE + rENTR + LAM, data = dfClean, family = binomial)
  summary(modelDecision)
  modelDecisionrENTR <- glm(DM ~ rENTR, data = dfClean, family = binomial)
  summary(modelDecisionrENTR)
 
  modelBody <- glm(BL ~ RR + DET + NRLINE + rENTR + LAM, data = dfClean, family = binomial)
  summary(modelBody) 
  modelBody <- glm(BL ~ rENTR, data = dfClean, family = binomial)
  summary(modelBody) 
  
#Dyad Plotting to look for non-linear relationships
  x <- dfClean$`DM`
  y <- dfClean$`rENTR`
  plot(y, x)
  
#Following stats.exchange.com/questions/181501
  library(party)
  cart.model <- ctree(DM ~ rENTR, dfClean)
  windows()
    plot(cart.model)
    confusionMatrix(predict(cart.model), dfClean$DM)
    
  library(caret)
  modelDecisionrENTR <- glm(DM ~ rENTR, data = dfClean, family = binomial)
  modelDecisionrENTR.preds <- predict(modelDecisionrENTR, type = "response")
  ord <- order(dfClean$rENTR)
  dfClean2 <- dfClean[ord,]  
  modelDecisionrENTR.preds <- modelDecisionrENTR.preds[ord]
  
  
  
  windows()
  with(dfClean2, plot(rENTR, ifelse(DM=="Y", 1, 0),
                      ylab="predicted probability of Y"))
  lines(dfClean2$rENTR, modelDecisionrENTR.preds)  
  
  modelClass <- ifelse(modelDecisionrENTR.preds < .5, "N", "Y")
  confusionMatrix(modelClass, dfClean2$DM)  
  table(modelClass, dfClean2$DM)
  
  #Sin normalizar rENTR
  library(dplyr)
  dv2 <- select(dv, 4)
  names(dv2) <- "DM"
  iv2 <- select(dfWide2Top, 8)
  names(iv2) <- "rENTR"
  df2 <- cbind(dv2, iv2)
  modelDecisionrENTR <- glm(DM ~ rENTR, data = df2, family = binomial)
  summary(modelDecisionrENTR)
  
  x <- df2$`DM`
  y <- df2$`rENTR`
  plot(y, x)
  
  modelDecisionrENTR.preds <- predict(modelDecisionrENTR, type = "response")
  ord <- order(df2$rENTR)
  df3 <- df2[ord,]  
  modelDecisionrENTR.preds <- modelDecisionrENTR.preds[ord]
  
  windows()
  with(df3, plot(rENTR, ifelse(DM=="Y", 1, 0),
                      ylab="predicted probability of Y"))
  lines(dfClean2$rENTR, modelDecisionrENTR.preds)  
  
  modelClass <- ifelse(modelDecisionrENTR.preds < .5, "N", "Y")
  confusionMatrix(modelClass, dfClean2$DM)  
  table(modelClass, df3$DM)
  
  #Logistic Regression with different CRQA predictors without standardization
  
  dfNS <- cbind(dv, dfWide2Top) 
  dfNS <- select(dfNS, -1, -2,-6)
  names(dfNS) <- c('PI', 'DM', 'BL', 'RR', 'DET', 'NRLINE', 'maxL', 'L', 'ENTR', 'rENTR', 'LAM', 'TT')
  
  modelMult1NS <- glm(DM ~ RR + DET + NRLINE + maxL + L + ENTR + rENTR + LAM + TT, data = dfNS, family = binomial)
  summary(modelMult1NS)
  vif(modelMult1NS)
  modelMult2 <- glm(DM ~ RR + DET + NRLINE + maxL + L + ENTR + rENTR, data = dfMult, family = binomial)
  summary(modelMult2)
  vif(modelMult2)
  modelMult3 <- glm(DM ~ RR + DET + NRLINE + L + ENTR + rENTR, data = dfMult, family = binomial)
  summary(modelMult3)
  vif(modelMult3)  
  modelMult4 <- glm(DM ~ RR + DET + NRLINE + maxL + ENTR + rENTR, data = dfMult, family = binomial)
  summary(modelMult4)
  vif(modelMult4)  
  modelMult5NS <- glm(DM ~ RR + NRLINE + L + ENTR + rENTR, data = dfNS, family = binomial)
  summary(modelMult5NS)
  vif(modelMult5NS)
  modelMult6NS <- glm(DM ~ RR + NRLINE + L + ENTR, data = dfNS, family = binomial)
  summary(modelMult6NS)
  vif(modelMult6NS)
  
##Using PCA to address multicollinearity
  
  library(corrplot)
  
  dfNSPredictors <- select(dfWide2Top, -1)
  m <- cor(dfNSPredictors)
  corrplot(m, method = 'number', order= 'hclust', type = 'full')
  
  #Remove low correlated (r<.7) and vertical measures (DET, NRLINE, rENTR, LAM, TT)
  dfNSPred <- select(dfNSPredictors, -2, -3, -7, -8, -9)    
  components <- prcomp(dfNSPred, scale = TRUE)
  plot(components)
  summary(components)
  #components$rotation <- -1*components$rotation
  #components$x <- -1*components$x
  #var_explained <- components$sdev^2 / sum(components$sdev^2)
  
  dfNSPCA <- components$x
  dfNSPCA <- select(as.data.frame(dfNSPCA), 1:2)
  
  #Create new dataframe for logistic regression with PC1, PC2, DET, NRLINE, and rENTR

  dfNSReduced <- select(dfNS, -4, -c(7:9), -11, -12)  
  dfLR <- cbind(dfNSReduced, dfNSPCA)
  
  modelMult1PCA <- glm(DM ~ DET + NRLINE + rENTR + LAM + PC1 + PC2, data = dfLR, family = binomial)
  summary(modelMult1PCA)
  vif(modelMult1PCA)
  
  modelMult2PCA <- glm(DM ~ DET + NRLINE + rENTR + LAM + PC1, data = dfLR, family = binomial)
  summary(modelMult2PCA)
  vif(modelMult2PCA)
  
  modelMult3PCA <- glm(DM ~ DET + NRLINE + rENTR + PC1 + PC2, data = dfLR, family = binomial)
  summary(modelMult3PCA)
  vif(modelMult3PCA)
  
  modelMult4PCA <- glm(DM ~ DET + NRLINE + rENTR + PC1, data = dfLR, family = binomial)
  summary(modelMult4PCA)
  vif(modelMult4PCA)
  
  modelMult5PCA <- glm(DM ~ rENTR, data = dfLR, family = binomial)
  summary(modelMult5PCA)
  vif(modelMult5PCA)
  