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


mapply(main, 1, 1:13)
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
  
  dcrp_blueOrange[3]
  # dcrp_orangeRed  <- drpfromts(ts1 = orangeRed$Target.x, 
  #                              ts2 = orangeRed$Target.y, windowsize = 50,
  #                              datatype = "continuous", radius = 0.05)
  # 
  # dcrp_blueRed    <- drpfromts(ts1 = blueRed$Target.x, 
  #                              ts2 = blueRed$Target.y, windowsize = 50,
  #                              datatype = "continuous", radius = 0.05)


# #DCRP plots
# 
#   dcrpPlot_blueOrange <- paste("./outcomes/dcrp_blueOrange_V",
#                                video, "_", index, ".png", sep = "")
#   dcrpPlot_orangeRed  <- paste("./outcomes/dcrp_orangeRed_V",
#                                video, "_", index, ".png", sep = "")
#   dcrpPlot_blueRed    <- paste("./outcomes/dcrp_blueRed_V",
#                                video, "_", index, ".png", sep = "")
# 
#   png(filename = dcrpPlot_blueOrange)
#   plot(-50:50, dcrp_blueOrange$profile, type = "l", xlab = "Lag", ylab = "%REC")
#   dev.off()
# 
#   png(filename = dcrpPlot_orangeRed)
#   plot(-50:50, dcrp_orangeRed$profile, type = "l", xlab = "Lag", ylab = "%REC")
#   dev.off()
# 
#   png(filename = dcrpPlot_blueRed)
#   plot(-50:50, dcrp_blueRed$profile, type = "l", xlab = "Lag", ylab = "%REC")
#   dev.off()


  #DCRP outcome measures
#   dcrpMeasures_blueOrange <- paste0("./outcomes/dcrp_blueOrange_V",
#                                     video, "_", index, ".txt")
#   dcrpMeasures_orangeRed  <- paste0("./outcomes/dcrp_orangeRed_V",
#                                     video, "_", index, ".txt")
#   dcrpMeasures_blueRed    <- paste0("./outcomes/dcrp_blueRed_V",
#                                     video, "_", index, ".txt")
#  
# 
#   write.table(unlist(dcrp_blueOrange[2:3]), file = dcrpMeasures_blueOrange,
#               col.names = FALSE)
#   write.table(unlist(dcrp_orangeRed[2:3]), file = dcrpMeasures_orangeRed,
#               col.names = FALSE)
#   write.table(unlist(dcrp_blueRed[2:3]), file = dcrpMeasures_blueRed,
#               col.names = FALSE)
#   
#   write.csv(unlist(dcrp_blueOrange[2:3]), file = dcrpMeasures_blueOrange)
}

createDF <- function(){
  
 list_of_files <- list.files(path = "./outcomes/", recursive = TRUE,
                             pattern = "\\.txt$", full.names = TRUE)
 
 library(tidyverse)
  
 dfLong <- list_of_files %>% set_names(.) %>% 
       map_df(read_table2, .id = "FileName", col_names = FALSE)
 colnames(dfLong) <- c('FileName', 'Measure', 'Value')
 dfLong %>% filter(!grepl('dcrp', FileName))  -> dfLongClean
 
 dfWide <- pivot_wider(dfLongClean, names_from = Measure, values_from = Value)
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
  dfNoNames <- select(dfNoNames1, 1, 2, 3, 4, 5, 7, 9)
  
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
  hFinal <- hclust(dist(dfNoNames[, 1:5, 7]), method = "ward.D")
  plot(hFinal)
  rect.hclust(hFinal, k=3)
  
  groups <- cutree(hFinal, k=3)
}


  
  
  
  
  
  
}