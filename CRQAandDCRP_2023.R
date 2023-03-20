library(plyr)
library(dplyr)
library(tidyr)
library(Cairo)
library(roxygen2)
library(crqa)


#' main
#' 
#' Apply All Calculations for a Specific Index
#' 
#' @description 
#' 
#' @return  

main <- function(index) {
  
  orangeRed    <- prepInterval('Orange', 'Red', index)
  blueOrange   <- prepInterval('Blue', 'Orange', index)
  blueRed      <- prepInterval('Blue', 'Red', index)
  
  crqaResults <- applyCRQA(blueOrange, orangeRed, blueRed, index)
  
  dcrpResults <- applyDCRP(blueOrange, orangeRed, blueRed, index)
  
  
  
}




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
                         index) {

  fileName1 <- paste0(color1, "_w", color2, "_V1.csv")
  read.csv(./inputData/fileName1, header = TRUE) %>%
           filter(Index == index) %>%
           select(Sync.Time, Target.Code) %>%
           separate(Sync.Time, c("Min", "Sec", "Tenth")) %>%
           transmute(time = as.numeric(Min)*600 + as.numeric(Sec)* 10 + 
                     as.numeric(substr(Tenth, 1, 1)), 
                     Target = Target.Code) %>% drop_na(time) -> dataF1
  
  fileName2 <- paste0(color2, "_w", color1, "_V1.csv")
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

#ts <- prepInterval('Red', 'Orange', 1)

  
##Create time-series data frames containing the inner join of two (to remove target NAs)
#orangeBlue_ts <- inner_join(orange, blue, by = "time")
#orangeRed_ts <- inner_join(orange, red, by = "time")
#redBlue_tsb <- inner_join(orangeRed_ts, orangeBlue_ts, by = "time")
#redBlue_ts <- select(redBlue_tsb, time = time, Target.x = Target.y.x, Target.y = Target.y.y)

##Create time-series data frames containing the inner join of three
#orangeBlueRed_ts <- inner_join(orangeBlue_ts, red, by = "time")
#orangeBlueRed_ts <- rename(orangeBlueRed_ts, Target.z = Target)


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
  outplot_blueOrange <- paste0("blueOrange_", index, ".png")
  outplot_orangeRed  <- paste0("orangeRed_", index, ".png")
  outplot_blueRed    <- paste0("blueRed_", index, ".png")

  png(filename = outplot_blueOrange)
  RP1 <- as.matrix(crqa_blueOrange$RP)
  z <- t(RP1[,ncol(RP1):1])
  x <- seq(1:nrow(z))
  y <- seq(1:ncol(z))
  title <- paste0("Interval ", index, " CRQA Plot")
  image(x, y, z, main = title,
        col = gray.colors(2, start = 0, end = 1, rev = TRUE),
        xlab = "Participant 2 Time (Tenths of a Second)",
        ylab = "Participant 3 Time (Tenths of a Second)")
  dev.off()

  png(filename = outplot_orangeRed)
  RP2 <- as.matrix(crqa_orangeRed$RP)
  z <- t(RP2[,ncol(RP2):1])
  x <- seq(1:nrow(z))
  y <- seq(1:ncol(z))
  title <- paste0("Interval ", index, " CRQA Plot")
  image(x, y, z, main = title,
        col = gray.colors(2, start = 0, end = 1, rev = TRUE),
        xlab = "Participant 1 Time (Tenths of a Second)",
        ylab = "Participant 2 Time (Tenths of a Second)")
  dev.off()

  png(filename = outplot_blueRed)
  RP3 <- as.matrix(crqa_blueRed$RP)
  z <- t(RP3[,ncol(RP3):1])
  x <- seq(1:nrow(z))
  y <- seq(1:ncol(z))
  title <- paste0("Interval ", index, " CRQA Plot")
  image(x, y, z, main = title,
        col = gray.colors(2, start = 0, end = 1, rev = TRUE),
        xlab = "Participant 1 Time (Tenths of a Second)",
        ylab = "Participant 3 Time (Tenths of a Second)")
  dev.off()


  #Compute cross-recurrence outcome measures
  outmeasures_blueOrange <- paste0("blueOrange_", index, ".txt")
  outmeasures_orangeRed  <- paste0("orangeRed_", index, ".txt")
  outmeasures_blueRed    <- paste0("blueRed_", index, ".txt")
  
  write.table(unlist(crqa_blueOrange[1:9]), file = outmeasures_blueOrange, 
              col.names = FALSE)
  write.table(unlist(crqa_orangeRed[1:9]), file = outmeasures_orangeRed, 
              col.names = FALSE)
  write.table(unlist(crqa_blueRed[1:9]), file = outmeasures_blueRed, 
              col.names = FALSE)
   
}
applyCRQA(1)

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
                      index) {
  
#Window size 100 tenths of second (lag around the main diagonal, 50 on each side)

dcrp_orangeBlue <- drpdfromts(ts1 = orangeBlue_ts$Target.x, ts2 = orangeBlue_ts$Target.y, ws = 50,
                              datatype = "categorical", radius = 0.05)

dcrp_orangeRed <- drpdfromts(ts1 = orangeRed_ts$Target.x, ts2 = orangeRed_ts$Target.y, ws = 50,
                              datatype = "categorical", radius = 0.05)

dcrp_redBlue <- drpdfromts(ts1 = redBlue_ts$Target.x, ts2 = redBlue_ts$Target.y, ws = 50,
                              datatype = "categorical", radius = 0.05)

wdrp_orangeBlue <- windowdrp(ts1 = orangeBlue_ts$Target.x, ts2 = orangeBlue_ts$Target.y, windowsize = 2300,
                              datatype = "categorical", radius = 0.05,  delay = 0, embed = 0,  
                             normalize = 0, rescale = 0, mindiagline = 2, minvertline = 2, lagwidth = 100)

wdrp_orangeRed <- windowdrp(ts1 = orangeRed_ts$Target.x, ts2 = orangeRed_ts$Target.y, ws = 50,
                             datatype = "categorical", radius = 0.05)

wdrp_redBlue <- windowdrp(ts1 = redBlue_ts$Target.x, ts2 = redBlue_ts$Target.y, ws = 50,
                           datatype = "categorical", radius = 0.05)

#DCRP plots

dcrp_plot_orange_blue <- paste("dcrp_orangeBlue", j, ".png", sep = "")
dcrp_plot_orange_Red <- paste("dcrp_orangeRed", j, ".png", sep = "")
dcrp_plot_red_blue <- paste("dcrp_redBlue", j, ".png", sep = "")

png(filename = dcrp_plot_orange_blue)
plot(-50:50, dcrp_orangeBlue$profile, type = "l", xlab = "Lag", ylab = "%REC")
dev.off()

png(filename = dcrp_plot_orange_Red)
plot(-50:50, dcrp_orangeRed$profile, type = "l", xlab = "Lag", ylab = "%REC")
dev.off()

png(filename = dcrp_plot_red_blue)
plot(-50:50, dcrp_redBlue$profile, type = "l", xlab = "Lag", ylab = "%REC")
dev.off()


wdrp_plot_orange_blue <- paste("wdrp_orangeBlue", j, ".png", sep = "")
wdrp_plot_orange_Red <- paste("wdrp_orangeRed", j, ".png", sep = "")
wdrp_plot_red_blue <- paste("wdrp_redBlue", j, ".png", sep = "")

png(filename = wdrp_plot_orange_blue)
plot(wdrp_orangeBlue$profile, type = "l", xlab = "Time (tenths of second)", ylab = "RR")
dev.off()

png(filename = wdrp_plot_orange_Red)
plot(-50:50, wdrp_orangeRed$profile, type = "l", xlab = "Lag", ylab = "")
dev.off()

png(filename = wdrp_plot_red_blue)
plot(-50:50, wdrp_redBlue$profile, type = "l", xlab = "Lag", ylab = "%REC")
dev.off()

#DCRP outcome measures

dcrp_orange_blue <- paste("dcrp_orangeBlue", j, ".txt", sep = "")
dcrp_orange_Red <- paste("dcrp_orangeRed", j, ".txt", sep = "")
dcrp_red_blue <- paste("dcrp_redBlue", j, ".txt", sep = "")

sink(dcrp_orange_blue)
dcrp_orangeBlue[2:3]
sink()

sink(dcrp_orange_Red)
dcrp_orangeRed[2:3]
sink()

sink(dcrp_red_blue)
dcrp_redBlue[2:3]
sink()

wdrp_orange_blue <- paste("wdrp_orangeBlue", j, ".txt", sep = "")
dcrp_orange_Red <- paste("dcrp_orangeRed", j, ".txt", sep = "")
dcrp_red_blue <- paste("dcrp_redBlue", j, ".txt", sep = "")

sink(wdrp_orange_blue)
wdrp_orangeBlue[2:3]
sink()

sink(dcrp_orange_Red)
dcrp_orangeRed[2:3]
sink()

sink(dcrp_red_blue)
dcrp_redBlue[2:3]
sink()

}


