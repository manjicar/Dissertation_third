library(plyr)
library(dplyr)
library(tidyr)
library(Cairo)
library(roxygen2)
library(crqa)

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


prepInterval <- function(color,
                         index) {

  fileName <- paste0(color, "_", index, ".csv")
  read.csv(fileName, header = TRUE) %>%
    select(Sync.Time, Target.Code) %>%
    separate(Sync.Time, c("Min", "Sec", "Tenth")) %>%
    transmute(time = as.numeric(Min)*600 + as.numeric(Sec)* 10 + 
              as.numeric(Tenth), 
              Target = Target.Code) %>% drop_na(time) -> dataF
 
  #Create a sequence of tenths of second with no gaps from the first to the last 
  #tenth of second
  tenthSequence <- data.frame(time = seq(max(dataF$time) - dataF$time[1]) +
                               dataF$time[1] - 1)
 
  #Create a data frame by joining tenthSequence and dataF. The new dataframe 
  #contains a continuous time column (at the level of tenths of a second) from
  #the beginning to the end of the time interval and an incomplete target code
  #column 
  timeSer <- left_join(tenthSequence, dataF, by = "time")
 
  #Create the time series to use in the analysis by filling in missing target 
  #values
  for (i in 1:nrow(timeSer)) {
    if (is.na(timeSer$Target[i])) {
      timeSer$Target[i] <- timeSer$Target[i-1]
    }
  }
 
  return(timeSer)
}

#prepInterval('Red', 1)

  
##Create time-series data frames containing the inner join of two (to remove target NAs)
#orangeBlue_ts <- inner_join(orange, blue, by = "time")
#orangeRed_ts <- inner_join(orange, red, by = "time")
#redBlue_tsb <- inner_join(orangeRed_ts, orangeBlue_ts, by = "time")
#redBlue_ts <- select(redBlue_tsb, time = time, Target.x = Target.y.x, Target.y = Target.y.y)

##Create time-series data frames containing the inner join of three
#orangeBlueRed_ts <- inner_join(orangeBlue_ts, red, by = "time")
#orangeBlueRed_ts <- rename(orangeBlueRed_ts, Target.z = Target)


#***************************************************
#*  *
#***************************************************

#' applyCRQA
#' 
#' Apply a Cross-Recurrence Quantification Analysis
#' 
#' @description 
#' 
#' @return  

applyCRQA <- function(index) {
  
  red    <- prepInterval('Red', index)
  #blue   <- prepInterval('Blue', index)
  orange <- prepInterval('Orange', index)
  
  #crqa_orangeBlue <- crqa(ts1 = orange$Target, ts2 = blue$Target, delay = 0,
                          #embed = 0, normalize = 0, rescale = 0, radius = 0.05,
                          # mindiagline = 2, minvertline = 2, tw = 0,
                          # whiteline = FALSE, side = "both")

  crqa_orangeRed <- crqa(ts1 = orange$Target, ts2 = red$Target, delay = 0,
                         embed = 0, normalize = 0, rescale = 0, radius = 0.05,
                         mindiagline = 2, minvertline = 2, tw = 0,
                         whiteline = FALSE, side = "both")

  #crqa_redBlue   <- crqa(ts1 = red$Target, ts2 = blue$Target, delay = 0,
                         # embed = 0, normalize = 0, rescale = 0, radius = 0.05,
                         # mindiagline = 2, minvertline = 2, tw = 0,
                         # whiteline = FALSE, side = "both")


  #Create cross-recurrence plots
  #outplot_orange_blue <- paste0("orangeBlue_", index, ".png")
  outplot_orange_Red  <- paste0("orangeRed_", index, ".png")
  #outplot_red_blue    <- paste0("redBlue_", index, ".png")

  # png(filename = outplot_orange_blue)
  # image(crqa_orangeBlue$RP)
  # dev.off()

 png(filename = outplot_orange_Red)
 RP <- as.matrix(crqa_orangeRed$RP)
 z <- t(RP[,ncol(RP):1])
 x <- seq(1:nrow(z))
 y <- seq(1:ncol(z))
 title <- paste0("Interval ", index, " CRQA Plot")
 image(x, y, z, main = title, 
       col = gray.colors(2, start = 0, end = 1, rev = TRUE),
       xlab = "Participant 1 Time (Tenths of a Second)",
       ylab = "Participant 2 Time (Tenths of a Second)")
 dev.off()

 # png(filename = outplot_red_blue)
  #image(crqa_redBlue$RP)
  #dev.off()


  #Compute cross-recurrence outcome measures
  #outmeasures_orange_blue <- paste0("orangeBlue_", index, ".txt")
   #outmeasures_orange_Red  <- paste0("orangeRed_", index, ".txt")
  # outmeasures_red_blue    <- paste0("redBlue_", index, ".txt")

  # sink(outmeasures_orange_blue)
  # crqa_orangeBlue[1:9]
  # sink()

  # sink(outmeasures_orange_Red)
  # crqa_orangeRed[1:9]
  # sink()
  # 
  # sink(outmeasures_red_blue)
  # crqa_redBlue[1:9]
  # sink()
#return(crqa_orangeRed)
   
 #  RP <- crqa_orangeRed$RP
 #  par = list(unit = 100, labelx = "Time", labely = "Time",
 #             cols = "blue", pcex = 1, pch = 19,
 #             labax = seq(0, nrow(RP), 100),
 #             labay = seq(0, nrow(RP), 100),
 #             las = 1)
 #  
 # plotRP(RP, par)
   
  
}
applyCRQA(1)
#*****************************************************
#* Diagonal Cross-Recurrence Profile (DCRP) Analysis *
#*****************************************************

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


