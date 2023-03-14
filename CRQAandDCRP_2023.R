library(plyr)
library(dplyr)
library(tidyr)
library(Cairo)

#The function intervalPrep loads the analysis interval (data frame), selects 
#the columns of interest (time and target code), splits and converts the 
#time column into tenths of second (NOTE: divide tenth by 100 depending on 
#format), and creates a quasi-continuous time column (with no gaps up to the
#level of tenths of second)
intervalPrep <- function(color, index) {

 fileName <- paste0(color, "_", index, ".csv")
 read.csv(fileName, header = TRUE) %>%
   select(Sync.Time, Target.Code) %>%
   separate(Sync.Time, c("Min", "Sec", "Tenth")) %>%
   transmute(time = as.numeric(Min)*600 + as.numeric(Sec)* 10 + as.numeric(Tenth), 
             Target = Target.Code) %>% drop_na(time) -> dataF
 
 return(dataF)
}

intervalPrep('Red', 1)

  


##Create a time column with no gaps (all single values in tenths of seconds) 

#Data frame with a counter for all tenths of seconds on the file
counter_orange <- data.frame(time = seq(max(orange$time) - orange$time[1]) + orange$time[1] - 1)
counter_blue <- data.frame(time = seq(max(blue$time) - blue$time[1]) + blue$time[1] - 1)
counter_red <- data.frame(time = seq(max(red$time) - red$time[1]) + red$time[1] - 1)

#Join data frames
orange <- left_join(counter_orange, orange)
blue <- left_join(counter_blue, blue)
red <- left_join(counter_red, red)

#Filling in missing target values
for (i in 1:nrow(orange)) {
  if (is.na(orange$Target[i])) {
    orange$Target[i] <- orange$Target[i-1]
  }
}

for (i in 1:nrow(blue)) {
  if (is.na(blue$Target[i])) {
    blue$Target[i] <- blue$Target[i-1]
  }
}

for (i in 1:nrow(red)) {
  if (is.na(red$Target[i])) {
    red$Target[i] <- red$Target[i-1]
  }
}
##Create time-series data frames containing the inner join of two (to remove target NAs)
orangeBlue_ts <- inner_join(orange, blue, by = "time")
orangeRed_ts <- inner_join(orange, red, by = "time")
redBlue_tsb <- inner_join(orangeRed_ts, orangeBlue_ts, by = "time")
redBlue_ts <- select(redBlue_tsb, time = time, Target.x = Target.y.x, Target.y = Target.y.y)

##Create time-series data frames containing the inner join of three
orangeBlueRed_ts <- inner_join(orangeBlue_ts, red, by = "time")
orangeBlueRed_ts <- rename(orangeBlueRed_ts, Target.z = Target)


#***************************************************
#* Cross-Recurrence Quantification Analysis (CRQA) *
#***************************************************

library(crqa)

crqa_orangeBlue <- crqa(ts1 = orangeBlue_ts$Target.x, ts2 = orangeBlue_ts$Target.y, delay = 0, embed = 0,  
                        normalize = 0, rescale = 0, radius = 0.05, mindiagline = 2, minvertline = 2, tw = 0, 
                        whiteline = FALSE, side = "both")
crqa_orangeRed <- crqa(ts1 = orangeRed_ts$Target.x, ts2 = orangeRed_ts$Target.y, delay = 0, embed = 0,  
                       normalize = 0, rescale = 0, radius = 0.05, mindiagline = 2, minvertline = 2, tw = 0, 
                       whiteline = FALSE, side = "both")
crqa_redBlue <- crqa(ts1 = redBlue_ts$Target.x, ts2 = redBlue_ts$Target.y, delay = 0, embed = 0,  
                     normalize = 0, rescale = 0, radius = 0.05, mindiagline = 2, minvertline = 2, tw = 0, 
                     whiteline = FALSE, side = "both")

#Cross-recurrence plots

outplot_orange_blue <- paste("orangeBlue", j, ".png", sep = "")
outplot_orange_Red <- paste("orangeRed", j, ".png", sep = "")
outplot_red_blue <- paste("redBlue", j, ".png", sep = "")

png(filename = outplot_orange_blue)
image(crqa_orangeBlue$RP)
dev.off()

png(filename = outplot_orange_Red)
image(crqa_orangeRed$RP)
dev.off()

png(filename = outplot_red_blue)
image(crqa_redBlue$RP)
dev.off()


#Cross-recurrence outcome measures

outmeasures_orange_blue <- paste("orangeBlue", j, ".txt", sep = "")
outmeasures_orange_Red <- paste("orangeRed", j, ".txt", sep = "")
outmeasures_red_blue <- paste("redBlue", j, ".txt", sep = "")

sink(outmeasures_orange_blue)
crqa_orangeBlue[1:9]
sink()

sink(outmeasures_orange_Red)
crqa_orangeRed[1:9]
sink()

sink(outmeasures_red_blue)
crqa_redBlue[1:9]
sink()



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



