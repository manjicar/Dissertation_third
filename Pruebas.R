library(crqa)
ts1 <- c("b", "a", "d" ,"c", "d","a", "b", "c" ,"d", "d" )
ts2 <- c("d", "d", "b" ,"d", "d","b", "d", "b" ,"d", "d")
prueba <- crqa(ts1, ts2, 
     delay = 0, embed = 0, normalize = 0, rescale = 0, 
     radius = 0.05, mindiagline = 2, minvertline = 2,
     tw = 0, whiteline = FALSE, side = "both")

png(filename = "./outcomes/prueba.png")
RP1 <- as.matrix(prueba$RP)
z <- t(RP1[,ncol(RP1):1])
x <- seq(1:10)
y <- seq(1:10)

image(x, y, z)
dev.off()

write.table(unlist(crqa_blueOrange[1:9]), file = crqaMeasures_blueOrange,
            col.names = FALSE)