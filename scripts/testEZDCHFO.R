library(matrixStats)
data("pt01EcoG")
timeWindow <- c(-10, 20)
ts<-pt01EcoG[,20001:50001]
fs=1000
sozIndex <- attr(pt01EcoG, "sozIndex")
windowParams<-c(0.25,0.1)

epoch <- Epoch(ts)
visuIEEGData(epoch)

pt01DCShift<-analyze_DCShift(epoch)
