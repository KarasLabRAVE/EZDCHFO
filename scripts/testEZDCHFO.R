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

lowPassTs<-pt01DCShift@lowPassTs[c(5,6,10),]

epochLow <- Epoch(lowPassTs)
visuIEEGData(epochLow)

hfoBand<-c(80, 250)

rangeBand<-hfoBand
powTimeWindow<-c(-10,20)
baseTimeWindow<-c(-30,-20)

# compute the mean power analysis over the frequency band (rangeBand) over time window (powTimeWindow) and baselined time window (baseTimeWindow)
hfoBandPow<-meanPowBaselineBand( epoch=epoch, fs=fs, windowParams=windowParams, rangeBand=hfoBand, powTimeWindow=powTimeWindow, baseTimeWindow=baseTimeWindow)


plotPowBand<-plotPowHeatmap(pow=hfoBandPow,sozIndex=sozIndex)
plotPowBand

hfoPow<-hfoBandPow$pow
electrodes<-hfoBandPow$electrodes
startTimes<-hfoBandPow$startTimes

rownames(hfoPow)<-electrodes
colnames(hfoPow)<-startTimes

pt01HFO<-analyze_hfoPow(hfoPow)


