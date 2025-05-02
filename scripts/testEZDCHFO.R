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

lowPassTs<-pt01DCShift@lowPassTs

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

#
# maxHfoPow=max(hfoBandPow$pow)
# threshold=maxHfoPow*0.1
# elecNum<-nrow(ts)
#
#
# testHfo<-vector(mode="numeric", length=elecNum)
# lengthHfo<-vector(mode="numeric", length=elecNum)
# maxHfo<-vector(mode="numeric", length=elecNum)
# startHfo<-vector(mode="numeric", length=elecNum)
#
# startHfo[1:elecNum]=NaN
#
# fsHfomap=ncol(hfoBandPow$pow)/(powTimeWindow[2]-powTimeWindow[1])
# lthfo=1.5*fsHfomap
#
#
# for(ie in 1:elecNum){
# #ie<-5
#   sige=hfoBandPow$pow[ie,]
#
#
#   seq<-which(sige>threshold)
#
#   resulthfo<- rle(diff(seq))
#   hfolengthp<-max(resulthfo$length)
#
#   if(hfolengthp>lthfo){
#     testHfo[ie]=1
#     lengthHfo[ie]=hfolengthp
#     maxHfo[ie]=max(sige)
#     startHfo[ie]=seq[1]
#   }
#
# }
#
# pt01HFO<-analyze_HFO(hfoBandPow$pow)
