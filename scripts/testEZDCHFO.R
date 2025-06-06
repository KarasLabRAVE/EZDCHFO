# test EZEI  with Epoch package
# data download one patient one seizure
dl <- EpochDownloader()
pt01sz1<-dl$Retrostudy_subpt01_1

#pt01sz1ieeg<-tblData(pt01sz1)

# crop
pt01sz1m10p20s<-crop(pt01sz1, start=-10, end=20)
sozIndex<-which(pt01sz1m10p20s@rowData$soz==TRUE)

display <- c(sozIndex, 75:82)
# subset on 14 electrodes
pt01sz1m10p20s14e<-pt01sz1m10p20s[display,]
pt01sz1_14e<-pt01sz1[display,]
sozIndex<-which(pt01sz1m10p20s14e@rowData$soz==TRUE)

visuIEEGData(epoch=pt01sz1m10p20s14e)

fs<-1000
thresholdDc<-0.5
lengthDc<-3
thresholdEndStartDc<-0.001

pt01DCShift<-analyze_DCShift(epoch=pt01sz1m10p20s14e)

pt01LowPass<-pt01DCShift@lowPassTs

epochLowPass <- pt01sz1m10p20s14e

tblData(epochLowPass)<-pt01LowPass

visuIEEGData(epoch=epochLowPass)


fs=1000
windowParams<-c(0.25,0.1)

hfoBand<-c(80, 250)

rangeBand<-hfoBand
powTimeWindow<-c(-10,20)
baseTimeWindow<-c(-30,-20)

# compute the mean power analysis over the frequency band (rangeBand) over time window (powTimeWindow) and baselined time window (baseTimeWindow)
hfoBandPow<-meanPowBaselineBand( epoch=pt01sz1_14e, fs=fs, windowParams=windowParams, rangeBand=hfoBand, powTimeWindow=powTimeWindow, baseTimeWindow=baseTimeWindow)


plotPowBand<-plotPowHeatmap(pow=hfoBandPow,sozIndex=sozIndex)
plotPowBand


hfoPow<-hfoBandPow$pow
electrodes<-hfoBandPow$electrodes
startTimes<-hfoBandPow$startTimes

rownames(hfoPow)<-electrodes
colnames(hfoPow)<-startTimes

pt01HFO<-analyze_hfoPow(hfoPow)

print(pt01DCShift$testDce)
print(pt01HFO$testHfo)

testDce<-pt01DCShift$testDce
testHfo<-pt01HFO$testHfo

indexCooc<-coocurrenceDCHFO(testDce,testHfo)
