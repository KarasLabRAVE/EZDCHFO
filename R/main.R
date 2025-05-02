standardizeIEEG <- function(data) {
  scaling <- 10^floor(log10(max(data)))
  plotData <- data / scaling
}

#' compute DC shift analysis
#'
#' @param epoch Matrix or Epoch object. iEEG data matrix or Epoch object. If matrix, the row names are the electrode names and the column names are the time points
#' @param fs Numeric. frequency of signal iEEG acquisition
#'
#' @return A DC shift analysis object
#' @export
#'
#' @examples
#' data("pt01EcoG")
#' ts<-pt01EcoG[,20001:50001]
#'fs=1000
#'sozIndex <- attr(pt01EcoG, "sozIndex")
#'windowParams<-c(0.25,0.1)
#'epoch <- Epoch(ts)
#'visuIEEGData(epoch)
analyze_DCShift <- function(epoch, fs=1000){


  elecNum <- nrow(epoch)
  timeNum <- ncol(epoch)

  # DC shift analysis

  sige   <- vector(mode="numeric", length=timeNum)

  lowPassTs=epoch$data

  # band pass filter DC
  fpass <- 1 # filter everything above 1Hz
  wpass <- fpass / (fs / 2)
  but <- signal::butter(5, wpass, "low")


  for(ie in 1:elecNum){
    #ie<-1
    sige=epoch$data[ie,]
    sigedc=signal::filter(but,sige)
    lowPassTs[ie,1:timeNum]<-sigedc

  }

  rownames(lowPassTs)<-rownames(ts)
  colnames(lowPassTs)<-colnames(ts)

  epochLow <- Epoch(lowPassTs)
  visuIEEGData(epochLow)


  lt2=2*fs

  maxelec=max(abs(lowPassTs))

  prCentThreshold=0.5
  thresholdn=-1.0*maxelec*prCentThreshold
  thresholdp=maxelec*prCentThreshold

  thresholdEndStart<-0.1*maxelec*0.01

  testDce   <-vector(mode="numeric", length=elecNum)
  lengthDce <-vector(mode="numeric", length=elecNum)
  maxDce    <-vector(mode="numeric", length=elecNum)
  startDce  <-vector(mode="numeric", length=elecNum)

  startDce[1:elecNum]=NaN
  lt3= 3*fs


  for(ie in 1:elecNum){

    sigedc=lowPassTs[ie,]
    testdc=0
    lengthdc=0
    startdc=NaN
    maxdc=0

    seqdcn<-which(sigedc<thresholdn)
    seqdcp<-which(sigedc>thresholdp)

    seqnp<-0
    if(length(seqdcn)>0) seqnp<-seqnp+1
    if(length(seqdcp)>0) seqnp<-seqnp+2

    if(seqnp==3){
      if(seqdcp[1]<seqdcn[1]){
        seqdc<-union(seqdcp,seqdcn)
      }else{
        seqdc<-union(seqdcn,seqdcp)
      }
    }else if(seqnp==1){
      seqdc=seqdcn
    }else if(seqnp==2){
      seqdc=seqdcp
    }

    if(seqnp>0){
      if(length(seqdc)>3*fs){

        startdc<-seqdc[1]
        lengthdc<-seqdc[length(seqdc)]-seqdc[1]+1
        enddc<-seqdc[length(seqdc)]
        maxdc<-max(abs(sigedc))

        alarm=0
        k=startdc
        while(alarm==0){
          k=k-1
          if(abs(sigedc[k])<thresholdEndStart){
            alarm=1
          }
          #pritimeNum(k)
        }
        startdc=k
        alarm=0
        k=enddc
        while((alarm==0)&(k<timeNum)){
          k=k+1
          if(abs(sigedc[k])<thresholdEndStart){
            alarm=1
          }
          #print(k)
        }
        enddc=k
        lengthdc<-enddc-startdc
        testdc=1
        maxdc=max(abs(sigedc[startdc:enddc]))
      }

      testDce[ie]=testdc
      lengthDce[ie]=lengthdc
      startDce[ie]=startdc
      maxDce[ie]=maxdc
    }
  }

  DCShift(
    lowPassTs=lowPassTs,
    testDce = testDce,
    lengthDce = lengthDce,
    startDce = startDce,
    maxDce = maxDce
  )


}

#' compute HFO analysis
#'
#' @param hfoPow Matrix of mean HFO power. the row names are the electrode names and the column names are the time points
#'
#' @return A HFO power object analysis
#' @export
#'
#' @examples
#' data("pt01EcoG")
analyze_hfoPow <- function(hfoPow){

  maxHfoPow=max(hfoPow)
  threshold=maxHfoPow*0.1
  elecNum<-nrow(hfoPow)

  testHfo<-vector(mode="numeric", length=elecNum)
  lengthHfo<-vector(mode="numeric", length=elecNum)
  maxHfo<-vector(mode="numeric", length=elecNum)
  startHfo<-vector(mode="numeric", length=elecNum)

  startHfo[1:elecNum]=NaN

  fsHfomap=ncol(hfoBandPow$pow)/(powTimeWindow[2]-powTimeWindow[1])
  lthfo=1.5*fsHfomap


  for(ie in 1:elecNum){
  #ie<-5
    sige=hfoBandPow$pow[ie,]


    seq<-which(sige>threshold)

    resulthfo<- rle(diff(seq))
    hfolengthp<-max(resulthfo$length)

    if(hfolengthp>lthfo){
      testHfo[ie]=1
      lengthHfo[ie]=hfolengthp
      maxHfo[ie]=max(sige)
      startHfo[ie]=seq[1]
    }

  }

  HFOMetrics(
    hfoPow=hfoPow,
    testHfo = testHfo,
    lengthHfo = lengthHfo,
    startHfo = startHfo,
    maxHfo = maxHfo
  )

}
