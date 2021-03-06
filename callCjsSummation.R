library(getWBData)
library(reshape2)
library(data.table)
library(jagsUI)

coreData<-createCoreData(sampleType="electrofishing") %>% 
  addTagProperties() %>%
  dplyr::filter(species=="bkt") %>%
  createCmrData(dateStart = as.POSIXct("2002-01-01"),censorDead=T) %>%
  fillSizeLocation() %>%
  addSampleProperties() %>%
  addEnvironmental(sampleFlow=T) %>%
  addKnownZ()

jagsData <- createJagsData(data.frame(coreData))

coreData<-data.table(coreData)

tempData<-tbl(conDplyr,"data_hourly_temperature") %>%
  # filter(river=="wb obear") %>%
  collect(n=Inf) %>%
  data.table() %>%
  .[datetime>=min(coreData$detectionDate)&
      datetime<=max(coreData$detectionDate)] %>%
  .[,.(temperature=max(temperature)),by=.(date=as.Date(datetime),
                                          river)] %>%
  setkey(river,date)
# 
# load("C:/Users/Evan/Desktop/Conte/perform/data/wbTemps.rData")
# tempData<-temp %>%
#   .[datetime>=min(coreData$detectionDate)&
#     datetime<=max(coreData$detectionDate)] %>%
#   .[,.(temperature=max(temperature)),by=.(date=as.Date(datetime),
#                                           river)] %>%
#   setkey(river,date)

# load("C:/Users/Evan/Desktop/Conte/perform/data/wbTemps.rData")
# tempData<-temp %>%
#   .[datetime>=min(coreData$detectionDate)&
#     datetime<=max(coreData$detectionDate)] %>%
#   .[,.(temperature=max(temperature)),by=.(date=as.Date(datetime),
#                                           river)] %>%
#   setkey(river,date)

if(exists("temp")){rm(temp)}
time<-tempData[river=="west brook",date] 


coreData[,time:=which(as.Date(detectionDate)==time),by=detectionDate]

flowData<-tbl(conDplyr,"data_daily_discharge") %>%
  collect(n=Inf) %>%
  data.table() %>%
  .[date>=as.Date(min(coreData$detectionDate))&
      date<=as.Date(max(coreData$detectionDate))] %>%
  .[,discharge:=log(discharge)] %>%
  .[,.(date,river,discharge)] %>%
  .[,river:=as.numeric(factor(river,
                                 levels=c("west brook",
                                          "wb jimmy",
                                          "wb mitchell",
                                          "wb obear"),
                                 ordered=T))] %>%
  melt(id.vars=c("date","river")) %>%
  acast(date~river)

# flowData<-tbl(conDplyr,"data_flow_extension") %>%
#   collect(n=Inf) %>%
#   data.table() %>%
#   .[date>=min(coreData$detectionDate)&
#       date<=max(coreData$detectionDate)] %>%
#   .[,discharge:=log(qPredicted+0.74)] %>%
#   .[,.(date=as.Date(date),river,discharge)]
# 
# flowData<-rbind(flowData,flowData,flowData,flowData) %>%
#   .[,river:=rep(c("west brook","wb jimmy","wb mitchell","wb obear"),each=nrow(flowData))] %>%
#   .[,river:=as.numeric(factor(river,
#                               levels=c("west brook",
#                                        "wb jimmy",
#                                        "wb mitchell",
#                                        "wb obear"),
#                               ordered=T))] %>%
#   melt(id.vars=c("date","river")) %>%
#   acast(date~river)

# 
# tempData<-tbl(conDplyr,"data_daily_temperature") %>%
#   # filter(river=="wb obear") %>%
#   collect(n=Inf) %>%
#   data.table() %>%
#   .[date>=min(coreData$detectionDate)&
#     date<=max(coreData$detectionDate)] %>%
#   .[,.(date=as.Date(date),river,temperature=daily_max_temp)]

tempData<-tempData  %>%
  .[,.(date,river,temperature)] %>%
  .[,river:=as.numeric(factor(river,
                              levels=c("west brook",
                                       "wb jimmy",
                                       "wb mitchell",
                                       "wb obear"),
                              ordered=T))] %>%
  melt(id.vars=c("date","river")) %>%
  acast(date~river)

scale2<-function(x){
  return(scale(x)[,1])
}

coreData[,nTimes:=c(NA,diff(time)+1),by=tag]
coreData[,flowForP:=scale(log(flowForP+0.08))[,1],by=river]


jagsData$stageDATA<-as.numeric(coreData$ageInSamples>3)+1
jagsData$flowForP<-coreData$flowForP
jagsData$z[jagsData$z==2]<-0
jagsData$lengthDATA<-coreData %>% 
                      group_by(river) %>%
                      transmute(length=scale(observedLength)[,1]) %>%
                      ungroup() %>%
                      data.table() %>%
                      .[,length]
jagsData$tempDATA<-apply(tempData,2,scale2)
jagsData$flowDATA<-apply(flowData,2,scale2)
jagsData$time<-coreData$time
jagsData$nTimes<-length(time)
jagsData$nTimesByRow<-coreData$nTimes
jagsData$sample<-coreData$sampleIndex
jagsData$nSamples<-max(coreData$sampleIndex)

# timesByRow<-array(NA,dim=c(nrow(coreData),max(coreData$nTimes,na.rm=T)))
# for(i in 1:nrow(coreData)){
#   if(is.na(coreData$nTimes[i])){next}
#   timesByRow[i,1:coreData$nTimes[i]]<-coreData$time[i-1]:coreData$time[i]
# }
# jagsData$timesByRow<-timesByRow

stds<-list(length=coreData %>% 
             group_by(river) %>%
             summarize(meanLength=mean(observedLength,na.rm=T),
                       sdLength=sd(observedLength,na.rm=T)),
           flowForP=coreData %>%
             summarize(meanFlow=mean(flowForP),
                       sdFlow=sd(flowForP)),
           flow=list(meanFlow=apply(flowData,2,mean),
                     sdFlow=apply(flowData,2,sd)),
           temp=list(meanTemp=apply(tempData,2,mean),
                     sdTemp=apply(tempData,2,sd)))

saveRDS(stds,"results/summationStandards.rds")

zInit<-jagsData$z+1
zInit[is.na(zInit)]<-0
zInit[jagsData$firstObsRows]<-NA
zInit[zInit %in% c(1,2)]<-NA
inits<- function(){
  list(z = zInit)      
}



# MCMC settings
na <- 500
nb <- 5000
ni <- 8000
nt <- 3
nc <- 3

varsToMonitor<-c('pBeta','phiBeta','phiSigma','phiEps')

gc()
Sys.time()

  out <- jags(
    data=jagsData,
    inits=inits,
    model = "CjsProcessSummationError.R",
    parameters.to.save = varsToMonitor,
    n.adapt=na,
    n.chains=nc,
    n.iter = ni,
    n.thin = nt,
    n.burnin=nb,
    parallel=T,
    codaOnly=c("pEps"))

saveRDS(out,"processSummationOut.rds")
