library(getWBData)
library(data.table)
library(jagsUI)
library(reshape2)

coreData<-createCoreData(sampleType="electrofishing") %>% 
  addTagProperties() %>%
  dplyr::filter(species=="bkt") %>%
  createCmrData() %>%
  fillSizeLocation() %>%
  addSampleProperties() %>%
  addEnvironmental(sampleFlow=T) %>%
  addKnownZ()

jagsData <- createJagsData(coreData)

coreData<-data.table(coreData)

# flowData<-tbl(conDplyr,"data_flow_extension") %>%
#           # filter(river=="wb obear") %>%
#           collect() %>%
#           data.table() %>%
#           .[date>=min(coreData$detectionDate)&
#             date<=max(coreData$detectionDate)] %>%
#           .[,discharge:=log(qPredicted+0.08)] %>%
#           .[,.(date,river,discharge)] %>%
#           melt(id.vars=c("date","river")) %>%
#           acast(date~river)
# 
# flowData<-cbind(flowData[,1],flowData[,1],flowData[,1],flowData[,1])

flowData<-tbl(conDplyr,"data_daily_discharge") %>%
  # filter(river=="wb obear") %>%
  collect() %>%
  data.table() %>%
  .[date>=as.Date(min(coreData$detectionDate))&
      date<=as.Date(max(coreData$detectionDate))] %>%
  .[,discharge:=log(discharge)] %>%
  .[,.(date,river,discharge)] %>%
  melt(id.vars=c("date","river")) %>%
  acast(date~river)


tempData<-tbl(conDplyr,"data_daily_temperature") %>%
  # filter(river=="wb obear") %>%
  collect() %>%
  data.table() %>%
  .[date>=min(coreData$detectionDate)&
    date<=max(coreData$detectionDate)] %>%
  .[,.(date=as.Date(date),river,temperature=daily_max_temp)]
  
time<-tempData[river=="west brook",date] 

tempData<-tempData  %>%
  .[,.(date,river,temperature)] %>%
  melt(id.vars=c("date","river")) %>%
  acast(date~river)

coreData[,time:=which(as.Date(detectionDate)==time),by=detectionDate]

scale2<-function(x){
  return(scale(x)[,1])
}

coreData[,nTimes:=c(NA,diff(time)+1),by=tag]

jagsData$stageDATA<-as.numeric(coreData$ageInSamples>3)+1
jagsData$flowForP<-scale(coreData$flowForP)[,1]
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

timesByRow<-array(NA,dim=c(nrow(coreData),max(coreData$nTimes,na.rm=T)))
for(i in 1:nrow(coreData)){
  if(is.na(coreData$nTimes[i])){next}
  timesByRow[i,1:coreData$nTimes[i]]<-coreData$time[i-1]:coreData$time[i]
}
jagsData$timesByRow<-timesByRow

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
nb <- 4000
ni <- 5000
nt <- 1
nc <- 3

varsToMonitor<-c('pBeta','phiBeta')

gc()

beforeJags<-Sys.time()
  out <- jags(
    data=jagsData,
    inits=inits,
    model = "CjsProcessSummation.R",
    parameters.to.save = varsToMonitor,
    n.chains=nc,
    n.iter = ni,
    n.thin = nt,
    n.burnin=nb,
    parallel=T)
done <- Sys.time() 
print(done - beforeJags)
