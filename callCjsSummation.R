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

# ##Need to fix the addKnownZ function so the sample numbers from the other drainage don't f it up
# coreData[coreData$knownZ==2&coreData$enc==1,"knownZ"]<-1
# coreData<-coreData %>%
#   group_by(tag) %>%
#   mutate(last1=max(detectionDate)) %>%
#   ungroup()
# 
# coreData[!is.na(coreData$knownZ)&coreData$detectionDate<coreData$last1&coreData$knownZ==2,"knownZ"]<-1
# coreData[is.na(coreData$knownZ)&coreData$detectionDate<coreData$last1,"knownZ"]<-1
# # end temporary fix for addKnownZ problem

jagsData <- createJagsData(coreData)

coreData<-data.table(coreData)

flowData<-tbl(conDplyr,"data_flow_extension") %>%
          # filter(river=="wb obear") %>%
          collect() %>%
          data.table() %>%
          .[date>=min(coreData$detectionDate)&
            date<=max(coreData$detectionDate)] %>%
          .[,discharge:=log(qPredicted+0.08)] %>%
          .[,.(date,river,discharge)] %>%
          melt(id.vars=c("date","river")) %>%
          acast(date~river)

flowData<-cbind(flowData[,1],flowData[,1],flowData[,1],flowData[,1])

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
