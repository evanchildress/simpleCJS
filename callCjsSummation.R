library(getWBData)
library(data.table)
library(jagsUI)

coreData<-createCoreData(sampleType="electrofishing") %>% 
  addTagProperties() %>%
  dplyr::filter(species=="bkt"&river=="wb obear") %>%
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

jagsData <- createJagsData(data.frame(coreData))

coreData<-data.table(coreData)

flowData<-tbl(conDplyr,"data_daily_discharge") %>%
          filter(river=="wb obear") %>%
          collect() %>%
          data.table() %>%
          .[date>=as.Date(min(coreData$detectionDate))&
            date<=as.Date(max(coreData$detectionDate))] %>%
          .[,discharge:=log(discharge)]


# tempData<-tbl(conDplyr,"data_hourly_temperature") %>%
#   # filter(river=="wb obear") %>%
#   collect() %>%
#   data.table() %>%
load("C:/Users/Evan/Desktop/Conte/perform/data/wbTemps.rData")
tempData<-temp %>%
  .[datetime>=min(coreData$detectionDate)&
    datetime<=max(coreData$detectionDate)] %>%
  .[,.(temperature=max(temperature)),by=.(date=as.Date(datetime))]

if(exists("temp")){rm(temp)}

coreData[,time:=which(as.Date(detectionDate)==tempData$date),by=detectionDate]

jagsData$stageDATA<-as.numeric(coreData$ageInSamples>3)+1
jagsData$flowForP<-scale(coreData$flowForP)[,1]
jagsData$z[jagsData$z==2]<-0
jagsData$lengthDATA<-coreData %>% 
                      group_by(river) %>%
                      transmute(length=scale(observedLength)[,1]) %>%
                      ungroup() %>%
                      data.table() %>%
                      .[,length]
jagsData$tempDATA<-tempData$temperature
jagsData$flowDATA<-flowData$discharge
jagsData$time<-coreData$time
jagsData$nTimes<-nrow(tempData)


stds<-list(length=coreData %>% 
                  group_by(river) %>%
                  summarize(meanLength=mean(observedLength,na.rm=T),
                            sdLength=sd(observedLength,na.rm=T)),
           flowForP=coreData %>%
                    summarize(meanFlow=mean(flowForP),
                              sdFlow=sd(flowForP)),
           flow=flowData %>% 
                summarize(meanFlow=mean(discharge),
                          sdFlow=sd(discharge)),
           temp=tempData %>%
                summarize(meanTemp=mean(temperature),
                          sdTemp=sd(temperature)))

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
    parallel=F)
done <- Sys.time() 
print(done - beforeJags)
