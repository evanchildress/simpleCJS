
library(plyr)
library(rjags)
library(ggplot2)
library(abind)
rjags::load.module("dic")


dMData$length[dMData$tagNumberCH=='1BF1FF6207' & dMData$season == 3 & dMData$year == 2005] <- NA
dMData$length[dMData$tagNumberCH=='1BF1FF6521' & dMData$season == 3 & dMData$year == 2005] <- NA
dMData$length[dMData$tagNumberCH=='1BF18CE7ED' & dMData$season == 2 & dMData$year == 2006] <- NA
dMData$length[dMData$tagNumberCH=='1BF20FF1B9' & dMData$season == 3 & dMData$year == 2005] <- NA
dMData$length[dMData$tagNumberCH=='257C67CA48' ] <- NA
dMData$length[dMData$tagNumberCH=='1BF20EB7A4' & dMData$season == 4 & dMData$year == 2008] <- NA



#dMData$riverOrdered <- factor(dMData$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)

# means for standardizing 
#####################################################################  
# stdBySeasonRiver <- ddply( dMData, .(riverOrdered,riverN,season), summarise,   
#  lengthMean=mean(length, na.rm=TRUE),                       
#  lengthSd=sd(length, na.rm=TRUE),
#  lengthLo = quantile(length,c(0.025), na.rm=TRUE),
#  lengthHi = quantile(length,c(0.975), na.rm=TRUE),
#  tempMean=mean(fullMeanT, na.rm=TRUE),
#  tempMeanP=mean(temperatureForP, na.rm=TRUE), 
#  tempSd=sd(fullMeanT, na.rm=TRUE),
#  tempSdP=sd(temperatureForP, na.rm=TRUE),
#  tempLo = quantile(fullMeanT,c(0.025), na.rm=TRUE),
#  tempHi = quantile(fullMeanT,c(0.975), na.rm=TRUE),
#  flowMean=mean(fullMeanD, na.rm=TRUE), 
#  flowSd=sd(fullMeanD, na.rm=TRUE),
#  dischMeanP=mean(dischargeForP,na.rm=T),
#  dischSdP=sd(dischargeForP,na.rm=T),
#  flowLo = quantile(fullMeanD,c(0.025), na.rm=TRUE),
#  flowHi = quantile(fullMeanD,c(0.975), na.rm=TRUE) )
# ############# To get rid of NA Rivers
# stdBySeasonRiver<-stdBySeasonRiver[!is.na(stdBySeasonRiver$riverN),]
# 
# #####################################################################  
# stdBySeason <- ddply( dMData, .(season), summarise,   
#  lengthMean=mean(length, na.rm=TRUE), 
#  lengthSd=sd(length, na.rm=TRUE),
#  lengthLo = quantile(length,c(0.025), na.rm=TRUE),
#  lengthHi = quantile(length,c(0.975), na.rm=TRUE),
#  tempMean=mean(fullMeanT, na.rm=TRUE),
#  tempMeanP=mean(temperatureForP, na.rm=TRUE), 
#  tempSd=sd(fullMeanT, na.rm=TRUE),
#  tempSdP=sd(temperatureForP, na.rm=TRUE),
#  tempLo = quantile(fullMeanT,c(0.025), na.rm=TRUE),
#  tempHi = quantile(fullMeanT,c(0.975), na.rm=TRUE),
#  flowMean=mean(fullMeanD, na.rm=TRUE), 
#  flowSd=sd(fullMeanD, na.rm=TRUE),
#  dischMeanP=mean(dischargeForP,na.rm=T),
#  dischSdP=sd(dischargeForP,na.rm=T),
#  flowLo = quantile(fullMeanD,c(0.025), na.rm=TRUE),
#  flowHi = quantile(fullMeanD,c(0.975), na.rm=TRUE) ) 
# 
# # standardize by river  - for age0 fall lengths
# stdByRiver <- ddply( dMData, .(riverOrdered,riverN), summarise,
# lengthSd0 = sd(subset( length, age == 0 & season == 3 ), na.rm=TRUE),
# lengthMean0 = mean(subset( length, age == 0 & season == 3 ), na.rm=TRUE) )
# 
# stdByRiver <- stdByRiver[!is.na(stdByRiver$riverN),]
# stdByRiver$river <- as.numeric(stdByRiver$riverOrdered)

#stdBySeasonRiver<-rbind(stdBySeasonRiver,c('zRiv1','0',rep(NA,ncol(stdBySeasonRiver)-2)))     

#####
# # fdDATA is flood and drought frequencies and durations
# fdDATA$year <- as.numeric( fdDATA$year )
# fdDATA$year2 <- fdDATA$year
# fdDATA$year <- fdDATA$year-min(fdDATA$year) + 1
# 
# floodDur <- matrix(0,max(fdDATA$season),max(fdDATA$year))
# droughtDur <- matrix(0,max(fdDATA$season),max(fdDATA$year))
# floodFreq <- matrix(0,max(fdDATA$season),max(fdDATA$year))
# for ( i in 1:nrow(fdDATA) ){
#   floodDur[fdDATA$season[i],fdDATA$year[i]] <- fdDATA$floodDur[i]
#   droughtDur[fdDATA$season[i],fdDATA$year[i]] <- fdDATA$droughtDur[i]
#   floodFreq[fdDATA$season[i],fdDATA$year[i]] <- fdDATA$floodFreq[i]
#   
# }
#####


# function to add dummy rows and columns for zRiv=1
addRowColMeans <- function(m){
  m <- cbind( rowMeans(m),m )
  m <- rbind( colMeans(m),m )
  return ( m )  
}
# function to add dummy columns for zRiv=1
addColMeans <- function(m){
  m <- cbind( rowMeans(m),m )
  return ( m )  
}



# tempForN<- array(NA,dim=c(4,5,max(dMData$year-min(dMData$year) + 1)))
# for(s in 1:4){            
#   for(y in 1:max(dMData$year-min(dMData$year) + 1)){
#       tempForN[s,1,y]<-(stdBySeason$tempMean[s]- stdBySeason$tempMean[s] ) / stdBySeason$tempSd[ s ]
#     for(r in 1:4){
#       tempForN[s,r+1,y]<-(mean(dMData$fullMeanT[dMData$season==s&as.numeric(dMData$riverOrdered)==r&(dMData$year-min(dMData$year) + 1)==y],na.rm=T)
#                           - stdBySeason$tempMean[ s] ) / stdBySeason$tempSd[ s ]
#       if(tempForN[s,r+1,y]=='NaN')tempForN[s,r+1,y]<-(stdBySeason$tempMean[s]- stdBySeason$tempMean[ s] ) / stdBySeason$tempSd[ s ]
#     }
#   }
# }

# flowForN<- array(NA,dim=c(4,5,max(dMData$year-min(dMData$year) + 1)))
# for(s in 1:4){            
#   for(y in 1:max(dMData$year-min(dMData$year) + 1)){
#       flowForN[s,1,y]<-(stdBySeason$flowMean[s]- stdBySeason$flowMean[s] ) / stdBySeason$flowSd[s]
#     for(r in 1:4){
#       flowForN[s,r+1,y]<-(mean(dMData$fullMeanD[dMData$season==s&as.numeric(dMData$riverOrdered)==r&(dMData$year-min(dMData$year) + 1)==y],na.rm=T)
#                           - stdBySeason$flowMean[s] ) / stdBySeason$flowSd[s]
#       if(flowForN[s,r+1,y]=='NaN')flowForN[s,r+1,y]<-(stdBySeason$flowMean[s]- stdBySeason$flowMean[s] ) / stdBySeason$flowSd[s]
#     }
#   }
# }


knownZ<-function(sN, first, last){#, river){
  z.iv <- array(NA, dim=length(first))
  z.iv[(sN>first)&(sN<=(last))] <- 1
  return(z.iv)
}

fillRiver<-function(river){
  known<-which(!is.na(river))
  unknown<-which(is.na(river))
  nKnown<-length(unique(river[known]))
  if(nKnown==1){river[unknown]<-river[known[1]]}else{
    for(i in unknown){
      river[i]<-river[known[max(which(i>known))]]
    }
  }
  return(river)
}

############ Predictors that are in a matrix have season in rows and river in columns
d <- within(
  data = list(),
  expr = {
    
    encDATA = as.numeric(dMData$enc) #$msEnc
    riverDATA = dMData[,fillRiver(riverN),by=tag]$V1 #-3
    nRivers = length(unique(dMData$riverN))-1 #may need to add one for unobs
    #lengthDATA = dMData$length
    #availableDATA = dMData$available01
    #ind = as.numeric(factor(dMData$tag))
    # For standardizing length
    #   lengthMean = addColMeans( matrix(stdBySeasonRiver$lengthMean,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )
    #   lengthSd =   addColMeans( matrix(stdBySeasonRiver$lengthSd,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )  
    #   
    #   lengthMean0 = stdByRiver$lengthMean0
    #   lengthSd0 = stdByRiver$lengthSd0
    # environmental covariates pertaining to intervals.  These are
    # covariates of growth and survival
    
    # For standardizing env predictors of growth and surv
    #   tempMean = addColMeans( matrix(stdBySeasonRiver$tempMean,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )
    #   tempSd =   addColMeans( matrix(stdBySeasonRiver$tempSd,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )  
    #   flowMean = addColMeans( matrix(stdBySeasonRiver$flowMean,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )
    #   flowSd =   addColMeans( matrix(stdBySeasonRiver$flowSd,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )  
    
    ## Predictors of phi for correcting N1 where countForN ==0
    #   tempForN = tempForN
    #   flowForN = flowForN
    
    # not standardizing by season,river because on NAs in river
    #   tempDATA = ( as.numeric(dMData$fullMeanT) - stdBySeason$tempMean[ as.numeric(dMData$season)] ) / stdBySeason$tempSd[ as.numeric(dMData$season) ]
    #   flowDATA = ( as.numeric(dMData$fullMeanD) - stdBySeason$flowMean[ as.numeric(dMData$season)] ) / stdBySeason$flowSd[ as.numeric(dMData$season) ]  
    
    # emPermNA, used to censor likelihood for permanent emigrants
    # 1 on line before last observation with subsequent bottom of the study site antenna hit. 0's before and after if em, NAs otherwise
    # trying emPerm without the NAs 
    #emPermDATA = dMData$emPerm 
    
    #intervalDays = as.numeric(dMData$fullMeanIntLen )
    # Environmental covariates for p 
    #flowP = as.numeric(dMData$dischargeForP)
    #temperatureP = as.numeric(dMData$temperatureForP)
    #For standardizing env predictors of p
    #   flowMeanP = addRowColMeans( matrix(stdBySeasonRiver$dischMeanP,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )
    #   flowSdP = addRowColMeans( matrix(stdBySeasonRiver$dischSdP,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )
    #   tempMeanP = addRowColMeans( matrix(stdBySeasonRiver$tempMeanP,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )
    #   tempSdP = addRowColMeans( matrix(stdBySeasonRiver$tempSdP,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )
    
    # , growthSd = sd(((dMData$lagLength - dMData$length)/(as.numeric(dMData$intervalLength)))*365/4, na.rm=TRUE)
    ######## NEVER!!!! #########  gr = (dMData$lagLength - dMData$length)/(as.numeric(dMData$intervalLength))
    # indexing of the input and state vectors
    year = dMData$year-min(dMData$year) + 1
    nYears = max(dMData$year)-min(dMData$year)+1
    season = as.numeric(as.character(dMData$season)) 
    nAllRows = nrow(dMData)
    nFirstObsRows = evalList$nFirstObsRows
    firstObsRows = evalList$firstObsRows
    #nOcc = length(unique(dMData$sampleNum))
    #occ = dMData$sampleNum-min(dMData$sampleNum)-1
    nEvalRows = evalList$nEvalRows # rows that will matter if we start using JS, and 
    evalRows = evalList$evalRows   # that matter now for the growth model
    z = dMData[,knownZ(sampleNum,first,last)]
    #lastPossibleRows = subset( 1:nrow(dMData),dMData$lastAIS==dMData$ageInSamples ) # need to put this in makedMData
    #nLastPossibleRows = evalList$nFirstObsRows
    
    #lastObsRows = evalList$lastObsRows
    #nLastObsRows = evalList$nLastObsRows
    
    #lastRows = lastPossibleRows
    #nLastRows = nLastPossibleRows
    
    #nOut = evalList$nEvalRows # evalRows to output for each trace
    
    #create variables that hold information on counts - data held in statsForN (made in makeDMData.R - based on pheno2Long, so has all cohorts. need to throw away years before dMData's first cohort)
    #minYear <- min(dMData$year)
    #firstYearIndex <- minYear-statsForN$minYear + 1
    # countForN has dummy river 1 in it
    #countForN <- statsForN$countForN[,firstYearIndex:dim(statsForN$countForN)[2],]
    
    #meanForN <- statsForN$meanForN
    #sdForN <- statsForN$sdForN
    
    #  dMDataF <- dMData[ dMData$first == dMData$sampleNum, ]
    #  nTagged1 <- table(dMDataF$season,dMDataF$year,dMDataF$riverOrdered)
    
    #Fill in random #s for zRiv=1
    #  nTagged <- abind(matrix(round(runif(4*nYears,10,50)), nrow=4,ncol=nYears),nTagged1)
    #   floodDurDATA <- floodDur
    #   droughtDurDATA <- droughtDur
    #   floodFreqDATA <- floodFreq
    
  }
)


# function to make initial z matrix, with 1s when known alive and NAs otherwise

zInit<-d$z
zInit[is.na(zInit)]<-0
zInit[d$firstObsRows]<-NA
zInit[zInit==1]<-NA

emPermInit <- function(e){
  eOut <- array(NA, dim=length(e))
  eOut <- ifelse( is.na(e), 0, e )
  return(eOut)
}

encInitMS<-function(sN, first, last, river){
  for (i in 1:(length(first))){
    river[i] <- river[i] - 0
    if ((sN[i] >= first[i]) & (sN[i] <= (last[i]))) {
      if( is.na(river[i]) ) river[i] <- river[i-1]  
    }
    else river[i] <- NA
  }  
  return(river + 1)
}

inits<- function(){
  list(phiBeta = array(0,dim=c(4,d$nYears,d$nRivers+1)),
       pBeta = array(0,dim=c(4,d$nYears,d$nRivers+1))
       #psiBeta = array(0, dim=c(4,d$nRivers,d$nRivers)),
       #size = dMData$length[evalList$firstObsRows]
       #z = zInit,
       #censored = emPermInit( d$emPermDATA )
       #zRiv = as.numeric(encInitMS(dMData$sampleNum,dMData$first,
       #                    dMData$last,dMData$riverN))
  )      
}



# MCMC settings
na <- 500
nb <- 2000
ni <- 5000
nt <- 5
nc <- 3

varsToMonitor<-c(
  
  
  'pBeta'
  
  , 'phiBeta'
  
  #  , 'grSigma' 
  #  , 'grBeta'
  
)

rm(dMData)
rm(evalList)
gc()

# 
(beforeAdapt <- Sys.time())
# print( beforeAdapt )
# adaptedModel<- jags.model(
#   file = bugsName,  
#   data = d,
#   inits = inits,
#   n.chains = nc,
#   n.adapt = na,             
# )
(afterAdapt <- Sys.time())
# afterAdapt - beforeAdapt
( beforeJags <- Sys.time() )
print( beforeJags )

out <- jags(
  data=d,
  inits=inits,
  model = "verySimpleCJS.txt",
  parameters.to.save = varsToMonitor,
  n.chains=nc,
  n.iter = ni,
  n.thin = nt,
  n.burnin=nb)

# out1=out  ## for running a second set of iters



# out <- jags.samples(
#   model = adaptedModel,
#   variable.names = varsToMonitor,
#   n.iter = ni,
#   thin = nt,
#   progress.bar = 'text'
# ) 

( done <- Sys.time() )
# 
print(afterAdapt - beforeAdapt) 
print(done - beforeJags)