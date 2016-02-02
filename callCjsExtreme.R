
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
    year = dMData$year-min(dMData$year) + 1
    nYears = max(dMData$year)-min(dMData$year)+1
    season = as.numeric(as.character(dMData$season)) 
    nAllRows = nrow(dMData)
    nFirstObsRows = evalList$nFirstObsRows
    firstObsRows = evalList$firstObsRows
    nEvalRows = evalList$nEvalRows # rows that will matter if we start using JS, and 
    evalRows = evalList$evalRows   # that matter now for the growth model
    z = dMData[,knownZ(sampleNum,first,last)]
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