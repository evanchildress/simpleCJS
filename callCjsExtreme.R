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

coreData<-createCoreData(sampleType="electrofishing",
                         columnsToAdd=c("sampleNumber","river",
                                        'observedLength','observedWeight')) %>% 
  addTagProperties() %>%
  dplyr::filter(species=="bkt") %>%
  createCmrData(maxAgeInSamples=20) %>%
  group_by(tag) %>%
  mutate(river=fillRiver(river)) %>%
  ungroup() %>%
  addSampleProperties() %>%
  addEnvironmental() %>%
  addKnownZ()

jagsData <- createJagsData(coreData)

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

zInit<-jagsData$z
zInit[is.na(zInit)]<-0
zInit[jagsData$firstObsRows]<-NA
zInit[zInit==1]<-NA
inits<- function(){
  list(phiBeta = array(0,dim=c(4,d$nYears,d$nRivers+1)),
       pBeta = array(0,dim=c(4,d$nYears,d$nRivers+1)),
       z = zInit
  )      
}



# MCMC settings
na <- 500
nb <- 2000
ni <- 5000
nt <- 5
nc <- 3

varsToMonitor<-c('pBeta','phiBeta')

  
)

gc()

(beforeJags<-Sys.time())
if(parallel==F){
  out <- jags(
    data=jagsData,
    inits=inits,
    model = modelFile,
    parameters.to.save = varsToMonitor,
    n.chains=nc,
    n.iter = ni,
    n.thin = nt,
    n.burnin=nb)
} else {
  coda.samples.wrapper <- function(j)
  { 
    temp.model = jags.model(modelFile, 
                            inits=inits, 
                            data=jagsData,
                            n.chains=nc,
                            n.adapt=na)
    coda.samples(temp.model, varsToMonitor, n.iter=ni, thin=nt) 
  }
  
  snow.start.time = proc.time()
  cl <- makeCluster(nc, "SOCK")
  ##Make sure the rjags library is loaded in each worker
  clusterEvalQ(cl, library(rjags))
  ##Send data to workers, then fit models. One disadvantage of this
  ##parallelization is that you lose the ability to watch the progress bar.
  clusterExport(cl, list('jagsData','ni','nt','nc','na','varsToMonitor','inits'))
  out = clusterApply(cl, 1:nc, coda.samples.wrapper)
  ##Reorganize 'par.samples' so that it is recognizeable as an 'mcmc.list' object
  for(i in 1:length(out)) { out[[i]] <- out[[i]][[1]] }
  class(out) <- "mcmc.list"
  stopCluster(cl)
  snow.end.time = proc.time()
  snow.dtime = snow.end.time - snow.start.time
}

( done <- Sys.time() ) 
print(done - beforeJags)
