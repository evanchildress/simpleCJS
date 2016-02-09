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

coreData<-createCoreData(sampleType="electrofishing") %>% 
  addTagProperties() %>%
  dplyr::filter(species=="bkt") %>%
  createCmrData() %>%
  group_by(tag) %>%
  mutate(river=fillRiver(river)) %>%
  ungroup() %>%
  addSampleProperties() %>%
  addEnvironmental(sampleFlow=T) %>%
  addKnownZ()

jagsData <- createJagsData(coreData)
jagsData$stageDATA<-as.numeric(coreData$ageInSamples>3)+1
jagsData$flowForP<-scale(coreData$flowForP)[,1]
jagsData$z[jagsData$z==2]<-0

#create sampleRows
aliveRows<-coreData %>% mutate(stage=as.numeric(ageInSamples>3)+1) %>%
             select(season,year,river,stage) %>%
             mutate(row=as.numeric(rownames(.))) %>%
             mutate(river=as.numeric(factor(river,levels=c('west brook','wb jimmy','wb mitchell','wb obear'),ordered=T))) %>%
             mutate(year=year-min(year)+1)

nAliveRows<-aliveRows %>%
             melt(id.vars=c("season","year","river","stage")) %>%
             acast(season~year~river~stage,length)
years<-as.numeric(dimnames(nAliveRows)[[2]])
aliveRowArray<-array(NA,dim=c(max(nAliveRows),dim(nAliveRows)))
for(s in 1:dim(nAliveRows)[1]){
  for(y in 1:dim(nAliveRows)[2]){
    for(r in 1:dim(nAliveRows)[3]){
      for(g in 1:dim(nAliveRows)[4]){
        if(nAliveRows[s,y,r,g]>0){
          aliveRowArray[1:nAliveRows[s,y,r,g],s,y,r,g]<-aliveRows %>%
                                                        filter(season==s,year==years[y],river==r,stage==g) %>%
                                                        select(row) %>%
                                                        as.matrix()
        }
      }
    }
  }
}

aliveRowsEval<-aliveRows %>% select(-row) %>% distinct() %>% as.matrix
nAliveRowsEval<-nrow(aliveRowsEval)
jagsData<-within(jagsData,
       {
         aliveRowArray=aliveRowArray
         nAliveRows=nAliveRows
         aliveRowsEval=aliveRowsEval
         nAliveRowsEval=nAliveRowsEval
       })

zInit<-jagsData$z+1
zInit[is.na(zInit)]<-0
zInit[jagsData$firstObsRows]<-NA
zInit[zInit %in% c(1,2)]<-NA
inits<- function(){
  list(z = zInit)      
}



# MCMC settings
na <- 3000
nb <- 3000
ni <- 5000
nt <- 5
nc <- 3

varsToMonitor<-c('pBeta','phiBeta','alive','pEps')

gc()

(beforeJags<-Sys.time())
if(parallel==F){
  out <- jags(
    data=jagsData,
    inits=inits,
    model = "model.txt",
    parameters.to.save = varsToMonitor,
    n.chains=nc,
    n.iter = ni,
    n.thin = nt,
    n.burnin=nb)
} else {
  coda.samples.wrapper <- function(j)
  { 
    temp.model = jags.model("model.txt", 
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
  clusterExport(cl, list('jagsData','ni','nt','nc','na','varsToMonitor','inits','zInit'))
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
