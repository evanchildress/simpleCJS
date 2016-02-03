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
jagsData$flowForP<-coreData$flowForP
jagsData$z[jagsData$z==2]<-0

#create sampleRows
sampleRows<-coreData %>% mutate(stage=as.numeric(ageInSamples>3)+1) %>%
             select(sampleIndex,river,stage) %>%
             mutate(row=as.numeric(rownames(.))) %>%
             mutate(river=as.numeric(factor(river,levels=c('west brook','wb jimmy','wb mitchell','wb obear'),ordered=T)))

nSampleRows<-sampleRows %>%
             melt(id.vars=c("sampleIndex","river","stage")) %>%
             acast(sampleIndex~river~stage,length)

sampleRowArray<-array(NA,dim=c(max(nSampleRows),dim(nSampleRows)))
for(s in 1:dim(nSampleRows)[1]){
  for(r in 1:dim(nSampleRows)[2]){
    for(g in 1:dim(nSampleRows)[3]){
      if(nSampleRows[s,r,g]>0){
        sampleRowArray[1:nSampleRows[s,r,g],s,r,g]<-sampleRows %>%
                                                    filter(sampleIndex==s,river==r,stage==g) %>%
                                                    select(row) %>%
                                                    as.matrix()
      }
    }
  }
}

sampleRowsEval<-sampleRows %>% select(-row) %>% distinct() %>% as.matrix
nSampleRowsEval<-nrow(sampleRowsEval)
jagsData<-within(jagsData,
       {
         sampleRowArray=sampleRowArray
         nSampleRows=nSampleRows
         sampleRowsEval=sampleRowsEval
         nSampleRowsEval=nSampleRowsEval
       })

zInit<-jagsData$z+1
zInit[is.na(zInit)]<-0
zInit[jagsData$firstObsRows]<-NA
zInit[zInit %in% c(1,2)]<-NA
inits<- function(){
  list(z = zInit)      
}



# MCMC settings
na <- 500
nb <- 2000
ni <- 5000
nt <- 5
nc <- 3

varsToMonitor<-c('pBeta','phiBeta','alive')

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
