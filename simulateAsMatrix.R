phiBeta<-c(8,-0.2,-0.2,-0.1)

temps<-tempData[,1]
flows<-flowData[,1]


simFlow<-seq(-2.7,4.1,0.1)
logitPhiFlow<-phiBeta[1]+phiBeta[2]*simFlow+phiBeta[3]*simFlow^2
phiFlow<-1/(1+exp(-logitPhiFlow))
plot(phiFlow~simFlow,type='l')

simTemp<-seq(0,22,0.1)
logitPhiTemp<-phiBeta[1]+phiBeta[4]*simTemp
phiTemp<-1/(1+exp(-logitPhiTemp))
plot(phiTemp~simTemp,type='l')

logitPhi<-phiBeta[1]+phiBeta[2]*flows+phiBeta[3]*flows^2+phiBeta[4]*temps
phi<-1/(1+exp(-logitPhi))

getSurvival<-function(startTime,endTime,flow,temp,beta,sigma=0){
  logitPhi<-phiBeta[1]+phiBeta[2]*flow[startTime:endTime]+
    phiBeta[3]*flow[startTime:endTime]^2+
    phiBeta[4]*temp[startTime:endTime]
  phi<-prod(1/(1+exp(-logitPhi)))
  
  phi<-plogis(qlogis(phi)+rnorm(1,0,sigma))
  return(phi)
}

times<-round(seq(1,length(flows),length.out=63))
startTimes<-times[1:(length(times)-1)]
endTimes<-times[2:length(times)]-1

n.occasions<-length(times)
marked<-rep(30,n.occasions-1)


p<-rep(0.8,n.occasions-1)

r.var<-0.2#residual temporal variance

times<-round(seq(1,length(flows),length.out=63))
startTimes<-times[1:(length(times)-1)]
endTimes<-times[2:length(times)]-1

phi<-rep(NA,n.occasions-1)
for(t in 1:(n.occasions-1)){
  phi[t]<-getSurvival(startTimes[t],endTimes[t],flows,temps,phiBeta,r.var)
}

PHI<-matrix(phi,ncol=n.occasions-1,nrow=sum(marked),byrow=T)
P<-matrix(p,ncol=n.occasions-1,nrow=sum(marked))

simul.cjs<-function(PHI,P,marked){
  n.occasions<-dim(PHI)[2]+1
  CH<-matrix(0,ncol=n.occasions,nrow=sum(marked)) 
  
  #when was the fish marked?
  mark.occ<-rep(1:length(marked),marked[1:length(marked)])
  
  #fill the capture history
  for(i in 1:sum(marked)){        
    CH[i,mark.occ[i]]<-1
    if(mark.occ[i]==n.occasions)next
    for(t in (mark.occ[i]+1):n.occasions){
      #Bernoulli trial for survival
      sur<-rbinom(1,1,PHI[i,t-1])
      if(sur==0) break #if dead move to the next individual
      #Bernoulli trial for recaptures
      rp<-rbinom(1,1,P[i,t-1])
      if(rp==1) CH[i,t]<-1
    }
  }
  return(CH)
}

CH<-simul.cjs(PHI,P,marked)

get.first<-function(x) min(which(x!=0))
f<-apply(CH,1,get.first)




cjs.init.z<-function(ch,f){
  for(i in 1:nrow(ch)){
    if(sum(ch[i,])==1)next
    n2<-max(which(ch[i,]==1))
    ch[i,f[i]:n2]<-NA}
  for(i in 1:nrow(ch)){
    ch[i,1:f[i]]<-NA
  }
  return(ch)
}

knownZ<-matrix(NA,nrow=nrow(CH),ncol=ncol(CH))
for(i in 1:nrow(CH)){
  whichMax<-max(which(CH[i,]==1))
  knownZ[i,f[i]:whichMax]<-1
}

zInits<-cjs.init.z(CH,f)

jagsData<-list(y=CH,
               flows=flows,
               temps=temps,
               f=f,
               nTimes=length(flows),
               startTimes=startTimes,
               endTimes=endTimes,
               nOccasions=ncol(CH),
               nInd=nrow(CH),
               z=knownZ)

inits<-function(){list(z=zInits)}

na <- 500
nb <- 1000
ni <- 2000
nt <- 3
nc <- 3

varsToMonitor<-c('pBeta','phiBeta','sdPhi')

out <- jags(
  data=jagsData,
  inits=inits,
  model = "modelAsMatrix.R",
  parameters.to.save = varsToMonitor,
  n.chains=nc,
  n.iter = ni,
  n.thin = nt,
  n.burnin=nb,
  parallel=T)

