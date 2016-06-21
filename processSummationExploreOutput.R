phi<-out$mean$phiBeta

flowSim<-seq(-4.5,4.3,0.1)
tempSim<-seq(0,25,0.1)
for(g in 1:2){
  plot(NA,xlim=range(flowSim),ylim=c(0,1))
  for(r in 1:4){
    
    surv<-phi[1,r,g]+phi[2,r,g]*flowSim+phi[3,r,g]*flowSim^2
    surv<-1/(1+exp(-surv))
    points(surv~flowSim,type='l',col=palette()[r])
  }}
for(g in 1:2){
plot(NA,xlim=range(tempSim),ylim=c(0.5,1))
for(r in 1:4){
  surv<-phi[1,r,g]+phi[4,r,g]*tempSim
  surv<-1/(1+exp(-surv))
  points(surv~tempSim,type='l',col=palette()[r])
}
}
