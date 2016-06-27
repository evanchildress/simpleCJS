out<-readRDS("processSummationOut.rds")

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

survFlow<-survTemp<-surv<-array(NA,dim=c(dim(flowData),2))
for(g in 1:2){
  for(r in 1:4){
    survFlow[,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*flowData[,r]^2
    survTemp[,r,g]<-phi[1,r,g]+phi[4,r,g]*tempData[,r]
    surv[,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*flowData[,r]^2+phi[4,r,g]*tempData[,r]
  }
}
survFlow<-melt(1/(1+exp(-survFlow))) %>% dcast(Var1~Var3+Var2) %>% data.table()
survTemp<-melt(1/(1+exp(-survTemp))) %>% dcast(Var1~Var3+Var2) %>% data.table()
surv<-melt(1/(1+exp(-surv))) %>% dcast(Var1~Var3+Var2) %>% data.table()

survFlow[,date:=as.Date(rownames(flowData))]
survTemp[,date:=as.Date(rownames(flowData))]
surv[,date:=as.Date(rownames(flowData))]

setnames(survFlow,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                    "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))
setnames(survTemp,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                    "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))
setnames(surv,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                    "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))

plot(westBrookYoy~date,data=surv,type='l')
points(westBrookYoy~date,data=survFlow,type='l',col='blue')
points(westBrookYoy~date,data=survTemp,type='l',col='red')

plot(jimmyYoy~date,data=surv,type='l')
points(jimmyYoy~date,data=survFlow,type='l',col='blue')
points(jimmyYoy~date,data=survTemp,type='l',col='red')

plot(mitchellYoy~date,data=surv,type='l')
points(mitchellYoy~date,data=survFlow,type='l',col='blue')
points(mitchellYoy~date,data=survTemp,type='l',col='red')

plot(westBrookAdult~date,data=surv,type='l')
points(westBrookAdult~date,data=survFlow,type='l',col='blue')
points(westBrookAdult~date,data=survTemp,type='l',col='red')

plot(jimmyAdult~date,data=surv,type='l')
points(jimmyAdult~date,data=survFlow,type='l',col='blue')
points(jimmyAdult~date,data=survTemp,type='l',col='red')

plot(mitchellAdult~date,data=surv,type='l')
points(mitchellAdult~date,data=survFlow,type='l',col='blue')
points(mitchellAdult~date,data=survTemp,type='l',col='red')


plot(westBrookYoy~date,data=surv,type='l',col='blue',ylim=c(0.85,1))
points(westBrookAdult~date,data=surv,type='l')

plot(jimmyYoy~date,data=surv,type='l',col='blue',ylim=c(0.85,1))
points(jimmyAdult~date,data=surv,type='l')

plot(mitchellYoy~date,data=surv,type='l',col='blue',ylim=c(0.85,1))
points(mitchellAdult~date,data=surv,type='l')

