out<-readRDS("results/cjsMcmc.rds")
library(data.table)
library(dplyr)
library(plotHacks)
outArray<-as.array(out)

wrangle<-function(x,margin=c(2)){
  getSummary<-function(x){
    meanX<-mean(x)
    quantiles<-quantile(x,c(0.025,0.975))
    return(c(meanX,quantiles))
  }
  stringSplit<-function(x,split,element){
    sapply(strsplit(x,split), "[[", element)
  }
  
  result<-data.table(dcast(melt(apply(x,MARGIN=margin,FUN=getSummary)),
                           var~Var1))
  setnames(result,c("var","mean","lower","upper"))
  result[,var:=as.character(var)]
  
  result[,parameter:=unlist(strsplit(var,"[[]"))[seq(1,nrow(result)*2-1,2)]]
  
  result[grep(",",var),
         season:=stringSplit(var,"[[]",2) %>%
                 stringSplit(",",1) %>%
                 as.numeric()]
  result[,year:=stringSplit(var,"[[]",2) %>%
                stringSplit(",",2) %>%
                as.numeric()]
  result[,river:=stringSplit(var,"[[]",2) %>%
                 stringSplit(",",3) %>%
                 as.numeric()]
  result[,stage:=stringSplit(var,"[[]",2) %>%
                 stringSplit(",",4) %>%
                 substr(1,1) %>%
                 as.numeric()]
  
  return(result)
}

results<-wrangle(outArray)
setkey(results,parameter,river,year,season)
alive<-results[parameter=="alive",list(mean,lower,upper,season,year,river,stage)]
phiBeta<-results[parameter=="phiBeta",list(mean,lower,upper,season,year,river,stage)]
pBeta<-results[parameter=="pBeta",list(mean,lower,upper,season,year,river,stage)]
setnames(pBeta,c("season","year"),c("beta","season"))

xFlow<-seq(-0.1,4,0.01)
for(a in 1:2){
  tiff.par(paste0("results/p",a,".tif"),
           mfrow=c(2,2))
  for(r in 1:4){
    plot(NA,xlim=c(-3,3),ylim=c(0,1))
    for(s in 1:4){
      b1<-pBeta[season==s&river==r&stage==a&beta==1,mean]
      b2<-pBeta[season==s&river==r&stage==a&beta==2,mean]
      logitP<-b1+b2*xFlow
      p<-1/(1+exp(-logitP))
      points(p~xFlow,type='l',col=palette()[s])
    }
  }
  dev.off()
}

# 
# toSummarize<-c("pYoy","pAdult","phiYoy","phiAdult","nYoy","nAdult")
# 
# rivers<-c("jimmy","mitchell","obear","west brook")
# 
# for(i in toSummarize){
#   tiff.par(paste0("~/westbrookJS/results/",i,".tif"),mfrow=c(4,1),mar=c(3,3,1,0),mgp=c(2,0.5,0))
#   for(r in 1:4){
#     plot(mean~sample,data=results[river==r & parameter==i],
#          pch=19,col=palette()[season],
#          ylim=if(grepl('n',i)) c(0,max(upper)) else c(0,1.1),
#          main=rivers[r],xlab="",ylab=i)
#     with(results[river==r & parameter==i],
#          error.bar(sample,mean,
#                    upper.y=upper,lower.y=lower,
#                    interval.type='bla')) 
#   }
#   #legend('bottom',c("spring","summer","fall","winter"),palette()[1:4],pch=19)
#   title(xlab="Sample Number")
#   dev.off()
# }
# 
# tiff.par("~/westbrookJS/results/nYoyFall.tif",mfrow=c(4,1),mar=c(3,3,1,0))
# for(r in 1:4){
#   plot(mean~year,data=results[river==r & season==3 & parameter=="nYoy"],
#        pch=19,type='b',col=palette()[r])
# }
# dev.off()
# 
# pAdult<-results[parameter=='pAdult',list(mean,lower,upper,
#                                          sample,river,season,year)]
# 
# obear<-data.table(mean=1,lower=1,upper=1,
#                   sample=pAdult[river==1,sample],
#                   river=3,
#                   season=pAdult[river==1,season],
#                   year=pAdult[river==1,year])
# pAdult<-rbind(pAdult,obear)
# 
# pYoy<-results[parameter=='pYoy',list(mean,lower,upper,
#                                      sample,river,season,year)]
# 
# 
# nAdult<-results[parameter=='nAdult',list(mean,lower,upper,
#                                          sample,river,season,year)]
# 
# densityPlot<-function(x,nchains=3,lwd=2,...){
#   plot(density(x[,1]),col=palette()[1],...)
#   for(i in 2:nchains){
#     points(density(x[,i]),col=palette()[i],
#            type='l')
#   }
# }
# 
# for(i in 506:672){
#   densityPlot(outArray[,i,],main=dimnames(outArray)[[2]][i])
# }
# 
# saveRDS(pAdult,"~/westbrookJS/results/pAdult.rds")
# saveRDS(pYoy,"~/westbrookJS/results/pYoy.rds")
# saveRDS(nAdult,"~/westbrookJS/results/nAdult.rds")