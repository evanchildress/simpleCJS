load("outMSRiver.RData")

result<-data.table(
  dcast(
    melt(
      apply(out$BUGSoutput$sims.list$pBeta,c(2,3,4),FUN=function(x){
        c(mean(inv.logit(x)),
          quantile(inv.logit(x),c(0.025,0.975)))})
    ),
    Var2+Var3+Var4~Var1
  )
)

setnames(result,c('season','year','river','pMean','pLower','pUpper'))

tiff.par('results/p.tiff',mfrow=c(4,4),oma=c(1,1,0,0))
for(r in 1:4){
  for(s in 1:4){
    plot(pMean~year,data=result[season==s&river==r],
         pch=19,ylim=c(0,1),xlab="",ylab='')
    with(result[season==s&river==r],error.bar(year,pMean,upper.y=pUpper,
                                              lower.y=pLower,interval.type='absolute'))
  }}
mtext("Detection Probability",side=2,line=-0.5,outer=T,las=0)
mtext("Year",side=1,line=0,outer=T)
dev.off()
