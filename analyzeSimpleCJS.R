library(ggplot2)
library(arm)
library(rjags)
library(splines)
library(plyr)
library(reshape)
source('D:/PITTAGMAIN/CMR Analyses/Hierach_Bugs/allSpp/baseThemeForPlots.r')

################################################################
#                                                  change         # #

####################

#####################################
#
setwd( "D:/PITTAGMAIN/CMR Analyses/Hierach_Bugs/allSpp/MSRiver2/simpleCJS/output-f5437f66e85-simpleCJS")

load('D:/PITTAGMAIN/CMR Analyses/Hierach_Bugs/allSpp/dMDataOutBKT2002_2011.RData')
load('outMSRiver.RData') 

ok <- 1:200
################
# set up data frames for mean p and phi

p <-   adply(invlogit(out$pBeta[,,2:5,ok,]), .margins=c(1,2,3), mean)
names(p) <- c('season','year2','river2','p')
p$river <- 'WEST BROOK'
p$river[p$river2 == 2] <- 'WB JIMMY'
p$river[p$river2 == 3] <- 'WB MITCHELL'
p$river[p$river2 == 4] <- 'WB OBEAR'

p$year <- as.numeric(as.character(p$year2)) + d$minYear - 1
p$season <- as.numeric(as.character(p$season))
p$riverOrdered <- factor(p$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)

phi <- adply(invlogit(out$phiBeta[,,2:5,ok,]), .margins=c(1,2,3), mean)
names(phi) <- c('season','year2','river2','phi')
phi$river <- 'WEST BROOK'
phi$river[phi$river2 == 2] <- 'WB JIMMY'
phi$river[phi$river2 == 3] <- 'WB MITCHELL'
phi$river[phi$river2 == 4] <- 'WB OBEAR'

phi$year <- as.numeric(as.character(phi$year2)) + d$minYear - 1
phi$season <- as.numeric(as.character(phi$season))
phi$riverOrdered <- factor(phi$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)

pPhi <- cbind(p,phi[,4])
names(pPhi)[8] <- c('phi')

####################################################
# merge in env data

env <- cbind(melt(d$flowForN), melt(d$tempForN))
env <- env[,-(5:7)]
names(env) <- c('season','river2','year2','stdF','stdT')

env$year <- env$year2 + d$minYear - 1
#env$year <- env$yearAd + 1

env <- env[env$river2 != 1,] ; env$river2 <- as.numeric(as.character(env$river2)) - 1
env$river <- 'WEST BROOK'
env$river[env$river2 == 2] <- 'WB JIMMY'
env$river[env$river2 == 3] <- 'WB MITCHELL'
env$river[env$river2 == 4] <- 'WB OBEAR'

env$riverOrdered <- factor(env$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)

env <- env[order(env$riverOrdered,env$year,env$season),]

pPhiEnv <- merge(x=pPhi, y=env[,c('season','year','riverOrdered','stdF','stdT')], 
                 by=c('season','year','riverOrdered'), all.x=T)

pPhiEnv$stdF[pPhiEnv$stdF==0] <- NA
pPhiEnv$stdF[pPhiEnv$stdT==0] <- NA
pPhiEnv <- pPhiEnv[pPhiEnv$year >2002 & pPhiEnv$year < 2012,] # no estimates
save(pPhi, file='D:/PITTAGMAIN/CMR Analyses/Hierach_Bugs/allSpp/MSRiver2/simpleCJS/pPhiFromSimpleCJS.RDATA')

######################
# plot data

ggplot(pPhiEnv, aes(stdF,phi)) +
  geom_point() +
#  geom_text(aes(label=year))+
  geom_smooth(method='lm')+
  facet_wrap(season~riverOrdered)

ggplot(pPhiEnv, aes(stdT,phi)) +
  geom_point() +
  #  geom_text(aes(label=year))+
  geom_smooth(method='lm')+
  facet_wrap(season~riverOrdered)


ggplot(pPhiEnv, aes(phi)) +
  geom_histogram() +
  #  geom_text(aes(label=year))+
  #  geom_smooth(method='lm')+
  facet_wrap(season~riverOrdered)

##########################################################
#  Simple linear models
##########################################################

mod <- lm( phi ~ stdF*stdT*river2*factor(season), data=pPhiEnv )
predDF <- data.frame(stdF=c(-1.5,0,1.5),stdT=rep(c(-1.5,0,1.5), each=3),river2=as.factor(rep(c(1:4), each=9)),season=as.factor(rep(c(1:4), each=36)))
pred <- cbind(predDF, phi=predict(mod,predDF))

ggplot(pred, aes(stdT,phi, color=factor(stdF))) + geom_point() + geom_line() + scale_colour_manual(values = c('blue', "green",'red')) + facet_grid(season~river2)

# just river=4 and season=4
mod2 <- lm( phi ~ stdF*stdT, data=pPhiEnv[ pPhiEnv$river2==4 & pPhiEnv$season==4,] )

predDF <- data.frame(stdF=seq(-2,2,.5),stdT=rep(seq(-2,2,.5), each=9))
pred <- cbind(predDF, p=predict(mod2,predDF))

ggplot(pred, aes(stdT,p, color=factor(stdF))) + geom_point() + geom_line() + ggtitle('river=IS, season=4')
######################
# add in counts

counts <- ddply( dMData[dMData$enc==1,], .(season,river,year),summarize, count=length(enc))
counts$riverOrdered <- factor(counts$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)
pPhiEnvC <- merge(x=pPhiEnv, y=counts[,c('season','year','riverOrdered','count')], 
                 by=c('season','year','riverOrdered'), all.x=T)
pPhiEnvC$countAdj <- pPhiEnvC$count/pPhiEnvC$p

save(pPhiEnvC, file='D:/PITTAGMAIN/CMR Analyses/Hierach_Bugs/allSpp/MSRiver2/simpleCJS/pPhiEnvCFromSimpleCJS.RDATA')
######################
# plot data

ggplot(pPhiEnvC, aes(countAdj,phi)) +
  geom_point() +
  #geom_text(aes(label=year))+
  geom_smooth(method='lm')+
  facet_grid(season~riverOrdered)

######################
# regression


#mod <- lm( phi ~ factor(season):river * countAdj, data=pPhiEnvC  )
#summary(mod)

#mod <- lm( phi ~ factor(season)*river * stdF, data=pPhiEnvC  )
#summary(mod)

# do piecewise regression for each var/season/river combo

mod <- list()
modSumm <- list()
for (var in c('countAdj','stdF','stdT')){
  for (r in c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR')){
    for (s in 1:4){
      dat <- pPhiEnvC[pPhiEnvC$season %in% s&pPhiEnvC$river==r,c('phi',var)]
      names(dat) <- c('y','x')
      mod[[var]][[r]][[s]] <- lm( y ~  x, data=dat  )
      print(c(var,r,s))
      print(summary(mod[[var]][[r]][[s]])$coef[2,]) #second row of the coefs = slope
      modSumm[[var]][[r]][[s]] <- summary(mod[[var]][[r]][[s]])$coef[2,] #second row of the coefs = slope
      
    }
  }
}






h <- cbind(d$ind,d$lengthDATA,d$riverDATA,d$encDATA,d$emPermDATA)
hER <- cbind(d$ind[evalList$evalRows],d$lengthDATA[evalList$evalRows],d$riverDATA[evalList$evalRows],d$encDATA[evalList$evalRows],d$emPermDATA[evalList$evalRows])
zInit <- as.numeric(encInit(dMData$sampleNum, dMData$first, dMData$last))
zInitER <- zInit[evalList$evalRows]

ok <- seq(1,200,1)

# temp2 <- melt((out$grSigma[,,2:5,ok,]))
# 
# ggplot(temp2, aes(X4,(value)
# #ggplot(temp2, aes((value)
# #                  , colour=(X4)  # turn on when >1 chain
#                   )) + 
#   facet_grid(X1~X2, scales='free') + 
#   geom_point() +
# #  geom_density(size=1.5) +
# #  geom_hline(yintercept=0) +
#   scale_colour_continuous()

#for traces with two  e.g. [1:4, 1:5,  1:100, 1]
# phiBeta[i,s,r]

temp3 <- melt(((invlogit(out$pBeta[,,2:5,ok,]))))
# pBeta[s,y,r]
temp3 <- melt((((invlogit(out$phiBeta[,,2:5,ok,])))))
# N1[s,y,r]
#temp3 <- melt((((out$N1[,,2:5,ok,]))))
# grBeta[i,s,r]
#temp3 <- melt(((out$grBeta[,,2:5,ok,])))

ggplot(temp3, aes(X4,(value) #X4
                  , colour=factor(X3)  
                  )) + 
  facet_grid(X1~X2 
             , scales='free' 
             ) + 
  geom_point(size=0.25) +
  scale_x_continuous('sim5') +
  geom_hline(yintercept=0)  

# with multiple chains
b <- 2
temp3 <- melt(((out$grBeta[b,,2:5,ok,])))

ggplot(temp3, aes(X3,(value) #X4
                  , colour=factor(X4)  
)) + 
  facet_grid(X1~X2 
             , scales='free' 
  ) + 
    geom_point(size=0.25) +
    scale_x_continuous('sim5') +
    geom_hline(yintercept=0)  




#### psi
temp3 <- melt(((out$psiBeta[,,,ok,])))

ggplot(temp3, aes(X4,(value) #X4
                  , colour=factor(X1)  # turn on when >1 chain
                  )) + 
                    facet_grid(X2~X3 
                               , scales='free' 
                               ) + 
                                 geom_point(size=0.25) +
                                 scale_x_continuous('sim5') +
                                 geom_hline(yintercept=0)  


#############################################################


#### plot curves of predicted values

preds <- function(beta,
                  lengthMean,lengthSd,
                  season,
                  flowMean,flowSd,
                  tempMean,tempSd ){

B2 <- data.frame(
        cbind(melt(beta[1,,2:5,,]),
              melt(beta[2,,2:5,,])[4],
              melt(beta[3,,2:5,,])[4],
              melt(beta[4,,2:5,,])[4],
              melt(beta[5,,2:5,,])[4] ,
              melt(beta[6,,2:5,,])[4],
              melt(beta[7,,2:5,,])[4],
              melt(beta[8,,2:5,,])[4] 
              ) )
              
names(B2) <- c('season','river','iter','b1','b2','b3','b4',
               'b5',
               'b6',
               'b7',
               'b8'
               )

xRange <-seq( -1.5, 1.5 ,.5 ) # c(-1,0,1)#

B <- data.frame(
       cbind(rep(B2$season, each=length(xRange)^3),
             rep(B2$river,  each=length(xRange)^3),
             rep(B2$iter,   each=length(xRange)^3),
             rep(B2$b1,     each=length(xRange)^3),
             rep(B2$b2,     each=length(xRange)^3),
             rep(B2$b3,     each=length(xRange)^3),
             rep(B2$b4,     each=length(xRange)^3),
             rep(B2$b5,     each=length(xRange)^3), 
             rep(B2$b6,     each=length(xRange)^3),
             rep(B2$b7,     each=length(xRange)^3),
             rep(B2$b8,     each=length(xRange)^3) 
             ) )

names(B) <- c('season','river','iter','b1','b2','b3','b4',
              'b5' ,
              'b6',
              'b7',
              'b8'
              )
B$x1Std <- rep(xRange)
B$x2Std <- rep(xRange, each=length(xRange)^1)
B$x3Std <- rep(xRange, each=length(xRange)^2)


#fwiw, x1 = Len, x2 = flow and x3 = temp
B$pred <-   B$b1 + 
            B$b2 * B$x1Std + 
            B$b3 * B$x2Std + 
            B$b4 * B$x3Std + 
            B$b5 * B$x2Std * B$x3Std +
            B$b6 * B$x1Std * B$x2Std + 
            B$b7 * B$x1Std * B$x3Std + 
            B$b8 * B$x1Std * B$x2Std * B$x3Std

lengthMeans <- melt(lengthMean[,2:5])
lengthSds <- melt(lengthSd[,2:5])
lengths <- cbind(lengthMeans,lengthSds[,3])
names(lengths) <- c('season','river','lengthMean','lengthSd')
B <- merge( x=B,y=lengths, by=c('season','river'), all.x=T)

seasonMeans <- data.frame(cbind( season,tempMean,tempSd,flowMean,flowSd ))
names(seasonMeans) <- c('season','tempMean','tempSd','flowMean','flowSd')
B <- merge( x=B,y=seasonMeans, by='season', all.x=T)

B$len  <- B$x1Std * B$lengthSd + B$lengthMean
B$temp <- B$x3Std * B$tempSd + B$tempMean
B$flow <- B$x2Std * B$flowSd + B$flowMean

return(B)
}

# for now, until new runs that put this in d
stdBySeason <- ddply( dMData, .(season), summarise,   
                      lengthMean=mean(length, na.rm=TRUE), 
                      lengthSd=sd(length, na.rm=TRUE),
                      lengthLo = quantile(length,c(0.025), na.rm=TRUE),
                      lengthHi = quantile(length,c(0.975), na.rm=TRUE),
                      tempMean=mean(fullMeanT, na.rm=TRUE),
                      tempMeanP=mean(temperatureForP, na.rm=TRUE), 
                      tempSd=sd(fullMeanT, na.rm=TRUE),
                      tempSdP=sd(temperatureForP, na.rm=TRUE),
                      tempLo = quantile(fullMeanT,c(0.025), na.rm=TRUE),
                      tempHi = quantile(fullMeanT,c(0.975), na.rm=TRUE),
                      flowMean=mean(fullMeanD, na.rm=TRUE), 
                      flowSd=sd(fullMeanD, na.rm=TRUE),
                      dischMeanP=mean(dischargeForP,na.rm=T),
                      dischSdP=sd(dischargeForP,na.rm=T),
                      flowLo = quantile(fullMeanD,c(0.025), na.rm=TRUE),
                      flowHi = quantile(fullMeanD,c(0.975), na.rm=TRUE) ) 
d$stdBySeason <- stdBySeason



# which iterations?
ok2 <- seq(1,dim(out$grBeta)[4],1)
x2 <- c(-1.5,0,1.5)

grPreds <- preds( out$grBeta,
                  d$lengthMean,d$lengthSd,
                  d$stdBySeason$season,
                  d$stdBySeason$flowMean,d$stdBySeason$flowSd,
                  d$stdBySeason$tempMean,d$stdBySeason$tempSd )

ggFile <- grPreds[grPreds$iter %in% ok2,]


riverLabeller <- function(var, value){
  value <- as.character(value)
  if (var=="river") { 
    value[value=="1"] <- "WB"
    value[value=="2"] <- "OL"
    value[value=="3"] <- "OS"
    value[value=="4"] <- "I"
  }
  return(value)
}
seasonLabeller <- function(var, value){
  value <- as.character(value)
  if (var=="season") { 
    value[value=="1"] <- "Spring"
    value[value=="2"] <- "Summer"
    value[value=="3"] <- "Autumn"
    value[value=="4"] <- "Winter"
  }
  return(value)
}
ggFile$riverText <- riverLabeller('river',ggFile$river)
ggFile$riverText <- factor(ggFile$riverText,levels=c("WB","OL","OS","I"), ordered=T)
# gr
# for plots of 2-way interactions
#           change           ###
ggplot(ggFile[
              ggFile$iter %in% ok2 &
              ggFile$x1Std == 0 &               #Len
              ggFile$x2Std %in% x2 #&            #flow
              #ggFile$x3Std %in% x2,             #temp
              , ], 
        aes(x=temp,y=(pred))) +                                #x"Std, set to commented out x

  geom_jitter(alpha = I(0.05), size = 1.25,
            position=position_jitter(width=0.01)
             ,aes(colour=factor(x2Std))) +                     #x"Std, set to %in% x2 x
  geom_smooth(aes(colour='black',
                group=factor(flow)                             # gropuing variable
                ), method='lm', formula = y ~ ns(x,3))+   #x"Std, set to %in% x2 x
 
  scale_x_continuous('Stream Temperature (C)')+#'Body size (standardized, 60-200 mm)')+#, breaks=seq(1,29,5), labels=seq(1,29,5)) +
  scale_y_continuous('Growth rate (mm/season)', lim=c(-3,50))+# 'Growth rate (mm/season)')+#'Probability of survival (/season)') +                  
  facet_wrap( ~ riverText ) +
  BaseThemeX90(20)+
  opts(   panel.background =  theme_rect(fill = "white", colour = NA), 
  				panel.border =      theme_rect(colour = 'grey90'), #theme_blank(), 
					panel.grid.major =  theme_line(colour = "white"),
					panel.grid.minor =  theme_line(colour = "white", size = 0.25),
					panel.margin =      unit(0.25, "lines"),
          legend.position = "none"
      )   +     
  scale_colour_manual(values = c('blue', "green",'red'))
  

scale_colour_brewer(pal='Dark2')      

#  phi
phiPreds <- preds(out$phiBeta,
                  d$lengthMean,d$lengthSd,
                  d$stdBySeason$season,
                  d$stdBySeason$flowMean,d$stdBySeason$flowSd,
                  d$stdBySeason$tempMean,d$stdBySeason$tempSd)

ggFile2 <- phiPreds[phiPreds$iter %in% ok2,]
ggFile2$riverText <- riverLabeller('river',ggFile2$river)
ggFile2$riverText <- factor(ggFile2$riverText,levels=c("WB","OL","OS","I"), ordered=T)

ggFile2$seasonText <- seasonLabeller('season',ggFile2$season)
ggFile2$seasonText <- factor(ggFile2$seasonText,levels=c("Spring","Summer","Autumn","Winter"), ordered=T)

ggplot(ggFile2[
  ggFile$iter %in% ok2 &
    ggFile$x1Std == 0 &               #Len
    ggFile$x2Std %in% x2 #&            #flow
   #ggFile$x3Std %in% x2,             #temp
  , ], 
       aes(x=x3Std,y=invlogit(pred))) +                                #x"Std, set to commented out x
         
         geom_jitter(alpha = I(0.05), size = 1.25,
                     position=position_jitter(width=0.01)
                     ,aes(colour=factor(x2Std))) +                     #x"Std, set to %in% x2 x
         geom_smooth(aes(colour=factor(x2Std),
                         group=factor(x2Std)                          # gropuing variable
                         ), method='lm', formula = y ~ ns(x,3))+      #x"Std, set to %in% x2 x
                           
         scale_x_continuous('Stream Temperature (C)')+#'Body size (standardized, 60-200 mm)')+#, breaks=seq(1,29,5), labels=seq(1,29,5)) +
         scale_y_continuous('Probability of survival (seasonal)')+# 'Growth rate (mm/season)')+#'Probability of survival (/season)') +                  
         facet_grid( seasonText ~ riverText, scales="free_x" ) +
         BaseThemeX90(20)+
         opts(   panel.background =  theme_rect(fill = "white", colour = NA), 
                 panel.border =      theme_rect(colour = 'darkgrey'), #theme_blank(), 
                 panel.grid.major =  theme_line(colour = "white"),
                 panel.grid.minor =  theme_line(colour = "white", size = 0.25),
                 panel.margin =      unit(0.25, "lines"),
                legend.position = "none"
                 ) +     
         scale_colour_manual(values = c('blue', "green",'red'))


##################################################
# get predicted values for subset of conditions ##
##################################################

getPredValue <- function( b,len,flow,temp, nBeta=5 ){
  
  #if only one iteration
  if( is.null(dim(b)) ) {
  out <- b[1] +
         b[2] * len +
         b[3] * flow +
         b[4] * temp 
     #    b[5] * den +
    #     b[6] * len * flow +
    #     b[7] * len * temp +
    #     b[8] * len * den
  } else
  #if > one iteration  
   out <- b[1,] +
          b[2,] * len +
          b[3,] * flow +
          b[4,] * temp 
   #       b[5,] * den +
  #        b[6,] * len * flow +
  #        b[7,] * len * temp +
  #        b[8,] * len * den
            
  return(out)
}

#str(out$phiBeta)
#mcarray [1:8, 1:4, 1:5, 1:200, 1]
#len,flow,temp,den
invlogit( getPredValue(out$phiBeta[,1,2,1:1,1],-1,0,0) )

dd <- data.frame(d$tempDATA,d$flowDATA,d$riverDATA,d$season,d$year)
names(dd) <- c('temp','flow','river','season','year')
ggplot(dd,aes(temp,flow)) +
  geom_point(aes(colour=factor(year))) +
  facet_wrap(season~river)


std <- ddply( dd, .(season,year,river), summarise,
              temp=mean(temp,na.rm=TRUE),
              flow=mean(flow,na.rm=TRUE)
              ) 

# these are also defined above in the individual trajectory section
iters <- seq(1,200,1) #80:100 # iterations to keep for means etc
numIOut <- length(iters) 

phis <- array(NA,c(4,d$nYear,5,numIOut))
for(s in 1:4){
  for(y in 1:d$nYear){
  for(r in 2:5){
    phis[s,y,r,] <- invlogit( getPredValue(out$phiBeta[,s,r,iters,1],
                                   0,
                                   std$flow[std$season==s & std$year == y & std$river == r-1][1],
                                   std$temp[std$season==s & std$year == y & std$river == r-1][1] ))
          #chagne to stdN                         out$N1[s,y,r,iters,1]) )
    
  }
 }
}

aaply(phis,.margins=c(1,2,3), mean)

# get phi for each row of d
phiEvalRows <- dLengthStd <- array(NA,length(d$occ))

for(i in 1:length(d$occ)){
  if(!is.na(d$riverDATA[i])) {
    
    dLengthStd[i] <- ( d$lengthDATA[i] - 
             d$lengthMean[ d$season[i],d$riverDATA[i] + 1 ] ) / 
               d$lengthSd[ d$season[i],d$riverDATA[i] + 1 ] 
    
               phiEvalRows[i] <- invlogit(
                                 getPredValue(out$phiBeta[,
                                             d$season[i],
                                             d$riverDATA[i] + 1,
                                             10, # for now
                                             1],
                                 dLengthStd[i],
                                 d$tempDATA[i],
                                 d$flowDATA[i]
                                 ) ) 
  }             
}

dd$phi <- as.numeric(phiEvalRows)

ggplot(dd[!is.na(dd$river) &
          dd$season==1,
          ],
       aes(phi))+
  geom_freqpoly()+
  facet_grid(river~year)

# make graphs of lastObs
dMData$lastObs01 <- ifelse(dMData$last == dMData$sampleNum, 1, 0 ) 

lObs <- ddply(dMData[
    dMData$sampleNum >= dMData$first &
    dMData$sampleNum <= dMData$last &
    !is.na(dMData$riverOrdered),],
              .(season,year,riverOrdered), summarise,
              sumLO = sum(lastObs01),
              lengthLO = length(lastObs01),
              propLO = sum(lastObs01)/length(lastObs01)
              )
lObs$river <- as.numeric(lObs$riverOrdered)
lObs$year <- as.numeric(factor(lObs$year))

stdN1 <- melt(aaply(out$stdN[,,2:5,,], .margin=c(1,2,3), mean) )
names(stdN1) <- c('season','year','river','stdN')

stdN <- merge(x=stdN1,y=lObs,all.y=T)

ggplot(stdN, aes(stdN,propLO, label=(year)))+
  geom_point()+
  geom_text()+
  facet_grid(season~river)



ggplot(dMData[
  dMData$sampleNum >= dMData$first &
  dMData$sampleNum <= dMData$last &
  !is.na(dMData$riverOrdered) &
  dMData$season == 4, ],
       aes(x=length,colour=factor(lastObs01))) +
  geom_freqpoly()+
#  facet_grid(season~year)
  facet_grid(year~riverOrdered)


###########################################################
# means of traces per indiviudal
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#zObs1 <- adply(out$zOut, .margins = 1, mean, rm.na=T )
#sizeObs1 <- adply(out$lengthOut, .margins = 1, mean, rm.na=T )

# individual traces-  states and input data together for d$nOut rows
#load('D:/PITTAGMAIN/CMR Analyses/Hierach_Bugs/allSpp/simpleGrowthSim/inputGR_AllAt1.RData')


nOut <- 2000 # number of evalRows outPut from jags - must match # in bugs code
iters <- seq(1,200,10) #80:100 # iterations to keep for means etc
numIOut <- length(iters) # number of iterations kept #niter/nthin

sizeMelt <- melt(out$lengthOut[,iters,])
sizeLag <- c(sizeMelt[2:(nrow(sizeMelt)),3],0)
zM <- cbind( rep(d$evalRows[1:nOut],numIOut),
             melt(out$zOut[,iters,]),
             rep(d$ind[d$evalRows][1:nOut],numIOut),
             sizeMelt[,3], 
             sizeLag,
             
             rep(d$lengthDATA[d$evalRows][1:nOut],numIOut),
             rep(       d$occ[d$evalRows][1:nOut],numIOut),
             rep(    d$season[d$evalRows][1:nOut],numIOut),
             rep(      d$year[d$evalRows][1:nOut],numIOut) #,
             #melt(out$grLengthOut[,iters,])[,3]
             )
names(zM) <- c('evalRows','X1','X2','z','ind','size','sizeLag','lengthDATA','occ','season','year')#,'grOut')

nHead <- 30
head(cbind(1:nHead,d$ind,d$occ,1:nHead %in% d$evalrows,d$lengthDATA),30)

zM$evalInFirst <- zM$evalRows %in% d$firstObsRows
zM$evalInFirstLag <- c(zM$evalInFirst[2:(nrow(zM))],0)


zM$z2 <- c(zM$z[2:nrow(zM)], 0 )
zM$gr <- zM$sizeLag-zM$size
zM$gr[zM$occ == 14] <- NA # 12's are also very negative when they are the last occ'

zM$grZ1 <- zM$z*zM$gr
zM$grZ1Z2 <- zM$z*zM$z2*zM$gr
zM$grZ1Z2NA <- zM$grZ1Z2 
zM$grZ1Z2NA[zM$grZ1Z2NA ==0 ]  <-  NA #seems faster than an ifelse
zM$grZ1Z2NA[zM$evalInFirstLag == 1 ]  <-  NA # turn off last obsrervation of fish that are alive on the last occ

zM$lengthDATALag <- c(zM$lengthDATA[2:nrow(zM)], 0 )
zM$grDATA <- zM$lengthDATALag - zM$lengthDATA
zM$grDATAZ1 <- zM$z*zM$grDATA
zM$grDATAZ1Z2 <- zM$z*zM$z2*zM$grDATA
zM$grDATAZ1Z2NA <- zM$grDATAZ1Z2 
zM$grDATAZ1Z2NA[zM$grDATAZ1Z2NA ==0 ]  <-  NA #seems faster than an ifelse
zM$grDATAZ1Z2NA[zM$evalInFirstLag == 1 ]  <-  NA # turn off last obsrervation of fish that are alive on the last occ


zM$sizeZ1 <- zM$z*zM$size
zM$sizeZ1[zM$sizeZ1 == 0 ] <- NA

zM$sY <- (zM$year+(zM$season-1)*0.25)

i=490
#ggplot(zM[zM$ind %in% i:(i+8) ,], aes(factor(sY),sizeZ1, group=X2)) +
ggplot(zM[zM$ind %in% c(11,12,14,17) ,], aes(factor(sY),sizeZ1, group=X2)) +
  geom_point(aes()) +
  geom_point(aes(factor(sY),lengthDATA), size=5)+
  geom_line( colour='darkgrey')+ #aes(colour=X2))
  scale_x_discrete('Time (year + season)')+#, breaks=seq(1,29,5), labels=seq(1,29,5)) +
  scale_y_continuous( 'Body size (mm)')+#'Probability of survival (/season)') +
  facet_wrap(~ind)
#########################################################################################

#############################################################
aaply(out$grBeta[1,,2:5,,], .margin=c(1,2), mean)
grBeta1 <- melt(out$grBeta[1,,2:5,,])
ggplot(grBeta1, aes(value))+
  geom_density(aes(colour=factor(X2)),size=1.)+
  facet_wrap(~X1) +
  scale_colour_brewer(pal='Dark2')

setwd("D:/PITTAGMAIN/CMR Analyses/Hierach_Bugs/allSpp/MSRiver/IPM")
load('outMetaLambda.RData')
lamPostMelt <- melt(lamPost)
ggplot(lamPostMelt, aes(value))+
  geom_density(size=1.5)+
  #xlim(c(min(lamPostMelt$value,na.rm=T)+0.01,max(lamPostMelt$value,na.rm=T)-0.1))+
  scale_colour_brewer(pal='Dark2') +
  BaseThemeX90(30)+
  
  scale_x_continuous('Population growth')+
   scale_y_continuous('Probability')+
   xlim(c(0.7,1.02))

load('outRiverLambda.RData')
lamPostMelt <- melt(lamPost)
ggplot(lamPostMelt[lamPostMelt$X2<4,], aes(value))+
  geom_density(aes(colour=factor(X2)),size=1.5)+
   scale_x_continuous('Population growth')+
   scale_y_continuous('Probability')+
  scale_colour_brewer(pal='Dark2')+
  BaseThemeX90(30)+
   xlim(c(0.15,1.1))+
  opts(legend.position = "none")













#for traces with five values e.g. [1:4, 1:4, 1:2, 1:29, 1:100, 1]
temp5 <- melt(out$grBeta[,,,,])

ggplot(temp5[temp5$X4 %in% c(1,8,16,24) &
             temp5$X3 %in% c(1,2),], aes(X5,(value))) + 
  geom_point(aes(colour=X3)) +
  facet_grid(X1~X2+X4)

ggplot(temp5, aes(factor(X4),value)) +
  geom_boxplot() +
  facet_wrap( ~X1+X2 )

ggplot(temp5, aes(x=(X4),y=value)) +
  #geom_point(alpha = I(0.05), size = 2.25 ) +
  geom_jitter(alpha = I(0.05), size = 2.25,
              position=position_jitter(width=0.1)
              ,aes(colour=factor(X3))) +  
 # geom_smooth( colour='white', size=0.75) +
  stat_smooth( aes(colour=factor(X3)),size=0.75 ) +
  scale_x_continuous('Body size (mm)', breaks=seq(1,29,5),
                     labels=seq(1,29,5)*5+50) +
  scale_y_continuous('Probability of survival (/season)') +
                       #geom_jitter() +
  facet_wrap( ~X1+X2  ) +
  opts(    panel.background =  theme_rect(fill = "white", colour = NA), 
					panel.border =      theme_rect(colour = 'grey90'), #theme_blank(), 
					panel.grid.major =  theme_line(colour = "white"),
					panel.grid.minor =  theme_line(colour = "white", size = 0.25),
					panel.margin =      unit(0.25, "lines")
      )    



