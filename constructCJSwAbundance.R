cat("
    model{
    
    ############## Recapture model
    for(i in 1:nEvalRows){
      logit(p[evalRows[i]]) <- pBeta[1,
                                     season[evalRows[i]],
                                     riverDATA[evalRows[i]],
                                     stageDATA[evalRows[i]]] +
                               pBeta[2,
                                     season[evalRows[i]],
                                     riverDATA[evalRows[i]],
                                     stageDATA[evalRows[i]]] *
                               flowForP[evalRows[i]]
    }
    
    ############## Recapture priors
    for(b in 1:2){ #betas: 1=intercept,2=slope with discharge during sampling
      for(s in 1:4 ){    #season
        for(r in 1:nRivers){#river
          for(g in 1:2){ #stage
            pBeta[b,s,r,g] ~ dnorm(0,0.667)
          }
        }
      }
    }
    
    
    ############## Survival model
    for(i in 1:nEvalRows){
      logit(phi[evalRows[i]-1]) <- phiBeta[season[evalRows[i]-1],
                                           year[evalRows[i]-1],
                                           riverDATA[evalRows[i]-1],
                                           stageDATA[evalRows[i]-1]]
    }
    
    
    ############## survival priors
    for( s in 1:4 ){    
      for(y in 1:nYears){  
        for( r in 1:nRivers){
          for(g in 1:2){
            phiBeta[s,y,r,g] ~ dnorm(0,0.667)
          }
        }
      }
    }
    
    
    ############## Likelihood
    
    for(i in 1:nEvalRows){
    # State of survival
      z[evalRows[i]] ~ dbern(survProb[evalRows[i]-1]) #Do or don't suvive to i
      survProb[evalRows[i]-1] <- phi[evalRows[i]-1] * z[evalRows[i]-1] 
    
    # Observation of live encounters
      encDATA[evalRows[i]] ~ dbern(obsProb[evalRows[i]])
    
      obsProb[evalRows[i]]<-p[evalRows[i]] * z[evalRows[i]] * proportionSampled[evalRows[i]]  #capture probability * logical alive * propSampled      
    }
    
  for(q in 1:nAliveRowsEval){
      alive[aliveRowsEval[q,1],
            aliveRowsEval[q,2],
            aliveRowsEval[q,3],
            aliveRowsEval[q,4]]<-
        sum(z[aliveRowArray[1:nAliveRows[aliveRowsEval[q,1],
                            aliveRowsEval[q,2],
                            aliveRowsEval[q,3],
                            aliveRowsEval[q,4]],
              aliveRowsEval[q,1],
              aliveRowsEval[q,2],
              aliveRowsEval[q,3],
              aliveRowsEval[q,4]]])
  }



    }" #model bracket
,file="~/simpleCJS/model.txt")