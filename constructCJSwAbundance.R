cat("
    model{
    
    ############## Recapture model
    for(i in 1:nEvalRows){
      logit( p[ evalRows[i]+1 ] ) <- pBeta[ season[ evalRows[i]+1 ],
                                          riverDATA[ evalRows[ i ]+1 ],
                                          stage[ evalRows[ i ]+1 ]] *
                                     sampleFlow[evalRows[i]+1]
    }
    
    ############## Recapture priors
    for(b in 1:2){ #betas: 1=intercept,2=slope with discharge during sampling
      for(s in 1:4 ){    #season
        for(r in 1:nRivers{#river
          for(g in 1:2){ #stage
            pBeta[b,s,r,g] ~ dnorm(0,0.667)
          }
        }
      }
    }
    
    
    ############## Survival model
    for(i in 1:nEvalRows){
      logit( phi[ evalRows[i] ] ) <- phiBeta[ season[ evalRows[i] ],year[ evalRows[i] ],
      riverDATA[ evalRows[ i ] ], stage[evalRows[i]] ]
    }
    
    
    ############## survival priors
    for( s in 1:4 ){    
      for(y in 1:nYears){  
        for( r in 1:(nRivers) ){
          for(g in 1:2){
            phiBeta[ s,y,r,g ] ~ dnorm( 0,0.667 )
          }
        }
      }
    }
    
    
    ############## Likelihood
    # Initial conditions:
    # 1) individuals enter the sample with probability 1
    # 2) individuals enter the sample alive, with probability 1
    for(i in 1:nFirstObsRows){
      z[ firstObsRows[i] ] <- 1
    }
    
    for(i in 1:nEvalRows){
    # State of survival
      z[ evalRows[i]+1 ] ~ dbern( survProb[ evalRows[i] ] ) #Do or don't suvive to i
      survProb[evalRows[i]] <-phi[ evalRows[i] ] * z[ evalRows[i] ] 
    
    # Observation of live encounters
      encDATA[ evalRows[i]+1 ] ~ dbern( obsProb[ evalRows[i]+1 ] )
    
      obsProb[ evalRows[i]+1 ]<-p[ evalRows[i]+1 ] * z[ evalRows[i]+1 ]   capture probability times logical alive      
    #* availableDATA[ evalRows[i]+1 ]                 # Must be on the study site to be capturable.
    }
    

  for(r in 1:nRivers){
    for(s in 1:nSamples){
      for(g in 1:2){
        for(n in 1:nSampleRows[s,r,g]){
          alive[n,s,r,g]<-sum(z[sampleRows[1:nSampleRows[s,r,g],s,r,g]])
        }
      }
    }
  }

    }" #model bracket
,file="~/simpleCJS/model.txt")