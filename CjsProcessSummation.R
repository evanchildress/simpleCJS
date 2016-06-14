model{
  ############## Recapture model
  for(i in 1:nEvalRows){
    logit( p[ evalRows[i] ] ) <- pBeta[1,
                                       season[evalRows[i]],
                                       riverDATA[evalRows[i]],
                                       stageDATA[evalRows[i]]]+
                                  pBeta[2,
                                        season[evalRows[i]],
                                        riverDATA[evalRows[i]],
                                        stageDATA[evalRows[i]]]*
                                  flowForP[evalRows[i]]
      
  }

  ############## Recapture priors
for(b in 1:2){
  for( s in 1:4 ){
      for( r in 1:(nRivers) ){
        for(g in 1:2){
          pBeta[ b,s,r,g ] ~ dnorm( 0,0.667 )
        }
      }
    }
  }

  for(b in 1:8){
    for(r in 1:nRivers){
      for(g in 1:2){
        phiBeta[b,r,g]~dnorm(0,0.667)
      }
    }
  }

  for(i in 1:nEvalRows){
    for(t in time[i-1]:time[i]){
      logitPhi[i,t]<-phiBeta[1,riverDATA[i]]+
                     phiBeta[2,riverDATA[i]]*flowDATA[t,riverDATA[i]]+
                     phiBeta[3,riverDATA[i]]*flowDATA[t,riverDATA[i]]^2+
                     phiBeta[4,riverDATA[i]]*tempDATA[t,riverDATA[i]]+
                     phiBeta[5,riverDATA[i]]*lengthDATA[i]+
                     phiBeta[6,riverDATA[i]]*lengthDATA[i]*flowDATA[t,riverDATA[i]]+
                     phiBeta[7,riverDATA[i]]*lengthDATA[i]*flowDATA[t,riverDATA[i]]^2
    }
    survProb[i]<-1/(1+exp(-prod(logitPhi[i,time[i-1]:time[i]])))
  }
  
  for(t in 1:nTimes){
    for(r in 1:nRivers){
      for(g in 1:2){
        logitPhi[t,r,g]<-phiBeta[1,r,g]+
          phiBeta[2,r,g]*flowDATA[t,r]+phiBeta[3,r,g]*flowDATA[t,r]^2+
          phiBeta[4,r,g]*tempDATA[t,r]+phiBeta[5,r,g]*tempDATA[t,r]^2+
          phiBeta[6,r,g]*lengthDATA[]
        phi[t,r,g]<-1/(1+exp(-logitPhi[t,r,g]))
      }
    }
  }

  for(i in 1:nEvalRows){
    # State of survival
    z[ evalRows[i] ] ~ dbern( survProb[ evalRows[i] ] ) #Do or don't suvive to i
    survProb[evalRows[i]] <-         prod(phi[time[evalRows[i]-1]:time[evalRows[i]],
                                              riverDATA[evalRows[i]-1],
                                              stageDATA[evalRows[i]-1]])*
                                            z[ evalRows[i]-1 ]

    # Observation of live encounters
    encDATA[ evalRows[i] ] ~ dbern( obsProb[ evalRows[i] ] )

    obsProb[ evalRows[i] ]<-        p[ evalRows[i]] * z[ evalRows[i] ]
    #* availableDATA[ evalRows[i]+1 ]                 # Must be on the study site to be capturable.
  }
}