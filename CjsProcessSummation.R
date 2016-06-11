model{
  ############## Recapture model
  for(i in 1:nEvalRows){
    logit( p[ evalRows[i] ] ) <- pBeta[season[evalRows[i]],
                                       year[evalRows[i]],
                                       riverDATA[evalRows[i]],
                                       stageDATA[evalRows[i]]]
  }

  ############## Recapture priors

  for( s in 1:4 ){
    for(y in 1:nYears){
      for( r in 1:(nRivers) ){
        for(g in 1:2){
          pBeta[ s,y,r,g ] ~ dnorm( 0,0.667 )
        }
      }
    }
  }

  for(b in 1:4){
    for(r in 1:nRivers){
      for(g in 1:2){
        phiBeta[b,r,g]~dnorm(0,0.667)
      }
    }
  }

  for(t in 1:nTimes){
    for(r in 1:nRivers){
      for(g in 1:2){
        logitPhi[t,r,g]<-phiBeta[1,r,g]+
          phiBeta[2,r,g]*flowDATA[t]+phiBeta[3,r,g]*flowDATA[t]^2+
          phiBeta[4,r,g]*tempDATA[t]
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