cat("
model{
  
  ############## Recapture model
  for(i in 1:nEvalRows){
    logit( p[ evalRows[i]+1 ] ) <- pBeta[ season[ evalRows[i]+1 ],year[ evalRows[i]+1 ],zRiv[ evalRows[ i ]+1 ] ]                                    
  }
  
  ############## Recapture priors
  
  for( s in 1:4 ){    
    for(y in 1:nYears){  
      for( r in 1:(nRivers+1) ){
        
        pBeta[ s,y,r ] ~ dnorm( 0,0.667 )
        
      }
    }
  }
  
  ############## Survival model
  for(i in 1:nEvalRows){
    logit( phi[ evalRows[i] ] ) <- phiBeta[ season[ evalRows[i] ],year[ evalRows[i] ],zRiv[ evalRows[ i ] ] ]
    
  }
  
  
  ############## survival priors
  for( s in 1:4 ){    
    for(y in 1:nYears){  
      for( r in 1:(nRivers+1) ){  
        
        phiBeta[ s,y,r ] ~ dnorm( 0,0.667 )
        
      }
    }
  }
  
  ############## Psi model
  for( i in 1:nEvalRows ){
    sumPsi[evalRows[i]]<-sum(ePsi[evalRows[i],])
    
    for( r2 in 1:nRivers ){
      # normal priors on logit
      lpsi[evalRows[i],r2] <- psiBeta[season[evalRows[i]],riverDATA[evalRows[i]],r2]     
      
      ePsi[evalRows[i],r2]<-exp(lpsi[evalRows[i],r2])*(1-(riverDATA[evalRows[i]]==r2))
      
      #Constrain each set of psi's to sum to one
      psi[evalRows[i],r2]<-( ePsi[evalRows[i],r2] / (1+sumPsi[evalRows[i]]) ) * ( 1-(riverDATA[evalRows[i]]==r2) )
      + ( 1 / (1+sumPsi[evalRows[i]]) )  *    (riverDATA[evalRows[i]]==r2)
    }
  }
  ############## Psi Priors
  for( s in 1:4 ) {
    for(r in 1:(nRivers)){
      for(r2 in 1:(nRivers)){
        
        psiBeta[s,r,r2]~dnorm(0,1/2.25)
        #psiSizeBetas[r,r2]~dnorm(0,1/2.25)
        
      }
    }
  }

  ############## Likelihood
  # Initial conditions:
  # 1) individuals enter the sample with probability 1
  # 2) individuals enter the sample alive, with probability 1
  for(i in 1:nFirstObsRows){
    z[ firstObsRows[i] ] <- 1
    zRiv[ firstObsRows[i] ] <- riverDATA[ firstObsRows[i] ] + 1
  }
  
  for(i in 1:nEvalRows){
    # State of survival
    z[ evalRows[i]+1 ] ~ dbern( survProb[ evalRows[i] ] ) #Do or don't suvive to i
    survProb[evalRows[i]] <-         phi[ evalRows[i] ] #^ ( intervalDays[ evalRows[i] ] * 4 / 365 ) 
    # z[ evalRows[i] ] 
    
    # State of location
    riverDATA[ evalRows[i]+1 ] ~ dcat( psi[ evalRows[i], ] )
    
    zRiv[evalRows[i]+1] <- riverDATA[ evalRows[i]+1 ]  
    * z[ evalRows[i]+1 ]  
    + 1
    
    # Observation of live encounters
    encDATA[ evalRows[i]+1 ] ~ dbern( obsProb[ evalRows[i]+1 ] )
    
    obsProb[ evalRows[i]+1 ]<-        p[ evalRows[i]+1 ]                 # Capture probability (calculated above).
    * z[ evalRows[i]+1 ]                 # Must be alive to be capturable.      
    #* availableDATA[ evalRows[i]+1 ]                 # Must be on the study site to be capturable.
  }
  
}" #model bracket
    ,file="~/simpleCJS/simpleCJS.txt")