cat("
    model{
    
    ############## Recapture model
    for(i in 1:nEvalRows){
    logit( p[ evalRows[i]+1 ] ) <- pBeta[ season[ evalRows[i]+1 ],year[ evalRows[i]+1 ],riverDATA[ evalRows[ i ]+1 ] ]                                    
    }
    
    ############## Recapture priors
    
    for( s in 1:4 ){    
    for(y in 1:nYears){  
    for( r in 1:(nRivers) ){
    
    pBeta[ s,y,r ] ~ dnorm( 0,0.667 )
    
    }
    }
    }
    
    ############## Survival model
    for(i in 1:nEvalRows){
    logit( phi[ evalRows[i] ] ) <- phiBeta[ season[ evalRows[i] ],year[ evalRows[i] ],
    riverDATA[ evalRows[ i ] ] ]
    
    }
    
    
    ############## survival priors
    for( s in 1:4 ){    
    for(y in 1:nYears){  
    for( r in 1:(nRivers) ){  
    
    phiBeta[ s,y,r ] ~ dnorm( 0,0.667 )
    
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
    survProb[evalRows[i]] <-         phi[ evalRows[i] ] #^ ( intervalDays[ evalRows[i] ] * 4 / 365 ) 
    * z[ evalRows[i] ] 
    
    # Observation of live encounters
    encDATA[ evalRows[i]+1 ] ~ dbern( obsProb[ evalRows[i]+1 ] )
    
    obsProb[ evalRows[i]+1 ]<-        p[ evalRows[i]+1 ]                 # Capture probability (calculated above).
    * z[ evalRows[i]+1 ]                 # Must be alive to be capturable.      
    #* availableDATA[ evalRows[i]+1 ]                 # Must be on the study site to be capturable.
    }
    
    }" #model bracket
,file="~/simpleCJS/verySimpleCJS.txt")