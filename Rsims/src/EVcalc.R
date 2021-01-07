# Function that ouptputs a Data Frame of expected values per state-step conjunction
EVcalc <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, hitMat=F, goalstepchance=F, inspect.Edges=F){
  
  # Brute-force a full HitMat if not provided (add later)
  if(!class(hitMat) %in% c( 'matrix', 'data.frame')){
    stop('You have not provided a hit-matrix. Currently the flexible calculation of a hit-matrix is not supported by this function.')
  }
  
  # Brute-force the chances of hitting from the starting position if not provided (for now dependent on Schapiro)
  if(class(goalstepchance) != 'numeric'){
    print('No information is provided about goal-attainment from starting position. Calculated using Schapiro-style layout.')
    # Get starting expectations
    propStart <- rep(0,length(1:(nSteps+1)))
    for(nV in 1:(nSteps+1)){
      paths   <- 0
      queue   <- list(c(0,1))
    
      while(length(queue) > 0){
        pCur <- queue[[1]]
        for(t in Edges[[tail(pCur,1)]]){
          if(t!= tail(pCur,2)[1] & length(c(pCur,t))<(nV+1)){         #No-backtrack
            #if(length(c(pCur,t))<nV){                              #Backtrack
            queue <- append(queue,list(c(pCur,t)))
          }else if(t!= tail(pCur,2)[1] & length(c(pCur,t))==(nV+1)){  #No-backtrack
            #}else if(length(c(pCur,t))==nV){                       #Backtrack
            paths <- paths+1
            if(vGoal == t & !vGoal %in% pCur){
              propStart[nV] <- propStart[nV]+1
            }
          }
        }
        queue[[1]] <- NULL
      }
    }
    propStart <- propStart[-1]
    goalstepchance <- rep(0,10)
    for(nSteps in 1:nSteps){
      goalstepchance[nSteps] <- propStart[nSteps]/(4*3^(nSteps-1))
    }
  }
  
  # Check if we know relevant edges to inspect
  if(inspect.Edges==F & !class(hitMat) %in% c('matrix','data.frame')){
    stop('You must either provide a hit-matrix containing the edges you wish to inspect as row names, or a list of Edges you wish to inspect.')
  }
  
  # Check if edges are legal in provided graph (edge list)
  
  EVmat <- matrix(nrow=nrow(hitMat.nBT), ncol=ncol(hitMat.nBT))
  rownames(EVmat) <- rownames(hitMat.nBT)
  colnames(EVmat) <- colnames(hitMat.nBT)
  for(leftSteps in 1:ncol(hitMat.nBT)){
    for(transid in rownames(hitMat.nBT)){
      intRew <- 0
      goalProb.s <- rep(0,leftSteps)
      for(s in 1:leftSteps){
        goalProb.s[s] <- hitMat.nBT[transid,s]/(3^s)
        intRew <- intRew + goalProb.s[s] * (gRew-(sCost*s))
      }
      EV <- intRew - (1-sum(goalProb.s))*sCost*leftSteps
      EVmat[transid,leftSteps] <- EV
    }
  }
  
  EVdat <- melt(EVmat, varnames=c('trans','sLeft'))
  EVdat <- rbind(EVdat, data.frame(trans='1<-|',sLeft=10,value=sum(goalstepchance * (gRew-(c(1:10)*sCost))) - ((1-sum(goalstepchance))*(10*sCost))))
  EVdat$trans <- factor(EVdat$trans, levels=c(as.character(EVdat[EVdat$sLeft==9,][order(-EVdat[EVdat$sLeft==9,]$value),]$trans),'1<-|'), ordered=T)
  return(EVdat)
}

EVcalc.nBT <- function(Edges, curV, preV, vGoal, nSteps, gRew, sCost, hitMat=F){
  if(!class(hitMat) %in% 'data.frame'){
    hitMat <- hitMat.calc.nBT(Edges=Edges, vGoal=vGoal, nSteps=nSteps, curV=curV, preV=preV, totP=3^nStep)
  }
  if(nSteps != max(hitMat$steps)){
    stop('Wrong hitmat specification')
  }
  EVdat <- data.frame(preVertex = preV, Vertex = curV, steps=1:nSteps, EV=0)
  
  for(leftSteps in 1:nSteps){
    intRew <- (1-sum(hitMat[hitMat$preVertex==preV & hitMat$Vertex==curV & hitMat$steps<=leftSteps,]$goalprob)) * (-sCost * leftSteps)
    for(i in 1:leftSteps){
      intRew <- intRew + hitMat[hitMat$preVertex==preV & hitMat$Vertex==curV & hitMat$steps==i,]$goalprob * (gRew - sCost*i) 
    }
    EVdat[EVdat$preVertex==preV & EVdat$Vertex==curV & EVdat$steps==leftSteps,]$EV = intRew
  }
  return(EVdat)
}










