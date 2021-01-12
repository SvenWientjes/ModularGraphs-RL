# Function that ouptputs a Data Frame of expected values per state-step conjunction
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

EVcalc <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, hitMat, Vertex){
  EVdat <- data.frame(vertex=Vertex, steps=1:nSteps, EV=0)
  for(leftSteps in 1:nSteps){
    intRew <- (1-sum(hitMat[hitMat$vertex==Vertex & hitMat$steps<=leftSteps,]$goalprob)) * (-sCost*leftSteps)
    for(i in 1:leftSteps){
      intRew <- intRew + hitMat[hitMat$vertex==Vertex & hitMat$steps==i,]$goalprob * (gRew-sCost*i)
    }
    EVdat[EVdat$vertex==Vertex & EVdat$steps==leftSteps,]$EV <- intRew
  }
  return(EVdat)
}








