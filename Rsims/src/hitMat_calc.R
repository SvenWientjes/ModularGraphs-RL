# Function that calculates the hit matrix from a graph for the desired transitions
hitMat.calc <- function(Edges, vGoal, nSteps, inspectVs, totP){
  hitMat <- data.frame(vertex = rep(inspectVs, each=nSteps), steps = rep(1:nSteps, length(inspectVs)), goalprob = 0)
  
  for(V in inspectVs){
    queue   <- list(V)
    while(length(queue) > 0){
      pCur <- queue[[1]]
      if(length(pCur) == nSteps & vGoal %in% Edges[[tail(pCur,1)]]){
        hitMat[hitMat$vertex==V & hitMat$steps==length(pCur),]$goalprob <- hitMat[hitMat$vertex==V & hitMat$steps==length(pCur),]$goalprob + 4^( nSteps - length(pCur) )
      }else if(length(pCur) < nSteps){
        for(t in Edges[[tail(pCur,1)]]){
          if(t == vGoal){
            hitMat[hitMat$vertex==V & hitMat$steps==length(pCur),]$goalprob <- hitMat[hitMat$vertex==V & hitMat$steps==length(pCur),]$goalprob + 4^( nSteps - length(pCur) ) #Length pCur works because the addition of t to the path compensates for the fact that we have one more state vs transition
          }else{
            queue <- append(queue, list(c(pCur,t)))
          }
        }
      }
      queue[[1]] <- NULL
    }
  }
  hitMat$goalprob <- hitMat$goalprob / totP
  return(hitMat)
}

MC.hitMat <- function(Edges, vStart, vGoal, nSteps, nSamp){
  hitMat <- data.frame(goal='init', nSteps=0)
  for(r in 1:nSamp){
    path <- c(vStart)
    while(length(path) < (nSteps+1)){
      path <- c(path, sample(Edges[[tail(path,1)]], 1))
      if(tail(path,1)==vGoal){
        hitMat <- rbind(hitMat, data.frame(goal='yes', nSteps=(length(path)-1)))
        break
      }else if(length(path)==(nSteps+1)){
        hitMat <- rbind(hitMat, data.frame(goal='no', nSteps=(length(path)-1)))
      }
    }
  }
  return(hitMat)
}

hitMat.calc.nBT <- function(Edges, vGoal, nSteps, curV, preV, totP){
  # nSteps here identifies the total steps AFTER the current event
  #   The first step from preV -> curV is not counted!
  hitMat <- data.frame(preVertex = preV, Vertex = curV, steps = 1:nSteps, goalprob = 0)
  queue <- list(c(preV,curV))
  while(length(queue)>0){
    pCur <- queue[[1]]
    if(length(pCur) == (nSteps+1) & vGoal %in% Edges[[tail(pCur,1)]]){
      hitMat[hitMat$preVertex==preV & hitMat$Vertex==curV & hitMat$steps==(length(pCur)-1),]$goalprob <- hitMat[hitMat$preVertex==preV & hitMat$Vertex==curV & hitMat$steps==(length(pCur)-1),]$goalprob + 3^( nSteps - length(pCur) + 1)
    }else if(length(pCur) < (nSteps+1)){
      for(t in Edges[[tail(pCur,1)]][-which(Edges[[tail(pCur,1)]]==tail(pCur,2)[1])]){
        if(t == vGoal){
          hitMat[hitMat$preVertex==preV & hitMat$Vertex==curV & hitMat$steps==(length(pCur)-1),]$goalprob <- hitMat[hitMat$preVertex==preV & hitMat$Vertex==curV & hitMat$steps==(length(pCur)-1),]$goalprob + 3^( nSteps - length(pCur) + 1)
        }else{
          queue <- append(queue, list(c(pCur,t)))
        }
      }
    }
    queue[[1]] <- NULL
  }
  hitMat$goalprob <- hitMat$goalprob / totP
  return(hitMat)
}
