# Function that calculates the hit matrix from a graph for the desired transitions
hitMat.calc <- function(Edges, vGoal, nSteps, inspectVs, totP){
  hitMat <- data.frame(vertex = rep(inspectVs, each=nSteps), steps = rep(1:nSteps, length(inspectVs)), goalprob = 0)
  
  for(V in inspectVs){
    queue   <- list(V)
    while(length(queue) > 0){
      pCur <- queue[[1]]
      if(length(pCur) == nSteps & vGoal %in% Edges[[tail(pCur,1)]]){
        hitMat[hitMat$vertex==V & hitMat$steps==length(pCur),]$goalprob <- hitMat[hitMat$vertex==V & hitMat$steps==length(pCur),]$goalprob + length(Edges[[tail(pCur,1)]])^( nSteps - length(pCur) )
      }else if(length(pCur) < nSteps){
        for(t in Edges[[tail(pCur,1)]]){
          if(t == vGoal){
            hitMat[hitMat$vertex==V & hitMat$steps==length(pCur),]$goalprob <- hitMat[hitMat$vertex==V & hitMat$steps==length(pCur),]$goalprob + length(Edges[[tail(pCur,1)]])^( nSteps - length(pCur) ) #Length pCur works because the addition of t to the path compensates for the fact that we have one more state vs transition
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

# Function that return posterior mean for sampled paths over graph
MC.hitMat <- function(Edges, vStart, vGoal, nSteps, nSamp){
  MCounter <- rep(0, nSteps+1)
  for(r in 1:nSamp){
    path <- c(vStart)
    while(length(path) < (nSteps+1)){
      path <- c(path, sample(Edges[[tail(path,1)]],1))
      if(tail(path,1)==vGoal){
        MCounter[length(path)-1] <- MCounter[length(path)-1]+1
        break
      }else if(length(path)==(nSteps+1)){
        MCounter[length(MCounter)] <- MCounter[length(MCounter)]+1
      }
    }
  }
  hitMat <- data.frame(Vertex=vStart, steps=1:nSteps, goalprob=(MCounter/sum(MCounter))[-length(MCounter)])
  return(hitMat)
}

# Function that can return multiple hitMats sampled from a Dirichlet constructed from sampled paths over the graph
Bayes.hitMat <- function(Edges, vStart, vGoal, nSteps, nSamp, nPost){
  hitMat     <- data.frame(Vertex=0, steps=0, goalprob=0)
  dirCounter <- rep(0, nSteps+1)
  for(r in 1:nSamp){
    path <- c(vStart)
    while(length(path) < (nSteps+1)){
      path <- c(path, sample(Edges[[tail(path,1)]],1))
      if(tail(path,1)==vGoal){
        dirCounter[length(path)-1] <- dirCounter[length(path)-1]+1
        break
      }else if(length(path)==(nSteps+1)){
        dirCounter[length(dirCounter)] <- dirCounter[length(dirCounter)]+1
      }
    }
  }
  postList <- list()
}

hitMat.calc.nBT <- function(Edges, vGoal, nSteps, curV, preV, totP){
  # nSteps here identifies the total steps AFTER the current event
  #   The first step from preV -> curV is not counted!
  hitMat <- data.frame(preVertex = preV, Vertex = curV, steps = 1:nSteps, goalprob = 0)
  queue <- list(c(preV,curV))
  while(length(queue)>0){
    pCur <- queue[[1]]
    if(length(pCur) == (nSteps+1) & vGoal %in% Edges[[tail(pCur,1)]]){
      hitMat[hitMat$preVertex==preV & hitMat$Vertex==curV & hitMat$steps==(length(pCur)-1),]$goalprob <- hitMat[hitMat$preVertex==preV & hitMat$Vertex==curV & hitMat$steps==(length(pCur)-1),]$goalprob + (length(Edges[[tail(pCur,1)]])-1)^( nSteps - length(pCur) + 1)
    }else if(length(pCur) < (nSteps+1)){
      for(t in Edges[[tail(pCur,1)]][-which(Edges[[tail(pCur,1)]]==tail(pCur,2)[1])]){
        if(t == vGoal){
          hitMat[hitMat$preVertex==preV & hitMat$Vertex==curV & hitMat$steps==(length(pCur)-1),]$goalprob <- hitMat[hitMat$preVertex==preV & hitMat$Vertex==curV & hitMat$steps==(length(pCur)-1),]$goalprob + (length(Edges[[tail(pCur,1)]])-1)^( nSteps - length(pCur) + 1)
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

MC.hitMat.nBT <- function(Edges, vGoal, nSteps, curV, preV, nSamp){
  MCounter <- rep(0, nSteps+1)
  for(r in 1:nSamp){
    path <- c(preV, curV)
    while(length(path) < (nSteps+2)){
      path <- c(path, sample(Edges[[tail(path,1)]][-which(Edges[[tail(path,1)]]==tail(path,2)[1])],1))
      if(tail(path,1)==vGoal){
        MCounter[length(path)-2] <- MCounter[length(path)-2]+1
        break
      }else if(length(path)==(nSteps+2)){
        MCounter[length(MCounter)] <- MCounter[length(MCounter)]+1
      }
    }
  }
  hitMat <- data.frame(preVertex=preV, Vertex=curV, steps=1:nSteps, goalprob=(MCounter/sum(MCounter))[-length(MCounter)])
  return(hitMat)
}
