# Function that calculates EVs for betting task
EVbet.calc <- function(Edges, hitMat, winM, Vertex, nSteps){
  EVdat <- data.table(vertex = Vertex, steps = 1:nSteps, EV = 0)
  for(leftSteps in 1:nSteps){
    EVdat[steps==leftSteps]$EV <- hitMat[vertex==Vertex & steps %in% 1:leftSteps, sum(goalprob)]*winM - (1 - hitMat[vertex==Vertex & steps %in% 1:leftSteps, sum(goalprob)])
  }
  return(EVdat)
}

Bet.gen <- function(Edges, nVisit, nTr, c.map, idmap.g, bt.map, parNum=1){
  experiment <- data.table(pp = 0, tr = 0, stepsleft = 0, v = 0, goal=0, trtype='init')
  
  for(tr in 1:nTr){
    # Sample a start node
    path <- sample(x=unique(unlist(Edges)), 1)
    
    # Determine starting cluster
    start.c <- names(sapply(idmap.g, function(m){path[1] %in% m})[which(sapply(idmap.g, function(m){path[1] %in% m}))])
    if(identical(start.c, character(0))){
      start.c <- names(sapply(bt.map, function(m){path[1] %in% m})[which(sapply(bt.map, function(m){path[1] %in% m}))])
    }
    
    # Sample a goal cluster and node from adjacent
    if(path[1] %in% unlist(idmap.g)){
      goal.c <- sample(c.map[sapply(idmap.g, function(m){path %in% m})][[1]],1) 
    }else if(path[1] %in% unlist(bt.map)){
      goal.c <- sample(c.map[sapply(bt.map, function(m){path %in% m})][[1]],1) 
    }
    goal <- sample(c(idmap.g[[goal.c]], bt.map[[goal.c]]), 1)
    
    # Determine trial goal type (deep node or bottleneck as goal) (bottleneck close or far)
    if(goal %in% unlist(idmap.g)){
      goaltype = 'deep'
    }else if(goal %in% unlist(bt.map) & bt.map[[goal.c]][which(c.map[[goal.c]]==start.c)]==goal){
      goaltype = 'goalclose'
    }else if(goal %in% unlist(bt.map) & !bt.map[[goal.c]][which(c.map[[goal.c]]==start.c)]==goal){
      goaltype = 'goalfar'
    }
    
    # Generate a path
    while(length(path) < nVisit & tail(path,1) != goal){
      path <- c(path, sample(Edges[[tail(path,1)]],size=1))
    }
    experiment <- rbind(experiment, data.table(pp=parNum, tr=tr, stepsleft=(nVisit-1):(nVisit-length(path)), v=path, goal=goal, trtype=goaltype))
  }
  return(experiment[-1,])
}

bet.sum <- function(vec, startP){
  cum <- rep(0, length(vec))
  cum[1] <- startP+vec[1]
  for(i in 2:length(vec)){
    cum[i] <- max(0, cum[i-1]+vec[i])
  }
  return(cum)
}

policyBet <- function(curV, goalV, goal.c, idmap.g, bt.map, c.map, idmap.d, idmap.bg, stopIdx){
  
  if(goalV %in% unlist(bt.map)){
    close.c <- c.map[[goal.c]][bt.map[[goal.c]]==goalV]
    far.c   <- c.map[[goal.c]][bt.map[[goal.c]]!=goalV]
    leave.c <- c('a','b','c','d')[!c('a','b','c','d') %in% c(goal.c,close.c,far.c)]
    
    newPol <- list(a = bt.map[[close.c]][c.map[[close.c]]!=goal.c],
                   b = idmap.g[[close.c]],
                   c = bt.map[[close.c]][c.map[[close.c]]==goal.c],
                   d = idmap.g[[goal.c]],
                   e = bt.map[[goal.c]][c.map[[goal.c]]==far.c],
                   f = bt.map[[far.c]][c.map[[far.c]]==goal.c],
                   g = idmap.g[[far.c]],
                   h = bt.map[[far.c]][c.map[[far.c]]!=goal.c],
                   i = bt.map[[leave.c]][c.map[[leave.c]]==far.c],
                   j = idmap.g[[leave.c]],
                   k = bt.map[[leave.c]][c.map[[leave.c]]==close.c]
    )
    return(stopIdx[vertex %in% idmap.bg[[names(which(sapply(newPol, function(m){curV %in% m})))]],V1])
  }else{
    lat.c <- names(which(sapply(c.map, function(m){goal.c %in% m})))
    leave.c <- c('a','b','c','d')[!c('a','b','c','d') %in% c(goal.c,lat.c)]
    
    newPol <- list(a = sapply(lat.c, function(m){bt.map[[m]][c.map[[m]]!=goal.c]}),
                   b = unlist(idmap.g[lat.c]),
                   c = sapply(lat.c, function(m){bt.map[[m]][c.map[[m]]==goal.c]}),
                   d = bt.map[[goal.c]],
                   e = idmap.g[[goal.c]][idmap.g[[goal.c]]!=goalV],
                   f = bt.map[[leave.c]],
                   g = idmap.g[[leave.c]])
    return(stopIdx[vertex %in% idmap.d[[names(which(sapply(newPol, function(m){curV %in% m})))]],V1])
  }
}

symmetry.get <- function(curV, goalV, goal.c, idmap.g, bt.map, c.map, idmap.d, idmap.bg){
  
  if(goalV %in% unlist(bt.map)){
    close.c <- c.map[[goal.c]][bt.map[[goal.c]]==goalV]
    far.c   <- c.map[[goal.c]][bt.map[[goal.c]]!=goalV]
    leave.c <- c('a','b','c','d')[!c('a','b','c','d') %in% c(goal.c,close.c,far.c)]
    
    newPol <- list(a = bt.map[[close.c]][c.map[[close.c]]!=goal.c],
                   b = idmap.g[[close.c]],
                   c = bt.map[[close.c]][c.map[[close.c]]==goal.c],
                   d = idmap.g[[goal.c]],
                   e = bt.map[[goal.c]][c.map[[goal.c]]==far.c],
                   f = bt.map[[far.c]][c.map[[far.c]]==goal.c],
                   g = idmap.g[[far.c]],
                   h = bt.map[[far.c]][c.map[[far.c]]!=goal.c],
                   i = bt.map[[leave.c]][c.map[[leave.c]]==far.c],
                   j = idmap.g[[leave.c]],
                   k = bt.map[[leave.c]][c.map[[leave.c]]==close.c]
    )
    return(names(which(sapply(newPol, function(m){curV %in% m}))))
  }else{
    lat.c <- names(which(sapply(c.map, function(m){goal.c %in% m})))
    leave.c <- c('a','b','c','d')[!c('a','b','c','d') %in% c(goal.c,lat.c)]
    
    newPol <- list(a = sapply(lat.c, function(m){bt.map[[m]][c.map[[m]]!=goal.c]}),
                   b = unlist(idmap.g[lat.c]),
                   c = sapply(lat.c, function(m){bt.map[[m]][c.map[[m]]==goal.c]}),
                   d = bt.map[[goal.c]],
                   e = idmap.g[[goal.c]][idmap.g[[goal.c]]!=goalV],
                   f = bt.map[[leave.c]],
                   g = idmap.g[[leave.c]])
    return(names(which(sapply(newPol, function(m){curV %in% m}))))  
  }
}

modchoice.get <- function(curV, start.c, goal.c, idmap.g, bt.map, noiseL){
  if(curV %in% c(bt.map[[start.c]], bt.map[[goal.c]], idmap.g[[start.c]], idmap.g[[goal.c]])){
    return(sample(c(0,1), prob=c(noiseL, 1-noiseL), size=1))
  }else if(!(curV %in% c(bt.map[[start.c]], bt.map[[goal.c]], idmap.g[[start.c]], idmap.g[[goal.c]]))){
    return(sample(c(0,1), prob=c(1-noiseL, noiseL), size=1))
  }else{
    stop('Logical condition not fulfilled')
  }
}

multichoice.get <- function(pp, ppES, EVR, Sl, modR, noiseL){
    choiceS <- rbinom(1, 1, prob = boot::inv.logit(ppES[pp,] %*% c(EVR,Sl,modR)))
    return(sample(c(choiceS,as.integer(!choiceS)), prob=c(1-noiseL, noiseL), size=1))
}

seq.SR.EV <- function(tr, v, goal, stepsleft, lr, gamm, ct, winM){
  SR.EV <- rep(NA,length(v)) # Initialize SR-based EVs that dynamically evolve as SR gets better learned
  tridx <- 1                 # Initialize indexing variable to fill SR.EV
  SR <- diag(20)             # Initialize SR as punctate base matrix
  for(trial in 1:max(tr)){
    # Store current trial state sequence
    episode <- v[tr==trial]
    # Get first state EV
    vt <- rep(-(ct^stepsleft[tridx]), 20); vt[goal[tridx]] <- winM
    SR.EV[tridx] <- (SR %*% vt)[episode[1]]
    tridx <- tridx+1
    for(experience in 2:length(episode)){
      I <- rep(0,20); I[episode[experience-1]] <- 1 # One-hot state t-1 (to be updated)
      er <- I + gamm * SR[episode[experience],] - SR[episode[experience-1],] # Get error of t-1 SR vs t SR
      SR[episode[experience-1],] = SR[episode[experience-1],] + lr*er # Update t-1 SR
      # Perform EV
      vt <- rep(-(ct^stepsleft[tridx]), 20); vt[goal[tridx]] <- winM
      SR.EV[tridx] <- (SR %*% vt)[episode[experience]]
      tridx <- tridx+1
    }
  }
  return(SR.EV)
}

seq.Lynn.EV <- function(tr, v, goal, stepsleft, betap, winM, loseM){
  library(matrixcalc)
  Lynn.EV <- rep(0,length(v))   # Initialize eventual predictions of EV
  tridx   <- 1                  # Initialize indexing variable to fill SR.EV
  ntilde  <- matrix(0,20,20)    # Initialize tally of transitions
  Ahat    <- matrix(0,20,20)    # Initialize empty estimated transition matrix
  
  for(tri in 1:max(tr)){
    vcur <- v[tr==tri]    # Select current episode of node experiences
    ## Perform EV estimate for first node
    # Get initial state vector
    I <- rep(0,20); I[vcur[1]] <- 1
    # Transition matrix with goal state as absorbing
    Ahat.goal <- Ahat
    Ahat.goal[goal[tridx],] = 0 
    hitVec <- rep(0,15) #vector of hitting chances
    for(s in stepsleft[tridx]:1){#maybe :1?
      hitC <- I %*% matrix.power(Ahat.goal, s)
      hitVec[s] <- hitC[goal[tridx]]
    }
    Lynn.EV[tridx] <- sum(hitVec)*winM + (1-sum(hitVec)) * loseM
    tridx <- tridx+1
    
    for(ep in 2:length(vcur)){
      ## Update ntilde and Ahat
      Pdt <- sapply((ep-1):1, function(dt){exp(-betap*dt) / sum(sapply((ep-1):1, function(sdt){exp(-betap*sdt)}))})
      for(epdt in 1:(ep-1)){
        ntilde[vcur[epdt],vcur[ep]] <- ntilde[vcur[epdt],vcur[ep]] + Pdt[epdt]
        Ahat[vcur[epdt],] <- ntilde[vcur[epdt],] / sum(ntilde[vcur[epdt],])
      }
      
      ## Get Lynn EV
      # Get initial state vector
      I <- rep(0,20); I[vcur[ep]] <- 1
      # Transition matrix with goal state as absorbing
      Ahat.goal <- Ahat
      Ahat.goal[goal[tridx],] = 0 
      hitVec <- rep(0,15) #vector of hitting chances
      for(s in stepsleft[tridx]:1){#maybe :1?
        hitC <- I %*% matrix.power(Ahat.goal, s)
        hitVec[s] <- hitC[goal[tridx]]
      }
      Lynn.EV[tridx] <- sum(hitVec)*winM + (1-sum(hitVec)) * loseM
      if(Lynn.EV[tridx] > 5){browser()}
      tridx <- tridx+1
    }
  }
  return(list(Ahat=Ahat, Lynn.EV=Lynn.EV))
}

get.opt.choice <- function(tr, v, goal, stepsleft, tMat, winM, loseM){
  opt.EV <- rep(0, length(v))
  for(toti in 1:length(v)){
    I <- rep(0,20); I[v[toti]]<-1
    if(stepsleft[toti]==0|v[toti]==goal[toti]){
      if(v[toti]==goal[toti]){opt.EV[toti]<-winM}else{opt.EV[toti]<-loseM}
    }else{
      hitVec <- rep(0,15)
      tMat.goal <- tMat
      tMat.goal[goal[toti],] <- 0
      for(s in stepsleft[toti]:1){
        hitC <- I %*% matrix.power(tMat.goal, s)
        hitVec[s] <- hitC[goal[toti]]
      }
      opt.EV[toti] <- sum(hitVec)*winM + (1-sum(hitVec)) * loseM
    }
  }
  opt.choice <- as.numeric(opt.EV>0)
  return(opt.choice)
}
