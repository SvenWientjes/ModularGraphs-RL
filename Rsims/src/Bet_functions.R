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






