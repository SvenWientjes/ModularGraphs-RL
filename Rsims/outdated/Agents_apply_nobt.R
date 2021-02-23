## Functions for applying a strategy to an experiment (set of pregenerated trajectories)
# Kind of only works for QuadSchap no backtrack...? -> assumes cluster a is start, cluster b is goal // '8' is goal, '2' is start
# nSteps is assumed 15 for now!
OS.apply.QS <- function(experiment, piMat, idmap.g, c.map, bt.map, gRew, sCost, parNum){
  AG.dat <- data.frame(pp=parNum, trial=1:max(experiment$tr), trRew=0, nSteps=0, endV=0, totRew=0, strat='OS')
  totRew <- 0
  for(tr in unique(experiment$tr)){
    goal <- experiment[experiment$tr==tr & experiment$step==0, 'goal']
    path <- experiment[experiment$tr==tr, 'v']
    
    # Recode policy matrix into proper start/goal permutation
    pi.t <- data.frame(vertex=1:max(experiment$v), stopid = 14)
    start.c <- sapply(idmap.g, function(m){head(path,1) %in% m})
    goal.c  <- names(idmap.g[sapply(idmap.g, function(m){goal %in% m})])
    
    # Deep nodes in goal cluster
    pi.t[pi.t$vertex %in% idmap.g[[goal.c]],'stopid'] <- piMat[piMat$vertex==7,'stopid']
    pi.t <- pi.t[!pi.t$vertex==goal,]
    
    # Bottlenecks in goal cluster
    pi.t[pi.t$vertex %in% bt.map[[goal.c]],'stopid'] <- piMat[piMat$vertex==6,'stopid']
    
    # Bottlenecks toward goal cluster
    pi.t[pi.t$vertex %in% unlist(mapply(function(x,y){x[y]}, bt.map, sapply(c.map, function(m){which(m==goal.c)}))),'stopid'] <- piMat[piMat$vertex==5,'stopid']
    
    # Deep nodes in side clusters
    pi.t[pi.t$vertex %in% unlist(idmap.g[c.map[[goal.c]]]),'stopid'] <- piMat[piMat$vertex==2,'stopid']
    
    # Bottlenecks toward opposite cluster
    
    # Bottlenecks within opposite cluster
    
    # Deep nodes in opposite cluster
    
    # Get AG data
    if(path[min(which(path==goal), which(!sapply(path, function(i){pi.t[pi.t$vertex==i,'stopid']}) < c(15:0)[1:length(path)]))]==goal){
      trRew  <- gRew - sCost*(which(path==goal)-1)
      totRew <- totRew + trRew
      AG.dat[AG.dat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=which(path==goal)-1, endV=goal, totRew=totRew, strat='OS')
    }else{
      trRew  <- -sCost*(min(which(path==goal), which(!sapply(path, function(i){pi.t[pi.t$vertex==i,'stopid']}) < c(15:0)[1:length(path)]))-1)
      totRew <- totRew + trRew
      AG.dat[AG.dat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(min(which(path==goal), which(!sapply(path, function(i){pi.t[pi.t$vertex==i,'stopid']}) < c(15:0)[1:length(path)]))-1), endV=path[min(which(path==goal), which(!sapply(path, function(i){pi.t[pi.t$vertex==i,'stopid']}) < c(15:0)[1:length(path)]))], totRew=totRew, strat='OS')
    }
    
  }
  return(AG.dat)
}

MS.apply.QS <- function(experiment, bt.map, c.map, idmap.g, gRew, sCost, parNum){
  AG.dat <- data.frame(pp=parNum, trial=1:max(experiment$tr), trRew=0, nSteps=0, endV=0, totRew=0, strat='MS')
  totRew <- 0
  for(tr in unique(experiment$tr)){
    goal <- experiment[experiment$tr==tr & experiment$step==0, 'goal']
    path <- experiment[experiment$tr==tr, 'v']
    
    # Find start and goal associated cluster
    start.c <- names(idmap.g[sapply(idmap.g, function(m){head(path,1) %in% m})])
    goal.c  <- names(idmap.g[sapply(idmap.g, function(m){goal %in% m})])
    
    # Find leaving cluster transitions
    modTrans <- c(bt.map[[start.c]][which(c.map[[start.c]] != goal.c)],bt.map[[c.map[[start.c]][c.map[[start.c]] != goal.c]]][which(c.map[[c.map[[start.c]][c.map[[start.c]] != goal.c]]]==start.c)])
    modTrans <- rbind(modTrans, t(rbind(bt.map[[goal.c]], sapply(c.map[[goal.c]], function(m){bt.map[[m]][which(c.map[[m]]==goal.c)]}))))
    
    for(p in 2:length(path)){
      if(path[p]==goal){
        trRew <- gRew - sCost*(p-1)
        totRew <- totRew + trRew
        AG.dat[AG.dat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=which(path==goal)-1, endV=goal, totRew=totRew, strat='MS')
        break
      }else if( (T %in% apply(modTrans, 1, function(x){identical(x,c(path[p-1],path[p]))})) | (p==length(path))){
        trRew <- -sCost * (p-1)
        totRew <- totRew + trRew
        AG.dat[AG.dat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=p-1, endV=path[p], totRew=totRew, strat='MS')
        break
      }
    }
  }
  return(AG.dat)
}

LW.apply.QS <- function(experiment, gRew, sCost, parNum){
  AG.dat <- data.frame(pp=parNum, trial=1:max(experiment$tr), trRew=0, nSteps=0, endV=0, totRew=0, strat='LW')
  totRew <- 0
  for(tr in unique(experiment$tr)){
    goal <- experiment[experiment$tr==tr & experiment$step==0, 'goal']
    path <- experiment[experiment$tr==tr, 'v']
    
    if(goal %in% path){
      trRew <- gRew - sCost * (which(path==goal)-1)
      totRew <- totRew + trRew
      AG.dat[AG.dat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=which(path==goal)-1, endV=goal, totRew=totRew, strat='LW')
    }else if(!goal %in% path){
      trRew <- -sCost*(length(path)-1)
      totRew <- totRew + trRew
      AG.dat[AG.dat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=length(path)-1, endV=tail(path,1), totRew=totRew, strat='LW')
    }
  }
  return(AG.dat)
}


