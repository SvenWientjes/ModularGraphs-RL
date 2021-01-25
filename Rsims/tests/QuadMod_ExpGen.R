############################################################################################################################
################## Generating different 100-block experiments with similar reward characteristics ##########################
############################################################################################################################
library(foreach)

# Load Functions from /src/
sapply(paste0('src/',list.files('src/')), source)

# Get different Defining Parameters
nSteps <- 15   # Nr of maximum steps in a miniblock (hitMat and EVcalc will use nSteps-1; agent simulations will use nSteps!)
gRew   <- 65   # Reward upon reaching vGoal
sCost  <- 1    # Points detracted from accumulated reward for each taken step

# Get parameters for agentic simulations
nPP <- 500
nTr <- 100

#Quad Schapiro-style edge matrix
Edges <- list(c(2, 3, 4, 20),
              c(1, 3, 4, 5),
              c(1, 2, 4, 5),
              c(1, 2, 3, 5),
              c(2, 3, 4, 6),
              c(5, 7, 8, 9),
              c(6, 8, 9, 10),
              c(6, 7, 9, 10),
              c(6, 7, 8, 10),
              c(7, 8, 9, 11),
              c(10,12,13,14),
              c(11,13,14,15),
              c(11,12,14,15),
              c(11,12,13,15),
              c(12,13,14,16),
              c(15,17,18,19),
              c(16,18,19,20),
              c(16,17,19,20),
              c(16,17,18,20),
              c(17,18,19, 1))
Edge.list <- foreach(i = 1:length(Edges), .combine=rbind) %do% {expand.grid(i, Edges[[i]])}

# Describe unique nodes for QuadSchap
inspect.Vertices <- c(1, 2, 5, 6, 7, 16, 17)

# Describe identical nodes for QuadSchap
idmap   <- list(a = c(1,15), b = c(2,3,4, 12,13,14), c = c(5,11), d = c(6,10), e = c(7,9), f = c(16,20), g = c(17,18,19))
idmap.g <- list(a = c(2,3,4), b = c(7,8,9), c = c(12,13,14), d = c(17,18,19))
c.map   <- list(a = c('d','b'), b = c('a', 'c'), c = c('b','d'), d=c('a','c'))
bt.map  <- list(a = c(1,5), b = c(6,10), c= c(11,15), d=c(20,16))

# Get the hitMat
hitMat <- read.csv('data/hitMat_quadSchap_step15.csv', row.names=1)

# Get expected hits in 1 exp
nHit <- ceiling(nTr * sum(hitMat[hitMat$vertex==2,'goalprob']))

# Get the Expected Values for each interaction of previous & current node, conditional upon steps left
EVmat <- foreach(v = inspect.Vertices, .combine=rbind) %do% {
  tempMat <- EVcalc(Edges=Edges, vGoal=vGoal, nSteps=nSteps-1, gRew=gRew, sCost=sCost, hitMat=hitMat, Vertex=v) 
  tempMat
}

piMat <- policy.generate(Edges=Edges, EVmat=EVmat, idmap=idmap)

# Actually generate experiments
full.exp <- data.frame(pp = 0, tr = 0, step = 0, v = 0, goal=0)
for(p in 1:nPP){
  cur.exp <- data.frame(pp=p, tr=0, step=0, v=0, goal=0)
  edge.eval <- rep(0, nrow(Edge.list))
  while( (abs(sum(cur.exp$v == cur.exp$goal)-nHit) > floor(0.2*nHit)) | !(floor(sum(edge.eval)/length(edge.eval))-min(edge.eval) < floor(0.5*sum(edge.eval)/length(edge.eval))) ){
    cur.exp <- data.frame(pp=p, tr=0, step=0, v=0, goal=0)
    edge.eval <- rep(0, nrow(Edge.list))
    for(t in 1:nTr){
      # Select a goal node
      goal <- sample(c(2,3,4,7,8,9,12,13,14,17,18,19),1)
      
      # Select an eligible starting node
      path <- sample(sample(idmap.g[c.map[sapply(idmap.g, function(m){goal %in% m})][[1]]], 1)[[1]],1)
      
      # Track nSteps
      stept <- 0
      
      while(stept < nSteps & (tail(path,1) != goal)){
        path <- c(path, sample(Edges[[tail(path,1)]], 1))
        stept <- stept + 1
        edge.eval[which(apply(Edge.list, 1, function(i){identical(as.numeric(i), tail(path,2))}))] <- edge.eval[which(apply(Edge.list, 1, function(i){identical(as.numeric(i), tail(path,2))}))]+1
      }
      cur.exp <- rbind(cur.exp, data.frame(pp=p, tr=t, step=0:stept, v=path, goal=goal))
    }
  }
  full.exp <- rbind(full.exp, cur.exp)
}
full.exp <- full.exp[!full.exp$tr==0,]

# Evaluate experiment under OS and MS
AG.dat <- data.frame(pp=0, trial=0, trRew=0, nSteps=0, endV=0, totRew=0, strat='init')
for(p in unique(full.exp$pp)){
  totRew.lw <- 0
  trRew.lw  <- 0
  totRew.os <- 0
  trRew.os  <- 0
  totRew.ms <- 0
  trRew.ms  <- 0
  for(tr in unique(full.exp$tr)){
    g <- full.exp[full.exp$pp==p & full.exp$tr==tr &full.exp$step==0, 'goal']
    path <- full.exp[full.exp$pp==p & full.exp$tr==tr,'v']
    
    pi.t <- data.frame(vertex = 1:20, stopid = 14)
    
    # Get same cluster deep nodes
    pi.t[pi.t$vertex%in%idmap.g[sapply(idmap.g, function(m){g %in% m})][[1]],'stopid'] <- piMat[piMat$vertex==7,'stopid']
    # Get same cluster exit bt nodes
    pi.t[pi.t$vertex %in% bt.map[sapply(idmap.g, function(m){g %in% m})][[1]],'stopid'] <- piMat[piMat$vertex==6,'stopid']
    # Get side cluster deep nodes
    pi.t[pi.t$vertex %in% c(sapply(idmap.g[c.map[sapply(idmap.g, function(m){g %in% m})][[1]]],c)), 'stopid'] <- piMat[piMat$vertex==2, 'stopid']
    # Get side cluster to goal bt nodes
    pi.t[pi.t$vertex %in% bt.map[sapply(c.map, function(i){names(which(sapply(idmap.g, function(m){g %in% m}))) %in% i})][[1]][which(c.map[sapply(c.map, function(i){names(which(sapply(idmap.g, function(m){g %in% m}))) %in% i})][[1]] == names(which(sapply(idmap.g, function(m){g %in% m}))))],'stopid'] <- piMat[piMat$vertex==5,'stopid']
    pi.t[pi.t$vertex %in% bt.map[sapply(c.map, function(i){names(which(sapply(idmap.g, function(m){g %in% m}))) %in% i})][[2]][which(c.map[sapply(c.map, function(i){names(which(sapply(idmap.g, function(m){g %in% m}))) %in% i})][[2]] == names(which(sapply(idmap.g, function(m){g %in% m}))))],'stopid'] <- piMat[piMat$vertex==5,'stopid']
    
    # Find Lazy Waiting strategy
    if(g %in% path){
      trRew.lw  <- gRew - sCost*(which(path==g)-1)
      totRew.lw <- totRew.lw + trRew.lw
      AG.dat <- rbind(AG.dat, data.frame(pp=p, trial=tr, trRew=trRew.lw, nSteps=which(path==g)-1, endV=g, totRew=totRew.lw, strat='LW'))
    }else{
      trRew.lw <- -nSteps*sCost
      totRew.lw <- totRew.lw + trRew.lw
      AG.dat <- rbind(AG.dat, data.frame(pp=p, trial=tr, trRew=trRew.lw, nSteps=nSteps, endV=tail(path,1), totRew=totRew.lw, strat='LW'))
    }
    
    # Find OS stategy
    if(path[min(which(path==g), which(!sapply(path, function(i){pi.t[pi.t$vertex==i,'stopid']}) < c(15:0)[1:length(path)]))]==g){
      trRew.os  <- gRew - sCost*(which(path==g)-1)
      totRew.os <- totRew.os + trRew.os
      AG.dat <- rbind(AG.dat, data.frame(pp=p, trial=tr, trRew=trRew.os, nSteps=which(path==g)-1, endV=g, totRew=totRew.os, strat='OS'))
    }else{
      trRew.os  <- -sCost*(min(which(path==g), which(!sapply(path, function(i){pi.t[pi.t$vertex==i,'stopid']}) < c(15:0)[1:length(path)]))-1)
      totRew.os <- totRew.os + trRew.os
      AG.dat <- rbind(AG.dat, data.frame(pp=p, trial=tr, trRew=trRew.os, nSteps=(min(which(path==g), which(!sapply(path, function(i){pi.t[pi.t$vertex==i,'stopid']}) < c(15:0)[1:length(path)]))-1), endV=path[min(which(path==g), which(!sapply(path, function(i){pi.t[pi.t$vertex==i,'stopid']}) < c(15:0)[1:length(path)]))], totRew=totRew.os, strat='OS'))
    }
    
    # Find MS strategy
    gc <- names(idmap.g[sapply(idmap.g, function(m){g %in% m})])
    sc <- names(idmap.g[sapply(idmap.g, function(m){path[1] %in% m})])
    wc <- c.map[sc][[1]][c.map[sc][[1]] != gc]
    lc <- c('a','b','c','d')[!c('a','b','c','d') %in% c(wc,gc,sc)]
    
    leavenode <- bt.map[[wc]][which(c.map[wc][[1]] == sc)]
    leavenode <- c(leavenode, bt.map[lc][[1]][c.map[lc][[1]] == gc])
    
    if(g %in% path){
      if(path[min(which(path==g), which(path %in% leavenode))]==g){
        trRew.ms  <- gRew - sCost*(which(path==g)-1)
        totRew.ms <- totRew.ms + trRew.ms
        AG.dat <- rbind(AG.dat, data.frame(pp=p, trial=tr, trRew=trRew.ms, nSteps=which(path==g)-1, endV=g, totRew=totRew.ms, strat='MS'))
      }else if(T %in% (leavenode %in% path)){
        trRew.ms  <- -sCost*(min(which(path %in% leavenode))-1)
        totRew.ms <- totRew.ms + trRew.ms
        AG.dat <- rbind(AG.dat, data.frame(pp=p, trial=tr, trRew=trRew.ms, nSteps=(min(which(path %in% leavenode))-1), endV=path[min(which(path %in% leavenode))], totRew=totRew.ms, strat='MS'))
      }
    }else if(T %in% (leavenode %in% path)){
      trRew.ms  <- -sCost*(min(which(path %in% leavenode))-1)
      totRew.ms <- totRew.ms + trRew.ms
      AG.dat <- rbind(AG.dat, data.frame(pp=p, trial=tr, trRew=trRew.ms, nSteps=(min(which(path %in% leavenode))-1), endV=path[min(which(path %in% leavenode))], totRew=totRew.ms, strat='MS'))
    }else{
      trRew.ms <- -nSteps*sCost
      totRew.ms <- totRew.ms + trRew.ms
      AG.dat <- rbind(AG.dat, data.frame(pp=p, trial=tr, trRew=trRew.ms, nSteps=nSteps, endV=tail(path,1), totRew=totRew.ms, strat='MS'))
    }
  }
}

AG.dat <- AG.dat[-1,]
AG.dat$strat <- droplevels(AG.dat$strat)

# Summarize agent simulations per participant
AG.dat.ppEval <- data.frame(pp=0, totRew=0, endV.p=0, strat='init')
for(pp in 1:nPP){
  for(strat in levels(AG.dat$strat)){
    totRew <- AG.dat[AG.dat$pp==pp & AG.dat$trial==max(AG.dat$trial) & AG.dat$strat==strat,]$totRew
    endV.p <- sum(AG.dat[AG.dat$pp==pp & AG.dat$strat==strat,]$endV==vGoal)
    AG.dat.ppEval <- rbind(AG.dat.ppEval, data.frame(pp=pp, totRew=totRew, endV.p=endV.p, strat=strat))
  }
}
AG.dat.ppEval <- AG.dat.ppEval[-1,]

# Plot agent data!
ggplot(AG.dat.ppEval, aes(x=totRew, col=strat)) +
  geom_density()
# Plot nSteps for every agent
ggplot(AG.dat, aes(fill=strat)) +
  geom_bar(aes(nSteps), position='dodge') +
  scale_x_continuous(breaks=1:15, labels=1:15)



