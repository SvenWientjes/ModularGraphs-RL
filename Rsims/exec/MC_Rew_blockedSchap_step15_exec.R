############################################################################################################################
####################### Test script for experiments using the Blocked graph with backtracking ##############################
############################################################################################################################
library(foreach)
library(doParallel)

sapply(paste0('src/',list.files('src/')), source)

# Get different Defining Parameters
vStart <- 1    # Nr of starting node
vGoal  <- 8    # Nr of goal (terminating, rewarding) node
nSteps <- 15   # Nr of maximum steps in a miniblock

# Get parameters for agentic simulations
nPP <- 250
nTr <- 100

#Blocked Schapiro-style edge matrix
Edges <- list(c(2, 3, 4, 5), 
              c(1, 3, 4, 5),
              c(1, 2, 4, 5),
              c(1, 2, 3, 6),
              c(1, 2, 3, 15),
              c(4, 7, 8, 9),
              c(6, 8, 9, 10),
              c(6, 7, 9, 10),
              c(6, 7, 8, 10),
              c(7, 8, 9),
              c(12,13,14),
              c(11,13,14,15),
              c(11,12,14,15),
              c(11,12,13,15),
              c(5, 12,13,14))

# Select unique nodes
inspect.Vertices <- c(1,4,5,6,7,10,11,12,15)

# Select identical nodes
idmap <- list(a = c(1,2,3), b = c(4), c = c(5), d = c(6), e = c(7,9), f = c(10), g = c(11), h = c(12,13,14), i = c(15))

# Get the HitMat
hitMat <- read.csv('data/hitMat_blockedSchap_step15.csv',row.names=1)

# Choose the grid of costs and rewards
tRew <- 30:70
tCost <- seq(0.1,1,0.02)

# Calculate all candidate combinations according to criteria:
  # Stop in starting module lower than 10 steps 
  # Stopping in 5 should be lower than stopping in 15 (modular transition)
candidates <- data.frame(gRew = 0, sCost = 0)
for(rew in tRew){
  for(cost in tCost){
    EVmat <- foreach(v = inspect.Vertices, .combine=rbind) %do% {
      tempMat <- EVcalc(Edges=Edges, vGoal=vGoal, nSteps=nSteps-1, gRew=rew, sCost=cost, hitMat=hitMat, Vertex=v) 
      tempMat
    }
    piMat <- policy.generate(Edges=Edges, EVmat=EVmat, idmap=idmap)
    if(piMat[piMat$vertex==1,2] < 10 & piMat[piMat$vertex==5,2] != piMat[piMat$vertex==15,2]){
      candidates <- rbind(candidates, data.frame(gRew=rew, sCost=cost))
    }
  }
}
candidates <- candidates[-1,]

# Set up parallel cluster
cl <- makeCluster(30)
registerDoParallel()

# Simulate experiments with candidate setups to look for large distance between Lazy Waiting and Optimal Stopping
largeSim <- foreach(exp = 1:nrow(candidates), .combine=rbind, .packages='foreach') %dopar% {
  # Get the Expected Values for each interaction of previous & current node, conditional upon steps left
  EVmat <- foreach(v = inspect.Vertices, .combine=rbind) %do% {
    tempMat <- EVcalc(Edges=Edges, vGoal=vGoal, nSteps=nSteps-1, gRew=candidates[exp,1], sCost=candidates[exp,2], hitMat=hitMat, Vertex=v) 
    tempMat
  }
  
  piMat <- policy.generate(Edges=Edges, EVmat=EVmat, idmap=idmap)
  
  # Run several agents on this task, all encompassing different heuristics
  AG.dat <- data.frame(pp=0, trial=0, trRew=0, nSteps=0, endV=0, totRew=0, strat='init')
  for(pp in 1:nPP){
    AG.dat <- rbind(AG.dat, RandomStopper( Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=candidates[exp,1], sCost=candidates[exp,2], nTrials=nTr, parNum=pp, startRew=0))
    AG.dat <- rbind(AG.dat, LazyWaiter(    Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=candidates[exp,1], sCost=candidates[exp,2], nTrials=nTr, parNum=pp, startRew=0))
    AG.dat <- rbind(AG.dat, RandomLengther(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=candidates[exp,1], sCost=candidates[exp,2], nTrials=nTr, parNum=pp, startRew=0))
    AG.dat <- rbind(AG.dat, OptimalStopper(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=candidates[exp,1], sCost=candidates[exp,2], nTrials=nTr, parNum=pp, startRew=0, piMat=piMat))
  }
  AG.dat <- AG.dat[-1,]
  AG.dat$strat <- droplevels(AG.dat$strat)
  
  # Summarize agent simulations per participant
  AG.dat.ppEval <- data.frame(pp=0, totRew=0, endV.p=0, strat='init', gRew=0, sCost=0)
  for(pp in 1:nPP){
    for(strat in levels(AG.dat$strat)){
      totRew <- AG.dat[AG.dat$pp==pp & AG.dat$trial==max(AG.dat$trial) & AG.dat$strat==strat,]$totRew
      endV.p <- sum(AG.dat[AG.dat$pp==pp & AG.dat$strat==strat,]$endV==vGoal)
      AG.dat.ppEval <- rbind(AG.dat.ppEval, data.frame(pp=pp, totRew=totRew, endV.p=endV.p, strat=strat, gRew=candidates[exp,1], sCost=candidates[exp,2]))
    }
  }
  AG.dat.ppEval[-1,]
}

write.csv(largeSim, file='data/MC_Rew_BlockedSchap.csv')





