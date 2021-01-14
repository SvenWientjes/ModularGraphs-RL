############################################################################################################################
############################ For testing with graphs that have 4 modules (and 24 nodes?) ###################################
############################################################################################################################
library(foreach)
# Load Functions from /src/
sapply(paste0('src/',list.files('src/')), source)

# Get different Defining Parameters
vStart <- 2    # Nr of starting node
vGoal  <- 8    # Nr of goal (terminating, rewarding) node
nSteps <- 10   # Nr of maximum steps in a miniblock (hitMat and EVcalc will use nSteps-1; agent simulations will use nSteps!)
gRew   <- 17    # Reward upon reaching vGoal
sCost  <- 0.14 # Points detracted from accumulated reward for each taken step

# Get parameters for agentic simulations
nPP <- 250
nTr <- 100

#Full Karuza-style quad-module edge matrix
Edges <- list(c(2, 3, 4, 5, 24), 
              c(1, 3, 4, 5, 6),
              c(1, 2, 4, 5, 6),
              c(1, 2, 3, 5, 6),
              c(1, 2, 3, 4, 6),
              c(2, 3, 4, 5, 7),
              c(6, 8, 9, 10,11),
              c(7, 9, 10,11,12),
              c(7, 8, 10,11,12),
              c(7, 8, 9, 11,12),
              c(7, 8, 9, 10,12),
              c(8, 9, 10,11,13),
              c(12,14,15,16,17),
              c(13,15,16,17,18),
              c(13,14,16,17,18),
              c(13,14,15,17,18),
              c(13,14,15,16,18),
              c(14,15,16,17,19),
              c(18,20,21,22,23),
              c(19,21,22,23,24),
              c(19,20,22,23,24),
              c(19,20,21,23,24),
              c(19,20,21,22,24),
              c(1, 20,21,22,23))

#Reduced Schapiro-style quad-module edge matrix
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

# Describe unique nodes for Karuza
inspect.Vertices <- c(1, 2, 6, 7, 9, 19, 20)

# Describe unique nodes for RSchap
inspect.Vertices <- c(1, 2, 5, 6, 7, 16, 17)

# Describe identical nodes for Karuza
idmap <- list(a = c(1,18), b = c(2,3,4,5, 14,15,16,17), c = c(6,13), d = c(7,12), e = c(9,10,11), f = c(19,24), g = c(20,21,22,23))

# Describe identical nodes for RSchap
idmap <- list(a = c(1,15), b = c(2,3,4, 12,13,14), c = c(5,11), d = c(6,10), e = c(7,9), f = c(16,20), g = c(17,18,19))

# Get the hitMat
hitMat <- foreach(v=inspect.Vertices, .combine=rbind) %do% {
  tempMat <- hitMat.calc(Edges=Edges, vGoal=vGoal, nSteps=nSteps-1, inspectVs=v, totP=4^(nSteps-1))
  tempMat
}

# Get the Expected Values for each interaction of previous & current node, conditional upon steps left
EVmat <- foreach(v = inspect.Vertices, .combine=rbind) %do% {
  tempMat <- EVcalc(Edges=Edges, vGoal=vGoal, nSteps=nSteps-1, gRew=gRew, sCost=sCost, hitMat=hitMat, Vertex=v) 
  tempMat
}

piMat <- policy.generate(Edges=Edges, EVmat=EVmat, idmap=idmap)

# Run several agents on this task, all encompassing different heuristics
AG.dat <- data.frame(pp=0, trial=0, trRew=0, nSteps=0, endV=0, totRew=0, strat='init')
for(pp in 1:nPP){
  AG.dat <- rbind(AG.dat, RandomStopper( Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0))
  AG.dat <- rbind(AG.dat, LazyWaiter(    Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0))
  AG.dat <- rbind(AG.dat, RandomLengther(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0))
  AG.dat <- rbind(AG.dat, OptimalStopper(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0, piMat=piMat))
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

##### Get candidate cost-reward setups that have decent optimal behavioural signatures -----
candidates <- data.frame(gRew = 0, sCost = 0)
for(rew in 5:20){
  for(cost in seq(0.1,1,0.02)){
    # Get the Expected Values for each interaction of previous & current node, conditional upon steps left
    EVmat <- foreach(v = inspect.Vertices, .combine=rbind) %do% {
      tempMat <- EVcalc(Edges=Edges, vGoal=vGoal, nSteps=nSteps-1, gRew=rew, sCost=cost, hitMat=hitMat, Vertex=v) 
      tempMat
    }
    
    piMat <- policy.generate(Edges=Edges, EVmat=EVmat, idmap=idmap)
    
    if(piMat[piMat$vertex==1,]$stopid < 8){
      candidates <- rbind(candidates, data.frame(gRew=rew, sCost=cost))
    }
  }
}










