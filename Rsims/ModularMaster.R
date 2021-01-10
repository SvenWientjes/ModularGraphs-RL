############################################################################################################################
###################### Master Script for Modular Graphs showcasing all functions and workflow ##############################
############################################################################################################################
# Load Packages
library(ggplot2)
library(reshape2)
library(ggthemes)
library(RColorBrewer)
library(ggstance)
library(lemon)
library(foreach)

# Load Functions from /src/
sapply(paste0('src/',list.files('src/')), source)

# Get different Defining Parameters
vStart <- 1    # Nr of starting node
vGoal  <- 8    # Nr of goal (terminating, rewarding) node
nSteps <- 10   # Nr of maximum steps in a miniblock (hitMat and EVcalc will use nSteps-1; agent simulations will use nSteps!)
gRew   <- 7    # Reward upon reaching vGoal
sCost  <- 0.31 # Points detracted from accumulated reward for each taken step

# Get parameters for agentic simulations
nPP <- 250
nTr <- 100

#Full Schapiro-style edge matrix
Edges <- list(c(2, 3, 4, 5), 
              c(1, 3, 4, 5),
              c(1, 2, 4, 5),
              c(1, 2, 3, 6),
              c(1, 2, 3, 15),
              c(4, 7, 8, 9),
              c(6, 8, 9, 10),
              c(6, 7, 9, 10),
              c(6, 7, 8, 10),
              c(11,7, 8, 9),
              c(10,12,13,14),
              c(11,13,14,15),
              c(11,12,14,15),
              c(11,12,13,15),
              c(5, 12,13,14))

############# No-BackTracking Schapiro Analysis ----------------------------
    # Note: should in principle work for ANY graph which has an equal degree for each node!
inspect.Edges <- rbind(c(1,2), #List all pre- and current vertices to inspect
                       c(1,4),
                       c(1,5),
                       c(4,1),
                       c(4,6),
                       c(5,1),
                       c(5,15),
                       c(6,4),
                       c(6,7),
                       c(7,6),
                       c(7,9))
idmap <- list(a = c(1,2,3, 12,13,14), b = c(4,11), c = c(5,15), d = c(6,10), e = c(7,9))

# Get the hitMat
hitMat.nBT <- foreach(preV = inspect.Edges[,1], curV = inspect.Edges[,2], .combine=rbind) %do% {
  tempMat <- hitMat.calc.nBT(Edges=Edges, vGoal=vGoal, nSteps=nSteps-1, curV=curV, preV=preV, totP=3^nSteps)
  tempMat
}

# Get the Expected Values for each interaction of previous & current node, conditional upon steps left
EVmat.nBT <- foreach(preV = inspect.Edges[,1], curV = inspect.Edges[,2], .combine=rbind) %do% {
  tempMat <- EVcalc.nBT(Edges=Edges, curV=curV, preV=preV, vGoal=vGoal, nSteps=nSteps-1, gRew=gRew, sCost=sCost, hitMat=hitMat.nBT) 
  tempMat
}

# Get the optimal stopping index (equals or lower) for each possible transition
piMat.nBT <- policy.generate.nBT(Edges=Edges, EVmat=EVmat.nBT, idmap=idmap)

# Run several agents on this task, all encompassing different heuristics
AG.dat <- data.frame(pp=0, trial=0, trRew=0, nSteps=0, endV=0, totRew=0, strat='init')
for(pp in 1:nPP){
  AG.dat <- rbind(AG.dat, RandomStopper.nBT( Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0))
  AG.dat <- rbind(AG.dat, LazyWaiter.nBT(    Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0))
  AG.dat <- rbind(AG.dat, RandomLengther.nBT(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0))
  AG.dat <- rbind(AG.dat, OptimalStopper.nBT(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0, piMat=piMat.nBT))
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

############# BackTracking Schapiro Analysis ----------------------------
'TBA'


