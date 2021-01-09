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
nSteps <- 9   # Nr of maximum steps in a miniblock
gRew   <- 7    # Reward upon reaching vGoal
sCost  <- 0.15 # Points detracted from accumulated reward for each taken step

Edges <- list(c(2, 3, 4, 5), #Full Schapiro-style edge matrix
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
  tempMat <- hitMat.calc.nBT(Edges=Edges, vGoal=vGoal, nSteps=nSteps, curV=curV, preV=preV, totP=3^nSteps)
  tempMat
}

# Get the Expected Values for each interaction of previous & current node, conditional upon steps left
EVmat.nBT <- foreach(preV = inspect.Edges[,1], curV = inspect.Edges[,2], .combine=rbind) %do% {
  tempMat <- EVcalc.nBT(Edges=Edges, curV=curV, preV=preV, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, hitMat=hitMat.nBT) 
  tempMat
}

# Get the optimal stopping index (equals or lower) for each possible transition
piMat.nBT <- policy.generate.nBT(Edges=Edges, EVmat=EVmat.nBT, idmap=idmap)

############# BackTracking Schapiro Analysis ----------------------------
'TBA'


