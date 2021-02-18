############################################################################################################################
########################### Generate trajectories with goals also possible in bottlenecks ##################################
############################################################################################################################
library(foreach)
library(ggplot2)
library(reshape2)
library(data.table)

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















