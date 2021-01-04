###############################################################################################################
### MC simulations of the reward experiment with variable input of Rewards, Costs, Trials, and Participants ###
###############################################################################################################
# Get commandArgs bash input
rewardV <- eval(parse(text=commandArgs(T)[1], keep.source=F)[[1]])
costV   <- eval(parse(text=commandArgs(T)[2], keep.s))
nPP     <- as.numeric(commandArgs(T)[3])
nTr     <- as.numeric(commandArgs(T)[4])

# Load functions into environment
source('src/Agents.R')
source('src/EVcalc.R')
library(reshape2)

# Full edges matrix (schapiro-style random walk)
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
hitMat.nBT  <- read.csv('data/no-BT_step-9_hit_unique.csv',row.names=1,check.names=F)
vStart <- 1
vGoal  <- 8

# Get chance of reaching goal for each n of steps----
# Get starting expectations
propStart <- rep(0,length(1:11))
for(nV in 1:11){
  paths   <- 0
  queue   <- list(c(0,1))
  
  while(length(queue) > 0){
    pCur <- queue[[1]]
    for(t in Edges[[tail(pCur,1)]]){
      if(t!= tail(pCur,2)[1] & length(c(pCur,t))<(nV+1)){         #No-backtrack
        #if(length(c(pCur,t))<nV){                              #Backtrack
        queue <- append(queue,list(c(pCur,t)))
      }else if(t!= tail(pCur,2)[1] & length(c(pCur,t))==(nV+1)){  #No-backtrack
        #}else if(length(c(pCur,t))==nV){                       #Backtrack
        paths <- paths+1
        if(vGoal == t & !vGoal %in% pCur){
          propStart[nV] <- propStart[nV]+1
        }
      }
    }
    queue[[1]] <- NULL
  }
}
propStart <- propStart[-1]
goalstepchance <- rep(0,10)
for(nSteps in 1:10){
  goalstepchance[nSteps] <- propStart[nSteps]/(4*3^(nSteps-1))
}

# Giant loop of MC simulations through the graph
AG.dat.ppEval <- data.frame(pp=0, reward=0, cost=0, totRew=0, endV.p=0, strat='init')
for(reward in rewardV){
  for(cost in costV){
    AG.dat <- data.frame(pp=0, trial=0, trRew=0, nSteps=0, endV=0, totRew=0, strat='init')
    for(pp in 1:nPP){
      AG.dat <- rbind(AG.dat, RandomStopper(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=reward, sCost=cost, nTrials=nTr, parNum=pp, startRew=0))
      AG.dat <- rbind(AG.dat, LazyWaiter(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=reward, sCost=cost, nTrials=nTr, parNum=pp, startRew=0))
      AG.dat <- rbind(AG.dat, RandomLengther(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=reward, sCost=cost, nTrials=nTr, parNum=pp, startRew=0))
      AG.dat <- rbind(AG.dat, OptimalStopper(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=reward, sCost=cost, nTrials=nTr, hitMat=hitMat.nBT, goalstepchance=goalstepchance, parNum=pp, startRew=0))
    }
    AG.dat <- AG.dat[-1,];
    AG.dat$strat <- droplevels(AG.dat$strat)
    
    for(pp in 1:nPP){
      for(strat in levels(AG.dat$strat)){
        totRew <- AG.dat[AG.dat$pp==pp & AG.dat$trial==max(AG.dat$trial) & AG.dat$strat==strat,]$totRew
        endV.p <- sum(AG.dat[AG.dat$pp==pp & AG.dat$strat==strat,]$endV==vGoal)
        AG.dat.ppEval <- rbind(AG.dat.ppEval, data.frame(pp=pp, reward=reward, cost=cost, totRew=totRew, endV.p=endV.p, strat=strat))
      }
    }
  }
}
AG.dat.ppEval <- AG.dat.ppEval[-1,]

save(AG.dat.ppEval, file='MC_Rew_noBT_data.RData')
