############################################################################################################################
################################# Analysis of Modular Graph with costs and rewards #########################################
############################################################################################################################
library(ggplot2)
library(reshape2)
library(ggthemes)
library(RColorBrewer)
library(ggstance)
library(lemon)

# Starting node and goal (rewarded) termination node
vStart <- 1
vGoal  <- 8

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

# Which transitions do we want to evaluate on value
inspect.Edges <- rbind(c(1,2),
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

hitMat.nBT  <- read.csv('data/hitMat_nBT_step9.csv',row.names=1,check.names=F)
rownames(hitMat.nBT) <- NULL

## Precalc goalstepchange ----
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
############################################################################################################################
## Plot the EVs over different transitions for different steps ----
EVdat <- do.call('rbind', apply(unique(hitMat.nBT[,c('preVertex','Vertex')]), 1, function(x){EVcalc.nBT(Edges=Edges, preV=as.integer(x[1]), curV=as.integer(x[2]), vGoal=vGoal, nSteps=9, gRew=7, sCost=0.15, hitMat=hitMat.nBT)}))
rownames(EVdat) <- NULL
EVplot.nBT(EV_data)

## Calculate the change points from positive to negative EV ----
changePoint <- data.frame(reward=0, cost=0, trans='0', cP=0)

for(reward in seq(5,15,0.1)){
  for(cost in seq(0.05,0.5,0.01)){
    EV_data <- EVcalc(Edges = Edges, vStart=vStart, vGoal=vGoal, nSteps=10, gRew=reward, sCost=cost, hitMat=hitMat.nBT, goalstepchance=goalstepchance)
    for(tr in levels(EV_data$trans)){
      if(T %in% (EV_data[EV_data$trans==tr,'value']<=0)){
        changePoint <- rbind(changePoint, data.frame(reward=reward, cost=cost, trans=tr, cP=max(which(EV_data[EV_data$trans==tr,'value']<=0))))
      }
    }
  }
}

changePoint <- changePoint[-1,]

## Plot the change points from positive to negative for a particular reward level, over all costs --
ggplot(changePoint[changePoint$reward==12,], aes(x=cP, y=cost,col=trans)) +
  lemon::geom_pointpath(position=position_dodge(width=0.3)) +
  facet_grid(~reward) +
  scale_x_continuous(name='Steps Left', breaks=c(1,2,3,4,5,6,7,8,9,10))

# Search for good candidate Reward/Cost levels
candidates <- c(0,0)
for(reward in unique(changePoint$reward)){
  for(cost in unique(changePoint$cost)){
    if(!'1<-|' %in% changePoint[changePoint$reward==reward & changePoint$cost==cost,]$trans &
       !T %in% duplicated(changePoint[changePoint$reward==reward & changePoint$cost==cost,]$cP)){
      candidates <- rbind(candidates, c(reward,cost))
    }
  }
}

## Compare Agents No Backtracking ----
AG.dat <- data.frame(pp=0, trial=0, trRew=0, nSteps=0, endV=0, totRew=0, strat='init')
nPP <- 250; nTr <- 100
gRew <- 7; sCost <- 0.15
for(pp in 1:nPP){
  AG.dat <- rbind(AG.dat, RandomStopper.nBT(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0))
  AG.dat <- rbind(AG.dat, LazyWaiter.nBT(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0))
  AG.dat <- rbind(AG.dat, RandomLengther.nBT(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0))
  AG.dat <- rbind(AG.dat, OptimalStopper.Schapiro.nBT(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, hitMat=hitMat.nBT, goalstepchance=goalstepchance, parNum=pp, startRew=0))
}
AG.dat <- AG.dat[-1,];
AG.dat$strat <- droplevels(AG.dat$strat)

AG.dat.ppEval <- data.frame(pp=0, totRew=0, endV.p=0, strat='init')
for(pp in 1:nPP){
  for(strat in levels(AG.dat$strat)){
    totRew <- AG.dat[AG.dat$pp==pp & AG.dat$trial==max(AG.dat$trial) & AG.dat$strat==strat,]$totRew
    endV.p <- sum(AG.dat[AG.dat$pp==pp & AG.dat$strat==strat,]$endV==vGoal)
    AG.dat.ppEval <- rbind(AG.dat.ppEval, data.frame(pp=pp, totRew=totRew, endV.p=endV.p, strat=strat))
  }
}
AG.dat.ppEval <- AG.dat.ppEval[-1,]

ggplot(AG.dat.ppEval, aes(x=totRew, col=strat)) +
  geom_density()

# Big loop that tests different agents with different cost/reward values
AG.dat.ppEval <- data.frame(pp=0, reward=0, cost=0, totRew=0, endV.p=0, strat='init')
nPP <- 250; nTr <- 100
for(reward in 7:15){
  for(cost in c(0.15, 0.17, 0.20, 0.21, 0.23, 0.25, 0.27, 0.30, 0.31)){
    AG.dat <- data.frame(pp=0, trial=0, trRew=0, nSteps=0, endV=0, totRew=0, strat='init')
    for(pp in 1:nPP){
      AG.dat <- rbind(AG.dat, RandomStopper.nBT(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=reward, sCost=cost, nTrials=nTr, parNum=pp, startRew=0))
      AG.dat <- rbind(AG.dat, LazyWaiter.nBT(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=reward, sCost=cost, nTrials=nTr, parNum=pp, startRew=0))
      AG.dat <- rbind(AG.dat, RandomLengther.nBT(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=reward, sCost=cost, nTrials=nTr, parNum=pp, startRew=0))
      AG.dat <- rbind(AG.dat, OptimalStopper.Schapiro.nBT(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=reward, sCost=cost, nTrials=nTr, hitMat=hitMat.nBT, goalstepchance=goalstepchance, parNum=pp, startRew=0))
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
ggplot(AG.dat.ppEval, aes(x=totRew, col=strat)) +
  geom_density() +
  facet_grid(reward~cost)

## Compare Agents With Backtracking ----
AG.dat <- data.frame(pp=0, trial=0, trRew=0, nSteps=0, endV=0, totRew=0, strat='init')
nPP <- 250; nTr <- 100
gRew <- 7; sCost <- 0.15
for(pp in 1:nPP){
  AG.dat <- rbind(AG.dat, RandomStopper(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0))
  AG.dat <- rbind(AG.dat, LazyWaiter(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0))
  AG.dat <- rbind(AG.dat, RandomLengther(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, parNum=pp, startRew=0))
  AG.dat <- rbind(AG.dat, ModularStopper(Edges=Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, nTrials=nTr, modTrans=rbind(c(6,4),c(5,15),c(10,11)), parNum=pp, startRew=0))
}
AG.dat <- AG.dat[-1,];
AG.dat$strat <- droplevels(AG.dat$strat)

AG.dat.ppEval <- data.frame(pp=0, totRew=0, endV.p=0, strat='init')
for(pp in 1:nPP){
  for(strat in levels(AG.dat$strat)){
    totRew <- AG.dat[AG.dat$pp==pp & AG.dat$trial==max(AG.dat$trial) & AG.dat$strat==strat,]$totRew
    endV.p <- sum(AG.dat[AG.dat$pp==pp & AG.dat$strat==strat,]$endV==vGoal)
    AG.dat.ppEval <- rbind(AG.dat.ppEval, data.frame(pp=pp, totRew=totRew, endV.p=endV.p, strat=strat))
  }
}
AG.dat.ppEval <- AG.dat.ppEval[-1,]

ggplot(AG.dat.ppEval, aes(x=totRew, col=strat)) +
  geom_density()







