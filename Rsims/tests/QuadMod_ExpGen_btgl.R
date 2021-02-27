############################################################################################################################
########################### Generate trajectories with goals also possible in bottlenecks ##################################
############################################################################################################################
library(foreach)
library(ggplot2)
library(reshape2)
library(data.table)

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
idmap.bg <- list(a = c(1), b = c(2,3,4), c = c(5), d = c(7,8,9), e = c(10), f = c(11), g = c(12,13,14), h = c(15), i = c(16), j = c(17,18,19), k = c(20))
idmap.g <- list(a = c(2,3,4), b = c(7,8,9), c = c(12,13,14), d = c(17,18,19))
c.map   <- list(a = c('d','b'), b = c('a', 'c'), c = c('b','d'), d=c('a','c'))
bt.map  <- list(a = c(1,5), b = c(6,10), c= c(11,15), d=c(20,16))

# Get the hitMats
hitMat.d <- read.csv('data/hitMat_quadSchap_step15.csv', row.names=1)
hitMat.b <- read.csv('data/hitMat_quadSchap_step15_btgl.csv', row.names=1)

# Get the Expected Values for each interaction of previous & current node, conditional upon steps left
EVmat.d <- foreach(v = inspect.Vertices, .combine=rbind) %do% {
  tempMat <- EVcalc(Edges=Edges, vGoal=vGoal, nSteps=nSteps-1, gRew=gRew, sCost=sCost, hitMat=hitMat.d, Vertex=v) 
  tempMat
}
EVmat.b <- foreach(v = unique(hitMat.b$vertex), .combine=rbind) %do% {
  tempMat <- EVcalc(Edges=Edges, vGoal=vGoal, nSteps=nSteps-1, gRew=gRew, sCost=sCost, hitMat=hitMat.b, Vertex=v) 
  tempMat
}

# Get policies for the deep & bottleneck node EVmats
piMat.d <- policy.generate(Edges=Edges, EVmat=EVmat.d, idmap=idmap)
piMat.b <- policy.generate(Edges=Edges, EVmat=EVmat.b, idmap=idmap.bg)

## Generate experiment according to criteria ----
sim.list <- foreach(i = 1:nPP, .combine=append) %do% {
  
  rewtol  = T
  
  while(rewtol){
    typetol = T
    while(typetol){
      expNow <- Exp.gen(Edges=Edges, nSteps=nSteps, nTr=nTr, c.map=c.map, idmap.g=idmap.g, bt.map=bt.map, parNum=i)
      typetol = !(abs(sum(expNow[step==0,trtype]=='deep') - 12/20*nTr) <= 0.1*nTr) | !(abs(sum(expNow[step==0,trtype]=='goalclose') - sum(expNow[step==0,trtype]=='goalfar')) < 0.05*nTr)
    }
    LW.strat <- LW.apply.QS(experiment=expNow, gRew=gRew, sCost=sCost, parNum=i)
    MS.strat <- MS.apply.QS(experiment=expNow, bt.map=bt.map, c.map=c.map, idmap.g=idmap.g, gRew=gRew, sCost=sCost, parNum=i)
    rewtol = !abs(LW.strat[trial==100, totRew] - 460) < 160
  }
  
  
  list(full.exp=expNow, AG.dat=rbind(LW.strat,MS.strat))
}
full.exp <- foreach(i = seq(1,nPP*2,2), .combine=rbind) %do% {sim.list[[i]]}
AG.dat   <- foreach(i = seq(2,nPP*2,2), .combine=rbind) %do% {sim.list[[i]]}
AG.dat$strat <- as.factor(AG.dat$strat)

# Summarize agent simulations per participant
AG.dat.ppEval <- data.table(pp=0, totRew=0, endV.p=0, strat='init')
for(ppn in 1:nPP){
  for(strate in levels(AG.dat$strat)){
    totRew <- AG.dat[pp==ppn & trial==max(AG.dat$trial) & strat==strate,]$totRew
    endV.p <- sum(AG.dat[pp==ppn & strat==strate,]$trRew>=0)
    AG.dat.ppEval <- rbind(AG.dat.ppEval, data.frame(pp=ppn, totRew=totRew, endV.p=endV.p, strat=strate))
  }
}
AG.dat.ppEval <- AG.dat.ppEval[-1,]

# Plot agent data!
ggplot(AG.dat.ppEval, aes(x=totRew, col=strat)) +
  geom_density() +
  geom_vline(xintercept=0)

# Get Edge distributions
Edge.eval <- full.exp[,list(prev=.SD[step %in% 0:(.N-2),v],
                            curv=.SD[step %in% 1:(.N-1),v]), by=.(pp,tr)
         ][,.(tab=list(table(prev,curv))),by=pp
           ][,reshape2::melt(tab[[1]]), by=pp][value!=0]

ggplot(Edge.eval, aes(x=interaction(prev,curv), y=value)) +
  geom_boxplot()

ggplot(Edge.eval, aes(x=value, group=pp, col=pp)) +
  geom_density()

