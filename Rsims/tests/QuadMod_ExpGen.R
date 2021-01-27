############################################################################################################################
################## Generating different 100-block experiments with similar reward characteristics ##########################
############################################################################################################################
library(foreach)
library(ggplot2)
library(reshape2)

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

## Generate experiment according to criteria
# Goal visits conditional upon optimal stopping strategy
# Cannot make more points by modular stopping vs optimal stopping
sim.list <- foreach(i=1:nPP, .combine=list) %do% {
  goalTol <- T
  stratTol <- T
  while(goalTol | stratTol){
    # Generate Data
    full.exp <- Exp.gen(Edges=Edges, e.nodes=c(2,3,4,7,8,9,12,13,14,17,18,19), nSteps=nSteps, nTr=nTr, c.map=c.map, idmap.g=idmap.g, parNum=i)
    OS.strat <- OS.apply.QS(experiment=full.exp, piMat=piMat, idmap.g=idmap.g, c.map=c.map, bt.map=bt.map, gRew=gRew, sCost=sCost, parNum=i)
    MS.strat <- MS.apply.QS(experiment=full.exp, bt.map=bt.map, c.map=c.map, idmap.g=idmap.g, gRew=gRew, sCost=sCost, parNum=i)
    LW.strat <- LW.apply.QS(experiment=full.exp, gRew=gRew, sCost=sCost, parNum=i)
    
    # Perform adequacy tests
    goalTol = !abs(sum(OS.strat$trRew > 0) - nHit) <= floor(0.2*nHit)
    stratTol = !tail(MS.strat$totRew,1) <= tail(OS.strat$totRew,1)
  }
  list(full.exp=full.exp, AG.dat=rbind(OS.strat,MS.strat,LW.strat))
}
full.exp <- foreach(i = 1:nPP, .combine=rbind) %do% {sim.list[[i]]$full.exp}
AG.dat   <- foreach(i = 1:nPP, .combine=rbind) %do% {sim.list[[i]]$AG.dat}
remove(sim.list)

# Evaluate edges in line plots to check equal distribution
Edge.eval <- matrix(rep(rep(0, nrow(Edge.list)),nPP), ncol=nPP)
for(p in 1:nPP){
  for(tr in 1:nTr){
    for(n in 2:length(full.exp[full.exp$tr==tr & full.exp$pp==p,]$v)){
      Edge.eval[which(Edge.list[,1]==full.exp[full.exp$tr==tr & full.exp$pp==p,]$v[n-1] & Edge.list[,2]==full.exp[full.exp$tr==tr & full.exp$pp==p,]$v[n]),p] <- Edge.eval[which(Edge.list[,1]==full.exp[full.exp$tr==tr & full.exp$pp==p,]$v[n-1] & Edge.list[,2]==full.exp[full.exp$tr==tr & full.exp$pp==p,]$v[n]),p] + 1
    }
  }
}
ggplot(melt(Edge.eval), aes(x=Var1, y=value)) +
  geom_line() + 
  facet_grid(melt(Edge.eval)$Var2)

# Summarize agent simulations per participant
AG.dat.ppEval <- data.frame(pp=0, totRew=0, endV.p=0, strat='init')
for(pp in 1:nPP){
  for(strat in levels(AG.dat$strat)){
    totRew <- AG.dat[AG.dat$pp==pp & AG.dat$trial==max(AG.dat$trial) & AG.dat$strat==strat,]$totRew
    endV.p <- sum(AG.dat[AG.dat$pp==pp & AG.dat$strat==strat,]$trRew>=0)
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
  scale_x_continuous(breaks=1:15, labels=1:15) #+
  #facet_grid(AG.dat$pp)


