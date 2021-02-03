############################################################################################################################
################## Generating different 100-block experiments with similar reward characteristics ##########################
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

## Generate experiment according to criteria ----
# Goal visits conditional upon optimal stopping strategy
# Cannot make more points by modular stopping vs optimal stopping
sim.list <- foreach(i=1:nPP, .combine=append) %do% {
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
full.exp <- foreach(i = seq(1,nPP*2,2), .combine=rbind) %do% {sim.list[[i]]}
AG.dat   <- foreach(i = seq(2,nPP*2,2), .combine=rbind) %do% {sim.list[[i]]}
remove(sim.list)
write.csv(full.exp, file='data/QS_ExpGen_fullexp.csv')
write.csv(AG.dat, file='data/QS_ExpGen_AG.csv')

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

# Calculate how often modular stopper sees wrong cluster!
apply(expand.grid(1:5,1:5),1, function(t){AG.dat[AG.dat$strat=='MS' & AG.dat$pp==t[1] & AG.dat$trial==t[2],]$nSteps < 15 & AG.dat[AG.dat$strat=='MS' & AG.dat$pp==t[1] & AG.dat$trial==t[2],]$trRew < 0})

modStopped <- data.frame(pp = 1:nPP, mS = 0)
for(p in 353:nPP){
  modStopped[p,'mS'] <- sum(sapply(1:nTr, function(t){AG.dat[AG.dat$strat=='MS' & AG.dat$pp==p & AG.dat$trial==t,]$nSteps < 15 & AG.dat[AG.dat$strat=='MS' & AG.dat$pp==p & AG.dat$trial==t,]$trRew < 0}))
}

ggplot(modStopped, aes(x=mS)) +
  geom_bar()

## Unique transition ID - uniformity of multinomial given goal-directed transition type
# Types: deep start, BTg, BTl, BIg, BIl, deep goal, deep leave, BTd, BId, deep disappoint

# Get experiment into data.table because it is better in every way
full.exp.d <- data.table(full.exp)

# Define start and goal cluster per trial
full.exp.d <- cbind(full.exp.d, full.exp.d[,list(step=step, 
                                                 startc=names(idmap.g[sapply(idmap.g, function(m){v[1] %in% m})]),
                                                 goalc=names(idmap.g[sapply(idmap.g, function(m){goal[1] %in% m})])),
                                           by=c('pp','tr')][,c('startc','goalc')])

# Define depending leave and disappoint cluster 
full.exp.d <- cbind(full.exp.d, full.exp.d[,list(step=step,
                                                 leavec=c.map[[startc[1]]][c.map[[startc[1]]]!=goalc[1]],
                                                 disc=c.map[[goalc[1]]][c.map[[goalc[1]]]!=startc[1]]),
                                           by=c('pp','tr')][,c('leavec','disc')])

# Identify all node identities!
full.exp.d <- full.exp.d[, list('ds'  = v %in% idmap.g[[startc]],
                                'btg' = v %in% bt.map[[startc]][which(c.map[[startc]]==goalc)],
                                'btl' = v %in% bt.map[[startc]][which(c.map[[startc]]!=goalc)],
                                'big' = v %in% bt.map[[goalc]][which(c.map[[goalc]]==startc)],
                                'bil' = v %in% bt.map[[leavec]][which(c.map[[leavec]]==startc)],
                                'dg'  = v %in% idmap.g[[goalc]],
                                'dl'  = v %in% idmap.g[[leavec]],
                                'bid' = v %in% bt.map[[disc]][which(c.map[[disc]]==goalc)],
                                'btd' = v %in% bt.map[[goalc]][which(c.map[[goalc]]!=startc)],
                                'dd'  = v %in% idmap.g[[disc]],
                                'bnd' = v %in% bt.map[[disc]][which(c.map[[disc]]==leavec)],
                                'bnl' = v %in% bt.map[[leavec]][which(c.map[[leavec]]==disc)]), by=c('pp','tr','step')]

# Reshape data.table into transition data instead of occupation data
full.exp.d <- cbind(full.exp.d[, .SD[step %in% 0:(.N-2)], by=c('pp','tr')], f=full.exp.d[, .SD[step %in% 1:.N,], by=c('pp','tr')][,c('ds','btg','btl','big','bil','dg','dl','bid','btd','dd','bnd','bnl')])

# Get experiment data into pre and post transition 
exp.trans <- apply(full.exp.d[,4:27], 1, function(i){names(i)[which(as.logical(i))]})
full.exp.d <- cbind(full.exp.d[,1:3], V1=exp.trans[1,], V2=exp.trans[2,])

# List all unique transitions
exp.trans <- data.table(exp.trans[1,], exp.trans[2,])
exp.trans <- exp.trans[!duplicated(exp.trans),]

# Make plots to identify if transitions deviate from random walk expectation W.R.T GOAL DIRECTEDNESS (not Edge/Node structural identity)
pL <- list()
k=c(0.25,0.75,0.5,0.25,0.25,0.75,0.5,0.25,0.25,0.75,0.25,0.75,0.25,0.25,0.75,0.25,0.75,0.25,0.25,0.75,0.5,0.25,0.5,0.25,0.75,0.25,0.25,0.25)
#k = c(0.5,0.25,0.75,0.25,0.25,0.75,0.25,0.25,0.75,0.5,0.25,0.75,0.25,0.75,0.25,0.25,0.75,0.5,0.25,0.25,0.25,0.75,0.25,0.5,0.75,0.25,0.25,0.25)
for(i in 1:nrow(exp.trans)){

  tempDat <- merge(full.exp.d[V1==exp.trans$V1[i] & V2==exp.trans$V2[i], .N, by=c('pp','step')][order(pp,step)],
                   full.exp.d[V1==exp.trans$V1[i], .N, by=c('pp','step')][order(pp,step)],
                   by=c('pp','step'),all.y=T)[is.na(N.x),N.x:=0][,'N.p':=N.x/N.y]
  tempDat$pp <- as.factor(tempDat$pp)
  tempMean <- tempDat[,mean(N.p),by='step']
  
  p = ggplot() +
        geom_line(data=tempDat, aes(x=step, y=N.p, col=pp, group=pp), alpha=0.2) +
        geom_line(data=tempMean, aes(x=step, y=V1)) +
        geom_hline(yintercept=k[i], color='red') +
        geom_smooth(data=tempDat, aes(x=step, y=N.p), method='gam', formula=y~s(x), fill='yellow') +
        theme(legend.position = 'none') +
        ggtitle(paste0('from ',exp.trans$V1[i],' to ',gsub('f.','',exp.trans$V2[i])))
  
  pL[[i]] <- p
}
pdf('figs/ExpGen_TransCheck_2.pdf')
for(i in 1:length(pL)){
  print(pL[[i]])
}
dev.off()

## Simulate experiments with Exp_gen function with more lenient goal reach criteria ----
sim.list <- foreach(i=1:nPP, .combine=append) %do% {
  goalTol  <- T
  stratTol <- T
  while(goalTol | stratTol){
    # Generate Data
    full.exp <- Exp.gen(Edges=Edges, e.nodes=c(2,3,4,7,8,9,12,13,14,17,18,19), nSteps=nSteps, nTr=nTr, c.map=c.map, idmap.g=idmap.g, parNum=i)
    OS.strat <- OS.apply.QS(experiment=full.exp, piMat=piMat, idmap.g=idmap.g, c.map=c.map, bt.map=bt.map, gRew=gRew, sCost=sCost, parNum=i)
    MS.strat <- MS.apply.QS(experiment=full.exp, bt.map=bt.map, c.map=c.map, idmap.g=idmap.g, gRew=gRew, sCost=sCost, parNum=i)
    LW.strat <- LW.apply.QS(experiment=full.exp, gRew=gRew, sCost=sCost, parNum=i)
    
    # Perform adequacy tests
    goalTol = !abs(sum(LW.strat$trRew > 0) - nHit) <= floor(0.2*nHit)
    stratTol = tail(OS.strat$totRew,1) < 250 | tail(MS.strat$totRew,1) < 250 | tail(OS.strat$totRew,1) > 750 | tail(MS.strat$totRew,1) > 750 | tail(MS.strat$totRew,1) > tail(OS.strat$totRew,1)
  }
  list(full.exp=full.exp, AG.dat=rbind(OS.strat,MS.strat,LW.strat))
}
full.exp <- foreach(i = seq(1,nPP*2,2), .combine=rbind) %do% {sim.list[[i]]}
AG.dat   <- foreach(i = seq(2,nPP*2,2), .combine=rbind) %do% {sim.list[[i]]}
remove(sim.list)

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

# Get experiment into data.table because it is better in every way
full.exp.d <- data.table(full.exp)

# Define start and goal cluster per trial
full.exp.d <- cbind(full.exp.d, full.exp.d[,list(step=step, 
                                                 startc=names(idmap.g[sapply(idmap.g, function(m){v[1] %in% m})]),
                                                 goalc=names(idmap.g[sapply(idmap.g, function(m){goal[1] %in% m})])),
                                           by=c('pp','tr')][,c('startc','goalc')])

# Define depending leave and disappoint cluster 
full.exp.d <- cbind(full.exp.d, full.exp.d[,list(step=step,
                                                 leavec=c.map[[startc[1]]][c.map[[startc[1]]]!=goalc[1]],
                                                 disc=c.map[[goalc[1]]][c.map[[goalc[1]]]!=startc[1]]),
                                           by=c('pp','tr')][,c('leavec','disc')])

# Identify all node identities!
full.exp.d <- full.exp.d[, list('ds'  = v %in% idmap.g[[startc]],
                                'btg' = v %in% bt.map[[startc]][which(c.map[[startc]]==goalc)],
                                'btl' = v %in% bt.map[[startc]][which(c.map[[startc]]!=goalc)],
                                'big' = v %in% bt.map[[goalc]][which(c.map[[goalc]]==startc)],
                                'bil' = v %in% bt.map[[leavec]][which(c.map[[leavec]]==startc)],
                                'dg'  = v %in% idmap.g[[goalc]],
                                'dl'  = v %in% idmap.g[[leavec]],
                                'bid' = v %in% bt.map[[disc]][which(c.map[[disc]]==goalc)],
                                'btd' = v %in% bt.map[[goalc]][which(c.map[[goalc]]!=startc)],
                                'dd'  = v %in% idmap.g[[disc]],
                                'bnd' = v %in% bt.map[[disc]][which(c.map[[disc]]==leavec)],
                                'bnl' = v %in% bt.map[[leavec]][which(c.map[[leavec]]==disc)]), by=c('pp','tr','step')]

# Reshape data.table into transition data instead of occupation data
full.exp.d <- cbind(full.exp.d[, .SD[step %in% 0:(.N-2)], by=c('pp','tr')], f=full.exp.d[, .SD[step %in% 1:.N,], by=c('pp','tr')][,c('ds','btg','btl','big','bil','dg','dl','bid','btd','dd','bnd','bnl')])

# Get experiment data into pre and post transition 
exp.trans <- apply(full.exp.d[,4:27], 1, function(i){names(i)[which(as.logical(i))]})
full.exp.d <- cbind(full.exp.d[,1:3], V1=exp.trans[1,], V2=exp.trans[2,])

# List all unique transitions
exp.trans <- data.table(exp.trans[1,], exp.trans[2,])
exp.trans <- exp.trans[!duplicated(exp.trans),]

# Make plots to identify if transitions deviate from random walk expectation W.R.T GOAL DIRECTEDNESS (not Edge/Node structural identity)
pL <- list()
k = c(0.5,0.25,0.25,0.75,0.25,0.25,0.75,0.5,0.25,0.25,0.75,0.5,0.25,0.75,0.75,0.25,0.25,0.25,0.25,0.75,0.25,0.75,0.5,0.25,0.25,0.75,0.25,0.25)
k = c(0.5,0.25,0.75,0.25,0.25,0.75,0.25,0.25,0.75,0.5,0.25,0.25,0.75,0.75,0.25,0.75,0.25,0.75,0.25,0.25,0.25,0.25,0.5,0.5,0.25,0.75,0.25,0.25)
for(i in 1:nrow(exp.trans)){
  
  tempDat <- merge(full.exp.d[V1==exp.trans$V1[i] & V2==exp.trans$V2[i], .N, by=c('pp','step')][order(pp,step)],
                   full.exp.d[V1==exp.trans$V1[i], .N, by=c('pp','step')][order(pp,step)],
                   by=c('pp','step'),all.y=T)[is.na(N.x),N.x:=0][,'N.p':=N.x/N.y]
  tempDat$pp <- as.factor(tempDat$pp)
  tempMean <- tempDat[,mean(N.p),by='step']
  
  p = ggplot() +
    geom_line(data=tempDat, aes(x=step, y=N.p, col=pp, group=pp), alpha=0.2) +
    geom_line(data=tempMean, aes(x=step, y=V1)) +
    geom_hline(yintercept=k[i], color='red') +
    geom_smooth(data=tempDat, aes(x=step, y=N.p), method='gam', formula=y~s(x), fill='yellow') +
    theme(legend.position = 'none') +
    ggtitle(paste0('from ',exp.trans$V1[i],' to ',gsub('f.','',exp.trans$V2[i])))
  
  pL[[i]] <- p
}
pdf('figs/ExpGen_TransCheck_4.pdf')
for(i in 1:length(pL)){
  print(pL[[i]])
}
dev.off()
