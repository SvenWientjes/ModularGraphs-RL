############################################################################################################################
########################## Script for playing around with gamble choices in the QuadSchap ##################################
############################################################################################################################
library(foreach)
library(ggplot2)
library(reshape2)
library(data.table)
library(zoo)
library(rstan)

# Load Functions from /src/
sapply(paste0('src/',list.files('src/')), source)

# Define point multiplier for winning
winM   <- 5
startP <- 5
nVisit <- 15
nTr    <- 100
nPP    <- 500

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

# Get vertex / edge mappings w.r.t. different goals
idmap.d  <- list(a = c(1,15), b = c(2,3,4, 12,13,14), c = c(5,11), d = c(6,10), e = c(7,9), f = c(16,20), g = c(17,18,19))
idmap.bg <- list(a = c(1), b = c(2,3,4), c = c(5), d = c(7,8,9), e = c(10), f = c(11), g = c(12,13,14), h = c(15), i = c(16), j = c(17,18,19), k = c(20))
idmap.g  <- list(a = c(2,3,4), b = c(7,8,9), c = c(12,13,14), d = c(17,18,19))
c.map    <- list(a = c('d','b'), b = c('a', 'c'), c = c('b','d'), d=c('a','c'))
bt.map   <- list(a = c(1,5), b = c(6,10), c= c(11,15), d=c(20,16))


# Get hitMats
hitMat.d <- as.data.table(read.csv('data/hitMat_quadSchap_step15.csv', row.names=1))
hitMat.b <- as.data.table(read.csv('data/hitMat_quadSchap_step15_btgl.csv', row.names=1))

# Calculate EV for different nodes
EVmat.d <- foreach(v = unique(hitMat.d$vertex), .combine=rbind) %do% {
  tempMat <- EVbet.calc(Edges=Edges, nSteps=nVisit-1, winM=winM, hitMat=hitMat.d, Vertex=v) 
  tempMat
}
EVmat.b <- foreach(v = unique(hitMat.b$vertex), .combine=rbind) %do% {
  tempMat <- EVbet.calc(Edges=Edges, nSteps=nVisit-1, winM=winM, hitMat=hitMat.b, Vertex=v) 
  tempMat
}

# Get policies for the deep & bottleneck node EVmats
stopIdx.d <- EVmat.d[, EV<0,by=.(vertex,steps)
                     ][,if(TRUE %in% V1){
                          .SD[V1==T,max(steps)]
                       }else{
                          as.integer(0)
                        }, 
                       by=vertex
                       ]
stopIdx.b <- EVmat.b[, EV<0,by=.(vertex,steps)
                     ][,if(TRUE %in% V1){
                       .SD[V1==T,max(steps)]
                     }else{
                       as.integer(0)
                     }, 
                     by=vertex
                     ]

## Get simulated trajectories ----
full.exp <- foreach(p = 1:nPP, .combine=rbind) %do% {
  # Initialize experimental experience
  full.exp <- Bet.gen(Edges, nVisit, nTr, c.map, idmap.g, bt.map, parNum=p)
  
  # Get cluster info
  full.exp[, goal.c:=substring(names(which(sapply(unlist(c(idmap.g, bt.map), use.names=T), function(m){goal[1] %in% m}))),1,1), by=tr
           ][, start.c:=substring(names(which(sapply(unlist(c(idmap.g, bt.map), use.names=T), function(m){v[1] %in% m}))),1,1), by=tr]
  
  # Define random strategy
  full.exp[v!=goal, rand.choice:=sample(c(-1,1),1), by=.(tr,stepsleft)       #Sample random choice increments
           ][,rand.trBet := na.locf(bet.sum(rand.choice,startP)),by=tr       #Add up to get bets, with startP starting points
             ][stepsleft == 0, rand.trRew := -rand.trBet, by=tr              #Determine losing bets
               ][v==goal, rand.trRew := winM*rand.trBet, by=tr               #Determine winning bets
                 ][!is.na(rand.trRew),rand.totRew := cumsum(rand.trRew)]     #Add up all losses and wins
  
  # Define modular strategy
  full.exp[v!=goal, mod.choice:=if(v %in% unlist(c(idmap.g[c(goal.c,start.c)], bt.map[c(goal.c,start.c)]))){1}else{-1}, by=.(tr,stepsleft)  # Determine modular choice (start+goal)
           ][,mod.trBet := na.locf(bet.sum(mod.choice, startP)),by=tr                                                                       # Add up to get bets, with startP starting points
             ][stepsleft == 0, mod.trRew := -mod.trBet, by=tr                                                                               # Determine losing bets
               ][v==goal, mod.trRew := winM*mod.trBet, by=tr                                                                                # Determine winning bets
                 ][!is.na(mod.trRew),mod.totRew := cumsum(mod.trRew)]                                                                       # Add up all losses and wins
  
  # Define optimal strategy
  full.exp[v!=goal, opt.choice := if(trtype == 'deep'){
    if(stepsleft > policyBet(v, goal, goal.c, idmap.g,bt.map,c.map,idmap.d,idmap.bg,stopIdx.d)){
      1
    }else{
      -1
    }
  }else{
    if(stepsleft > policyBet(v, goal, goal.c, idmap.g,bt.map,c.map,idmap.d,idmap.bg,stopIdx.b)){
      1
    }else{
      -1
    }
  }, by = .(tr,stepsleft)
  ][,opt.trBet := na.locf(bet.sum(opt.choice, startP)),by=tr      # Add up to get bets, with startP starting points
    ][stepsleft == 0, opt.trRew := -opt.trBet, by=tr              # Determine losing bets
      ][v==goal, opt.trRew := winM*opt.trBet, by=tr               # Determine winning bets
        ][!is.na(opt.trRew),opt.totRew := cumsum(opt.trRew)]      # Add up all losses and wins
  
  return(full.exp)
}

# Plot final rewards with different strategies
ggplot(data=full.exp[tr==100 & !is.na(opt.totRew)]) +
  geom_density(aes(x=opt.totRew), col='green') + 
  geom_density(aes(x=mod.totRew), col='blue') +
  geom_density(aes(x=rand.totRew), col='red')

# Plot nr of betting choices made
ggplot() +
  geom_density(data=full.exp[,.SD[opt.choice==1,.N]/.SD[,.N],by=pp], aes(x=V1), col='green') +
  geom_density(data=full.exp[,.SD[mod.choice==1,.N]/.SD[,.N],by=pp], aes(x=V1), col='blue') +
  geom_density(data=full.exp[,.SD[rand.choice==1,.N]/.SD[,.N],by=pp], aes(x=V1), col='red')

# Get leading nv -> use for tracking transitions
full.exp[,nv:=shift(v,1,type='lead'), by=.(pp,tr)]

# Get distribution of total edge visits
ggplot(full.exp[!is.na(nv), .N, .(v,nv,pp)][,sum(N),by=pp]) +
  geom_density(aes(x=V1))

# Get nr of visits per unique edge per pp
ggplot(full.exp[!is.na(nv), .N, .(v,nv,pp)]) +
  geom_line(aes(x=interaction(v,nv), y=N, group=pp, col=pp))

## Simulate probit regressions on normative choices with set noise level ----
noiseL <- 0.2 # proportion of non-normative choices

# Get non-normative choices
full.exp[!is.na(opt.choice), c('mod.choice.e','opt.choice.e'):=list(sample(c(mod.choice,-mod.choice),size=1,prob=c(1-noiseL,noiseL)),
                                                                    sample(c(opt.choice,-opt.choice),size=1,prob=c(1-noiseL,noiseL))
                                                                ), by=.(pp,tr,stepsleft)]

# stan?












