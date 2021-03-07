############################################################################################################################
########################## Script for playing around with gamble choices in the QuadSchap ##################################
############################################################################################################################
library(foreach)
library(ggplot2)
library(reshape2)
library(data.table)
library(zoo)
library(rstan)
library(bayesplot)

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
  
  # Get mirror symmetry identity w.r.t. goal
  full.exp[, sym.id:=symmetry.get(v, goal, goal.c, idmap.g, bt.map, c.map, idmap.d, idmap.bg), by=.(pp,tr,stepsleft)]
  
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
  full.exp[!is.na(sym.id),opt.choice:=if(trtype=='deep'){
    if(stepsleft > stopIdx.d[vertex%in%idmap.d[[sym.id]]]$V1){
      1
    }else{
      -1
    }
  }else{
    if(stepsleft > stopIdx.b[vertex%in%idmap.bg[[sym.id]]]$V1){
      1
    }else{
      -1
    }
  }, by=.(pp,tr,stepsleft)
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
# Get policy used for separation in analysis
full.exp[,pol.type:=if(trtype=='deep'){'deep'}else{'bottleneck'},by=.(pp,tr,stepsleft)]

noiseL <- 0.1 # proportion of non-normative choices

# Get non-normative choices
full.exp[!is.na(opt.choice), c('mod.choice.e','opt.choice.e'):=list(sample(c(mod.choice,-mod.choice),size=1,prob=c(1-noiseL,noiseL)),
                                                                    sample(c(opt.choice,-opt.choice),size=1,prob=c(1-noiseL,noiseL))
                                                                ), by=.(pp,tr,stepsleft)]

stepProp <- full.exp[!is.na(sym.id) & pp%in%1:500, .SD[opt.choice.e==1,.N]/.N, by=.(pp,pol.type,sym.id,stepsleft)]
stepDat <- stepProp[,mean(V1), by=.(pol.type,sym.id,stepsleft)]
stepDat$sdC <- stepProp[,sd(V1), by=.(pol.type,sym.id,stepsleft)]$V1

ggplot(stepDat, aes(x=stepsleft, y=V1, group=sym.id, col=sym.id))+
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=V1-sdC, ymax=V1+sdC)) +
  facet_grid(pol.type~sym.id)

agProp <- full.exp[!is.na(sym.id), .SD[stan.opt.choice==1,.N]/.N, by=.(pp,pol.type,sym.id)]

ggplot(agProp, aes(x=sym.id, y=V1, fill=pol.type)) +
  geom_bar(stat='identity', position=position_dodge())



# Get full.exp data into stan-appropriate data
full.exp[,stan.opt.choice:=opt.choice.e][opt.choice.e==-1, stan.opt.choice:=0]
x <- full.exp[,reg.id:=interaction(pol.type,sym.id)][!is.na(reg.id) & pp%in%1:500,reg.id]
x <- droplevels(x)
x.d <- fastDummies::dummy_cols(x)

standata_list <- list(
  P = 500,
  N = nrow(full.exp[pp%in%1:500 & !is.na(opt.choice.e)]),
  K = length(levels(x)),
  y = full.exp[pp%in%1:500 & !is.na(opt.choice.e), stan.opt.choice],
  x = x.d[,2:19],
  pn = full.exp[pp%in%1:500 & !is.na(opt.choice.e),pp],
  sl = full.exp[pp%in%1:500 & !is.na(opt.choice.e),stepsleft]
)

fit1 <- stan(
  file = "src/sim1test.stan",
  data = standata_list,
  chains = 2,
  warmup = 10,
  iter = 20,
  cores = 2,
  verbose = T
)
posterior <- as.matrix(fit1)
mcmc_areas(posterior, pars='pi[1]',prob=0.8)

## Model that will have indexing variables for proportions and direct binomial distribution fit in Stan ----
full.exp[,pol.type:=if(trtype=='deep'){'deep'}else{'bottleneck'},by=.(pp,tr,stepsleft)]

noiseL <- 0.4 # proportion of non-normative choices

# Get non-normative choices
full.exp[!is.na(opt.choice), c('mod.choice.e','opt.choice.e'):=list(sample(c(mod.choice,-mod.choice),size=1,prob=c(1-noiseL,noiseL)),
                                                                    sample(c(opt.choice,-opt.choice),size=1,prob=c(1-noiseL,noiseL))
), by=.(pp,tr,stepsleft)]

# Get binary variables for Stan
full.exp[,stan.opt.choice:=opt.choice.e][opt.choice.e==-1, stan.opt.choice:=0]

# Get index variable for regression
full.exp[,reg.id:=interaction(pol.type,sym.id,stepsleft)]
full.exp$reg.id <- droplevels(full.exp$reg.id)

# Get into stan-appropriate data table
Stan.dat <- full.exp[!is.na(stan.opt.choice) & pp%in%1:100, list(bet = .SD[stan.opt.choice==1,.N], N = .N), by=.(pp,reg.id)
         ][,list(pp, bet, N, reg.id.code=match(reg.id,levels(reg.id)))]

standata_list <- list(
  K = max(Stan.dat$reg.id.code),
  M = nrow(Stan.dat),
  N = Stan.dat$N,
  B = Stan.dat$bet,
  Vx = Stan.dat$reg.id.code
)

fit1 <- stan(
  file = "src/sim1test2.stan",
  data = standata_list,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 2,
  verbose = T
)
# Get summary to play around with data
fit1.a <- summary(fit1)$summary

# Get into data.table for plotting
plotdat.fit1 <- data.table(reg.id=levels(full.exp$reg.id), meanT=fit1.a[-nrow(fit1.a),1], lowT=fit1.a[-nrow(fit1.a),4], highT=fit1.a[-nrow(fit1.a),8])
plotdat.fit1[,c('pol.type', 'sym.id', 'stepsleft'):=as.list(unlist(strsplit(reg.id, '[.]'))), by=.(reg.id)]
plotdat.fit1$stepsleft <- as.integer(plotdat.fit1$stepsleft)

# Make plot!
ggplot(plotdat.fit1, aes(x=stepsleft, y=meanT, group=sym.id, col=sym.id))+
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=lowT, ymax=highT)) +
  facet_grid(pol.type~sym.id)

