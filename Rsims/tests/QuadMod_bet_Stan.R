############################################################################################################################
####################### Script for fitting Stan models to different betting datasets QuadMod ###############################
############################################################################################################################
library(ggplot2)
library(data.table)
library(rstan)
library(bayesplot)
library(stringr)
library(bridgesampling)

# Load Functions from /src/
load('data/QuadMod-Bet_testDat-1.RData')
sapply(paste0('src/',list.files('src/')), source)

# Define point multiplier for winning
winM   <- 5
startP <- 5
nVisit <- 15
nTr    <- 100
nPP    <- 500


## Model that will have indexing variables for proportions and direct binomial distribution fit in Stan ----

# Get policy used (defines different stopping behaviours / symmetries)
full.exp[,pol.type:=if(trtype=='deep'){'deep'}else{'bottleneck'},by=.(pp,tr,stepsleft)]

# Proportion of non-normative choices
noiseL <- 0.4

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
  file = "src/Stan/sim1test2.stan",
  data = standata_list,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 2,
  verbose = T
)


## Model that will fit similar values as Binomial but uses Bernoulli - check for speedup ----
noiseL <- 0.1

bern.exp <- full.exp[pp%in%1:10 & !is.na(opt.choice),list(sym.id,
                           pol.type=if(trtype=='deep'){'deep'}else{'bottleneck'},
                           stan.opt.choice=sample(c(opt.choice, -opt.choice), prob=c(1-noiseL, noiseL), size=1)),by=.(pp,tr,stepsleft)
         ][stan.opt.choice==-1, stan.opt.choice:=0
           ][,list(pp,tr,stepsleft,stan.opt.choice, reg.id=interaction(pol.type,sym.id,stepsleft))
             ][,reg.id:=droplevels(reg.id)
               ][,list(pp,tr,stepsleft,stan.opt.choice,reg.id,reg.code=match(reg.id,levels(reg.id)))]

standata_list <- list(
  P  = max(bern.exp$pp),
  K  = max(bern.exp$reg.code),
  M  = nrow(bern.exp),
  Vx = bern.exp$reg.code,
  y  = bern.exp$stan.opt.choice,
  Pn = bern.exp$pp
)

fit1 <- stan(
  file = "src/Stan/sim1test5.stan",
  data = standata_list,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 2,
  verbose = T
)

fit1summary <- summary(fit1)$summary

fit1plotdat <- data.table(reg.id = levels(bern.exp$reg.id), meanT=fit1summary[1:257,1], minT=fit1summary[1:257,4], maxT=fit1summary[1:257,8])
fit1plotdat[,c('pol.type','sym.id','stepsleft'):=as.list(unlist(strsplit(reg.id, '[.]'))), by=.(reg.id)
            ][,stepsleft:=as.numeric(stepsleft)]

fit1pardat <-  data.table(pp= rep(1:max(bern.exp$pp), each=length(levels(bern.exp$reg.id))), reg.id = rep(levels(bern.exp$reg.id), times=max(bern.exp$pp)),
                          meanT=fit1summary[258:(257+length(levels(bern.exp$reg.id))*max(bern.exp$pp)),1],
                          minT =fit1summary[258:(257+length(levels(bern.exp$reg.id))*max(bern.exp$pp)),4],
                          maxT =fit1summary[258:(257+length(levels(bern.exp$reg.id))*max(bern.exp$pp)),8])
fit1pardat[,c('pol.type','sym.id','stepsleft'):=as.list(unlist(strsplit(reg.id, '[.]'))), by=.(reg.id)
           ][,stepsleft:=as.numeric(stepsleft)]
                          
ggplot(fit1plotdat, aes(x=stepsleft, y=meanT, group=sym.id, col=sym.id))+
  geom_line() +
  geom_line(data=fit1pardat, aes(x=stepsleft, y=meanT, group=interaction(sym.id,pp)),col='gray', alpha=0.5) +
  geom_point() +
  geom_errorbar(aes(ymin=minT, ymax=maxT)) +
  geom_errorbar(data=fit1pardat, aes(ymin=minT, ymax=maxT, group=interaction(sym.id,pp),x=stepsleft),col='gray', alpha=0.5) +
  facet_grid(pol.type~sym.id)

which(levels(bern.exp$reg.id)=='deep.c.3')
which(levels(bern.exp$reg.id)=='deep.d.3')
plot(density(post.samps[,which(levels(bern.exp$reg.id)=='deep.d.3')] - post.samps[,which(levels(bern.exp$reg.id)=='deep.c.3')]))
post.samps[,which(colnames(post.samps == paste('beta[', which(levels(bern.exp$reg.id)=='deep.c.3'), ']', sep='')))]


mcmc_areas(post.samps, pars=c('beta[60]','beta[62]'), prob=0.8)

## Model that gets linear regressions on stepsleft with intercept 0step ----
noiseL <- 0.1

stepreg.exp <- full.exp[pp%in%1:100 & !is.na(opt.choice),list(sym.id,
                                                           pol.type=if(trtype=='deep'){'deep'}else{'bottleneck'},
                                                           stan.opt.choice=sample(c(opt.choice, -opt.choice), prob=c(1-noiseL, noiseL), size=1)),by=.(pp,tr,stepsleft)
                     ][stan.opt.choice==-1, stan.opt.choice:=0
                       ][,list(pp,tr,stepsleft,stan.opt.choice, reg.id=interaction(pol.type,sym.id))
                         ][,reg.id:=droplevels(reg.id)
                           ][,list(pp,tr,stepsleft,stan.opt.choice,reg.id,reg.code=match(reg.id,levels(reg.id)))]

standata_list <- list(
  P  = max(stepreg.exp$pp),
  K  = max(stepreg.exp$reg.code),
  M  = nrow(stepreg.exp),
  S  = length(unique(stepreg.exp$stepsleft)),
  Vx = stepreg.exp$reg.code,
  y  = stepreg.exp$stan.opt.choice,
  Pn = stepreg.exp$pp,
  Sl = stepreg.exp$stepsleft
)

fit1 <- stan(
  file = "src/Stan/sim1test6.stan",
  data = standata_list,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 2,
  verbose = T
)

fit1summary <- summary(fit1)$summary

# Get data into plottable probability table
probplot.dat <- as.data.table(keep.rownames=T, fit1summary[as.numeric(which(sapply(rownames(fit1summary), function(s){grepl('theta',s,fixed=T)}))),])
probplot.dat[,c('reg.code','stepsleft'):=as.list(unlist(strsplit(str_sub(sub("^[^[]*", "", rn),2,-2), '[,]'))),by=rn
             ][,stepsleft:=as.integer(stepsleft)-1
               ][,c('pol.type','sym.id'):=as.list(unlist(strsplit(levels(stepreg.exp$reg.id)[as.integer(reg.code)], '[.]'))), by=rn]
# Plot probs
ggplot(probplot.dat, aes(x=stepsleft, y=mean, group=sym.id, col=sym.id))+
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=probplot.dat$'2.5%', ymax=probplot.dat$'97.5%', group=sym.id, x=stepsleft)) +
  facet_grid(pol.type~sym.id)

# Repeat for log-odd space
# Get data into plottable probability table
probplot.dat <- as.data.table(keep.rownames=T, fit1summary[as.numeric(which(sapply(rownames(fit1summary), function(s){grepl('oddreg',s,fixed=T)}))),])
probplot.dat[,c('reg.code','stepsleft'):=as.list(unlist(strsplit(str_sub(sub("^[^[]*", "", rn),2,-2), '[,]'))),by=rn
             ][,stepsleft:=as.integer(stepsleft)-1
               ][,c('pol.type','sym.id'):=as.list(unlist(strsplit(levels(stepreg.exp$reg.id)[as.integer(reg.code)], '[.]'))), by=rn]
# Plot probs
ggplot(probplot.dat, aes(x=stepsleft, y=mean, group=sym.id, col=sym.id))+
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=probplot.dat$'2.5%', ymax=probplot.dat$'97.5%', group=sym.id, x=stepsleft)) +
  facet_grid(pol.type~sym.id)

# Extract posteriors for all parameters
draw.posterior <- as.matrix(fit1)

# Compare specific nodes by stepsleft
deep.bn.compare <- foreach(sl = 1:15, .combine=cbind) %do% {
  draw.posterior[,which(colnames(draw.posterior) == paste('oddreg[', which(levels(stepreg.exp$reg.id)=='bottleneck.d'), ',', sl, ']', sep=''))] - 
    draw.posterior[,which(colnames(draw.posterior) == paste('oddreg[', which(levels(stepreg.exp$reg.id)=='bottleneck.c'), ',', sl, ']', sep=''))]
}

# Extract HDIs
ggplot(data=data.table(stepsleft = 1:15,
           MAP  = as.numeric(sapply(1:15, function(s){point_estimate(deep.bn.compare[,s], centrality='MAP')})), 
           Cmin = as.numeric(sapply(1:15, function(s){hdi(deep.bn.compare[,s])$CI_low})),
           Cmax = as.numeric(sapply(1:15, function(s){hdi(deep.bn.compare[,s])$CI_high})))) +
  geom_line(aes(x=stepsleft, y=MAP)) +
  geom_point(aes(x=stepsleft, y=MAP)) +
  geom_errorbar(aes(ymin=Cmin, ymax=Cmax, x=stepsleft)) +
  geom_hline(yintercept=0, col='red') +
  ggtitle('Bottlegoal comparison with identical policy', 'MAP with 89% HDI')

## Model that gets linear regressions with free intercept ----
noiseL <- 0.1

stepreg.exp <- full.exp[pp%in%1:100 & !is.na(opt.choice),list(sym.id,
                                                              pol.type=if(trtype=='deep'){'deep'}else{'bottleneck'},
                                                              stan.opt.choice=sample(c(opt.choice, -opt.choice), prob=c(1-noiseL, noiseL), size=1)),by=.(pp,tr,stepsleft)
                        ][stan.opt.choice==-1, stan.opt.choice:=0
                          ][,list(pp,tr,stepsleft,stan.opt.choice, reg.id=interaction(pol.type,sym.id))
                            ][,reg.id:=droplevels(reg.id)
                              ][,list(pp,tr,stepsleft,stan.opt.choice,reg.id,reg.code=match(reg.id,levels(reg.id)))
                                ][,stepsleft:=stepsleft+1]

standata_list <- list(
  P  = max(stepreg.exp$pp),
  K  = max(stepreg.exp$reg.code),
  M  = nrow(stepreg.exp),
  S  = length(unique(stepreg.exp$stepsleft)),
  Vx = stepreg.exp$reg.code,
  y  = stepreg.exp$stan.opt.choice,
  Pn = stepreg.exp$pp,
  Sl = stepreg.exp$stepsleft
)

fit1 <- stan(
  file = "src/Stan/sim1test7.stan",
  data = standata_list,
  chains = 4,
  warmup = 1500,
  iter = 3000,
  cores = 2,
  verbose = T
)

fit1summary <- summary(fit1)$summary

# Get data into plottable probability table
probplot.dat <- as.data.table(keep.rownames=T, fit1summary[as.numeric(which(sapply(rownames(fit1summary), function(s){grepl('theta',s,fixed=T)}))),])
probplot.dat[,c('reg.code','stepsleft'):=as.list(unlist(strsplit(str_sub(sub("^[^[]*", "", rn),2,-2), '[,]'))),by=rn
             ][,stepsleft:=as.integer(stepsleft)-1
               ][,c('pol.type','sym.id'):=as.list(unlist(strsplit(levels(stepreg.exp$reg.id)[as.integer(reg.code)], '[.]'))), by=rn]
# Plot probs
ggplot(probplot.dat, aes(x=stepsleft, y=mean, group=sym.id, col=sym.id))+
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=probplot.dat$'2.5%', ymax=probplot.dat$'97.5%', group=sym.id, x=stepsleft)) +
  facet_grid(pol.type~sym.id)

## Model that uses splines to estimate ----
noiseL <- 0.1

splinereg.exp <- full.exp[pp%in%1:10 & !is.na(opt.choice),list(sym.id,
                                                              pol.type=if(trtype=='deep'){'deep'}else{'bottleneck'},
                                                              stan.opt.choice=sample(c(opt.choice, -opt.choice), prob=c(1-noiseL, noiseL), size=1)),by=.(pp,tr,stepsleft)
                        ][stan.opt.choice==-1, stan.opt.choice:=0
                          ][,list(pp,tr,stepsleft,stan.opt.choice, reg.id=interaction(pol.type,sym.id))
                            ][,reg.id:=droplevels(reg.id)
                              ][,list(pp,tr,stepsleft,stan.opt.choice,reg.id,reg.code=match(reg.id,levels(reg.id)))
                                ][,stepsleft:=stepsleft+1]
num_knots <- 4
spline_degree <- 3
knots <- unname(quantile(splinereg.exp$stepsleft,probs=seq(from=0, to=1, length.out = num_knots)))

splinedat <- list(
  P  = max(splinereg.exp$pp),
  K  = max(splinereg.exp$reg.code),
  M  = nrow(splinereg.exp),
  S  = length(unique(splinereg.exp$stepsleft)),
  Vx = splinereg.exp$reg.code,
  y  = splinereg.exp$stan.opt.choice,
  Pn = splinereg.exp$pp,
  Sl = splinereg.exp$stepsleft,
  num_knots = num_knots,
  knots = knots,
  spline_degree = spline_degree
)

splinefit <- stan(
  file = "src/Stan/spline1test1.stan",
  data = splinedat,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 4,
  verbose = T
)

splinesum <- as.data.table(summary(splinefit)$summary, keep.rownames = T)[grepl('Y_hat',rn, fixed=T),]

splineplot.dat <- data.table(pol.type='a', sym.id='a', stepsleft=0, mean=0, minC=0, maxC=0)
for(id in levels(splinereg.exp$reg.id)){
  for(s in unique(splinereg.exp$stepsleft)){
    splineplot.dat <- rbind(splineplot.dat, data.table(pol.type=strsplit(id, '[.]')[[1]][1], sym.id=strsplit(id, '[.]')[[1]][2], stepsleft=s, mean=splinesum[which(splinereg.exp$reg.id==id & splinereg.exp$stepsleft==s)[1],]$mean, 
                                                       minC=splinesum[which(splinereg.exp$reg.id==id & splinereg.exp$stepsleft==s)[1],]$'2.5%', maxC=splinesum[which(splinereg.exp$reg.id==id & splinereg.exp$stepsleft==s)[1],]$'97.5%'))
  }
}
splineplot.dat <- splineplot.dat[-1,]
splineplot.dat[,c('mean','minC','maxC'):=list(boot::inv.logit(mean), boot::inv.logit(minC), boot::inv.logit(maxC)), by=.(pol.type,sym.id,stepsleft)]

ggplot(splineplot.dat, aes(x=stepsleft, y=mean, group=sym.id, col=sym.id))+
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=minC, ymax=maxC, group=sym.id, x=stepsleft)) +
  facet_grid(pol.type~sym.id)


## Model with participant level spline regressors ----
noiseL <- 0.1

splinereg.exp <- full.exp[pp%in%1:10 & !is.na(opt.choice),list(sym.id,
                                                               pol.type=if(trtype=='deep'){'deep'}else{'bottleneck'},
                                                               stan.opt.choice=sample(c(opt.choice, -opt.choice), prob=c(1-noiseL, noiseL), size=1)),by=.(pp,tr,stepsleft)
                          ][stan.opt.choice==-1, stan.opt.choice:=0
                            ][,list(pp,tr,stepsleft,stan.opt.choice, reg.id=interaction(pol.type,sym.id))
                              ][,reg.id:=droplevels(reg.id)
                                ][,list(pp,tr,stepsleft,stan.opt.choice,reg.id,reg.code=match(reg.id,levels(reg.id)))
                                  ][,stepsleft:=stepsleft+1]
num_knots <- 4
spline_degree <- 3
knots <- unname(quantile(splinereg.exp$stepsleft,probs=seq(from=0, to=1, length.out = num_knots)))

splinedat <- list(
  P  = max(splinereg.exp$pp),
  K  = max(splinereg.exp$reg.code),
  M  = nrow(splinereg.exp),
  S  = length(unique(splinereg.exp$stepsleft)),
  Vx = splinereg.exp$reg.code,
  y  = splinereg.exp$stan.opt.choice,
  Pn = splinereg.exp$pp,
  Sl = splinereg.exp$stepsleft,
  num_knots = num_knots,
  knots = knots,
  spline_degree = spline_degree
)

splinefit2 <- stan(
  file = "src/Stan/spline1test2.stan",
  data = splinedat,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 4,
  verbose = T
)

splinesum2 <- as.data.table(summary(splinefit2)$summary, keep.rownames = T)[grepl('Y_hat',rn, fixed=T),]

splineplot.dat2 <- data.table(pp=0, pol.type='a', sym.id='a', stepsleft=0, mean=0, minC=0, maxC=0)
for(p in 1:10){
  for(id in levels(splinereg.exp$reg.id)){
    for(s in unique(splinereg.exp$stepsleft)){
      splineplot.dat2 <- rbind(splineplot.dat2, data.table(pp=p, pol.type=strsplit(id, '[.]')[[1]][1], sym.id=strsplit(id, '[.]')[[1]][2], stepsleft=s, mean=splinesum2[which(splinereg.exp$pp==p & splinereg.exp$reg.id==id & splinereg.exp$stepsleft==s)[1],]$mean, 
                                                         minC=splinesum2[which(splinereg.exp$pp==p & splinereg.exp$reg.id==id & splinereg.exp$stepsleft==s)[1],]$'2.5%', maxC=splinesum2[which(splinereg.exp$pp==p & splinereg.exp$reg.id==id & splinereg.exp$stepsleft==s)[1],]$'97.5%'))
    }
  }
}

splineplot.dat2 <- splineplot.dat2[-1,]
splineplot.dat2[,c('mean','minC','maxC'):=list(boot::inv.logit(mean), boot::inv.logit(minC), boot::inv.logit(maxC)), by=.(pp,pol.type,sym.id,stepsleft)
                ][,pp:=as.factor(pp)]

ggplot(splineplot.dat2, aes(x=stepsleft, y=mean, group=sym.id, col=pp))+
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=minC, ymax=maxC, group=sym.id, x=stepsleft)) +
  facet_grid(pol.type~sym.id)

bridge_sampler(splinefit2)

bridge_sampler(splinefit)

## One dataset, four hierarchical models - Free, Linear, Spline, Optimal ----
noiseL <- 0.1

comp.exp <- full.exp[pp%in%1:2 & !is.na(opt.choice),list(sym.id, opt.choice,
                                                               pol.type=if(trtype=='deep'){'deep'}else{'bottleneck'},
                                                               stan.opt.choice=sample(c(opt.choice, -opt.choice), prob=c(1-noiseL, noiseL), size=1)),by=.(pp,tr,stepsleft)
                          ][stan.opt.choice==-1, stan.opt.choice:=0
                            ][opt.choice==-1, opt.choice:=0
                              ][,list(pp,tr,stepsleft,opt.choice,stan.opt.choice, reg.id=interaction(pol.type,sym.id), free.id=interaction(pol.type,sym.id,stepsleft))
                                ][,c('reg.id','free.id'):=list(droplevels(reg.id), droplevels(free.id))
                                  ][,list(pp,tr,stepsleft,opt.choice,stan.opt.choice,reg.id,free.id,reg.code=match(reg.id,levels(reg.id)),free.code=match(free.id,levels(free.id)))
                                    ][,stepsleft:=stepsleft+1]

num_knots <- 4
spline_degree <- 3
knots <- unname(quantile(comp.exp$stepsleft,probs=seq(from=0, to=1, length.out = num_knots)))

freedata_list <- list(
  P  = max(comp.exp$pp),
  K  = max(comp.exp$free.code),
  M  = nrow(comp.exp),
  Vx = comp.exp$free.code,
  y  = comp.exp$stan.opt.choice,
  Pn = comp.exp$pp
)

freefit <- stan(
  file = "src/Stan/sim1test5.stan",
  data = freedata_list,
  chains = 4,
  warmup = 1500,
  iter = 10000,
  cores = 4,
  verbose = T,
  save_warmup=F
)

lindata_list <- list(
  P  = max(comp.exp$pp),
  K  = max(comp.exp$reg.code),
  M  = nrow(comp.exp),
  S  = length(unique(comp.exp$stepsleft)),
  Vx = comp.exp$reg.code,
  y  = comp.exp$stan.opt.choice,
  Pn = comp.exp$pp,
  Sl = comp.exp$stepsleft
)

linfit <- stan(
  file = "src/Stan/sim1test7.stan",
  data = lindata_list,
  chains = 4,
  warmup = 1500,
  iter = 5000,
  cores = 4,
  verbose = T,
  save_warmup=F
)

splinedata_list <- list(
  P  = max(comp.exp$pp),
  K  = max(comp.exp$reg.code),
  M  = nrow(comp.exp),
  S  = length(unique(comp.exp$stepsleft)),
  Vx = comp.exp$reg.code,
  y  = comp.exp$stan.opt.choice,
  Pn = comp.exp$pp,
  Sl = comp.exp$stepsleft,
  num_knots = num_knots,
  knots = knots,
  spline_degree = spline_degree
)

splinefit <- stan(
  file = "src/Stan/spline1test2.stan",
  data = splinedata_list,
  chains = 4,
  warmup = 1500,
  iter = 3000,
  cores = 4,
  verbose = T,
  save_warmup=F
)

optdata_list <- list(
  P  = max(comp.exp$pp),
  M  = nrow(comp.exp),
  y  = comp.exp$stan.opt.choice,
  Opt = comp.exp$opt.choice,
  Pn = comp.exp$pp
)

optfit <- stan(
  file = "src/Stan/opt_regress.stan",
  data = optdata_list,
  chains = 4,
  warmup = 1500,
  iter = 3000,
  cores = 4,
  verbose = T,
  save_warmup=F
)
## Choices based upon EV of current state ----

# Get symmetry and points from hitMats
EVcMat <- rbind(hitMat.d[,list(vertex,steps,goalprob,cumgoal=cumsum(goalprob)),by=.(vertex)
                  ][,list(sym.it   = symmetry.get(vertex,8, 'b', idmap.g, bt.map, c.map, idmap.d, idmap.bg),
                    EVcp     = min(1,cumgoal*5),
                    pol.it = 'deep'),by=.(vertex,steps,goalprob)],
                hitMat.b[,list(vertex,steps,goalprob,cumgoal=cumsum(goalprob)),by=.(vertex)
                  ][,list(sym.it   = symmetry.get(vertex,6, 'b', idmap.g, bt.map, c.map, idmap.d, idmap.bg),
                    EVcp     = min(1,cumgoal*5),
                    pol.it = 'bottleneck'),by=.(vertex,steps,goalprob)])
EVcMat <- rbind(EVcMat,
                data.table(vertex=unique(EVcMat[pol.it=='deep']$vertex),
                           steps=0, goalprob=0, sym.it=unique(EVcMat[pol.it=='deep']$sym.it),
                           pol.it='deep', EVcp=0),
                data.table(vertex=unique(EVcMat[pol.it=='bottleneck']$vertex),
                           steps=0, goalprob=0, sym.it=unique(EVcMat[pol.it=='bottleneck']$sym.it),
                           pol.it='bottleneck', EVcp=0))

EVregMat <- rbind(EVmat.d[,list(sym.it = symmetry.get(vertex, 8, 'b', idmap.g, bt.map, c.map, idmap.d, idmap.bg),
                    pol.id = 'deep',EV),by=.(vertex,steps)],
                  EVmat.b[,list(sym.it = symmetry.get(vertex, 6, 'b', idmap.g, bt.map, c.map, idmap.d, idmap.bg),
                    pol.id = 'bottleneck',EV),by=.(vertex,steps)])
EVregMat <- rbind(EVregMat,
                  data.table(vertex=unique(EVregMat[pol.id=='deep']$vertex),
                             steps=0, sym.it=unique(EVregMat[pol.id=='deep']$sym.it),
                             pol.id='deep', EV=-1),
                  data.table(vertex=unique(EVregMat[pol.id=='bottleneck']$vertex),
                             steps=0, sym.it=unique(EVregMat[pol.id=='bottleneck']$sym.it),
                             pol.id='bottleneck', EV=-1))

EV.exp <- full.exp[pp %in% 1:20 & !is.na(opt.choice), list(sym.id, opt.choice,
                                                          pol.type=if(trtype=='deep'){'deep'}else{'bottleneck'})
                                                          ,by=.(pp,tr,stepsleft)
                   ][,list(sym.id, pol.type, opt.choice, EV.choice = sample(c(1,0), prob=c(EVcMat[sym.it==sym.id & 
                                                                 pol.it==pol.type & 
                                                                 steps==stepsleft]$EVcp,
                                                        1 - EVcMat[sym.it==sym.id & 
                                                                     pol.it==pol.type & 
                                                                     steps==stepsleft]$EVcp),
                                         size=1)), by=.(pp,tr,stepsleft)
                     ][opt.choice==-1, opt.choice:=0
                       ][,list(pp,tr,stepsleft,sym.id,pol.type,opt.choice,EV.choice,reg.id=interaction(pol.type,sym.id), free.id=interaction(pol.type,sym.id,stepsleft))
                         ][,c('reg.id','free,id'):=list(droplevels(reg.id), droplevels(free.id))
                           ][,list(pp,tr,stepsleft,sym.id,pol.type,opt.choice,EV.choice,reg.id,free.id,reg.code=match(reg.id,levels(reg.id)),free.code=match(free.id,levels(free.id)))
                             ][,stepsleft:=stepsleft+1
                               ][,list(sym.id,pol.type,opt.choice,EV.choice,reg.id,free.id,reg.code,free.code,
                                       EV.reg = EVregMat[sym.it==sym.id & pol.id==pol.type & steps==(stepsleft-1)]$EV),by=.(pp,tr,stepsleft)
                                 ]

ggplot(EV.exp[,sum(EV.choice==1)/.N,by=.(pp,sym.id,pol.type,stepsleft)]) +
  geom_line(aes(x=stepsleft, y=V1, col=pp)) +
  geom_point(aes(x=stepsleft, y=V1, col=pp)) +
  facet_grid(pol.type~sym.id)

ggplot(EV.exp) +
  geom_line(aes(x=stepsleft, y=EV.reg, col=sym.id)) +
  geom_point(aes(x=stepsleft, y=EV.reg, col=sym.id)) +
  facet_grid(pol.type~sym.id)

# Fit spline
num_knots <- 4
spline_degree <- 3
knots <- unname(quantile(EV.exp$stepsleft,probs=seq(from=0, to=1, length.out = num_knots)))

splinedata_list <- list(
  P  = max(EV.exp$pp),
  K  = max(EV.exp$reg.code),
  M  = nrow(EV.exp),
  S  = length(unique(EV.exp$stepsleft)),
  Vx = EV.exp$reg.code,
  y  = EV.exp$EV.choice,
  Pn = EV.exp$pp,
  Sl = EV.exp$stepsleft,
  num_knots = num_knots,
  knots = knots,
  spline_degree = spline_degree
)

splinefit <- stan(
  file = "src/Stan/spline1test2.stan",
  data = splinedata_list,
  chains = 4,
  warmup = 1500,
  iter = 5000,
  cores = 4,
  verbose = T,
  save_warmup=F
)

# Fit linear model
lindata_list <- list(
  P  = max(EV.exp$pp),
  K  = max(EV.exp$reg.code),
  M  = nrow(EV.exp),
  S  = length(unique(EV.exp$stepsleft)),
  Vx = EV.exp$reg.code,
  y  = EV.exp$EV.choice,
  Pn = EV.exp$pp,
  Sl = EV.exp$stepsleft
)

linfit <- stan(
  file = "src/Stan/sim1test7.stan",
  data = lindata_list,
  chains = 4,
  warmup = 1500,
  iter = 5000,
  cores = 4,
  verbose = T,
  save_warmup=F
)

