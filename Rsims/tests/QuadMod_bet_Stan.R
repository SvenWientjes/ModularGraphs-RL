############################################################################################################################
####################### Script for fitting Stan models to different betting datasets QuadMod ###############################
############################################################################################################################
library(foreach)
library(ggplot2)
library(reshape2)
library(data.table)
library(zoo)
library(rstan)
library(bayesplot)

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

bern.exp <- full.exp[pp%in%1:100 & !is.na(opt.choice),list(sym.id,
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
post.samps <- as.matrix(fit1)
post.samps[,which(colnames(post.samps == paste('beta[', which(levels(bern.exp$reg.id)=='deep.c.3'), ']', sep='')))]





