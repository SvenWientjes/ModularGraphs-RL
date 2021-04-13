#####################################################################################################
## Script for playing around with SR-matrix learning and selecting actions based on clamped values ##
#####################################################################################################
# Load initial requirements
load('data/QuadMod-Bet_testDat-1.RData')
sapply(paste0('src/',list.files('src/')), source)
winM   <- 5
nVisit <- 15
nTr    <- 100
nPP    <- 40

# Play around with SR learning
lr <- 0.05
gamm <- 0.95
SR <- diag(20)

for(trial in 1:nTr){
  v <- full.exp[pp==1 & tr==trial,v]
  for(s in 2:length(v)){
    I <- rep(0,20); I[v[s-1]] <- 1
    er <- I + gamm * SR[v[s],] - SR[v[s-1],]
    SR[v[s-1],] = SR[v[s-1],] + lr*er
    
  }
}
heatmap(SR, Rowv = NA, Colv = NA)

# Make choice predictions?
ct <- 0.60
vt <- rep(-(ct^3), 20); vt[8] <- winM
SR %*% vt

# Plot SREV over time
SR.EVreg <- seq.SR.EV(full.exp[pp==1,tr], full.exp[pp==1,v], full.exp[pp==1,goal], full.exp[pp==1,stepsleft], lr, gamm, ct, winM=3)
plot(SR.EVreg,
     type='l')
abline(0, b=0, col='red')

# Get SREV based choices! 
choiceS <- sapply(boot::inv.logit(2*SR.EVreg), function(b){rbinom(1,1,b)})
plot.dat <- data.table(sym.id = full.exp[pp==1,sym.id], pol.type = full.exp[pp==1,pol.type], stepsleft=full.exp[pp==1,stepsleft],choiceS=choiceS)

ggplot(plot.dat[,sum(choiceS==1)/.N, by=.(sym.id,pol.type,stepsleft)]) +
  geom_line(aes(x=stepsleft, y=V1)) +
  geom_point(aes(x=stepsleft, y=V1)) +
  facet_grid(pol.type~sym.id)

#### LYNN EV tryouts ----
Lynn.EVreg <- seq.Lynn.EV(full.exp[pp==1,tr], full.exp[pp==1,v], full.exp[pp==1,goal], full.exp[pp==1,stepsleft], betap=0.3, winM=3, loseM=-1)

heatmap(Lynn.EVreg$Ahat, Rowv = NA, Colv = NA)

plot(Lynn.EVreg$Lynn.EV, type='l')
abline(0, b=0, col='red')

choiceS <- sapply(boot::inv.logit(2*Lynn.EVreg$Lynn.EV), function(b){rbinom(1,1,b)})
plot.dat <- data.table(sym.id = full.exp[pp==1,sym.id], pol.type = full.exp[pp==1, pol.type], stepsleft = full.exp[pp==1,stepsleft], choiceS=choiceS)
ggplot(plot.dat[,sum(choiceS==1)/.N, by=.(sym.id,pol.type,stepsleft)]) +
  geom_line(aes(x=stepsleft, y=V1)) +
  geom_point(aes(x=stepsleft, y=V1)) +
  facet_grid(pol.type~sym.id)

# Get choices into data.table
lc.exp <- full.exp[pp==1,list(pp,tr,stepsleft,v,goal,choiceS=choiceS,result)
                   ][choiceS==0,choiceS:=-1
                   ][,betS:=bet.sum(choiceS,5),by=.(pp,tr)
                   ][stepsleft==0 & v!=goal, eRew:=-betS
                   ][v==goal, eRew:=betS*3
                   ][,opt.choice:=get.opt.choice(tr,v,goal,stepsleft,tMat,winM=3,loseM=-1)
                   ][opt.choice==0,opt.choice:=-1
                   ][,opt.bet:=bet.sum(opt.choice,5),by=.(pp,tr)
                   ][stepsleft==0 & v!=goal, opt.Rew:=-opt.bet
                   ][v==goal, opt.Rew:=opt.bet*3]

lc.exp[,bet.sum(na.omit(eRew),0)]

# Plot accumulation of reward throughout experiment: Lynnlearn v optEV
plot(lc.exp[!is.na(opt.Rew),bet.sum(na.omit(opt.Rew),0)],type='l', col='green')
lines(lc.exp[!is.na(eRew),bet.sum(na.omit(eRew),0)], type='l', col='purple')

# Reward rate Lynnlearn / optEV
ggplot(lc.exp[!is.na(opt.Rew),list(tr, rwrat=eRew-opt.Rew)])+
  geom_line(aes(x=tr,y=rwrat))

# Proportion of optimal choices (choiceS==opt.choice/.N)
ggplot(lc.exp[,sum(choiceS==opt.choice)/.N, by=tr]) +
  geom_line(aes(x=tr,y=V1))

# Per 4 trials, proportion of optimal choices
lc.exp[,list(group=sum((tr/1:25)>4)+1, opt.choice,choiceS),by=.(pp,tr,stepsleft)
       ][,sum(opt.choice==choiceS)/.N, by=group
       ][,ggplot(.SD, aes(x=group,y=V1))+
           geom_line()]

## Get LynnEV and choices per participant - Compare with 
lc.exp <- full.exp[,list(pp,tr,stepsleft,v,goal)
                   ][,LynnEV:=seq.Lynn.EV(tr,v,goal,stepsleft,betap=0.2,winM=1,loseM=-1)$Lynn.EV,by=pp
                   ][stepsleft==0 & v!=goal, LynnEV:=-1
                   ][v==goal, LynnEV:=1]
lc.exp[,LynnC:=rbinom(1,1,boot::inv.logit(Inf*LynnEV)),by=.(pp,tr,stepsleft)
       ][LynnC==0,LynnC:=-1
       ][,OptC:=get.opt.choice(tr,v,goal,stepsleft,tMat,winM=1,loseM=-1),by=pp
       ][OptC==0,OptC:=-1
       ][,c('LynnBet','OptBet'):=list(bet.sum(LynnC,5), bet.sum(OptC,5)), by=.(pp,tr)
       ][stepsleft==0 & v!=goal, c('LynnRew','OptRew'):=list(-LynnBet, -OptBet)
       ][v==goal, c('LynnRew','OptRew'):=list(LynnBet*1, OptBet*1)]

# Plot of proportion of optimal choices by trial
lc.exp[,list(group=sum((tr/1:25)>4)+1, LynnC, OptC),by=.(pp,tr,stepsleft)
       ][,pp:=as.factor(pp)
       ][,list(propC=sum(LynnC==OptC)/.N), by=.(pp,tr)
       ][,ggplot(.SD)+
           geom_line(aes(group=pp, x=tr, y=propC, col=pp))+
           geom_line(data=.SD[,list(meanProp=mean(propC)),by=tr],aes(x=tr,y=meanProp))]
# Plot of proportion of optimal choices by group
lc.exp[,list(group=sum((tr/1:25)>4)+1, LynnC, OptC),by=.(pp,tr,stepsleft)
       ][,pp:=as.factor(pp)
       ][,list(propC=sum(LynnC==OptC)/.N), by=.(pp,group)
       ][,ggplot(.SD)+
           geom_line(aes(group=pp, x=group,y=propC,col=pp),alpha=0.2)+
           geom_line(data=.SD[,list(meanProp=mean(propC)),by=group], aes(x=group,y=meanProp))]
# Plot of reward rate over group
lc.exp[!is.na(LynnRew),list(group=sum((tr/1:25)>4)+1, LynnRew, OptRew),by=.(pp,tr,stepsleft)
       ][,pp:=as.factor(pp)
       ][,list(grRew=sum(LynnRew), grOpt=sum(OptRew)),by=.(pp,group)
       ][,ggplot(.SD)+
           geom_line(aes(group=pp, x=group, y=grRew),col='purple',alpha=0.2)+
           geom_line(aes(group=pp, x=group, y=grOpt),col='green',alpha=0.2)+
           geom_line(data=.SD[,list(m=mean(grRew)),by=group], aes(x=group, y=m), col='purple')+
           geom_line(data=.SD[,list(m=mean(grOpt)),by=group], aes(x=group, y=m), col='green')]


## Get SREV and choices
sr.exp <- full.exp[,list(pp,tr,stepsleft,v,goal)
                   ][,SREV:=seq.SR.EV(tr,v,goal,stepsleft,lr=0.05,gamm=0.95,ct=0.6,winM=5),by=pp
                   ][,SRC:=rbinom(1,1,boot::inv.logit(Inf*SREV)),by=.(pp,tr,stepsleft)
                   ][SRC==0, SRC:=-1
                   ][,OptC:=get.opt.choice(tr,v,goal,stepsleft,tMat,winM=1,loseM=-1),by=pp
                   ][OptC==0, OptC:=-1
                   ][,c('SRBet','OptBet'):=list(bet.sum(SRC,5), bet.sum(OptC,5)), by=.(pp,tr)
                   ][stepsleft==0 & v!=goal, c('SRRew','OptRew'):=list(-SRBet, -OptBet)
                   ][v==goal, c('SRRew','OptRew'):=list(SRBet*1, OptBet*1)]

sr.exp[,list(group=sum((tr/1:25)>4)+1,SRC,OptC),by=.(pp,tr,stepsleft)
       ][,pp:=as.factor(pp)
       ][,list(propC=sum(SRC==OptC)/.N), by=.(pp,tr)
       ][,ggplot(.SD)+
           geom_line(aes(group=pp,col=pp,x=tr,y=propC))+
           geom_line(data=.SD[,list(m=mean(propC)),by=tr],aes(x=tr,y=m))]

sr.exp[!is.na(SRRew),list(group=sum((tr/1:25)>4)+1,SRRew,OptRew),by=.(pp,tr,stepsleft)
       ][,pp:=as.factor(pp)
       ][,list(grRew=sum(SRRew), grOpt=sum(OptRew)),by=.(pp,group)
       ][,ggplot(.SD)+
           geom_line(aes(group=pp,x=group,y=grRew),col='purple',alpha=0.1)+
           geom_line(data=.SD[,list(m=mean(grRew)),by=group],aes(x=group,y=m),col='purple')+
           geom_line(aes(x=group,y=grOpt,group=pp),col='green',alpha=0.1)+
           geom_line(data=.SD[,list(m=mean(grOpt)),by=group],aes(x=group,y=m),col='green')]

# Compare with heuristic strategies! 
sr.exp[,RiskC:=1
       ][,RiskBet:=bet.sum(RiskC,5), by=.(pp,tr)
       ][stepsleft==0&v!=goal, RiskRew:=-RiskBet
       ][v==goal, RiskRew:=RiskBet*1]
sr.exp[,StepC:=1
       ][stepsleft<7&v!=goal, StepC:=-1
       ][,StepBet:=bet.sum(StepC,5), by=.(pp,tr)
       ][stepsleft==0&v!=goal, StepRew:=-StepBet
       ][v==goal, StepRew:=StepBet*1]
# Plot heuristic & informed choice densities
sr.exp[!is.na(SRRew), list(totS=sum(SRRew), totO=sum(OptRew), totR=sum(RiskRew), totSt=sum(StepRew)),by=pp
       ][,ggplot(.SD)+geom_density(aes(x=totS),col='purple')+geom_density(aes(x=totO),col='green')+
           geom_density(aes(x=totR),col='red')+geom_density(aes(x=totSt),col='blue')]

### Model True vs Fake optimal choices!
opt.exp <- full.exp[,list(pp,tr,stepsleft,v,goal)
                    ][,gOptC:=get.opt.choice(tr,v,goal,stepsleft,tMat,winM=1,loseM=-1),by=pp
                    ][,tOptC:=true.opt.choice(tr,v,goal,stepsleft,tMat,winM=1,loseM=-1),by=pp
                    ][gOptC==0,gOptC:=-1
                    ][tOptC==0,tOptC:=-1
                    ][,c('gOptBet','tOptBet'):=list(bet.sum(gOptC,15), bet.sum(tOptC,15)), by=.(pp,tr)
                    ][stepsleft==0&v!=goal, c('gOptRew','tOptRew'):=list(-gOptBet, -tOptBet)
                    ][v==goal, c('gOptRew','tOptRew'):=list(gOptBet*1, tOptBet*1)]
#Plot densities - fake is actually better?
opt.exp[!is.na(gOptRew), list(totG=sum(gOptRew), totT=sum(tOptRew)),by=pp
        ][,ggplot(.SD)+geom_density(aes(x=totG),col='red')+geom_density(aes(x=totT),col='green')]

opt.exp[,list(betc = sum(gOptC==1), totC = .N, propC=sum(gOptC==1)/.N), by=pp]

### Model heuristic: always bet up until - 9 stepsleft
opt.exp <- full.exp[,list(pp,tr,stepsleft,v,goal)
                    ][,gOptC:=get.opt.choice(tr,v,goal,stepsleft,tMat,winM=1,loseM=-1),by=pp
                    ][,tOptC:=true.opt.choice(tr,v,goal,stepsleft,tMat,winM=1,loseM=-1),by=pp
                    ][gOptC==0,gOptC:=-0
                    ][tOptC==0,tOptC:=-0
                    ][,c('gHeurC','tHeurC'):=list(gOptC,tOptC)
                    ][stepsleft>9,c('gHeurC','tHeurC'):=list(1,1)
                    ][,c('gOptBet','tOptBet','gHeurBet','tHeurBet'):=list(bet.sum(gOptC,0),bet.sum(tOptC,0),bet.sum(gHeurC,0),bet.sum(tHeurC,0)),by=.(pp,tr)
                    ][stepsleft==0&v!=goal, c('gOptRew','tOptRew','gHeurRew','tHeurRew'):=list(-gOptBet,-tOptBet,-gHeurBet,-tHeurBet)
                    ][v==goal, c('gOptRew','tOptRew','gHeurRew','tHeurRew'):=list(gOptBet*1, tOptBet*1, gHeurBet*1, tHeurBet*1)]
# Plot densities
opt.exp[!is.na(gOptRew), list(mGopt=sum(gOptRew), mTopt=sum(tOptRew), mGheur=sum(gHeurRew), mTheur=sum(tHeurRew)),by=pp
        ][,ggplot(.SD)+geom_density(aes(x=mGopt),col='red')+geom_density(aes(x=mTopt),col='green')+
            geom_density(aes(x=mGheur),col='blue')+geom_density(aes(x=mTheur),col='purple')]



