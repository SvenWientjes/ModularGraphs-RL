#####################################################################################################
## Script for playing around with SR-matrix learning and selecting actions based on clamped values ##
#####################################################################################################
# Load initial requirements
load('data/QuadMod-Bet_testDat-1.RData')
sapply(paste0('src/',list.files('src/')), source)
winM   <- 5
nVisit <- 15
nTr    <- 100
nPP    <- 500

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
heatmap(eigen(SR)$vectors[,1:7], Rowv=NA, Colv=NA)

# Make choice predictions?
ct <- 0.60
vt <- rep(-(ct^3), 20); vt[8] <- winM
SR %*% vt

# Plot SREV over time
SR.EVreg <- seq.SR.EV(full.exp[pp==1,tr], full.exp[pp==1,v], full.exp[pp==1,goal], full.exp[pp==1,stepsleft], lr, gamm, ct, winM)
plot(SR.EVreg,
     type='l')
abline(0, b=0, col='red')

# Get SREV based choices! 
choiceS <- sapply(boot::inv.logit(2*SR.EVreg), function(b){rbinom(1,1,b)})
plot.dat <- data.table(sym.id = full.exp[pp==1,sym.id], pol.type = full.exp[pp==1, if(trtype=='deep'){'deep'}else{'bottleneck'}, by=.(tr,stepsleft)]$V1,
                       stepsleft = full.exp[pp==1,stepsleft], choiceS=choiceS)

ggplot(plot.dat[,sum(choiceS==1)/.N, by=.(sym.id,pol.type,stepsleft)]) +
  geom_line(aes(x=stepsleft, y=V1)) +
  geom_point(aes(x=stepsleft, y=V1)) +
  facet_grid(pol.type~sym.id)
