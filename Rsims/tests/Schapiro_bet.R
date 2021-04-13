################################################################################
############################ Schapiro - Bet tests ##############################
################################################################################
library(data.table)
library(foreach)
library(doParallel)
################################################################################
# Task Parameters
nTrs <- 100
winM <- 1
nVisit <- 15
# Transitions in list and in matrix form
Edges <- list(c(02,03,04,15),
              c(01,03,04,05),
              c(01,02,04,05),
              c(01,02,03,05),
              c(02,03,04,06),
              c(05,07,08,09),
              c(06,08,09,10),
              c(06,07,09,10),
              c(06,07,08,10),
              c(07,08,09,11),
              c(10,12,13,15),
              c(11,13,14,15),
              c(11,12,14,15),
              c(11,12,13,15),
              c(12,13,14,01))
a <- 0.25
tMat <- rbind(c(0,a,a,a,0,0,0,0,0,0,0,0,0,0,a),
              c(a,0,a,a,a,0,0,0,0,0,0,0,0,0,0),
              c(a,a,0,a,a,0,0,0,0,0,0,0,0,0,0),
              c(a,a,a,0,a,0,0,0,0,0,0,0,0,0,0),
              c(0,a,a,a,0,a,0,0,0,0,0,0,0,0,0),
              c(0,0,0,0,a,0,a,a,a,0,0,0,0,0,0),
              c(0,0,0,0,0,a,0,a,a,a,0,0,0,0,0),
              c(0,0,0,0,0,a,a,0,a,a,0,0,0,0,0),
              c(0,0,0,0,0,a,a,a,0,a,0,0,0,0,0),
              c(0,0,0,0,0,0,a,a,a,0,a,0,0,0,0),
              c(0,0,0,0,0,0,0,0,0,a,0,a,a,a,0),
              c(0,0,0,0,0,0,0,0,0,0,a,0,a,a,a),
              c(0,0,0,0,0,0,0,0,0,0,a,a,0,a,a),
              c(0,0,0,0,0,0,0,0,0,0,a,a,a,0,a),
              c(a,0,0,0,0,0,0,0,0,0,0,a,a,a,0))
################################################################################
# Symmetry assistance
idmap.d  <- list(a=c(2,3,4),b=c(7,8,9),c=c(12,13,14))    # All deep nodes
idmap.bt <- list(a=c(1,5), b=c(6,10), c=c(11,15))        # All bottleneck nodes
c.map <- list(a=c('c','b'), b=c('a','c'), c=c('b','a'))  # Cluster maps of bottlenecks
idmap.dg  <- list(a=c(1,15), b=c(2,3,4, 12,13,14), c=c(5,11), d=c(6,10), e=c(8,9)) # Symmetries to Deep Goal (b) ; goal = 7
idmap.bg  <- list(a=c(1), b=c(2,3,4), c=c(5), d=c(7,8,9), e=c(10), f=c(11),g=c(12,13,14),h=c(15)) # Symmetries to Bottleneck Goal (b) ; goal = 6
################################################################################
#### Analysis of goal reaches and expected values ----
## Goal Hits per start & goal type & stepsleft ----
# Organize different start and goal types
s.types <- c('Deep','Bottleneck')
g.types <- list('Deep'       = c('Deep','Bottleneck Close', 'Bottleneck Far'),
                'Bottleneck' = c('Close Deep', 'Far Deep', 'Close Bottleneck Close', 'Close Bottleneck Far', 'Far Bottleneck Close', 'Far Bottleneck Far'))
# Which trial (node presentation) is the goal reached?
TrReach = 2:15

# Initialize data.table to get all values
Analytics <- data.table(rbind(expand.grid(s.types[1], g.types[[1]], TrReach), 
                              expand.grid(s.types[2], g.types[[2]], TrReach)))
names(Analytics) <- c('s.type', 'g.type', 't.reach')
Analytics$goalp  <- 0
Analytics$startp <- 0
Analytics$reachp <- 0

# Create representative starting and goal nodes for analysis
start.v <- list('Deep'       = c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0),
                'Bottleneck' = c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0))
goal.v  <- list('Deep' = c(7,6,10), 'Bottleneck' = c(7,12,6,10,15,11))

# Fill the Analytics matrix with the values
for(i in 1:nrow(Analytics)){
  # Get start/goal type
  st <- Analytics[i,s.type]
  gl <- Analytics[i,g.type]
  # Get actual nodes
  st.v <- start.v[[st]] #Vector form
  gl.v <- goal.v[[st]][which(g.types[[st]]==gl)]
  # Get absorbing transition matrix
  tMat.abs <- tMat
  tMat.abs[gl.v,] <- 0
  # Get chance of reaching the goal at EXACTLY node nr t.reach
  Analytics$reachp[i] <- (st.v %*% matrix.power(tMat.abs, Analytics[i, t.reach-1]))[gl.v]
  # Get chances of having sampled this start/goal type
  if(st=='Deep'){
    Analytics$startp[i] <- 3/5
    if(gl=='Deep'){
      Analytics$goalp[i] <- 3/5
    }else{
      Analytics$goalp[i] <- 1/5
    }
  }else if(st=='Bottleneck'){
    Analytics$startp[i] <- 2/5
    if(grepl('Deep',gl,fixed=T)){
      Analytics$goalp[i] <- 3/10
    }else{
      Analytics$goalp[i] <- 1/10
    }
  }
}
# Attach an overall trajectory chance of occurrence
Analytics[,trajp:=goalp*startp*reachp, by=.(s.type,g.type,t.reach)]
# Overall goal hit rate!
Analytics[,sum(trajp)] * nTrs

## Trial types of start-goal combinations and number of expected wins
trTypes <- Analytics[,list(startp=unique(startp),goalp=unique(goalp), trajp=sum(trajp)), by=.(s.type,g.type)
                     ][,list(nTrs=startp*goalp*nTrs, nWin=nTrs*trajp), by=.(s.type,g.type)
                     ][,list(nTrs, nWin=floor(nWin),resWin=nWin-floor(nWin)), by=.(s.type,g.type)
                     ][s.type=='Deep'&g.type=='Deep'|s.type=='Bottleneck'&g.type=='Far Bottleneck Far'|s.type=='Deep'&g.type=='Bottleneck Far'
                       |s.type=='Bottleneck'&g.type=='Far Deep'|s.type=='Deep'&g.type=='Bottleneck Close', nWin:=nWin+1
                     ][,list(s.type,g.type,nTrs,nWin,nLose=nTrs-nWin)
                     ][c(6,7,8,9,2,3,4,5,1)]

## Expected value by win multiplier ----
# Get ids for different goals
goal.vs <- c(7,6)
EV.anal <- data.table(start.v=0, goal.v=0, stepsleft=0, EV=0, pol.type='none', sym.id='z')
for(i in 1:2){
  symlist <- list(idmap.dg, idmap.bg)[[i]] # Get appropriate symmetry iDs
  goal.v <- goal.vs[i] # Set relevant goal
  pol.type <- c('deep', 'bottleneck')[i]
  # Absorbing transition matrix
  tMat.abs <- tMat
  tMat.abs[goal.v,] <- 0
  for(j in 1:length(symlist)){
    cur.v <- symlist[[j]][1] # Extract an exemplar node from symmetry
    sym.id <- names(symlist)[j]
    stateVec <- rep(0,15)    # Into vector representation
    stateVec[cur.v] <- 1
    hitC <- rep(0, 14)
    for(s in 14:1){
      hitC[s] <- (stateVec %*% matrix.power(tMat.abs, s))[goal.v]
    }
    relEV <- cumsum(hitC) * winM + (1-cumsum(hitC)) * -1
    EV.anal <- rbind(EV.anal, data.table(start.v=cur.v, goal.v=goal.v, stepsleft=1:14, EV=relEV, pol.type=pol.type, sym.id=sym.id))
  }
}
EV.anal <- EV.anal[-1,]
EV.anal$goal.v <- as.factor(EV.anal$goal.v)
EV.anal$start.v <- as.factor(EV.anal$start.v)

ggplot(EV.anal, aes(x=stepsleft, y=EV, col=start.v)) +
  geom_line() +
  geom_hline(yintercept=0, col='red') +
  facet_grid(.~goal.v)

#### Generate experiments according to likely characteristics ----
# Tracker for starts / goals (little bit of overshoot possible! - 10 more)

# Sample suitable goals!
startCount <- rep(7, 15); goalCount <- rep(7, 15)
telomeres <- matrix(c(0,0), nrow=1, ncol=2);colnames(telomeres) <- c('Var1','Var2')
for(trtp in 1:nrow(trTypes)){
  for(tridx in 1:trTypes[trtp,nTrs]){
    # Sample the ends
    sampEnd <- elig.ends(trTypes[trtp,s.type], trTypes[trtp,g.type], startCount, goalCount, telomeres, idmap.d, idmap.bt, c.map)
    # Decrease the counters
    startCount[sampEnd[,1]] <- startCount[sampEnd[,1]] - 1
    goalCount[sampEnd[,2]] <- goalCount[sampEnd[,2]] - 1
    # Bind the ends
    telomeres <- rbind(telomeres, sampEnd)
    # Check if the 6s constraint is satisfied and impute additional start/goal to solve final inconsistencies
    if(trTypes[trtp,s.type]=='Deep' & trTypes[trtp,g.type]=='Deep'){
      startMat <- matrix(startCount[unlist(idmap.d)],3,3)
      goalMat <- matrix(goalCount[unlist(idmap.d)],3,3)
      avstart.c <- !sapply(1:nrow(startMat), function(mc){all(startMat[,mc]==0)})
      avgoal.c <- !sapply(1:nrow(goalMat), function(mc){all(goalMat[,mc]==0)})
      # If only one cluster left as start and goal AND it is identical across start and goal:
      if(sum(avstart.c)==1 & sum(avgoal.c)==1 & identical(avstart.c, avgoal.c)){
        if(sample(1:2,1) == 1){
          
          startCount
        }
      }
    }
  }
}



telomeres <- as.data.table(telomeres[-1,])
colnames(telomeres) <- c('start.v', 'goal.v')
telomeres <- cbind(telomeres, trTypes[,list(s.type = rep(s.type,nTrs)),by=g.type])[,c(4,3,1,2)]
#Initialize list of trajectories
win.exp  <- data.table(pp = 0, id = 0, v = 0, goal = 0, result='win', s.type='None', g.type='None')
widx <- 1
# Sample winning trajectories
for(trtp in 1:nrow(trTypes)){
  for(tridx in 1:trTypes[trtp,nWin]){
    start.v <- telomeres[g.type == trTypes[trtp,g.type]][tridx,start.v]
    goal.v  <- telomeres[g.type == trTypes[trtp,g.type]][tridx,goal.v]
    #Sample Trajectory
    v  <- samp.win(start.v, goal.v, nVisit, Edges)
    #Append result
    win.exp <- rbind(win.exp, data.table(pp=1, id=widx, v=v, goal=goal.v, result='win', s.type=trTypes[trtp,s.type], g.type=trTypes[trtp,g.type]))
    widx    <- widx+1
  }
}
tCount <- cbind(foreach(i = 1:15, .combine=rbind) %do% {expand.grid(i, Edges[[i]])}, 16)
tCount <- as.data.table(tCount)
names(tCount) <- c('from', 'to', 'leftover')

samp.lose(1, 6, tCount, nVisit, Edges)
lose.exp <- data.table(pp = 0, id = 0, v = 0, goal = 0, result='lose', s.type='None', g.type='None')
lidx <- 1
# Sample losing trajectories
for(trtp in 1:nrow(trTypes)){
  for(tridx in trTypes[trtp,nWin+1]:trTypes[trtp,nTrs]){
    start.v <- telomeres[g.type == trTypes[trtp,g.type]][tridx,start.v]
    goal.v  <- telomeres[g.type == trTypes[trtp,g.type]][tridx,goal.v]
    #Sample Trajectory
    reslist  <- samp.lose(start.v, goal.v, tCount, nVisit, Edges)
    v <- reslist$v; tCount <- reslist$tCounter
    lose.exp <- rbind(lose.exp, data.table(pp=1, id=lidx, v=v, goal=goal.v, result='lose', s.type=trTypes[trtp,s.type], g.type=trTypes[trtp,g.type]))
    lidx    <- lidx+1
  }
}



