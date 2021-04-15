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
s.types <- c('deep','bottleneck')
g.types <- c('deep','bottleneck close', 'bottleneck far')
# Which trial (node presentation) is the goal reached?
TrReach = 2:15

# Initialize data.table to get all values
Analytics <- data.table(rbind(expand.grid(s.types[1], g.types, NA, TrReach), 
                              expand.grid(s.types[2], g.types, c('close','far'), TrReach)))
names(Analytics) <- c('s.type', 'g.type', 'clust.loc', 't.reach')
Analytics$goalp  <- 0
Analytics$startp <- 0
Analytics$reachp <- 0

# Create representative starting and goal nodes for analysis
start.v <- list('deep'       = c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0),
                'bottleneck' = c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0))
#goal.v  <- list('deep' = c(7,6,10), 'bottleneck' = c(7,12,6,10,15,11))
goal.v <- list('deep' = c(7,6,10), 'bottleneck' = list('close'=c(7,6,10), 'far'=c(12,15,11)))

# Fill the Analytics matrix with the values
for(i in 1:nrow(Analytics)){
  # Get start/goal type
  st <- Analytics[i,s.type]
  gl <- Analytics[i,g.type]
  # Get actual nodes
  st.v <- start.v[[st]] #Vector form
  if(st=='deep'){gl.v <- goal.v[[st]][which(g.types==gl)]
  }else if(st=='bottleneck'){gl.v <- goal.v[[st]][[Analytics[i,clust.loc]]][which(g.types==gl)]}
  
  # Get absorbing transition matrix
  tMat.abs <- tMat
  tMat.abs[gl.v,] <- 0
  # Get chance of reaching the goal at EXACTLY node nr t.reach
  Analytics$reachp[i] <- (st.v %*% matrix.power(tMat.abs, Analytics[i, t.reach-1]))[gl.v]
  # Get chances of having sampled this start/goal type
  if(st=='deep'){
    Analytics$startp[i] <- 3/5
    if(gl=='deep'){
      Analytics$goalp[i] <- 3/5
    }else{
      Analytics$goalp[i] <- 1/5
    }
  }else if(st=='bottleneck'){
    Analytics$startp[i] <- 2/5
    if(grepl('deep',gl,fixed=T)){
      Analytics$goalp[i] <- 3/10
    }else{
      Analytics$goalp[i] <- 1/10
    }
  }
}
## Attach an overall trajectory chance of occurrence ----
Analytics[,trajp:=goalp*startp*reachp, by=.(s.type,g.type,t.reach)]
# Overall goal hit rate!
Analytics[,sum(trajp)] * nTrs

## Trial types of start-goal combinations and number of expected wins
trTypes <- Analytics[,list(startp=unique(startp),goalp=unique(goalp),clust.loc=unique(clust.loc), trajp=sum(trajp)), by=.(s.type,g.type,clust.loc)
                     ][,list(nTrs=startp*goalp*nTrs, nWin=nTrs*trajp), by=.(s.type,g.type,clust.loc)
                     ][,list(nTrs, nWin=floor(nWin),resWin=nWin-floor(nWin)), by=.(s.type,g.type,clust.loc)
                     ][s.type=='deep'&g.type=='deep'|s.type=='bottleneck'&g.type=='bottleneck far' & clust.loc=='far'|s.type=='deep'&g.type=='bottleneck far'
                       |s.type=='bottleneck'&g.type=='deep'&clust.loc=='far'|s.type=='deep'&g.type=='bottleneck close', nWin:=nWin+1
                     ][,list(s.type,g.type,clust.loc,nTrs,nWin,nLose=nTrs-nWin)
                     ][c(5,6,8,9,4,7,2,3,1)]
################################################################################
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
################################################################################
# Generate experiments according to likely characteristics #
#### Set up all eligeble start-goal combinations ----
ElGoals <- data.table(rbind(expand.grid(1:5, 6:10),
                            expand.grid(1:5, 11:15),
                            expand.grid(6:10, 1:5),
                            expand.grid(6:10, 11:15),
                            expand.grid(11:15, 1:5),
                            expand.grid(11:15, 6:10))
)
# Bind associated cluster identity
ElGoals <- cbind(ElGoals, c(rep('a',50), rep('b',50), rep('c',50)),
                 c(rep('b',25), rep('c',25), rep('a',25), rep('c',25), rep('a',25), rep('b',25)))
# Give names to columns
names(ElGoals) <- c('start.v','goal.v', 'start.c', 'goal.c')
# Set deep or bottleneck identity per node
ElGoals[,s.type:=if(start.v %in% unlist(idmap.d)){'deep'}else{'bottleneck'},by=.(start.v,goal.v)]
ElGoals[,g.type:=if(goal.v %in% unlist(idmap.d)){'deep'}else{'bottleneck'},by=.(start.v,goal.v)]
# Identify miniblock type based on start-goal relationship
ElGoals[g.type=='bottleneck', g.type:=if(c.map[[goal.c]][which(idmap.bt[[goal.c]]==goal.v)]==start.c){'bottleneck close'}else{'bottleneck far'}, by=.(start.v,goal.v)]
ElGoals[s.type=='bottleneck', clust.loc:=if(c.map[[start.c]][which(idmap.bt[[start.c]]==start.v)]==goal.c){'close'}else{'far'}, by=.(start.v, goal.v)]

#### Get the starts and ends of a concrete experiment ----
telomeres <- data.table(start.v=0,goal.v=0,start.c='z',goal.c='z',s.type='init',g.type='init',clust.loc='init')

# Place constraints upon cluster-transitions for goals
ctransCount <- rbind(expand.grid('a', c('b','c')), expand.grid('b', c('a','c')), expand.grid('c', c('a','b')))
ctransCount <- as.data.table(ctransCount)
names(ctransCount) <- c('start.c','goal.c')

#### SAMPLE THE ACTUAL TELOMERES ###
for(trtp in 1:nrow(trTypes)){
  evlB <- F
  if(trtp==1){
    # Sample 1 of each cluster as a start
    LRsamp <- sample(1:2)
    startsamp.1 <- sapply(idmap.bt, function(bc){bc[LRsamp[1]]})
    # Add one more (cluster repetition)
    startbonus.1 <- sample(unlist(idmap.bt)[which(!unlist(idmap.bt) %in% startsamp.1)], 1)
    goalbonus.1 <- ElGoals[s.type=='bottleneck' & g.type=='bottleneck close' & clust.loc=='close' & start.v == startbonus.1,goal.v]
    # There is only one available goal per start - will this balance out automatically or make it impossible??
    telomeres <- rbind(telomeres, ElGoals[s.type=='bottleneck' & g.type=='bottleneck close' & clust.loc=='close' & start.v %in% c(startsamp.1, startbonus.1)])
  }
  if(trtp==2){
    LRsamp2 <- sample(1:2)
    startsamp.2 <- sapply(idmap.bt, function(bc){bc[LRsamp2[1]]})
    # Eligible starts
    el.startbonus.2 <- unlist(idmap.bt)[which(!unlist(idmap.bt) %in% c(startsamp.2,startbonus.1))]
    # Check eligibility of goal
    startbonus.2 <- ElGoals[s.type=='bottleneck'&g.type=='bottleneck far'&clust.loc=='close'& start.v %in% el.startbonus.2 & !goal.v %in% goalbonus.1,][sample(.N,1),start.v]
    goalbonus.2 <- ElGoals[s.type=='bottleneck'&g.type=='bottleneck far'&clust.loc=='close'& start.v %in% startbonus.2,goal.v]
    # Add one more (cluster repetition)
    #startbonus.2 <- sample(unlist(idmap.bt)[which(!unlist(idmap.bt) %in% c(startsamp.2,startbonus.1))], 1)
    # There is only one available goal per start - will this balance out automatically or make it impossible??
    telomeres <- rbind(telomeres, ElGoals[s.type=='bottleneck' & g.type=='bottleneck far' & clust.loc=='close' & start.v %in% c(startsamp.2, startbonus.2)])
  }
  if(trtp==3){
    startsamp.3 <- sapply(idmap.bt, function(bc){bc[LRsamp2[2]]})
    # Eligible starts
    el.startbonus.3 <- unlist(idmap.bt)[which(!unlist(idmap.bt) %in% c(startsamp.3,startbonus.1,startbonus.2))]
    # Check eligibility of goal
    startbonus.3 <- ElGoals[s.type=='bottleneck'&g.type=='bottleneck close'&clust.loc=='far'& start.v %in% el.startbonus.3 & !goal.v %in% c(goalbonus.1,goalbonus.2),][sample(.N,1),start.v]
    goalbonus.3 <- ElGoals[s.type=='bottleneck'&g.type=='bottleneck close'&clust.loc=='far'& start.v == startbonus.3,goal.v]
    # There is only one available goal per start - will this balance out automatically or make it impossible??
    telomeres <- rbind(telomeres, ElGoals[s.type=='bottleneck' & g.type=='bottleneck close' & clust.loc=='far' & start.v %in% c(startsamp.3, startbonus.3)])
  }
  if(trtp==4){
    startsamp.4 <- sapply(idmap.bt, function(bc){bc[LRsamp[2]]})
    # Eligible starts
    el.startbonus.4 <- unlist(idmap.bt)[which(!unlist(idmap.bt) %in% c(startsamp.4,startbonus.1,startbonus.2,startbonus.3))]
    # Check for eligibility of goal
    startbonus.4 <- ElGoals[s.type=='bottleneck'&g.type=='bottleneck far'&clust.loc=='far'& start.v %in% el.startbonus.4 & !goal.v %in% c(goalbonus.1,goalbonus.2,goalbonus.3),][sample(.N,1),start.v]
    goalbonus.4 <- ElGoals[s.type=='bottleneck'&g.type=='bottleneck far'&clust.loc=='far'& start.v == startbonus.4,goal.v]
    # Wow
    telomeres <- rbind(telomeres, ElGoals[s.type=='bottleneck' & g.type=='bottleneck far' & clust.loc=='far' & start.v %in% c(startsamp.4, startbonus.4)])
  }
  if(trtp==5){
    startCount <- rep(2,15)
    
    goalCount <- rep(1,15)
    goalbonus.1 <- sapply(idmap.d, function(dc){sample(dc,1)})
    goalCount[goalbonus.1] <- goalCount[goalbonus.1]+1
    ctransCount[,count:=2]
    evlB <- T
  }
  if(trtp==6){
    startCount <- rep(2,15)
    goalCount <- rep(1,15)
    goalbonus.2 <- sapply(idmap.d, function(dc){sample(dc[which(!dc %in% goalbonus.1)],1)})
    goalCount[goalbonus.2] <- goalCount[goalbonus.2]+1
    ctransCount[,count:=2]
    evlB <- T
  }
  if(trtp==7){
    goalCount <- rep(2,15)
    startCount <- rep(1,15)
    startbonus.1 <- sapply(idmap.d, function(dc){sample(dc,1)})
    startCount[startbonus.1] <- startCount[startbonus.1]+1
    ctransCount[,count:=2]
    evlB <- T
  }
  if(trtp==8){
    goalCount <- rep(2,15)
    startCount <- rep(1,15)
    startbonus.2 <- sapply(idmap.d, function(dc){sample(dc[which(!dc %in% startbonus.1)],1)})
    startCount[startbonus.2] <- startCount[startbonus.2]+1
    ctransCount[,count:=2]
    evlB <- T
  }
  if(trtp==9){
    startCount <- rep(4,15)
    goalCount <- rep(4,15)
    ctransCount[,count:=6]
    evlB <- T
  }
  if(evlB){
    for(tridx in 1:trTypes[trtp,nTrs]){
      st <- trTypes[trtp,s.type] # Select starting type
      gl <- trTypes[trtp,g.type] # Select goal type
      cloc <- trTypes[trtp,clust.loc] # Select cluster location (far/close; bottleneck start only!)
      
      clust.idx <- ElGoals[start.v %in% which(startCount!=0) & goal.v %in% which(goalCount!=0) & s.type==st & g.type==gl][,identical(clust.loc,cloc),by=.(start.v,goal.v)]$V1
      ElCombs <- ElGoals[start.v %in% which(startCount!=0) & goal.v %in% which(goalCount!=0) & s.type==st & g.type==gl][clust.idx,]
      # Do not include cluster-transitions that were already sampled often enough
      ElCombs[,count:=1][ctransCount[count==0], count:=0, on=.(start.c,goal.c)]
      # Sample trial
      DrawnEnds <- ElCombs[count!=0][sample(1:.N,1),]
      
      # Decrease cluster transition counter
      ctransCount[start.c==DrawnEnds[,start.c] & goal.c==DrawnEnds[,goal.c],count:=count-1]
      # Decrease node sampling counters
      startCount[DrawnEnds[,start.v]] <- startCount[DrawnEnds[,start.v]]-1
      goalCount[DrawnEnds[,goal.v]] <- goalCount[DrawnEnds[,goal.v]]-1
      telomeres <- rbind(telomeres, DrawnEnds[,1:7])
    }
  }
}
telomeres <- telomeres[-1,]
# Shuffle for reward/non-reward stuff
telomeres <- telomeres[telomeres[,.I[sample(.N)], by=.(s.type,g.type,clust.loc)]$V1,]
# Create string for deep clust.loc (helps sampling experiments)
telomeres[is.na(clust.loc), clust.loc:='deepNA']

#### Get the winning trajectories ----
#Initialize list of trajectories
win.exp  <- data.table(pp = 0, id = 0, v = 0, goal = 0, result='win', s.type='None', g.type='None', clust.loc='None')
widx <- 1
# Sample winning trajectories
for(trtp in 1:nrow(trTypes)){
  for(tridx in 1:trTypes[trtp,nWin]){
    c.loc <- trTypes[trtp,clust.loc]
    if(is.na(c.loc)){c.loc <- 'deepNA'}
    start.v <- telomeres[s.type == trTypes[trtp,s.type] & g.type == trTypes[trtp,g.type] & clust.loc == c.loc][tridx,start.v]
    goal.v  <- telomeres[s.type == trTypes[trtp,s.type] & g.type == trTypes[trtp,g.type] & clust.loc == c.loc][tridx,goal.v]
    #Sample Trajectory
    v  <- samp.win(start.v, goal.v, nVisit, Edges)
    #Append result
    win.exp <- rbind(win.exp, data.table(pp=1, id=widx, v=v, goal=goal.v, result='win', s.type=trTypes[trtp,s.type], g.type=trTypes[trtp,g.type], clust.loc=trTypes[trtp,clust.loc]))
    widx    <- widx+1
  }
}

#### Get the losing trajectories ----
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



