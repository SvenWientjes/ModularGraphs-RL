#####################################################################################################
######## Generating fair tranjectories for the QuadSchap task incl. bottlenecks as start/goal #######
#####################################################################################################
library(data.table)
library(foreach)
library(doParallel)

# Define point multiplier for winning
nVisit <- 15
nTr    <- 100
nWin   <- 25
nLose  <- 75
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
a <- 0.25
tMat <- rbind(c(0, a, a, a, 0, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0 ,0 ,0 ,0 ,0, a),
              c(a, 0, a, a, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              c(a, a, 0, a, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              c(a, a, a, 0, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              c(0, a, a, a, 0, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              c(0, 0, 0, 0, a, 0, a, a, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              c(0, 0, 0, 0, 0, a, 0, a, a, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              c(0, 0, 0, 0, 0, a, a, 0, a, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              c(0, 0, 0, 0, 0, a, a, a, 0, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              c(0, 0, 0, 0, 0, 0, a, a, a, 0, a, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              c(0, 0, 0, 0, 0, 0, 0, 0, 0, a, 0, a, a, a, 0, 0, 0, 0, 0, 0),
              c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, 0, a, a, a, 0, 0, 0, 0, 0),
              c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, 0, a, a, 0, 0, 0, 0, 0),
              c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, 0, a, 0, 0, 0, 0, 0),
              c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, 0, a, 0, 0, 0, 0),
              c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, 0, a, a, a, 0),
              c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, 0, a, a, a),
              c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, 0, a, a),
              c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, 0, a),
              c(a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, 0))

# Get vertex / edge mappings w.r.t. different goals
idmap.d  <- list(a = c(1,15), b = c(2,3,4, 12,13,14), c = c(5,11), d = c(6,10), e = c(7,9), f = c(16,20), g = c(17,18,19))
idmap.bg <- list(a = c(1), b = c(2,3,4), c = c(5), d = c(7,8,9), e = c(10), f = c(11), g = c(12,13,14), h = c(15), i = c(16), j = c(17,18,19), k = c(20))
idmap.g  <- list(a = c(2,3,4), b = c(7,8,9), c = c(12,13,14), d = c(17,18,19))
c.map    <- list(a = c('d','b'), b = c('a', 'c'), c = c('b','d'), d=c('a','c'))
bt.map   <- list(a = c(1,5), b = c(6,10), c= c(11,15), d=c(20,16))
fc.map   <- list(a = 1:5, b=6:10, c=11:15, d=16:20)

## Analytic Expectations ----
StartType = c('Deep', 'Bottleneck')
GoalType = list('Deep'       = c('Deep', 'Bottleneck Close', 'Bottleneck Far'), 
                'Bottleneck' = c('Close Deep', 'Far Deep', 'Close Bottleneck Close', 'Close Bottleneck Far', 'Far Bottleneck Close' ,'Far Bottleneck Far'))
TrReach = 2:15

Analytics <- data.table(rbind(expand.grid(StartType[1], GoalType[[1]], TrReach), 
                              expand.grid(StartType[2], GoalType[[2]], TrReach)))
names(Analytics) <- c('StartType', 'GoalType', 'TrReach')
Analytics$goalp  <- 0
Analytics$startp <- 0
Analytics$reachp <- 0

# Get Analytic chance of each situation happening
starts = list('Deep'       = c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), #Deep       = node 2
              'Bottleneck' = c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) #Bottleneck = node 1
goals  = list('Deep' = c(7, 6, 10), 'Bottleneck' = c(19, 7, 20, 16, 6, 10))

for(i in 1:nrow(Analytics)){
  st <- Analytics[i,StartType]
  gl <- Analytics[i,GoalType]
  tMat.abs <- tMat
  tMat.abs[goals[[st]][which(GoalType[[st]] == Analytics[i,GoalType])],] <- 0
  distState <- starts[[st]] %*% matrix.power(tMat.abs,Analytics[i,TrReach]-1)
  Analytics[i,]$reachp <- distState[goals[[st]][which(GoalType[[st]] == Analytics[i,GoalType])]]
  if(st == 'Deep'){
    Analytics[i,]$startp = 3/5
    if(gl == 'Deep'){Analytics[i,]$goalp = 3/5}else{Analytics[i,]$goalp = 1/5}
  }else if(st=='Bottleneck'){
    Analytics[i,]$startp = 2/5
    if(gl == 'Close Deep' | gl == 'Far Deep'){Analytics[i,]$goalp = 3/10}else{Analytics[i,]$goalp = 1/10}  
  }
}
Analytics[,trajp:=goalp*startp*reachp, by=.(StartType,GoalType,TrReach)]
Analytics[,sum(trajp)]

# Winning analysis
trTypes <- Analytics[Analytics[,.(StartType, GoalType)][,!duplicated(.SD)],.(StartType, GoalType,startp, goalp)]
trTypes[,nTrs:=startp*goalp*nWin]
trTypes[,nLose:=startp*goalp*nLose]
trTypes <- trTypes[c(6,7,8,9,2,3,4,5,1),]

#### Plot of optimal Expected Values by winM ----
d.start <- c(2)
d.goals <- c(6,8,10)
b.start <- c(5)
b.goals <- c(6,7,10,20,19,16)
winM <- 3
maxSteps <- 14
EVanalytic <- data.table(start.v=0, goal.v=0, stepsleft=0, EV=0)
  
for(i in 1:3){
  stateVec <- rep(0,20)
  stateVec[d.start] <- 1
  tMat.goal <- tMat
  tMat.goal[d.goals[i],] <- 0
  hitC <- rep(0,maxSteps)
  for(s in maxSteps:1){
    hitC[s] <- (stateVec %*% matrix.power(tMat.goal,s))[d.goals[i]]
  }
  curEV <- cumsum(hitC)*winM + (1-cumsum(hitC))*-1
  EVanalytic <- rbind(EVanalytic, data.table(start.v=d.start, goal.v=d.goals[i], stepsleft=1:maxSteps, EV=curEV))
}


## Generating trajectories ----
registerDoParallel(cores=20)
# Initialize full experiment
full.exp <- foreach(pp = 1:40, .combine=rbind) %dopar% {
  # Initialize counters/trackers
  startCount <- rep(5,20); goalCount <- rep(5,20)
  telomeres <- matrix(c(0,0), nrow=1, ncol=2);colnames(telomeres) <- c('Var1','Var2')
  # Sample suitable goals!
  for(trtp in 1:nrow(trTypes)){
    for(tridx in 1:trTypes[trtp,nTrs+nLose]){
      sampEnd <- elig.ends(trTypes[trtp,StartType], trTypes[trtp,GoalType], startCount, goalCount, idmap.g, bt.map, c.map)
      startCount[sampEnd[,1]] <- startCount[sampEnd[,1]] - 1
      goalCount[sampEnd[,2]] <- goalCount[sampEnd[,2]] - 1
      telomeres <- rbind(telomeres, sampEnd)
    }
  }
  # Get suitable start-goal combinations into data.table
  telomeres <- as.data.table(telomeres[-1,])
  colnames(telomeres) <- c('start.v', 'goal.v')
  telomeres <- cbind(telomeres, trTypes[,list(StartType = rep(StartType,nTrs+nLose)),by=GoalType])[,c(4,3,1,2)]
  
  #Initialize list of trajectories
  win.exp  <- data.table(pp = 0, id = 0, v = 0, goal = 0, result='win', StartType='None', GoalType='None')
  widx <- 1
  # Sample winning trajectories
  for(trtp in 1:nrow(trTypes)){
    for(tridx in 1:trTypes[trtp,nTrs]){
      start.v <- telomeres[GoalType == trTypes[trtp,GoalType]][tridx,start.v]
      goal.v  <- telomeres[GoalType == trTypes[trtp,GoalType]][tridx,goal.v]
      #Sample Trajectory
      v  <- samp.win(start.v, goal.v, nVisit, Edges)
      #Append result
      win.exp <- rbind(win.exp, data.table(pp=pp, id=widx, v=v, goal=goal.v, result='win', StartType=trTypes[trtp,StartType], GoalType=trTypes[trtp,GoalType]))
      widx    <- widx+1
    }
  }
  unbalanced <- 1
  while(unbalanced){
    lose.exp <- data.table(pp = 0, id = 0, v = 0, goal = 0, result='lose', StartType='None', GoalType='None')
    lidx <- 1
    # Sample losing trajectories
    for(trtp in 1:nrow(trTypes)){
      for(tridx in trTypes[trtp,nTrs+1]:trTypes[trtp,nTrs+nLose]){
        start.v <- telomeres[GoalType == trTypes[trtp,GoalType]][tridx,start.v]
        goal.v  <- telomeres[GoalType == trTypes[trtp,GoalType]][tridx,goal.v]
        #Sample Trajectory
        v  <- samp.lose(start.v, goal.v, nVisit, Edges)
        lose.exp <- rbind(lose.exp, data.table(pp=pp, id=lidx, v=v, goal=goal.v, result='lose', StartType=trTypes[trtp,StartType], GoalType=trTypes[trtp,GoalType]))
        lidx    <- lidx+1
      }
    }
    
    unbalanced = !4 <= lose.exp[,list(prev = shift(v, 1L, type='lag'),v),by=id
    ][,list(tmatje = list(table(prev,v)))
    ][,min(tmatje[[1]][tmatje[[1]]!=0])]
  }
  
  
  # Initialize full participant experiment
  par.exp <- data.table(pp = 0, tr = 0, v = 0, goal = 0, result='None', StartType='None', GoalType='None')
  # Combine into quadruplets
  loseorder <- sample(1:75); winorder <- sample(1:25)
  winlocs <- sapply(1:25, function(sp){sample(1:4,1)})
  widx <- 1; lidx <- 1; pidx <- 1
  for(i in 1:25){
    for(j in 1:4){
      if(j == winlocs[i]){
        par.exp <- rbind(par.exp, win.exp[id==winorder[widx]][,list(pp,tr=pidx,v,goal,result,StartType,GoalType)])
        widx=widx+1;pidx=pidx+1
      }else{
        par.exp <- rbind(par.exp, lose.exp[id==loseorder[lidx]][,list(pp,tr=pidx,v,goal,result,StartType,GoalType)])
        lidx=lidx+1;pidx=pidx+1
      }
    }
  }
  par.exp[-1,]
}
full.exp$v <- as.factor(full.exp$v)

for(its in 1:10){
  prop.exp <- foreach(pp = 1:40, .combine=rbind) %dopar% {
    # Initialize counters/trackers
    startCount <- rep(5,20); goalCount <- rep(5,20)
    telomeres <- matrix(c(0,0), nrow=1, ncol=2);colnames(telomeres) <- c('Var1','Var2')
    # Sample suitable goals!
    for(trtp in 1:nrow(trTypes)){
      for(tridx in 1:trTypes[trtp,nTrs+nLose]){
        sampEnd <- elig.ends(trTypes[trtp,StartType], trTypes[trtp,GoalType], startCount, goalCount, idmap.g, bt.map, c.map)
        startCount[sampEnd[,1]] <- startCount[sampEnd[,1]] - 1
        goalCount[sampEnd[,2]] <- goalCount[sampEnd[,2]] - 1
        telomeres <- rbind(telomeres, sampEnd)
      }
    }
    # Get suitable start-goal combinations into data.table
    telomeres <- as.data.table(telomeres[-1,])
    colnames(telomeres) <- c('start.v', 'goal.v')
    telomeres <- cbind(telomeres, trTypes[,list(StartType = rep(StartType,nTrs+nLose)),by=GoalType])[,c(4,3,1,2)]
    
    #Initialize list of trajectories
    win.exp  <- data.table(pp = 0, id = 0, v = 0, goal = 0, result='win', StartType='None', GoalType='None')
    widx <- 1
    # Sample winning trajectories
    for(trtp in 1:nrow(trTypes)){
      for(tridx in 1:trTypes[trtp,nTrs]){
        start.v <- telomeres[GoalType == trTypes[trtp,GoalType]][tridx,start.v]
        goal.v  <- telomeres[GoalType == trTypes[trtp,GoalType]][tridx,goal.v]
        #Sample Trajectory
        v  <- samp.win(start.v, goal.v, nVisit, Edges)
        #Append result
        win.exp <- rbind(win.exp, data.table(pp=pp, id=widx, v=v, goal=goal.v, result='win', StartType=trTypes[trtp,StartType], GoalType=trTypes[trtp,GoalType]))
        widx    <- widx+1
      }
    }
    unbalanced <- 1
    while(unbalanced){
      lose.exp <- data.table(pp = 0, id = 0, v = 0, goal = 0, result='lose', StartType='None', GoalType='None')
      lidx <- 1
      # Sample losing trajectories
      for(trtp in 1:nrow(trTypes)){
        for(tridx in trTypes[trtp,nTrs+1]:trTypes[trtp,nTrs+nLose]){
          start.v <- telomeres[GoalType == trTypes[trtp,GoalType]][tridx,start.v]
          goal.v  <- telomeres[GoalType == trTypes[trtp,GoalType]][tridx,goal.v]
          #Sample Trajectory
          v  <- samp.lose(start.v, goal.v, nVisit, Edges)
          lose.exp <- rbind(lose.exp, data.table(pp=pp, id=lidx, v=v, goal=goal.v, result='lose', StartType=trTypes[trtp,StartType], GoalType=trTypes[trtp,GoalType]))
          lidx    <- lidx+1
        }
      }
      
      unbalanced = !4 <= lose.exp[,list(prev = shift(v, 1L, type='lag'),v),by=id
      ][,list(tmatje = list(table(prev,v)))
      ][,min(tmatje[[1]][tmatje[[1]]!=0])]
    }
    
    
    # Initialize full participant experiment
    par.exp <- data.table(pp = 0, tr = 0, v = 0, goal = 0, result='None', StartType='None', GoalType='None')
    # Combine into quadruplets
    loseorder <- sample(1:75); winorder <- sample(1:25)
    winlocs <- sapply(1:25, function(sp){sample(1:4,1)})
    widx <- 1; lidx <- 1; pidx <- 1
    for(i in 1:25){
      for(j in 1:4){
        if(j == winlocs[i]){
          par.exp <- rbind(par.exp, win.exp[id==winorder[widx]][,list(pp,tr=pidx,v,goal,result,StartType,GoalType)])
          widx=widx+1;pidx=pidx+1
        }else{
          par.exp <- rbind(par.exp, lose.exp[id==loseorder[lidx]][,list(pp,tr=pidx,v,goal,result,StartType,GoalType)])
          lidx=lidx+1;pidx=pidx+1
        }
      }
    }
    par.exp[-1,]
  }
  prop.exp$v <- as.factor(prop.exp$v)
  prop.tCost <- prop.exp[result=='lose', list(pp, tr, v, prev=shift(v,1L,type='lag')), by=.(pp,tr)
                         ][,list(tmatje=list(table(prev,v))),by=.(pp,tr)
                         ][,list(bigmat=list(Reduce('+',tmatje))), by=pp
                         ][,list(tCost = sum((bigmat[[1]][bigmat[[1]]!=0]-13.125)^2)),by=pp
                         ][order(tCost),]
  addidx  <- 1
  fullbad <- full.exp[result=='lose', list(pp, tr, v, prev=shift(v,1L,type='lag')), by=.(pp,tr)
                      ][,list(tmatje=list(table(prev,v))),by=.(pp,tr)
                      ][,list(bigmat=list(Reduce('+',tmatje))), by=pp
                      ][,list(tCost = sum((bigmat[[1]][bigmat[[1]]!=0]-13.125)^2)),by=pp
                      ][order(-tCost),]
  while(prop.tCost[addidx,tCost] < fullbad[addidx,tCost]){
    full.exp <- full.exp[pp!=fullbad[addidx,pp],] #remove worst participant
    full.exp <- rbind(full.exp, prop.exp[pp==prop.tCost[addidx,pp],][,list(pp=fullbad[addidx,pp], tr,v,goal,result,StartType,GoalType)])
    addidx=addidx+1
  }
}
full.exp <- full.exp[,stepsleft:=(14:(15-.N)),by=.(pp,tr)]
full.exp <- full.exp[order(pp),]
full.exp[,goal.c:=names(fc.map)[sapply(fc.map, function(cc){goal[1] %in% cc})],by=.(pp,tr)
         ][,sym.id:=symmetry.get(v,goal,goal.c,idmap.g,bt.map,c.map,idmap.d,idmap.bg),by=.(pp,tr,stepsleft)
         ][v==goal,sym.id:='win'
         ][,pol.type:=if(grepl('Deep', GoalType,fixed=T)){'deep'}else{'bottleneck'},by=.(pp,tr,stepsleft)]

## Evaluating trajectories ----
full.exp[result=='lose', list(pp, tr, v, prev=shift(v,1L,type='lag')), by=.(pp,tr)
         ][,list(tmatje=list(table(prev,v))),by=.(pp,tr)
         ][,list(bigmat=list(Reduce('+',tmatje))), by=pp
         ][,list(tCost = sum((bigmat[[1]][bigmat[[1]]!=0]-13.125)^2)),by=pp
         ][order(tCost),]

full.exp[result=='lose', list(pp, tr, v, prev=shift(v,1L,type='lag')), by=.(pp,tr)
         ][,list(tmatje=list(table(prev,v))),by=.(pp,tr)
         ][,list(bigmat=list(Reduce('+',tmatje))), by=pp
         ][pp==37,min(bigmat[[1]][bigmat[[1]]!=0])]


