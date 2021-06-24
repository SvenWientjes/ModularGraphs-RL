################################################################################
############## Simulating aspects of the Schapiro-exhaustion task ##############
################################################################################
library(data.table)
library(foreach)
library(doParallel)
library(matrixcalc)
################################################################################
# Task Parameters
nTrs <- 75
winM <- 1
nVisit <- 50
nRW <- 15
nHam <- 15
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
              c(10,12,13,14),
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
idmap.dg2 <- list(a=c(10,11),b=c(7,8,9,12,13,14), c=c(6,15))
idmap.bg5 <- list(a=c(10), b=c(7,8,9), c=c(6), d=c(2,3,4), e=c(1), f=c(15), g=c(12,13,14),h=c(11)) # Symmetries to Bottleneck Goal (b) ; goal = 5
gsym5 <- ((5+c(0,5,10))-1)%%15 + 1 #x-1 modulo +1 maps 15 to 1 instead of 0
idmap.bg6  <- list(a=c(1), b=c(2,3,4), c=c(5), d=c(7,8,9), e=c(10), f=c(11),g=c(12,13,14),h=c(15)) # Symmetries to Bottleneck Goal (b) ; goal = 6
gsym6 <- ((6+c(0,5,10))-1)%%15 + 1
################################################################################
#### How likely is it to reach a particular goal type from a particular state type? ####
# Organize different start and goal types
s.types <- c('deep','bottleneck')
g.types <- c('deep','bottleneck close', 'bottleneck far')

# Initialize data.table to get all values
Analytics <- data.table(rbind(expand.grid(s.types[1], g.types, NA, 1:nVisit), 
                              expand.grid(s.types[2], g.types, c('close','far'), 1:nVisit)))
# Name variables
names(Analytics) <- c('s.type', 'g.type', 'clust.loc', 't.reach')
Analytics <- Analytics[order(t.reach)]
Analytics$goalp  <- 0
Analytics$startp <- 0
Analytics$reachp <- 0

# Get start IDs
#Analytics[s.type=='deep',s.id:=2]
#Analytics[s.type=='bottleneck',s.id:=1]
# Get goal IDs
#Analytics[,g.id:=c(7,6,10,14,15,11,7,6,10)]

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

# Get number of selections of start/goal combinations
goalytics <- unique(Analytics[,!"reachp"][,!"t.reach"])
goalytics[,selectp:=goalp*startp,by=.(s.type,g.type,clust.loc)]
goalytics[,numselect:=selectp*nTrs, by=.(s.type,g.type,clust.loc)]
goalytics[s.type=='deep'&g.type=='bottleneck far',sym.id:='a']
goalytics[s.type=='deep'&g.type=='deep',sym.id:='b']
goalytics[s.type=='deep'&g.type=='bottleneck close',sym.id:='c']
goalytics[s.type=='bottleneck'&g.type=='bottleneck far'&clust.loc=='close',sym.id:='a']
goalytics[s.type=='bottleneck'&g.type=='deep'&clust.loc=='close',sym.id:='b']
goalytics[s.type=='bottleneck'&g.type=='bottleneck close'&clust.loc=='close',sym.id:='c']
goalytics[s.type=='bottleneck'&g.type=='bottleneck close'&clust.loc=='far',sym.id:='f']
goalytics[s.type=='bottleneck'&g.type=='deep'&clust.loc=='far',sym.id:='g']
goalytics[s.type=='bottleneck'&g.type=='bottleneck far'&clust.loc=='far',sym.id:='h']


# Merge goalytics into Analytics
setkey(goalytics, s.type,g.type,clust.loc)
setkey(Analytics, s.type,g.type,clust.loc)
Analytics <- Analytics[goalytics[,.(s.type,g.type,clust.loc,sym.id,numselect)]]

# Expect select
Analytics[,Eselect:=reachp*numselect,by=.(s.type,g.type,clust.loc,t.reach)]

# Get hitvec for a particular Analytics row
# for(r in 1:1){----
#   # Initialize hitvec of all checked steps
#   hitvec <- rep(0, nVisit)
#   # Absorbing goal matrix
#   tMat.goal <- tMat
#   tMat.goal[Analytics[r,g.id],] <- 0
#   # One hot state ID
#   I <- rep(0,15); I[Analytics[r,s.id]] <- 1
#   
#   # Fill hitvec
#   for(s in 1:nVisit){
#     hitvec[s] <- (I %*% matrix.power(tMat.goal, s))[Analytics[r,g.id]]
#   }
# }
################################################################################
#### Dwell times under RW and RW+HAM ####
## Dwell time under RW
# Initialize tracker of dwell time
dwellvec <- rbind(rep(0, nVisit), rep(0, nVisit))
for(s.id in 1:2){
  # Transition matrix zeroed out for non-cluster
  tMat.dwell <- tMat
  tMat.dwell[6:15,] <- 0
  # One hot state ID
  I <- rep(0,15); I[s.id]<-1
  
  # Fill dwellvec
  for(s in 1:nVisit){
    dwellvec[s.id,s] <- sum((I %*% matrix.power(tMat.dwell,s))[1:5])
  }
}
plot(1:30, dwellvec[2,], type='l')
lines(1:30, dwellvec[1,], type='l', col='red')

## Dwell time under RW+HAM
# One hot state ID
dwellvec <- rbind(rep(0, nRW+nHam-2), rep(0, nRW+nHam-2))
for(s.id in 1:2){
  # Transition matrix zeroed out for non-cluster
  tMat.dwell <- tMat
  tMat.dwell[6:15,] <- 0
  # One hot state ID
  I <- rep(0,15); I[s.id]<-1
  
  # Fill dwellvec for RW
  for(s in 1:(nRW-1)){
    dwellvec[s.id,s] <- sum((I %*% matrix.power(tMat.dwell,s))[1:5])
  }
  # Get state occupation vector after 15
  dwellP <- sum((I %*% matrix.power(tMat.dwell,(nRW-1)))[1:5])
  deepP <- sum((I %*% matrix.power(tMat.dwell,(nRW-1)))[2:4])
  botP <- sum((I %*% matrix.power(tMat.dwell,(nRW-1)))[c(1,5)])
  # Hand code hamiltonian policy
  dwellvec[s.id,nRW] <- dwellvec[s.id,nRW-1] - (botP*1/4)  #Chance to immediately leave
  dwellvec[s.id,nRW+1] <- dwellvec[s.id,nRW] - (deepP*1/3) #Chance to leave after transitioning to bottleneck in one step
  dwellvec[s.id,nRW+2] <- dwellvec[s.id,nRW+1] -(deepP*1/3)#Chance to leave after transitioning to bottleneck in two steps
  dwellvec[s.id,nRW+3] <- dwellvec[s.id,nRW+2] -(deepP*1/3)#Chance to leave after transitioning to bottleneck in three steps
  dwellvec[s.id,nRW+4] <- dwellvec[s.id,nRW+3] -(botP*3/4) #Chance to leave after bottleneck - full internal cycle
}
plot(1:(nRW+nHam-2), dwellvec[2,], type='l')
lines(1:(nRW+nHam-2), dwellvec[1,], type='l', col='red')
I<-rep(0,15); I[1]<-1
sum((I %*% matrix.power(tMat.dwell,14))[c(1,5)])
################################################################################
#### Monte Carlo experiment ####
rew  <- 5   #Reward per item delivered
car  <- 1 #Cost per item per carry
pick <- 0.25 #Cost for lollygagging
cap = 5
# Initialize Expected Value matrix
Vs <- array(0, dim=c(15,(cap+1),15))

#Full Asynchronous Value Iteration - carry payment upon transition for all possible goals (use this one)
for(goal in 1:15){
  # Clamp value of goal immediately, no need for computation
  Vs[goal,,goal] <- c(0:cap)*rew
  nValit <- 1e5
  for(i in 1:nValit){
    state.node <- sample(c(1:15)[-goal],1) #Randomly pick a state
    state.carry <- sample(1:(cap+1),1)     #Randomly pick a carrying hold
    
    # Init reward vectors
    r <- rep(0,3)
    
    # Penalty for lollygag
    if(state.carry==1){
      r[1] = r[1]-pick
    }else if(state.carry==(cap+1)){
      r[3] = r[3]-pick
    }
    # subtract carry based on selected action
    r[1] = r[1] - car*(max(state.carry-1,1)-1)
    r[2] = r[2] - car*(state.carry-1)
    r[3] = r[3] - car*(min(state.carry+1,(cap+1))-1)
    
    # Avg reward of potential successors
    r[1] <- r[1] + sum(sapply(which(tMat[state.node,]!=0), function(fs){1/4*Vs[fs,max(state.carry-1,1),goal]}))
    r[2] <- r[2] + sum(sapply(which(tMat[state.node,]!=0), function(fs){1/4*Vs[fs,state.carry,goal]}))
    r[3] <- r[3] + sum(sapply(which(tMat[state.node,]!=0), function(fs){1/4*Vs[fs,min(state.carry+1,cap+1),goal]}))
    
    Vs[state.node, state.carry,goal] <- max(r)
  }
}

# Get policy from value iterated state values
pol <- array(0, dim=c(15,(cap+1),15))

for(goal in 1:15){
  for(st in 1:15){
    for(carry in 1:(cap+1)){
      # Init reward vectors
      r <- rep(0,3) 
      # Penalty for lollygag
      if(carry==1){
        r[1] = r[1]-pick
      }else if(carry==(cap+1)){
        r[3] = r[3]-pick
      }
      # subtract carry based on selected action
      r[1] = r[1] - car*(max(carry-1,1)-1)
      r[2] = r[2] - car*(carry-1)
      r[3] = r[3] - car*(min(carry+1,(cap+1))-1)
      # Avg reward of potential successors
      r[1] <- r[1] + sum(sapply(which(tMat[st,]!=0), function(fs){1/4*Vs[fs,max(carry-1,1),goal]}))
      r[2] <- r[2] + sum(sapply(which(tMat[st,]!=0), function(fs){1/4*Vs[fs,carry,goal]}))
      r[3] <- r[3] + sum(sapply(which(tMat[st,]!=0), function(fs){1/4*Vs[fs,min(carry+1,cap+1),goal]}))
      pol[st, carry, goal] <- max(which(r == max(r))) #Deep nodes of cluster of bottleneck goal have equivalent value for states 1:4
    }
  }
}
################################################################################
#### Generating trajectories? ####
nSelect <- rep(nTrs/15, 15)
runSelector <- Analytics[1,]     #Tracks nr of steps (t.reach) to select during run
## Runselector based on most likely reachp
for(r in 1:nrow(goalytics)){
  st <- goalytics[r,s.type]; gl <- goalytics[r,g.type]; cl <- goalytics[r,clust.loc]; ns <- goalytics[r,numselect]
  runSelector <- rbind(runSelector, Analytics[s.type==st & g.type==gl & (clust.loc==cl|is.na(clust.loc)),][order(-Eselect)][1:ns,])
}
## Runselector based on accept/reject of reachp
for(r in 1:nrow(goalytics)){#1:nrow(goalytics)
  st <- goalytics[r,s.type]; gl <- goalytics[r,g.type]; cl <- goalytics[r,clust.loc]; ns <- goalytics[r,numselect]
  for(i in 1:ns){
    delt <- 0
    while(!delt){
      prop <- Analytics[s.type==st&g.type==gl & (clust.loc==cl|is.na(clust.loc)),][sample(.N,1),]
      delt = runif(1) <= prop$reachp
    }
    print(prop)
    runSelector <- rbind(runSelector, prop)
  }
}
# Remove 
runSelector <- runSelector[-1,]
## Sampler for the experiment
# Tracks particular camp sites (first start + all goals)
camps <- data.table(v = 0, s.type='start',g.type='start', t.reach=0)
# Initialize by selecting first node
camps$v <- sample(1:15, 1)
scamp <- camps$v
deepbridge=0; botbridge=0

# Get all the campsites in a chain (not balanced?)
for(cmp in 2:(nTrs+1)){#
  # Sample run and attach valid symmetry
  if(scamp %in% unlist(idmap.d)){ # Deep start
    if(deepbridge){# If not return to deep is possible from bottleneck
      curRun <- runSelector[s.type=='deep'&g.type=='deep',][sample(1:.N,1),]
    }else{
      curRun <- runSelector[s.type=='deep',][sample(1:.N,1),]
    }
    val.sym <- idmap.dg2
  }else if(scamp %in% gsym5){ # Rotational symmetry w.r.t. 5 start
    if(botbridge){ # If not return to bottleneck is possible from bridge
      curRun <- runSelector[s.type=='bottleneck'&g.type!='deep',][sample(1:.N,1),]
    }else{
      curRun <- runSelector[s.type=='bottleneck',][sample(1:.N,1),]
    }    
    val.sym <- idmap.bg5
  }else if(scamp %in% gsym6){ # Rotational symmetry w.r.t. 6 start
    if(botbridge){ # If not return to bottleneck is possible from bridge
      curRun <- runSelector[s.type=='bottleneck'&g.type!='deep',][sample(1:.N,1),]
    }else{
      curRun <- runSelector[s.type=='bottleneck',][sample(1:.N,1),]
    }
    val.sym <- idmap.bg6
  }else{
    stop('Non-accounted-for node number?')
  }
  # Remove sampled row
  #if(sum(duplicated(rbind(curRun, runSelector)))!=1){stop('Will remove more than one')}
  #runSelector <- runSelector[-(which(duplicated(rbind(curRun, runSelector)))-1),]
  rm <- which(runSelector$s.type == curRun$s.type & runSelector$g.type==curRun$g.type & 
          (runSelector$clust.loc==curRun$clust.loc | is.na(runSelector$clust.loc)) 
        & runSelector$t.reach == curRun$t.reach)[1]
  runSelector <- runSelector[-rm,]
  # Figure out rotation
  rotval <- max(which(sapply(idmap.d, function(li){scamp %in% li}))-1, which(gsym6==scamp)-1, which(gsym5==scamp)-1)*5
  # Get eligible goal nodes
  val.set <- ((val.sym[[curRun$sym.id]] - 1 +rotval) %% 15) + 1
  
  # Check if still available for balanced sampling
  val.nodes <- val.set[nSelect[val.set]!=0]
  if(length(val.nodes)==0){stop('No more eligible nodes left')}
  # Sample eligible node
  nxtcamp <- val.nodes[sample(length(val.nodes),1)]
  # Decrement nSelect
  #nSelect[nxtcamp] <- nSelect[nxtcamp]-1
  # Attach to camp data.table
  camps <- rbind(camps, data.table(v=nxtcamp, s.type='nan', g.type=curRun$g.type, t.reach=curRun$t.reach))
  # Get next starting node
  scamp <- nxtcamp
  # Check if bridges from deep to bottleneck still exist
  botbridge <- (runSelector[s.type=='deep'&g.type!='deep',.N] == 0) & (runSelector[s.type=='bottleneck' & g.type!='deep',.N]>0)
  # Check if bridges from bottleneck to deep still exist
  deepbridge <- (runSelector[s.type=='bottleneck'&g.type=='deep',.N] == 0) & (runSelector[s.type=='deep' & g.type=='deep',.N]>0)
}

# Append miniblock number
camps[,miniblock:=0:nTrs]

# Monte Carlo sampling of trajectories with t.reach requirements
full.exp <- data.table(miniblock=1, v=camps[1,v], nSteps=camps[2,t.reach-1], goal=camps[2,v])
for(cmp in 1:nTrs){
  goal.v <- camps[miniblock==cmp,v]
  start.v <- camps[miniblock==(cmp-1),v]
  t.reach <- camps[miniblock==cmp,t.reach]
  v <- MC.get.trajectory(start.v,goal.v,t.reach,Edges)
  print(cmp)
  full.exp <- rbind(full.exp, data.table(miniblock=cmp, v=v[-1], nSteps=((t.reach-1):1)-1, goal=goal.v))
}
# Get cluster ID attached
full.exp[v%in%c(1:5),c.id:='a']
full.exp[v%in%c(6:10),c.id:='b']
full.exp[v%in%c(11:15),c.id:='c']
# Count current dwell time
full.exp[,dwell:=rowid(rleid(c.id))]
# Plot final dwell times
ggplot(full.exp[shift(full.exp[,dwell==1],1,type='lead'),], aes(x=dwell))+
  geom_histogram(aes(y=..density..),alpha=0.6) +
  geom_density()
# Plot distribution of transitions until goal
ggplot(full.exp[,list(t.reach=max(nSteps)),by=miniblock], aes(x=t.reach))+
  geom_histogram()
full.exp[,max(nSteps),by=miniblock][,mean(V1)]
full.exp[,max(nSteps),by=miniblock][,var(V1)]
################################################################################
#### Comparing dwell times of MC experiment to general MC ####
# Initialize data.table to keep track of dwell times
dwell.timer <- data.table(expgen=0, c.id=0, dwell=0, type='wow')
tCounter <- 0 # Counts number of transitions across all cur.exp
for(i in 1:100){ #Get Experiment t.reach adhering
  cur.exp <- MC.full.exp(nTrs, idmap.dg2, idmap.bg5, idmap.bg6, gsym5, gsym6, idmap.d, Edges, Analytics, goalytics)
  print(i)
  tCounter = tCounter + nrow(cur.exp)
  dwell.timer <- rbind(dwell.timer, cur.exp[shift(cur.exp[,dwell==1],1,type='lead'),.(c.id, dwell)][,expgen:=i][,type:='MC.exp'])
}
# Get complete random walk for same number of transitions
rand.walk <- random.walk(Edges, tCounter)
dwell.timer <- rbind(dwell.timer, rand.walk[shift(rand.walk[,dwell==1],1,type='lead'),.(c.id,dwell)][,expgen:=1][,type:='RW'])
# Remove init row
dwell.timer<-dwell.timer[-1,]
# Declare factors
dwell.timer$expgen <- as.factor(dwell.timer$expgen)
ggplot(dwell.timer[type=='MC.exp',], aes(x=dwell))+
  geom_density(aes(col=expgen)) + 
  geom_density(data=dwell.timer[type=='RW',], aes(x=dwell),col='black')+
  theme(legend.position = 'none')
# Get expectations
dwell.timer[type=='MC.exp',mean(dwell)]
dwell.timer[type=='MC.exp',var(dwell)]
dwell.timer[type=='RW',mean(dwell)]
dwell.timer[type=='RW',var(dwell)]
################################################################################
#### Define agents ####
## Optimal agent using policy calculated before ----
opt.dat <- copy(full.exp)
opt.dat[,cur.car:=-1]
opt.dat[1,cur.car:=0]
opt.dat[,cur.rew:=0]
opt.dat[,action:=0]
opt.dat[,goal:=shift(goal,1,type='lead')]
for(tr in 1:nrow(opt.dat)){
  # Determine if goal -> give points
  gained.points <- opt.dat[tr,cur.car]*rew*(opt.dat[tr,nSteps]==0)
  opt.dat[tr,cur.rew:=cur.rew+gained.points]
  # Determine if goal -> set cur.car to zero
  opt.dat[tr,cur.car:=if(nSteps==0){0}else{cur.car}]
  # Determine action
  cur.act <- pol[opt.dat[tr,v],opt.dat[tr,cur.car+1],opt.dat[tr,goal]]
  opt.dat[tr,action:=cur.act]
  # Set cur.car of next trial one up
  next.car <- opt.dat[tr,cur.car] + (cur.act-2)
  # Decrease current reward by carry + next action
  opt.dat[tr,cur.rew:=cur.rew - next.car*car]
  opt.dat[tr+1,cur.car:=next.car]
  opt.dat[tr+1,cur.rew:=opt.dat[tr,cur.rew]]
}
opt.dat[.N,cur.rew:=opt.dat[.N-1,cur.rew]+opt.dat[.N-1,cur.car+action-2]]

## Plot some things of agent
opt.dat[,rowidx:=1:.N] # Get row index for timeseries (each trial)
# Plot point evolution
ggplot(opt.dat, aes(x=rowidx, y=cur.rew))+
  geom_line()
# Plot choice series
ggplot(opt.dat, aes(x=rowidx, y=action))+
  geom_line()
ggplot(opt.dat, aes(x=action))+
  geom_bar()
opt.dat[nSteps==0|action!=2|cur.car!=0,.N]/opt.dat[,.N]
# Plot carrying series
ggplot(opt.dat, aes(x=rowidx, y=cur.car))+
  geom_line()
opt.dat[action!=2,.N]/opt.dat[,.N]

## SR Agent ---- 
SR.dat <- data.table()
# Quick SR matrix just for plot of final representation
lr <- 0.05
gamm <- 0.95
SR <- diag(15)

# MC exp
for(tr in 2:nrow(full.exp)){
  v <- full.exp[,v]
  I <- rep(0,15); I[v[tr]] <- 1
  er <- I + gamm * SR[v[tr],] - SR[v[tr-1],]
  SR[v[tr-1],] = SR[v[tr-1],] + lr*er
}
heatmap(SR, Rowv = NA, Colv = NA)
# True Random Walk
for(tr in 2:nrow(full.exp)){
  v <- rand.walk[,v]
  I <- rep(0,15); I[v[tr]] <- 1
  er <- I + gamm * SR[v[tr],] - SR[v[tr-1],]
  SR[v[tr-1],] = SR[v[tr-1],] + lr*er
}
heatmap(SR, Rowv = NA, Colv = NA)
# Transition table
table(full.exp$v[-length(full.exp$v)], full.exp$v[-1])
################################################################################
#### Large scale reward experiment ####
multi.exp <- data.table(pp=0, miniblock=0, v=0, nSteps=0, goal=0, c.id='z', dwell=0, cur.car=-1, cur.rew=-1, action=-1)
for(ppn in 1:150){
  cur.exp <- MC.full.exp(nTrs, idmap.dg2, idmap.bg5, idmap.bg6, gsym5, gsym6, idmap.d, Edges, Analytics, goalytics)
  cur.opt <- copy(cur.exp)
  cur.opt[,cur.car:=-1]
  cur.opt[1,cur.car:=0]
  cur.opt[,cur.rew:=0]
  cur.opt[,action:=0]
  cur.opt[,goal:=shift(goal,1,type='lead')]
  for(tr in 1:(nrow(cur.opt)-1)){
    # Determine if goal -> give points
    gained.points <- cur.opt[tr,cur.car]*rew*(cur.opt[tr,nSteps]==0)
    cur.opt[tr,cur.rew:=cur.rew+gained.points]
    # Determine if goal -> set cur.car to zero
    cur.opt[tr,cur.car:=if(nSteps==0){0}else{cur.car}]
    # Determine action
    cur.act <- pol[cur.opt[tr,v],cur.opt[tr,cur.car+1],cur.opt[tr,goal]]
    cur.opt[tr,action:=cur.act]
    # Set cur.car of next trial one up
    next.car <- cur.opt[tr,cur.car] + (cur.act-2)
    # Decrease current reward by carry + next action
    cur.opt[tr,cur.rew:=cur.rew - next.car*car]
    cur.opt[tr+1,cur.car:=next.car]
    cur.opt[tr+1,cur.rew:=cur.opt[tr,cur.rew]]
  }
  cur.opt[.N,cur.rew:=cur.opt[.N-1,cur.rew]+cur.opt[.N-1,cur.car+action-2]]
  cur.opt[,pp:=ppn]
  multi.exp <- rbind(multi.exp,cur.opt)
}
multi.exp <- multi.exp[-1,]
multi.exp[,rowidx:=1:.N,by=pp]
multi.exp$pp <- as.factor(multi.exp$pp)
# Plot spaghetti cur.rew trajectory
ggplot(multi.exp, aes(x=rowidx,y=cur.rew,col=pp))+
  geom_line() +
  theme(legend.position='none')
# Plot distribution of end outcomes
ggplot(multi.exp[miniblock==75&nSteps==0,cur.rew,by=pp], aes(x=cur.rew))+
  geom_density()
################################################################################
#### Get balanced experiment for use with JS####
nPP = 3
HamCyc=13
exp.list <- gen.exhaust.experiment(nTrs, goalytics, idmap.d, idmap.dg2, idmap.bg5, idmap.bg6, gsym5, gsym6, Edges, nVisit, nPP, HamCyc)

exp.to.js(exp.list, nTrs+HamCyc*2-1, nPP)






