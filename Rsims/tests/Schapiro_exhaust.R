################################################################################
############## Simulating aspects of the Schapiro-exhaustion task ##############
################################################################################
library(data.table)
library(foreach)
library(doParallel)
library(matrixcalc)
################################################################################
# Task Parameters
nTrs <- 100
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
idmap.bg  <- list(a=c(1), b=c(2,3,4), c=c(5), d=c(7,8,9), e=c(10), f=c(11),g=c(12,13,14),h=c(15)) # Symmetries to Bottleneck Goal (b) ; goal = 6
################################################################################
#### How likely is it to reach a particular goal type from a particular state type? ####
# Organize different start and goal types
s.types <- c('deep','bottleneck')
g.types <- c('deep','bottleneck close', 'bottleneck far')

# Initialize data.table to get all values
Analytics <- data.table(rbind(expand.grid(s.types[1], g.types, NA), 
                              expand.grid(s.types[2], g.types, c('close','far'))))
# Name variables
names(Analytics) <- c('s.type', 'g.type', 'c.type')

# Get start IDs
Analytics[s.type=='deep',s.id:=2]
Analytics[s.type=='bottleneck',s.id:=1]
# Get goal IDs
Analytics[,g.id:=c(7,6,10,14,15,11,7,6,10)]

# Get hitvec for a particular Analytics row
for(r in 1:1){
  # Initialize hitvec of all checked steps
  hitvec <- rep(0, nVisit)
  # Absorbing goal matrix
  tMat.goal <- tMat
  tMat.goal[Analytics[r,g.id],] <- 0
  # One hot state ID
  I <- rep(0,15); I[Analytics[r,s.id]] <- 1
  
  # Fill hitvec
  for(s in 1:nVisit){
    hitvec[s] <- (I %*% matrix.power(tMat.goal, s))[Analytics[r,g.id]]
  }
}
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
rew  <- 1   #Reward per item delivered
car  <- 0.1 #Cost per item per carry
pick <- 0.1 #Cost for lollygagging
cap = 3
# Initialize Expected Value matrix
V <- matrix(0, 15,(cap+1))

#Asynchronous Value Iteration - carry payment in state
for(trType in 1:1){
  # Get goal (instructed)
  goal <- Analytics[trType,g.id]
  # Clamp value of goal immediately, no need for computation
  V[goal,] <- c(0:cap)*rew - c(0:cap)*car
  nValit <- 1e5
  for(i in 1:nValit){
    state.node  <- sample(c(1:15)[-goal],1) #Randomly pick a state
    state.carry <- sample(1:(cap+1),1)            #Randomly pick a carrying hold
    # Inspect values for action
      #actions: 1=decrease, 2=maintain, 3=increase
    r <- rep(-car*(state.carry-1),3) 
    if(state.carry==1){
      r[1] = r[1]-pick
    }else if(state.carry==(cap+1)){
      r[3] = r[3]-pick
    }
    # Avg reward of potential successors
    r[1] <- r[1] + sum(sapply(which(tMat[state.node,]!=0), function(fs){1/4*V[fs,max(state.carry-1,1)]}))
    r[2] <- r[2] + sum(sapply(which(tMat[state.node,]!=0), function(fs){1/4*V[fs,state.carry]}))
    r[3] <- r[3] + sum(sapply(which(tMat[state.node,]!=0), function(fs){1/4*V[fs,min(state.carry+1,cap+1)]}))
    # Update V
    V[state.node, state.carry] <- max(r)
  }
}

#Asynchronous Value Iteration - carry payment upon transition
for(trType in 1:1){
  # Get goal (instructed)
  goal <- Analytics[trType,g.id]
  # Clamp value of goal immediately, no need for computation
  V[goal,] <- c(0:cap)*rew
  nValit <- 1e5
  for(i in 1:nValit){
    state.node  <- sample(c(1:15)[-goal],1) #Randomly pick a state
    state.carry <- sample(1:(cap+1),1)      #Randomly pick a carrying hold
    
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
    r[1] <- r[1] + sum(sapply(which(tMat[state.node,]!=0), function(fs){1/4*V[fs,max(state.carry-1,1)]}))
    r[2] <- r[2] + sum(sapply(which(tMat[state.node,]!=0), function(fs){1/4*V[fs,state.carry]}))
    r[3] <- r[3] + sum(sapply(which(tMat[state.node,]!=0), function(fs){1/4*V[fs,min(state.carry+1,cap+1)]}))
    
    V[state.node, state.carry] <- max(r)
  }
}









