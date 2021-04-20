# Function that calculates optimal choices for Schapiro-Bet task
get.opt.choice <- function(tr, v, goal, stepsleft, tMat, winM, loseM){
  opt.EV <- rep(0, length(v))
  for(toti in 1:length(v)){
    I <- rep(0,15); I[v[toti]]<-1
    if(stepsleft[toti]==0|v[toti]==goal[toti]){
      if(v[toti]==goal[toti]){opt.EV[toti]<-winM}else{opt.EV[toti]<-loseM}
    }else{
      hitVec <- rep(0,15)
      tMat.goal <- tMat
      tMat.goal[goal[toti],] <- 0
      for(s in stepsleft[toti]:1){
        hitC <- I %*% matrix.power(tMat.goal, s)
        hitVec[s] <- hitC[goal[toti]]
      }
      opt.EV[toti] <- sum(hitVec)*winM + (1-sum(hitVec)) * loseM
    }
  }
  opt.choice <- as.numeric(opt.EV>0)
  opt.choice[which(opt.choice==0)] <- -1
  return(opt.choice)
}

# Function to sample a start-end combination satisfying availability and miniblock-type constraints
bet.sum <- function(vec, startP){
  cum <- rep(0, length(vec))
  cum[1] <- max(startP+vec[1],0)
  for(i in 2:length(vec)){
    cum[i] <- max(0, cum[i-1]+vec[i])
  }
  return(cum)
}

# Function for sampling trajectories that have a specific start and reach a specific goal
samp.win <- function(start.v, goal.v, nVisit, Edges){
  goal.absent <- 1
  while(goal.absent){
    v <- start.v
    for(nextexp in 2:nVisit){
      v[nextexp] <- sample(Edges[[v[nextexp-1]]], 1)
      if(v[nextexp]==goal.v){
        goal.absent <- 0
        break
      }
    }
  }
  return(v)
}

# Function for sampling trajectories that have a specific start and do not reach a specific goal
samp.lose <- function(start.v, goal.v, nVisit, Edges){
  goal.present <- 1
  while(goal.present){
    v <- start.v
    for(nextexp in 2:nVisit){
      v[nextexp] <- sample(Edges[[v[nextexp-1]]],1)
    }
    if(!goal.v %in% v){
      goal.present = 0
      return(v)
    }
  }
}




