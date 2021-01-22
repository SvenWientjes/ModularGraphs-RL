## Functions defining agents following policies in the Fairy graph task
RandomStopper.nBT <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, parNum=1, startRew=0){
  # Initialize data.frame for keeping track of experiment
  parDat <- data.frame(pp=parNum, trial=1:nTrials, trRew=0, nSteps=0, endV=0, totRew=0, strat='RS')
  totRew <- startRew
  for(tr in 1:nTrials){
    path   <- c(vStart, sample(Edges[[vStart]],1)) #Take the first step
    totRew <- totRew-sCost                         #Pay the first cost
    trRew  <- -sCost
    for(st in 2:(nSteps+1)){
      if(tail(path,1)==vGoal){ #If you reach the goal
        totRew <- totRew+gRew
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=(trRew+gRew), nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='RS')
        break
      }else if(ceiling(runif(1,0,nSteps)) == 1 | st==(nSteps+1)){
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='RS')
        break
      }else{
         path <- c(path, sample(Edges[[tail(path,1)]][-which(Edges[[tail(path,1)]]==tail(path,2)[1])],1))
         totRew <- totRew-sCost
         trRew  <- trRew - sCost
      }
    }
  }
  return(parDat)
}

LazyWaiter.nBT <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, parNum=1, startRew=0){
  # Initialize data.frame for keeping track of experiment
  parDat <- data.frame(pp=parNum, trial=1:nTrials, trRew=0, nSteps=0, endV=0, totRew=0, strat='LW')
  totRew <- startRew
  for(tr in 1:nTrials){
    path   <- c(vStart, sample(Edges[[vStart]],1)) #Take the first step
    totRew <- totRew-sCost                         #Pay the first cost
    trRew  <- -sCost
    for(st in 2:(nSteps+1)){
      if(tail(path,1)==vGoal){ #If you reach the goal
        totRew <- totRew+gRew
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=(trRew+gRew), nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='LW')
        break
      }else if(st==(nSteps+1)){
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='LW')
        break
      }else{
        path <- c(path, sample(Edges[[tail(path,1)]][-which(Edges[[tail(path,1)]]==tail(path,2)[1])],1))
        totRew <- totRew-sCost
        trRew  <- trRew - sCost
      }
    }
  }
  return(parDat)
}

RandomLengther.nBT <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, parNum=1, startRew=0){
  parDat <- data.frame(pp=parNum, trial=1:nTrials, trRew=0, nSteps=0, endV=0, totRew=0, strat='RLen')
  totRew <- startRew
  for(tr in 1:nTrials){
    path   <- c(vStart, sample(Edges[[vStart]],1)) #Take the first step
    totRew <- totRew-sCost                         #Pay the first cost
    trRew  <- -sCost
    sampL  <- ceiling(runif(1, 0,nSteps))
    for(st in 2:(nSteps+1)){
      if(tail(path,1)==vGoal){ #If you reach the goal
        totRew <- totRew+gRew
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=(trRew+gRew), nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='RLen')
        break
      }else if(st==(nSteps+1) | length(path)>=(sampL+1)){
        parDat[parDat$trial==tr,] <-data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='RLen')
        break
      }else{
        path <- c(path, sample(Edges[[tail(path,1)]][-which(Edges[[tail(path,1)]]==tail(path,2)[1])],1))
        totRew <- totRew-sCost
        trRew  <- trRew - sCost
      }
    }
  }
  return(parDat)
}

OptimalStopper.nBT <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, piMat, parNum=1, startRew=0){
  parDat <- data.frame(pp=parNum, trial=1:nTrials, trRew=0, nSteps=0, endV=0, totRew=0, strat='OS')
  totRew <- startRew
  for(tr in 1:nTrials){
    path   <- c(vStart, sample(Edges[[vStart]],1)) #First step (forced)
    totRew <- totRew-sCost
    trRew  <- -sCost
    for(st in 2:(nSteps+1)){
      if(tail(path,1)==vGoal){
        totRew <- totRew+gRew
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=(trRew+gRew), nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='OS')
        break
      }else if((nSteps - length(path)+1) <= piMat[which(as.logical(apply(piMat[,c(1,2)], 1, all.equal, current=tail(path,2), check.attributes=F))),3]){
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='OS')
        break
      }else{
        path <- c(path, sample(Edges[[tail(path,1)]][-which(Edges[[tail(path,1)]]==tail(path,2)[1])],1))
        totRew <- totRew- sCost
        trRew  <- trRew - sCost
      }
    }
  }
  return(parDat[-1,])
}

ModularStopper <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, modTrans, parNum=1, startRew=0){
  parDat <- data.frame(pp=parNum, trial=1:nTrials, trRew=0, nSteps=0, endV=0, totRew=0, strat='MStop')
  totRew <- startRew
  for(tr in 1:nTrials){
    path   <- c(vStart, sample(Edges[[vStart]],1))
    totRew <- totRew-sCost
    trRew  <- -sCost
    for(st in 2:(nSteps+1)){
      if(tail(path,1)==vGoal){
        totRew <- totRew+gRew
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=(trRew+gRew), nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='MStop')
        break
      }else if(st==(nSteps+1) |( T %in% apply(modTrans, 1, function(x){identical(x, tail(path,2))}) )){
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='MStop')
        break
      }else{
        path <- c(path, sample(Edges[[tail(path,1)]],1))
        totRew <- totRew - sCost
        trRew  <- trRew  - sCost
      }
    }
  }
  return(parDat)
}

RandomStopper  <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, parNum=1, startRew=0){
  # Initialize data.frame for keeping track of experiment
  parDat <- data.frame(pp=parNum, trial=1:nTrials, trRew=0, nSteps=0, endV=0, totRew=0, strat='RS')
  totRew <- startRew
  for(tr in 1:nTrials){
    path   <- c(vStart, sample(Edges[[vStart]],1)) #Take the first step
    totRew <- totRew-sCost                         #Pay the first cost
    trRew  <- -sCost
    for(st in 2:(nSteps+1)){
      if(tail(path,1)==vGoal){ #If you reach the goal
        totRew <- totRew+gRew
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=(trRew+gRew), nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='RS')
        break
      }else if(ceiling(runif(1,0,nSteps)) == 1 | st==(nSteps+1)){
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='RS')
        break
      }else{
        path <- c(path, sample(Edges[[tail(path,1)]],1))
        totRew <- totRew-sCost
        trRew  <- trRew - sCost
      }
    }
  }
  return(parDat)
}

LazyWaiter <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, parNum=1, startRew=0){
  # Initialize data.frame for keeping track of experiment
  parDat <- data.frame(pp=parNum, trial=1:nTrials, trRew=0, nSteps=0, endV=0, totRew=0, strat='LW')
  totRew <- startRew
  for(tr in 1:nTrials){
    path   <- c(vStart, sample(Edges[[vStart]],1)) #Take the first step
    totRew <- totRew-sCost                         #Pay the first cost
    trRew  <- -sCost
    for(st in 2:(nSteps+1)){
      if(tail(path,1)==vGoal){ #If you reach the goal
        totRew <- totRew+gRew
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=(trRew+gRew), nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='LW')
        break
      }else if(st==(nSteps+1)){
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='LW')
        break
      }else{
        path <- c(path, sample(Edges[[tail(path,1)]],1))
        totRew <- totRew-sCost
        trRew  <- trRew - sCost
      }
    }
  }
  return(parDat)
}

RandomLengther <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, parNum=1, startRew=0){
  parDat <- data.frame(pp=parNum, trial=1:nTrials, trRew=0, nSteps=0, endV=0, totRew=0, strat='RLen')
  totRew <- startRew
  for(tr in 1:nTrials){
    path   <- c(vStart, sample(Edges[[vStart]],1)) #Take the first step
    totRew <- totRew-sCost                         #Pay the first cost
    trRew  <- -sCost
    sampL  <- ceiling(runif(1, 0,nSteps))
    for(st in 2:(nSteps+1)){
      if(tail(path,1)==vGoal){ #If you reach the goal
        totRew <- totRew+gRew
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=(trRew+gRew), nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='RLen')
        break
      }else if(st==(nSteps+1) | length(path)>=(sampL+1)){
        parDat[parDat$trial==tr,] <-data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='RLen')
        break
      }else{
        path <- c(path, sample(Edges[[tail(path,1)]],1))
        totRew <- totRew-sCost
        trRew  <- trRew - sCost
      }
    }
  }
  return(parDat)
}

OptimalStopper <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, piMat, parNum=1, startRew=0){
  parDat <- data.frame(pp=parNum, trial=1:nTrials, trRew=0, nSteps=0, endV=0, totRew=0, strat='OS')
  totRew <- startRew
  for(tr in 1:nTrials){
    path   <- c(vStart, sample(Edges[[vStart]],1))
    totRew <- totRew-sCost
    trRew  <- -sCost
    for(st in 2:(nSteps+1)){
      if(tail(path,1)==vGoal){
        totRew <- totRew+gRew
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=(trRew+gRew), nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='OS')
        break
      }else if( (nSteps-length(path)+1) <= piMat[piMat$vertex == tail(path,1), 'stopid']){
        parDat[parDat$trial==tr,] <- data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='OS')
        break
      }else{
        path <- c(path,sample(Edges[[tail(path,1)]],1))
        totRew <- totRew-sCost
        trRew  <- trRew-sCost
      }
    }
  }
  return(parDat)
}






