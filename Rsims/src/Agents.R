## Functions defining agents following policies in the Fairy graph task
RandomStopper <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, parNum=1, startRew=0){
  # Initialize data.frame for keeping track of experiment
  parDat <- data.frame(pp=parNum, trial=0, trRew=0, nSteps=0, endV=0, totRew=0, strat='init')
  totRew <- startRew
  for(tr in 1:nTrials){
    path   <- c(vStart, sample(Edges[[vStart]],1)) #Take the first step
    totRew <- totRew-sCost                         #Pay the first cost
    trRew  <- -sCost
    for(st in 2:(nSteps+1)){
      if(tail(path,1)==vGoal){ #If you reach the goal
        totRew <- totRew+gRew
        parDat <- rbind(parDat, data.frame(pp=parNum, trial=tr, trRew=(trRew+gRew), nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='RS'))
        break
      }else if(ceiling(runif(1,0,10)) == 1 | st==(nSteps+1)){
        parDat <- rbind(parDat, data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='RS'))
        break
      }else{
         path <- c(path, sample(Edges[[tail(path,1)]][-which(Edges[[tail(path,1)]]==tail(path,2)[1])],1))
         totRew <- totRew-sCost
         trRew  <- trRew - sCost
      }
    }
  }
  return(parDat[-1,])
}

LazyWaiter <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, parNum=1, startRew=0){
  # Initialize data.frame for keeping track of experiment
  parDat <- data.frame(pp=parNum, trial=0, trRew=0, nSteps=0, endV=0, totRew=0, strat='init')
  totRew <- startRew
  for(tr in 1:nTrials){
    path   <- c(vStart, sample(Edges[[vStart]],1)) #Take the first step
    totRew <- totRew-sCost                         #Pay the first cost
    trRew  <- -sCost
    for(st in 2:(nSteps+1)){
      if(tail(path,1)==vGoal){ #If you reach the goal
        totRew <- totRew+gRew
        parDat <- rbind(parDat, data.frame(pp=parNum, trial=tr, trRew=(trRew+gRew), nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='LW'))
        break
      }else if(st==(nSteps+1)){
        parDat <- rbind(parDat, data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='LW'))
        break
      }else{
        path <- c(path, sample(Edges[[tail(path,1)]][-which(Edges[[tail(path,1)]]==tail(path,2)[1])],1))
        totRew <- totRew-sCost
        trRew  <- trRew - sCost
      }
    }
  }
  return(parDat[-1,])
}

RandomLengther <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, parNum=1, startRew=0){
  parDat <- data.frame(pp=parNum, trial=0, trRew=0, nSteps=0, endV=0, totRew=0, strat='init')
  totRew <- startRew
  for(tr in 1:nTrials){
    path   <- c(vStart, sample(Edges[[vStart]],1)) #Take the first step
    totRew <- totRew-sCost                         #Pay the first cost
    trRew  <- -sCost
    sampL  <- ceiling(runif(1, 0,10))
    for(st in 2:(nSteps+1)){
      if(tail(path,1)==vGoal){ #If you reach the goal
        totRew <- totRew+gRew
        parDat <- rbind(parDat, data.frame(pp=parNum, trial=tr, trRew=(trRew+gRew), nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='RLen'))
        break
      }else if(st==(nSteps+1) | length(path)>=(sampL+1)){
        parDat <- rbind(parDat, data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='RLen'))
        break
      }else{
        path <- c(path, sample(Edges[[tail(path,1)]][-which(Edges[[tail(path,1)]]==tail(path,2)[1])],1))
        totRew <- totRew-sCost
        trRew  <- trRew - sCost
      }
    }
  }
  return(parDat[-1,])
}

OptimalStopper <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, hitMat, goalstepchance, parNum=1, startRew=0){
  parDat <- data.frame(pp=parNum, trial=0, trRew=0, nSteps=0, endV=0, totRew=0, strat='init')
  
  # Get the points where EV becomes negative - now we want to stop!
  EV_data <- EVcalc(Edges = Edges, vStart=vStart, vGoal=vGoal, nSteps=nSteps, gRew=gRew, sCost=sCost, hitMat=hitMat, goalstepchance=goalstepchance)
  changePoint <- data.frame(trans=c('1<-4','1<-2','1<-5','6<-4', '7<-6', '7<-9', '6<-7', '4<-1','5<-15','5<-1','4<-6'), cP=0)
  
  for(tr in levels(EV_data$trans)){
    if(T %in% (EV_data[EV_data$trans==tr,'value']<=0)){
      #changePoint <- rbind(changePoint, data.frame(trans=tr, cP=max(which(EV_data[EV_data$trans==tr,'value']<=0))))
      changePoint[changePoint$trans==tr,]$cP <- max(which(EV_data[EV_data$trans==tr,'value']<=0))
    }
  }
  
  totRew <- startRew
  for(tr in 1:nTrials){
    path <- c(vStart, sample(Edges[[vStart]],1))
    totRew <- totRew-sCost                         #Pay the first cost
    trRew  <- -sCost
    
    for(st in 2:(nSteps+1)){
      if(tail(path,1)==vGoal){ #If you reach the goal
        totRew <- totRew+gRew
        parDat <- rbind(parDat, data.frame(pp=parNum, trial=tr, trRew=(trRew+gRew), nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='OS'))
        break
      }else if(st==(nSteps+1)){ #If you reached the maximum number of steps in a trial
        parDat <- rbind(parDat, data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='OS'))
        break
        # CHECK ALL CLASSES for optimal stopping
      }else if(tail(path,1) %in% c( 1, 2, 3) & tail(path,2)[1] %in% c( 4)       & (nSteps-st+1) <= changePoint[changePoint$trans=='1<-4',]$cP |
               tail(path,1) %in% c(12,13,14) & tail(path,2)[1] %in% c(11)       & (nSteps-st+1) <= changePoint[changePoint$trans=='1<-4',]$cP |
               tail(path,1) %in% c(12,13,14) & tail(path,2)[1] %in% c(12,13,14) & (nSteps-st+1) <= changePoint[changePoint$trans=='1<-2',]$cP |
               tail(path,1) %in% c( 1, 2, 3) & tail(path,2)[1] %in% c( 1, 2, 3) & (nSteps-st+1) <= changePoint[changePoint$trans=='1<-2',]$cP |
               tail(path,1) %in% c( 1, 2, 3) & tail(path,2)[1] %in% c( 5)       & (nSteps-st+1) <= changePoint[changePoint$trans=='1<-5',]$cP |
               tail(path,1) %in% c(12,13,14) & tail(path,2)[1] %in% c(15)       & (nSteps-st+1) <= changePoint[changePoint$trans=='1<-5',]$cP |
               tail(path,1) %in% c( 6,10)    & tail(path,2)[1] %in% c( 4,11)    & (nSteps-st+1) <= changePoint[changePoint$trans=='6<-4',]$cP |
               tail(path,1) %in% c( 7, 9)    & tail(path,2)[1] %in% c( 6,10)    & (nSteps-st+1) <= changePoint[changePoint$trans=='7<-6',]$cP |
               tail(path,1) %in% c( 7, 9)    & tail(path,2)[1] %in% c( 7, 9)    & (nSteps-st+1) <= changePoint[changePoint$trans=='7<-9',]$cP |
               tail(path,1) %in% c( 6,10)    & tail(path,2)[1] %in% c( 7, 9)    & (nSteps-st+1) <= changePoint[changePoint$trans=='6<-7',]$cP |
               tail(path,1) %in% c( 4)       & tail(path,2)[1] %in% c( 1, 2, 3) & (nSteps-st+1) <= changePoint[changePoint$trans=='4<-1',]$cP |
               tail(path,1) %in% c(11)       & tail(path,2)[1] %in% c(12,13,14) & (nSteps-st+1) <= changePoint[changePoint$trans=='4<-1',]$cP |
               tail(path,1) %in% c( 5,15)    & tail(path,2)[1] %in% c( 5,15)    & (nSteps-st+1) <= changePoint[changePoint$trans=='5<-15',]$cP|
               tail(path,1) %in% c( 5)       & tail(path,2)[1] %in% c( 1, 2, 3) & (nSteps-st+1) <= changePoint[changePoint$trans=='5<-1',]$cP |
               tail(path,1) %in% c(15)       & tail(path,2)[1] %in% c(12,13,14) & (nSteps-st+1) <= changePoint[changePoint$trans=='5<-1',]$cP |
               tail(path,1) %in% c( 4,11)    & tail(path,2)[1] %in% c( 6,10)    & (nSteps-st+1) <= changePoint[changePoint$trans=='4<-6',]$cP){ 
        parDat <- rbind(parDat, data.frame(pp=parNum, trial=tr, trRew=trRew, nSteps=(length(path)-1), endV=tail(path,1), totRew=totRew, strat='OS'))
        break
      }else{
        path <- c(path, sample(Edges[[tail(path,1)]][-which(Edges[[tail(path,1)]]==tail(path,2)[1])],1))
        totRew <- totRew-sCost
        trRew  <- trRew - sCost
      }
    }
  }
  return(parDat[-1,])
}

ModularStopper <- function(Edges, vStart, vGoal, nSteps, gRew, sCost, nTrials, hitMat, goalstepchance, parNum=1, startRew=0){
  ab
}
