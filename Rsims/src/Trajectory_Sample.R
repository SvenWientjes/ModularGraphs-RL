## Function for sampling goal adhering to required goal type
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

#samp.lose <- function(start.v, goal.v, nVisit, Edges, Tcount.v){
  goal.present <- 1
  while(goal.present){
    Tcounter <- Tcount.v
    v <- start.v
    for(nextexp in 2:nVisit){
      if(!F %in% Tcounter[Var1==v[nextexp-1],V3==0]){
        stop('Transitions have become impossible - All exceed max count')
      }
      v[nextexp] <- Tcounter[Var1==v[nextexp-1] & V3!=0,sample(Var2,1)]
      Tcounter[Var1==v[nextexp-1] & Var2==v[nextexp],]$V3 <- Tcounter[Var1==v[nextexp-1] & Var2==v[nextexp],]$V3 - 1
    }
    if(!goal.v %in% v){
      goal.present <- 0
      return(list(v=v, Tcount=Tcounter))
    }
  }
}

elig.ends <- function(startType, goalType, startCount, goalCount, idmap.g, bt.map, c.map){
  all.el <- matrix(c(0,0),nrow=1,ncol=2)
  colnames(all.el) <- c('Var1', 'Var2')
  if(startType == 'Deep'){
    el.start <- unlist(idmap.g)
    el.start <- el.start[which(startCount[el.start]!=0)]
    
  }else if(startType == 'Bottleneck'){
    el.start <- unlist(bt.map)
    el.start <- el.start[which(startCount[el.start]!=0)]
  }
  # Check which goals are still available for each available start!
  if(goalType == 'Deep'){
    agoal <- unlist(idmap.g)
    agoal <- agoal[which(goalCount[agoal]!=0)]
    for(Is in el.start){
      start.c <- names(c.map)[sapply(idmap.g, function(cc){Is %in% cc})]
      el.c    <- names(c.map)[sapply(c.map, function(cm){start.c %in% cm})]
      goalp   <- c(sapply(el.c, function(idg){idmap.g[[idg]]}))
      tgoals  <- goalp[which(goalp %in% agoal)]
      all.el <- rbind(all.el, expand.grid(Is, tgoals))
    }
  }else if(goalType == 'Bottleneck Close'){
    agoal <- unlist(bt.map)
    agoal <- agoal[which(goalCount[agoal]!=0)]
    for(Is in el.start){
      start.c <- names(c.map)[sapply(idmap.g, function(cc){Is %in% cc})]
      el.c    <- names(c.map)[sapply(c.map, function(cm){start.c %in% cm})]
      el.nidx <- sapply(el.c, function(cv){which(c.map[[cv]] == start.c)})
      goalp  <- sapply(1:2, function(cidx){bt.map[[el.c[cidx]]][el.nidx[cidx]]})
      tgoals  <- goalp[which(goalp %in% agoal)]
      all.el <- rbind(all.el, expand.grid(Is, tgoals))
    }
  }else if(goalType == 'Bottleneck Far'){
    agoal <- unlist(bt.map)
    agoal <- agoal[which(goalCount[agoal]!=0)]
    for(Is in el.start){
      start.c <- names(c.map)[sapply(idmap.g, function(cc){Is %in% cc})]
      el.c    <- names(c.map)[sapply(c.map, function(cm){start.c %in% cm})]
      el.nidx <- sapply(el.c, function(cv){which(c.map[[cv]] != start.c)})
      goalp  <- sapply(1:2, function(cidx){bt.map[[el.c[cidx]]][el.nidx[cidx]]})
      tgoals  <- goalp[which(goalp %in% agoal)]
      all.el <- rbind(all.el, expand.grid(Is, tgoals))
    }
  }else if(goalType == 'Close Bottleneck Close'){
    agoal <- unlist(bt.map)
    agoal <- agoal[which(goalCount[agoal]!=0)]
    for(Is in el.start){
      start.c <- names(c.map)[sapply(bt.map, function(cc){Is %in% cc})]
      el.c    <- c.map[[start.c]][which(bt.map[[start.c]]==Is)]
      goalp <- bt.map[[el.c]][which(c.map[[el.c]]==start.c)]
      tgoals  <- goalp[which(goalp %in% agoal)]
      all.el <- rbind(all.el, expand.grid(Is, tgoals))
    }
  }else if(goalType == 'Close Bottleneck Far'){
    agoal <- unlist(bt.map)
    agoal <- agoal[which(goalCount[agoal]!=0)]
    for(Is in el.start){
      start.c <- names(c.map)[sapply(bt.map, function(cc){Is %in% cc})]
      el.c    <- c.map[[start.c]][which(bt.map[[start.c]]==Is)]
      goalp <- bt.map[[el.c]][which(c.map[[el.c]]!=start.c)]
      tgoals  <- goalp[which(goalp %in% agoal)]
      all.el <- rbind(all.el, expand.grid(Is, tgoals))
    }
  }else if(goalType == 'Far Bottleneck Close'){
    agoal <- unlist(bt.map)
    agoal <- agoal[which(goalCount[agoal]!=0)]
    for(Is in el.start){
      start.c <- names(c.map)[sapply(bt.map, function(cc){Is %in% cc})]
      el.c    <- c.map[[start.c]][which(bt.map[[start.c]]!=Is)]
      goalp <- bt.map[[el.c]][which(c.map[[el.c]]==start.c)]
      tgoals  <- goalp[which(goalp %in% agoal)]
      all.el <- rbind(all.el, expand.grid(Is, tgoals))
    }
  }else if(goalType == 'Far Bottleneck Far'){
    agoal <- unlist(bt.map)
    agoal <- agoal[which(goalCount[agoal]!=0)]
    for(Is in el.start){
      start.c <- names(c.map)[sapply(bt.map, function(cc){Is %in% cc})]
      el.c    <- c.map[[start.c]][which(bt.map[[start.c]]!=Is)]
      goalp <- bt.map[[el.c]][which(c.map[[el.c]]!=start.c)]
      tgoals  <- goalp[which(goalp %in% agoal)]
      all.el <- rbind(all.el, expand.grid(Is, tgoals))
    }
  }else if(goalType == 'Far Deep'){
    agoal <- unlist(idmap.g)
    agoal <- agoal[which(goalCount[agoal]!=0)]
    for(Is in el.start){
      start.c <- names(c.map)[sapply(bt.map, function(cc){Is %in% cc})]
      el.c    <- c.map[[start.c]][which(bt.map[[start.c]]!=Is)]
      goalp <- idmap.g[[el.c]]
      tgoals  <- goalp[which(goalp %in% agoal)]
      all.el <- rbind(all.el, expand.grid(Is, tgoals))
    }
  }else if(goalType == 'Close Deep'){
    agoal <- unlist(idmap.g)
    agoal <- agoal[which(goalCount[agoal]!=0)]
    for(Is in el.start){
      start.c <- names(c.map)[sapply(bt.map, function(cc){Is %in% cc})]
      el.c    <- c.map[[start.c]][which(bt.map[[start.c]]==Is)]
      goalp <- idmap.g[[el.c]]
      tgoals  <- goalp[which(goalp %in% agoal)]
      all.el <- rbind(all.el, expand.grid(Is, tgoals))
    }
  }
  

  if(nrow(all.el) <= 1){
    stop('There was no eligible goal found...')
  }else{
    all.el <- all.el[-1,]
    return(all.el[sample(1:nrow(all.el),1),])
  }
}


