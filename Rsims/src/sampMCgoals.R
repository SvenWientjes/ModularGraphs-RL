sampMCgoals <- function(nTrs, goalytics, idmap.d, idmap.dg2, idmap.bg5, idmap.bg6, gsym5, gsym6){
  tryt = 1 # Tracker for visual feedback
  nStart <- rep(nTrs/15, 15)
  nGoal <- rep(nTrs/15, 15)
  trTracker <- goalytics[,.(s.type,g.type,clust.loc,numselect,sym.id)]

while(any(nStart!=0)|any(nGoal!=0)){
  print(paste('Goal attempt',tryt))
  tryt <- tryt+1
  nStart <- rep(nTrs/15, 15)
  nGoal <- rep(nTrs/15, 15)
  trTracker <- goalytics[,.(s.type,g.type,clust.loc,numselect,sym.id)]
  
  telomeres <- data.table(s.type='init',g.type='init',clust.loc='init',v=0,goal=0,sym.id='init')
  # Identify current starting node
  curS <- nStart[which(nStart>0)][sample(1:length(which(nStart>0)),1)]
  for(i in 1:nTrs){
    # Type deep or bottleneck start?
    curStype <- if(curS %in% unlist(idmap.d)){'deep'}else{'bottleneck'}
    # Which symmetry is valid?
    val.sym <- if(curS %in% unlist(idmap.d)){idmap.dg2}else if(curS %in% gsym5){idmap.bg5}else if(curS %in% gsym6){idmap.bg6}
    # Which symmetries are not completed?
    el.sym <- trTracker[s.type==curStype & numselect>0,sym.id]
    # What is the correct rotation?
    rotval <- max(which(sapply(idmap.d, function(li){curS %in% li}))-1, which(gsym6==curS)-1, which(gsym5==curS)-1)*5
    # Which nodes are in the correct rotation of el.sym?
    samp.el <- data.table(v=0, sym='init')
    for(j in el.sym){
      # Get rotated valid nodes
      el.nodes.1 <- ((val.sym[[j]]-1+rotval)%%15)+1
      for(k in el.nodes.1){
        if(nGoal[k]>0){ # Check if node still available
          # Bind available nodes to sampling data.table
          samp.el <- rbind(samp.el, data.table(v=k, sym=j))
        }
      }
    }
    samp.el <- samp.el[-1,]
    if(nrow(samp.el)==0){break}
    # Sample one goal
    curG <- samp.el[sample(.N,1),]
    # Decrement counters
    nStart[curS] <- nStart[curS]-1
    nGoal[curG$v] <- nGoal[curG$v]-1
    trTracker[s.type==curStype & sym.id==curG$sym, numselect:=numselect-1]
    # Append to data.table
    telomeres <- rbind(telomeres, data.table(s.type=curStype, 
                                             g.type=goalytics[s.type==curStype & sym.id==curG$sym,g.type],
                                             clust.loc=goalytics[s.type==curStype & sym.id==curG$sym,clust.loc],
                                             v=curS, goal=curG$v, sym.id=curG$sym))
    # Next start is current goal
    curS <- curG$v
  }
}
print('Goal success!')
return(telomeres[-1,])

}

trajSamp.max <- function(start.v, goal.v, Edges, maxL){
  trT <- 0 # tracks renewals
  goal.absent = T
  while(goal.absent){
    v <- start.v # trajectory
    for(st in 2:maxL){
      v[st] <- sample(Edges[[v[st-1]]],1)
      if(v[st]==goal.v){
        return(v)
      }
    }
    trT <- trT+1
    print(paste('Trajectory retry',trT))
  }
}

pad.random.walk <- function(start.v, end.v, goal.v, Edges, minL, maxL){
  # Start RW
  v <- start.v
  # Get true RW up until min length
  while(length(v)<minL | goal.v %in% v | !v[length(v)]==end.v){
    v <- start.v
    while((length(v)<minL | !v[length(v)]==end.v) & !length(v)>maxL){
      v[length(v)+1] <- sample(Edges[[v[length(v)]]],1)
    }
  }
  return(v)
}

gen.exhaust.experiment <- function(nTrs, goalytics, idmap.d, idmap.dg2, idmap.bg5, idmap.bg6, gsym5, gsym6, Edges, nVisit, nPP, HamCyc=0){
  all.telomeres <- data.table(pp=0, s.type='init', g.type='init', clust.loc='init', v=0, goal=0, sym.id='init')
  all.exp <- data.table(pp=0, miniblock=0, v=0, nSteps=0, goal=0)
  for(ppn in 1:nPP){
    cHamCyc <- HamCyc
    telomeres <- sampMCgoals(nTrs, goalytics, idmap.d, idmap.dg2, idmap.bg5, idmap.bg6, gsym5, gsym6)
    # Rework into sequence of 'camps' (intermediate goal/starts)
    camps <- data.table(v.type = as.character(shift(telomeres$g.type, 1, type='lag')), v = telomeres$v)
    camps <- rbind(camps, data.table(v.type=as.character(telomeres[.N,g.type]), v=telomeres[.N,goal]))
    camps$v.type[1] <- 'start'
    camps$miniblock <- 0:nTrs
    
    # Monte Carlo sampling of trajectories with t.reach requirements
    gen.exp <- data.table(miniblock=0, v=0, nSteps=0, goal=0)
    for(cmp in 1:nTrs){
      goal.v <- camps[miniblock==cmp,v]
      start.v <- camps[miniblock==(cmp-1),v]
      v <- trajSamp.max(start.v,goal.v,Edges,nVisit)
      print(cmp)
      gen.exp <- rbind(gen.exp, data.table(miniblock=cmp, v=v, nSteps=((length(v)):1)-1, goal=goal.v))
    }
    gen.exp <- gen.exp[-1,]
    
    # Get tally for bottleneck random walks in between hamiltonian runs
    btypeTally <- rep(floor(HamCyc-1)/2,2)
    while(cHamCyc > 0){
      # Generate a Hamiltonian trial
      v <- hamcyc.run(gen.exp[.N,v])
      # Append run to gen.exp
      gen.exp <- rbind(gen.exp, data.table(miniblock=gen.exp[,max(miniblock)+1], v=v, nSteps=((length(v)):1)-1, goal=v[length(v)]))
      # Check what to append to telomeres
      if(v[1]%in%unlist(idmap.d)){
        telomeres <- rbind(telomeres, data.table(s.type='deep', g.type='deep', clust.loc='na', v=v[1], goal=v[length(v)], sym.id='b'))
      }else if(v[1]%in%unlist(idmap.bt)){
        telomeres <- rbind(telomeres, data.table(s.type='bottleneck', g.type='deep',clust.loc='close',v=v[1],goal=v[length(v)],sym.id='b'))
      }
      # Interleave RW to btn in between HamCycs to deep nodes
      if(cHamCyc > 1){
        # Generate a random walk to a bottleneck
        rb.list <- randBTN.run(gen.exp[.N,v], idmap.dg2, btypeTally, Edges, nVisit, idmap.dg2b, idmap.bg5, idmap.bg6, idmap.d, gsym5, gsym6)
        v <- rb.list[[1]]
        btypeTally <- rb.list[[2]]
        gen.exp <- rbind(gen.exp, data.table(miniblock=gen.exp[,max(miniblock)+1], v=v, nSteps=((length(v)):1)-1, goal=v[length(v)]))
        
        telomeres <- rbind(telomeres, data.table(s.type='deep', g.type=goalytics[s.type=='deep'&sym.id==rb.list[[3]],g.type], 
                                                 clust.loc='na', v=v[1], goal=v[length(v)], sym.id=rb.list[[3]]))
      }
      cHamCyc <- cHamCyc-1
    }
    
    
    all.exp <- rbind(all.exp, gen.exp[,pp:=ppn])
    all.telomeres <- rbind(all.telomeres,telomeres[,pp:=ppn])
  }
  return(list(all.exp[-1,], all.telomeres[-1,]))
}

exp.to.js <- function(exp.list, nTrs, nPP){
  telomeres <- exp.list[[2]]
  telomeres[,tr:=1:nTrs,by=pp]
  
  goallist <- paste0('var goallist = [',paste(sapply(1:nPP, function(p){
                paste0('[',paste(sapply(1:nTrs, function(tri){
                  telomeres[pp==p & tr==tri,goal]-1
                }), collapse=', '),']')
              }), collapse=', \n'),'];')
  
  write1sub <- file('data/goallist.js')
  writeLines(goallist, write1sub)
  close(write1sub)
  
  trajs <- exp.list[[1]]
  test2sub <- paste0('var trajectories = [',paste(sapply(1:nPP, function(p){
                paste0('[',paste(sapply(1:nTrs, function(tri){
                  paste0('[',toString(trajs[pp==p & miniblock==tri,v]-1),']')
                }), collapse=', \n'),']')
              }), collapse= ', \n'), '];')
  
  write1sub <- file('data/trajectories.js')
  writeLines(test2sub, write1sub)
  close(write1sub)
}

gen.train <- function(nTrs, goalytics, idmap.d, idmap.dg2, idmap.bg5, idmap.bg6, gsym5, gsym6, Edges, nVisit, nPP){
  all.telomeres <- data.table(pp=0, s.type='init', g.type='init', clust.loc='init', v=0, goal=0, sym.id='init')
  all.exp <- data.table(pp=0, miniblock=0, v=0, nSteps=0, goal=0)
  for(ppn in 1:nPP){
    telomeres <- sampMCgoals(nTrs, goalytics, idmap.d, idmap.dg2, idmap.bg5, idmap.bg6, gsym5, gsym6)
    # Rework into sequence of 'camps' (intermediate goal/starts)
    camps <- data.table(v.type = as.character(shift(telomeres$g.type, 1, type='lag')), v = telomeres$v)
    camps <- rbind(camps, data.table(v.type=as.character(telomeres[.N,g.type]), v=telomeres[.N,goal]))
    camps$v.type[1] <- 'start'
    camps$miniblock <- 0:nTrs
    
    # Monte Carlo sampling of trajectories with t.reach requirements
    gen.exp <- data.table(miniblock=0, v=0, nSteps=0, goal=0)
    for(cmp in 1:nTrs){
      goal.v <- camps[miniblock==cmp,v]
      start.v <- camps[miniblock==(cmp-1),v]
      v <- trajSamp.max(start.v,goal.v,Edges,nVisit)
      print(cmp)
      gen.exp <- rbind(gen.exp, data.table(miniblock=cmp, v=v, nSteps=((length(v)):1)-1, goal=goal.v))
    }
    gen.exp <- gen.exp[-1,]
    all.exp <- rbind(all.exp, gen.exp[,pp:=ppn])
    all.telomeres <- rbind(all.telomeres,telomeres[,pp:=ppn])
  }
  return(list(all.exp[-1,], all.telomeres[-1,]))
}

gen.D.Ham <- function(start.v, goal.v, minRW, maxRW, idmap.d, idmap.dg2, c.map, idmap.bt, Edges){
  #minRW/maxRW: Minimum / maximum length of initial padded random walk
  #v.start    : True starting identity (start of RW)
  #v.goal     : True identity of goal node
  ##################################################################
  # Get valid symmetry for goal
  rotval <- (which(sapply(idmap.d, function(li){goal.v %in% li}))-1) * 5
  # Get valid start for Hamiltonian portion
  ham.s <- sample((((idmap.dg2[['b']]+rotval)-1)%%15)+1, 1)
  # Get symmetries (start, goal, lateral)
  ham.s.sym <- names(idmap.d)[which(sapply(idmap.d, function(li){ham.s %in% li}))]
  g.sym <- names(idmap.d)[which(sapply(idmap.d, function(li){goal.v %in% li}))]
  l.sym <- c('a','b','c')[which(!c('a','b','c')%in%c(ham.s.sym,g.sym))]
  # Get initial random walk (last value of RW is already b1)
  v <- pad.random.walk(start.v, ham.s, goal.v, Edges, minRW+1, maxRW+1)
  
  ## Get relevant test walk
  # Same cluster as ham.s
  el.b <- idmap.d[[ham.s.sym]]
  v <- c(v, sample(el.b[which(el.b != ham.s)]))
  # Between transition
  v <- c(v, idmap.bt[[ham.s.sym]][which(c.map[[ham.s.sym]]==l.sym)])
  v <- c(v, idmap.bt[[l.sym]][which(c.map[[l.sym]]==ham.s.sym)])
  # Deep nodes of lateral cluster
  v <- c(v, sample(idmap.d[[l.sym]]))
  # Into the goal cluster
  v <- c(v, idmap.bt[[l.sym]][which(c.map[[l.sym]]==g.sym)])
  v <- c(v, idmap.bt[[g.sym]][which(c.map[[g.sym]]==l.sym)])
  # Two non-goal deep goal nodes
  v <- c(v, sample(idmap.d[[g.sym]][which(idmap.d[[g.sym]]!=goal.v)]))
  v <- c(v, idmap.bt[[g.sym]][which(c.map[[g.sym]]==ham.s.sym)])
  v <- c(v, goal.v)
  return(v)
}

gen.D.Min <- function(start.v, goal.v, minRW, maxRW, idmap.d, idmap.dg2, c.map, idmap.bt, Edges){
  #minRW/maxRW: Minimum / maximum length of initial padded random walk
  #v.start    : True starting identity (start of RW)
  #v.goal     : True identity of goal node
  ##################################################################
  # Get valid symmetry for goal
  rotval <- (which(sapply(idmap.d, function(li){goal.v %in% li}))-1) * 5
  # Get valid start for Hamiltonian portion
  ham.s <- sample((((idmap.dg2[['c']]+rotval)-1)%%15)+1, 1)
  # Get symmetries (start, goal, lateral)
  ham.s.sym <- names(idmap.d)[which(sapply(idmap.bt, function(li){ham.s %in% li}))]
  g.sym <- names(idmap.d)[which(sapply(idmap.d, function(li){goal.v %in% li}))]
  l.sym <- c('a','b','c')[which(!c('a','b','c')%in%c(ham.s.sym,g.sym))]
  # Get initial random walk
  v <- pad.random.walk(start.v, ham.s, goal.v, Edges, minRW, maxRW)
  # Quick move through goal cluster
  v <- c(v, idmap.bt[[g.sym]][which(c.map[[g.sym]]==ham.s.sym)])
  v <- c(v, sample(idmap.d[[g.sym]][which(idmap.d[[g.sym]]!=goal.v)],1))
  v <- c(v, idmap.bt[[g.sym]][which(c.map[[g.sym]]==l.sym)])
  # Move into lateral sym and get all nodes
  v <- c(v, idmap.bt[[l.sym]][which(c.map[[l.sym]]==g.sym)])
  v <- c(v, sample(idmap.d[[l.sym]]))
  # b -> a transition and successive a -> a transition!
  v <- c(v, idmap.bt[[l.sym]][which(c.map[[l.sym]]==ham.s.sym)])
  v <- c(v, idmap.bt[[ham.s.sym]][which(c.map[[ham.s.sym]]==l.sym)])
  # Sample one node from s sym and move into goal cluster
  v <- c(v, sample(idmap.d[[ham.s.sym]],1))
  v <- c(v, idmap.bt[[ham.s.sym]][which(c.map[[ham.s.sym]]==g.sym)])
  v <- c(v, idmap.bt[[g.sym]][which(c.map[[g.sym]]==ham.s.sym)])
  # Sample two deep goal nodes
  v <- c(v, sample(idmap.d[[g.sym]][which(idmap.d[[g.sym]]!=goal.v)]))
  v <- c(v, idmap.bt[[g.sym]][which(c.map[[g.sym]]==l.sym)])
  v <- c(v, goal.v)
  return(v)
}

gen.B.Min <- function(start.v, goal.v, minRW, maxRW, idmap.d, idmap.bg5,idmap.bg6, gsym5, gsym6, c.map, idmap.bt, Edges){
  if(goal.v %in% gsym5){ # Rotational symmetry w.r.t. 5 start
    val.sym <- idmap.bg5
    rotval <- (which(gsym5==goal.v)-1)*5
  }else if(goal.v %in% gsym6){ # Rotational symmetry w.r.t. 6 start
    val.sym <- idmap.bg6
    rotval <- (which(gsym6==goal.v)-1)*5
  }
  # Get valid start for Hamiltonian portion
  ham.s <- sample((((val.sym[['g']]+rotval)-1)%%15)+1, 1)
  val.sym <- sapply(val.sym, function(li){((li+rotval-1)%%15)+1})
  # Get symmetries (start, goal, lateral)
  ham.s.sym <- names(idmap.d)[which(sapply(idmap.bt, function(li){ham.s %in% li}))]
  g.sym <- names(idmap.d)[which(sapply(idmap.bt, function(li){goal.v %in% li}))]
  l.sym <- c('a','b','c')[which(!c('a','b','c')%in%c(ham.s.sym,g.sym))]
  # Get initial random walk (last value of RW is already b1)
  v <- pad.random.walk(start.v, ham.s, goal.v, Edges, minRW+1, maxRW+1)
  # Move to the cluster with c and capture c
  v <- c(v, val.sym[['h']], val.sym[['a']])
  bs <- sample(val.sym[['b']])
  v <- c(v, bs[c(1,2)], val.sym[['c']], bs[3], val.sym[['a']], val.sym[['h']])
  # Move through starting cluster, avoiding starting deep node
  v <- c(v, sample(val.sym[['g']][which(val.sym[['g']]!=ham.s)]), val.sym[['f']])
  v <- c(v, val.sym[['e']], sample(val.sym[['d']]), goal.v)
  return(v)
}

gen.exp <- function(nDh, nDm, nBm, minRW, maxRW, nTrs, nVisit, nPP, goalytics, idmap.d, idmap.bt, c.map, idmap.dg2, idmap.bg5, idmap.bg6, gsym5, gsym6, Edges){
  full.exp <- data.table(pp=0, miniblock=0, v=0, nSteps=0, goal=0)
  all.telomeres <- data.table(pp=0, s.type='init',g.type='init',clust.loc='init',v=0,goal=0,sym.id='init')
  for(ppn in 1:nPP){
    get.exp <- gen.train(nTrs, goalytics, idmap.d, idmap.dg2, idmap.bg5, idmap.bg6, gsym5, gsym6, Edges, nVisit, 1)
    get.exp[[1]][,pp:=ppn]
    get.exp[[2]][,pp:=ppn]
    full.exp <- rbind(full.exp, get.exp[[1]])
    all.telomeres <- rbind(all.telomeres, get.exp[[2]])
    # Tally of test trials
    test.tally <- c(nDh, nDm, nBm)
    while(sum(test.tally)>0){
      # Sample type of test trial
      cur.T <- which(test.tally>0)[sample(length(which(test.tally>0)),1)]
      # Check which node we have ended in
      all.n <- list('a'=1:5, 'b'=6:10, 'c'=11:15)
      # Check which symmetry it has
      cur.s <- names(all.n)[sapply(all.n, function(li){full.exp[.N,v] %in% li})]
      # Sample other cluster as goal cluster
      goal.s <- sample(names(all.n)[which(names(all.n)!=cur.s)],1)
      if(cur.T==1){
        # Sample goal
        goal.v <- sample(idmap.d[[goal.s]],1)
        # Sample full trajectory
        v <- gen.D.Ham(full.exp[.N,v], goal.v, minRW, maxRW, idmap.d, idmap.dg2, c.map, idmap.bt, Edges)
        # Append telomeres
        all.telomeres <- rbind(all.telomeres, data.table(pp=ppn, s.type='test', g.type='deep ham', clust.loc='test', v=v[1], goal=v[length(v)],sym.id='ham'))
      }else if(cur.T==2){
        # Sample goal
        goal.v <- sample(idmap.d[[goal.s]],1)
        # Sample full trajectory
        v <- gen.D.Min(full.exp[.N,v], goal.v, minRW, maxRW, idmap.d, idmap.dg2, c.map, idmap.bt, Edges)
        # Append telomeres
        all.telomeres <- rbind(all.telomeres, data.table(pp=ppn, s.type='test', g.type='deep min', clust.loc='test', v=v[1], goal=v[length(v)],sym.id='min'))
      }else if(cur.T==3){
        # Sample goal
        goal.v <- sample(idmap.bt[[goal.s]],1)
        # Sample full trajectory
        v <- gen.B.Min(full.exp[.N,v], goal.v, minRW, maxRW, idmap.d, idmap.bg5,idmap.bg6, gsym5, gsym6, c.map, idmap.bt, Edges)
        # Append telomeres
        all.telomeres <- rbind(all.telomeres, data.table(pp=ppn, s.type='test', g.type='btn min', clust.loc='test', v=v[1], goal=v[length(v)],sym.id='min'))
      }
      # Append actual trajectory
      full.exp <- rbind(full.exp, data.table(pp=ppn, miniblock=full.exp[.N,miniblock+1], v=v, nSteps=(length(v)-1):0, goal=v[length(v)]))
      test.tally[cur.T] <- test.tally[cur.T]-1
    }
  }
  return(list(full.exp[-1,], all.telomeres[-1,]))
}



