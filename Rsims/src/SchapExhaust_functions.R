gen.ham.schap <- function(){
  # path of sequence -> will be started at random points and sometimes reversed
  ham.seq <- c(1, sample(2:4), 5, 6, sample(7:9), 10, 11, sample(12:14), 15)
  return(ham.seq)
}

MC.get.trajectory <- function(start.v, goal.v, t.reach, Edges){
  v <- rep(0, t.reach)
  while(v[t.reach]!=goal.v | goal.v %in% v[-length(v)]){
    v <- rep(0, t.reach)
    v[1] <- start.v
    for(s in 2:t.reach){
      v[s] <- sample(Edges[[v[s-1]]],1)
    }
  }
  return(v)
}

MC.full.exp <- function(nTrs, idmap.dg2, idmap.bg5, idmap.bg6, gsym5, gsym6, idmap.d, Edges, Analytics, goalytics, method='AcceptReject'){
  nSelect <- rep(nTrs/15, 15)
  runSelector <- Analytics[1,]     #Tracks nr of steps (t.reach) to select during run
  if(method=='ML'){
    for(r in 1:nrow(goalytics)){
      st <- goalytics[r,s.type]; gl <- goalytics[r,g.type]; cl <- goalytics[r,clust.loc]; ns <- goalytics[r,numselect]
      runSelector <- rbind(runSelector, Analytics[s.type==st & g.type==gl & (clust.loc==cl|is.na(clust.loc)),][order(-Eselect)][1:ns,])
    }
  }else if(method=='AcceptReject'){
    ## Runselector based on accept/reject of reachp
    for(r in 1:nrow(goalytics)){
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
  }
  runSelector <- runSelector[-1,]
  ## Sampler for the experiment
  # Tracks particular camp sites (first start + all goals)
  camps <- data.table(v = 0, s.type='start',g.type='start', t.reach=0)
  # Initialize by selecting first node
  camps$v <- sample(1:15, 1)
  scamp <- camps$v
  deepbridge=0; botbridge=0
  
  # Get all the campsites in a chain (not balanced?)
  for(cmp in 2:(nTrs+1)){
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
  return(full.exp)
}

random.walk <- function(Edges, nVisit){
  walk <- data.table(v=rep(0,nVisit))
  walk$v[1] <- sample(1:15,1)
  for(i in 2:nVisit){
    walk$v[i] <- sample(Edges[[walk$v[(i-1)]]],1)
  }
  walk[v%in%c(1:5),c.id:='a']
  walk[v%in%c(6:10),c.id:='b']
  walk[v%in%c(11:15),c.id:='c']
  walk[,dwell:=rowid(rleid(c.id))]
  return(walk)
}

get.symmetry <- function(v, g, idmap.dg2b, idmap.bg5, idmap.bg6, idmap.d, gsym5, gsym6){
  # Initialize vector of symmetries
  sym.v <- rep('z', length(v))
  # Correct for 0-indexing in JS
  v <- v+1
  g <- g+1
  # Determine valid symmtry
  if(g %in% unlist(idmap.d)){ # Deep goal
    val.sym <- idmap.dg2b
  }else if(g %in% gsym5){ # Rotational symmetry w.r.t. 5 start
    val.sym <- idmap.bg5
  }else if(g %in% gsym6){ # Rotational symmetry w.r.t. 6 start
    val.sym <- idmap.bg6
  }
  # Determine rotation
  rotval <- max(which(sapply(idmap.d, function(li){g %in% li}))-1, which(gsym6==g)-1, which(gsym5==g)-1)*5
  # Determine node symmetry
  for(i in 1:length(v)){
    cand <- names(val.sym)[sapply(val.sym, function(vs){v[i] %in% (((vs - 1 + rotval) %% 15)+1)})]
    sym.v[i] <- names(val.sym)[sapply(val.sym, function(vs){v[i] %in% (((vs - 1 + rotval) %% 15)+1)})]
  }
  return(sym.v)
}
