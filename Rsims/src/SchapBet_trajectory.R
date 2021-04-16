# Set up all eligeble start-goal combinations ----
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

## Set up counters and trackers and loopers etc. ----
telomeres <- data.table(start.v=0,goal.v=0,start.c='z',goal.c='z',s.type='init',g.type='init',clust.loc='init')

# Place constraints upon cluster-transitions for goals
ctransCount <- rbind(expand.grid('a', c('b','c')), expand.grid('b', c('a','c')), expand.grid('c', c('a','b')))
ctransCount <- as.data.table(ctransCount)
names(ctransCount) <- c('start.c','goal.c')

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


