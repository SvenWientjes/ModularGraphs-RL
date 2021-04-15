# Set up all eligeble start-goal combinations
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

# Decide which 10 nodes can get an oversample randomly (?)
startCount <- rep(6, 15); goalCount <- rep(6, 15)
# 4 extra bottleneck starts
# 6 extra deep starts
# 4 extra bottleneck goals
# 6 extra deep goals           => how to combine?
# Sample all 6 bottlenecks in random order
bon.bt <- sample(unlist(idmap.bt))
# Sample 2 additional ones, not the last 2 of the previous (so we never give a bonus to the same node twice)
bon.bt <- c(bon.bt, sample(unlist(idmap.bt)[-which(unlist(idmap.bt) %in% bon.bt[5:6])], 2))
# Increment counters by bonus (4-4)
startCount[bon.bt[1:4]] <- startCount[bon.bt[1:4]] + 1
goalCount[bon.bt[5:8]] <- goalCount[bon.bt[5:8]] + 1
# Sample all 9 deep nodes in random order
bon.d <- sample(unlist(idmap.d))
# Sample 3 additional nodes, not overlapping with the last 3 nodes (no bonus to the same node twice)
bon.d <- c(bon.d, sample(unlist(idmap.d)[-which(unlist(idmap.d) %in% bon.d[7:9])],3))
# Increment counters by bonus (6-6)
startCount[bon.d[1:6]] <- startCount[bon.d[1:6]] + 1
goalCount[bon.d[7:12]] <- goalCount[bon.d[7:12]] + 1

# Place constraints upon cluster-transitions for goals
ctransCount <- rbind(expand.grid('a', c('b','c')), expand.grid('b', c('a','c')), expand.grid('c', c('a','b')))
ctransCount <- as.data.table(ctransCount)
names(ctransCount) <- c('start.c','goal.c')

# Draw all the start-goal combo's? (Maybe draw during simulation - check for transition availability - at least for lose trials?)
telomeres <- data.table(start.v=0,goal.v=0,start.c='z',goal.c='z',s.type='init',g.type='init',clust.loc='init')
for(trtp in 1:nrow(trTypes)){
   if(trtp %in% c(1,2,7,8)){ # Bottleneck - Deep combinations (either start-goal or goal-start)
      ctransCount[,count:=2]
   }else if(trtp %in% c(3,4,5,6)){
      ctransCount[,count:=1]
   }else{
      ctransCount[,count:=10]
   }
   for(tridx in 1:trTypes[trtp,nTrs]){
      st <- trTypes[trtp,s.type] # Select starting type
      gl <- trTypes[trtp,g.type] # Select goal type
      cloc <- trTypes[trtp,clust.loc] # Select cluster location (far/close; bottleneck start only!)
      
      clust.idx <- ElGoals[start.v %in% which(startCount!=0) & goal.v %in% which(goalCount!=0) & s.type==st & g.type==gl][,identical(clust.loc,cloc),by=.(start.v,goal.v)]$V1
      ElCombs <- ElGoals[start.v %in% which(startCount!=0) & goal.v %in% which(goalCount!=0) & s.type==st & g.type==gl][clust.idx,]
      ElCombs[,count:=1][ctransCount[count==0], count:=0, on=.(start.c,goal.c)]
      # Select among available cluster ID
      DrawnEnds <- ElCombs[count==1][sample(1:.N,1),]
      # Decrease cluster transition counter
      ctransCount[start.c==DrawnEnds[,start.c] & goal.c==DrawnEnds[,goal.c],count:=count-1]
      
      #ElGoals <- ElGoals[-(which(duplicated(rbind(DrawnEnds[,1:7], ElGoals)))-1),]
      #print(nrow(ElGoals))
      startCount[DrawnEnds[,start.v]] <- startCount[DrawnEnds[,start.v]]-1
      goalCount[DrawnEnds[,goal.v]] <- goalCount[DrawnEnds[,goal.v]]-1
      telomeres <- rbind(telomeres, DrawnEnds[,1:7])
   }
}



telomeres[,list(list(table(goal.v))), by=.(s.type,g.type,clust.loc)][6,V1]

telomeres[s.type=='bottleneck', table(goal.v)]


