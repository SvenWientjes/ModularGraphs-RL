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

# Decide which nodes can get an oversample randomly (?)

# Draw all the start-goal combo's? (Maybe draw during simulation - check for transition availability - at least for lose trials?)
