## Function that can generate an experiment according to several criteria:
#   - Roughly equal visitation of each edge in each direction
#   - Roughly equal use of each eligible node as start and goal
#   - Roughly equal nr of goal visits for every participant conditioned on strategy (?)
#   - OS should be better than MS (?)
Exp.gen.dirichlet <- function(Edges, e.nodes, nSteps, nTr, c.map, idmap.g, parNum=1){
  library(gtools)
  experiment <- data.frame(pp = 0, tr = 0, step = 0, v = 0, goal=0)
  # List all edges in both directions & count traversals
  Edge.list <- foreach(i = 1:length(Edges), .combine=rbind) %do% {expand.grid(i, Edges[[i]])}
  Edge.eval <- rep(nTr*nSteps/nrow(Edge.list), nrow(Edge.list))
  
  # Track nr of eligible nodes used  as start / goal
  start.track <- rep(nTr/length(e.nodes), length(e.nodes))
  goal.track  <- rep(nTr/length(e.nodes), length(e.nodes))
  
  for(tr in 1:nTr){
    # Sample a start node
    path <- sample(x=e.nodes, size=1, prob=rdirichlet(1, sapply(start.track*10, function(i){max(1,i)})))
    # Subtract sampled node from  future start probabilities
    start.track[which(e.nodes==path)] <- start.track[which(e.nodes==path)] - 1
    
    # Sample an eligible goal node
    goal.c <- sample(c.map[sapply(idmap.g, function(m){path %in% m})][[1]],1)
    goal   <- sample(idmap.g[[goal.c]], size=1, prob=rdirichlet(1, sapply(start.track[which(e.nodes %in% idmap.g[[goal.c]])]*10, function(i){max(1,i)})))
    # Subtract sampled goal from future goal probabilities
    goal.track[which(e.nodes==goal)] <- goal.track[which(e.nodes==goal)] - 1
    
    # Generate a path
    while(length(path) < (nSteps+1) & tail(path,1) != goal){
      path <- c(path, sample(Edges[[tail(path,1)]],size=1, prob=rdirichlet(1, sapply(Edge.eval[which(Edge.list[,1]==tail(path,1))]*10, function(i){max(1,i)}))))
      Edge.eval[which(Edge.list[,1]==tail(path,2)[1] & Edge.list[,2]==tail(path,1))] <- Edge.eval[which(Edge.list[,1]==tail(path,2)[1] & Edge.list[,2]==tail(path,1))] - 1
    }
    experiment <- rbind(experiment, data.frame(pp=parNum, tr=tr, step=0:(length(path)-1), v=path, goal=goal))
  }
  return(experiment[-1,])
}
