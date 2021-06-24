hamcyc.run <- function(lastv){
  # Get the cluster we must place the goal in
  if(lastv %in% unlist(idmap.bt)){
    # Get current symmetry from lastv
    cur.sym <- names(idmap.bt)[sapply(idmap.bt, function(li){lastv %in% li})]
    # Get next symmetry as close cluster from lastv
    goal.sym <- c.map[[cur.sym]][which(idmap.bt[[cur.sym]]==lastv)]
    # Begin trajectory by moving into goal.sym
    v <- c(lastv, idmap.bt[[goal.sym]][which(c.map[[goal.sym]]==cur.sym)])
  }else{
    # Get current symmetry from lastv
    cur.sym <- names(idmap.d)[sapply(idmap.d, function(li){lastv %in% li})]
    # Get next symmetry as random sample of two other clusters
    goal.sym <- sample(c('a','b','c')[which(c('a','b','c')!=cur.sym)] , 1)
    # Begin trajectory by moving into goal.sym
    v <- c(lastv, idmap.bt[[cur.sym]][which(c.map[[cur.sym]]==goal.sym)],
           idmap.bt[[goal.sym]][which(c.map[[goal.sym]]==cur.sym)])
  }
  # Return into previous cluster
  v <- c(v, idmap.bt[[cur.sym]][which(c.map[[cur.sym]]==goal.sym)])
  # Sample all nodes of first hamiltonian cluster
  v <- c(v, sample(idmap.d[[cur.sym]]))
  # Move to second cluster
  lat.sym <- c('a','b','c')[which(!c('a','b','c') %in% c(cur.sym,goal.sym))]
  v <- c(v, idmap.bt[[cur.sym]][which(c.map[[cur.sym]]==lat.sym)],
         idmap.bt[[lat.sym]][which(c.map[[lat.sym]]==cur.sym)])
  # Sample all nodes of second hamiltonian cluster
  v <- c(v, sample(idmap.d[[lat.sym]]))
  # Move to goal cluster
  v <- c(v, idmap.bt[[lat.sym]][which(c.map[[lat.sym]]==goal.sym)],
         idmap.bt[[goal.sym]][which(c.map[[goal.sym]]==lat.sym)])
  # Sample all deep nodes of goal cluster - last one is goal!
  v <- c(v, sample(idmap.d[[goal.sym]]))
  return(v)
}

randBTN.run <- function(lastv, idmap.dg2, btypeTally, Edges, nVisit){
  rotval <- (which(sapply(idmap.d, function(li){lastv %in% li}))-1) * 5
  val.type <- c('a','c')[btypeTally > 0]
  val.nodes <- c()
  for(i in val.type){
    val.nodes <- c(val.nodes, ((idmap.dg2[[i]]-1+rotval)%%15)+1)
  }
  b.goal <- val.nodes[sample(length(val.nodes),1)]

  # subtract respective btypeTally as well
  b.type <- names(idmap.dg2)[sapply(idmap.dg2, function(vs){b.goal %in% (((vs-1+rotval)%%15)+1)})]
  if(b.type=='a'){
    btypeTally[1] <- btypeTally[1]-1
  }else if(b.type=='c'){
    btypeTally[2] <- btypeTally[2]-1
  }
  
  # Get v towards b.goal
  v <- trajSamp.max(lastv, b.goal, Edges, nVisit)
  return(list(v,btypeTally,b.type))
}
