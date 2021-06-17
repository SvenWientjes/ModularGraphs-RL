gen.ham.schap <- function(){
  # path of sequence -> will be started at random points and sometimes reversed
  ham.seq <- c(1, sample(2:4), 5, 6, sample(7:9), 10, 11, sample(12:14), 15)
  return(ham.seq)
}

MC.get.trajectory <- function(start.v, goal.v, t.reach, Edges){
  v <- rep(0, t.reach)
  while(v[t.reach]!=goal.v){
    v <- rep(0, t.reach)
    v[1] <- start.v
    for(s in 2:t.reach){
      v[s] <- sample(Edges[[v[s-1]]],1)
    }
  }
  return(v)
}
