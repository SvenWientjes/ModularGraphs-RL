# Function that calculates the hit matrix from a graph for the desired transitions
hitMat.calc <- function(Edges, vStart, vGoal, nSteps){
  
}

MC.hitmat <- function(Edges, vStart, vGoal, nSteps, nSamp){
  hitMat <- data.frame(goal='init', nSteps=0)
  for(r in 1:nSamp){
    path <- c(vStart)
    while(length(path) < (nSteps+1)){
      path <- c(path, sample(Edges[[tail(path,1)]], 1))
      if(tail(path,1)==vGoal){
        hitMat <- rbind(hitMat, data.frame(goal='yes', nSteps=(length(path)-1)))
        break
      }else if(length(path)==(nSteps+1)){
        hitMat <- rbind(hitMat, data.frame(goal='no', nSteps=(length(path)-1)))
      }
    }
  }
  return(hitMat)
}
