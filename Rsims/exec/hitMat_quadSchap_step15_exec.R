# Server executable for 15-step QuadSchap hitMat with backtracking
library(foreach)
library(parallel)
library(reshape2)
library(doParallel)

# Get different Defining Parameters
vGoal  <- 8    # Nr of goal (terminating, rewarding) node
nSteps <- 15   # Nr of steps within a miniblock (max)

#Quad Schapiro-style edge matrix
Edges <- list(c(2, 3, 4, 20),
              c(1, 3, 4, 5),
              c(1, 2, 4, 5),
              c(1, 2, 3, 5),
              c(2, 3, 4, 6),
              c(5, 7, 8, 9),
              c(6, 8, 9, 10),
              c(6, 7, 9, 10),
              c(6, 7, 8, 10),
              c(7, 8, 9, 11),
              c(10,12,13,14),
              c(11,13,14,15),
              c(11,12,14,15),
              c(11,12,13,15),
              c(12,13,14,16),
              c(15,17,18,19),
              c(16,18,19,20),
              c(16,17,19,20),
              c(16,17,18,20),
              c(17,18,19, 1))

# Describe unique nodes for QuadSchap
inspect.Vertices <- c(1, 2, 5, 6, 7, 16, 17)

# Set up parallel cluster
cl <- makeCluster(30)
registerDoParallel()

# Initialize hitMat
hitMat <- data.frame(vertex=rep(inspect.Vertices,each=nSteps), steps=rep(1:nSteps,length(inspect.Vertices)), goalprob=0)

for(v in inspect.Vertices){
  starttime <- Sys.time() #Get time of loop
  hitMatCount <- foreach(i=1:30, .combine=rbind) %dopar% {
    
    MCounter <- rep(0, nSteps+1)
    while(Sys.time() < (starttime + 2*60*60)){ #Run for set amount of time (hr*min*sec)
      
      path <- c(v)
      while(length(path) < (nSteps+1)){
        path <- c(path, sample(Edges[[tail(path,1)]],1))
        if(tail(path,1)==vGoal){
          MCounter[length(path)-1] <- MCounter[length(path)-1]+1
          break
        }else if(length(path)==(nSteps+1)){
          MCounter[length(MCounter)] <- MCounter[length(MCounter)]+1
        }
      }
    }
    MCounter
  }
  
  hitMat[hitMat$vertex==v,]$goalprob <- apply(hitMatCount, 2, sum)[1:nSteps]/sum(hitMatCount)
}
write.csv(hitMat, file='data/hitMat_quadSchap_step15.csv')









