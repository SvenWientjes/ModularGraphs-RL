# Server executable for 15-step blocked Schapiro hitMat with backtracking
library(foreach)
library(parallel)
library(reshape2)
library(doParallel)

# Load Functions from /src/
sapply(paste0('src/',list.files('src/')), source)

# Get different Defining Parameters
vStart <- 1    # Nr of starting node
vGoal  <- 8    # Nr of goal (terminating, rewarding) node
nSteps <- 15   # Nr of steps within a miniblock (max)

#Full Schapiro-style edge matrix
Edges <- list(c(2, 3, 4, 5), 
              c(1, 3, 4, 5),
              c(1, 2, 4, 5),
              c(1, 2, 3, 6),
              c(1, 2, 3, 15),
              c(4, 7, 8, 9),
              c(6, 8, 9, 10),
              c(6, 7, 9, 10),
              c(6, 7, 8, 10),
              c(7, 8, 9),
              c(12,13,14),
              c(11,13,14,15),
              c(11,12,14,15),
              c(11,12,13,15),
              c(5, 12,13,14))

inspect.Vertices <- c(1,4,5,6,7,10,11,12,15)

# Set up parallel cluster
cl <- makeCluster(5)
registerDoParallel()

hitCountList <- list()
for(v in inspect.Vertices){
  starttime <- Sys.time() #Get time of loop
  hitMatCount <- foreach(i=1:5, .combine=rbind) %dopar% {
      
    MCounter <- rep(0, nSteps+1)
    while(Sys.time() < (starttime + 1*1*60)){ #Run for set amount of time (hr*min*sec)
      
      path <- c(vStart)
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
}

write.csv(hitMatCount, file='data/hitMat_blockedSchap_step15.csv')












