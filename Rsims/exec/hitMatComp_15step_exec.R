###############################################################################################################
################ Parallel hitmat computations for different starting vertices for 15 max steps ################
###############################################################################################################
# Get Requirements
library(foreach)
library(doParallel)
source('src/hitMat_calc.R')

# Set Up Parameters
Edges <- list(c(2, 3, 4, 5),
              c(1, 3, 4, 5),
              c(1, 2, 4, 5),
              c(1, 2, 3, 6),
              c(1, 2, 3, 15),
              c(4, 7, 8, 9),
              c(6, 8, 9, 10),
              c(6, 7, 9, 10),
              c(6, 7, 8, 10),
              c(11,7, 8, 9),
              c(10,12,13,14),
              c(11,13,14,15),
              c(11,12,14,15),
              c(11,12,13,15),
              c(5, 12,13,14))
inspectVertices <- c(1,4,5,6,7)
vGoal <- 8

# Set up parallel cluster
cl <- makeCluster(5)
registerDoParallel(cl)

hitMat.15 <- foreach(v = inspectVertices, .combine=rbind) %dopar% {
  hitMat <- hitMat.calc(Edges, vGoal, 15, v, 4^15)
  
  hitMat
}
