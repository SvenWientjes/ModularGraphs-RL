############################################################################################################################
########################## Script for playing around with gamble choices in the QuadSchap ##################################
############################################################################################################################
library(foreach)
library(ggplot2)
library(reshape2)
library(data.table)
library(zoo)

# Load Functions from /src/
sapply(paste0('src/',list.files('src/')), source)

# Define point multiplier for winning
winM   <- 5
startP <- 5
nVisit <- 15
nTr    <- 100

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

# Get vertex / edge mappings w.r.t. different goals
idmap.d  <- list(a = c(1,15), b = c(2,3,4, 12,13,14), c = c(5,11), d = c(6,10), e = c(7,9), f = c(16,20), g = c(17,18,19))
idmap.bg <- list(a = c(1), b = c(2,3,4), c = c(5), d = c(7,8,9), e = c(10), f = c(11), g = c(12,13,14), h = c(15), i = c(16), j = c(17,18,19), k = c(20))
idmap.g  <- list(a = c(2,3,4), b = c(7,8,9), c = c(12,13,14), d = c(17,18,19))
c.map    <- list(a = c('d','b'), b = c('a', 'c'), c = c('b','d'), d=c('a','c'))
bt.map   <- list(a = c(1,5), b = c(6,10), c= c(11,15), d=c(20,16))


# Get hitMats
hitMat.d <- as.data.table(read.csv('data/hitMat_quadSchap_step15.csv', row.names=1))
hitMat.b <- as.data.table(read.csv('data/hitMat_quadSchap_step15_btgl.csv', row.names=1))

# Calculate EV for different nodes
EVmat.d <- foreach(v = unique(hitMat.d$vertex), .combine=rbind) %do% {
  tempMat <- EVbet.calc(Edges=Edges, nSteps=nVisit-1, winM=winM, hitMat=hitMat.d, Vertex=v) 
  tempMat
}
EVmat.b <- foreach(v = unique(hitMat.b$vertex), .combine=rbind) %do% {
  tempMat <- EVbet.calc(Edges=Edges, nSteps=nVisit-1, winM=winM, hitMat=hitMat.b, Vertex=v) 
  tempMat
}

# Get policies for the deep & bottleneck node EVmats
stopIdx.d <- EVmat.d[, EV<0,by=.(vertex,steps)
                     ][,if(TRUE %in% V1){
                          .SD[V1==T,max(steps)]
                       }else{
                          as.integer(0)
                        }, 
                       by=vertex
                       ]
stopIdx.b <- EVmat.b[, EV<0,by=.(vertex,steps)
                     ][,if(TRUE %in% V1){
                       .SD[V1==T,max(steps)]
                     }else{
                       as.integer(0)
                     }, 
                     by=vertex
                     ]

## Get simulated trajectories ----
# Initialize experimental experience
full.exp <- Bet.gen(Edges, nVisit, nTr, c.map, idmap.g, bt.map)

# Define random strategy
full.exp[v!=goal, rand.choice:=sample(c(-1,1),1), by=.(tr,stepsleft)         #Sample random choice increments
         ][,rand.trBet := na.locf(cumsum(rand.choice))+startP,by=tr          #Add up to get bets, with 5 starting points
           ][rand.trBet<0, rand.trBet:=0                                     #Cannot bet below zero
             ][stepsleft == 0, rand.trRew := -rand.trBet, by=tr              #Determine losing bets
               ][v==goal, rand.trRew := winM*rand.trBet, by=tr               #Determine winning bets
                 ][!is.na(rand.trRew),rand.totRew := cumsum(rand.trRew)]     #Add up all losses and wins

# Define modular strategy







