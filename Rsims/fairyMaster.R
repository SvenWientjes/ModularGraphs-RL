library(reshape2)
library(ggplot2)

nV     <- 15
vStart <- 1
vGoal  <- 8
nVisit <- 10 #one more than the number of transitions (the first state counts, but is not transitioned into)

# Full edges matrix (schapiro-style random walk)
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

# Calculate nr of 'hits' in nSteps of complete random walk ----
paths   <- list()
queue   <- list(vStart)
pathIdx <- 1
hits <- 0

while(length(queue) > 0){
  pCur <- queue[[1]]
  if(length(pCur)==nVisit){
    paths[[pathIdx]] <- pCur
    pathIdx <- pathIdx+1
  }else{
    for(t in Edges[[tail(pCur,1)]]){
      if(t==vGoal){
        paths[[pathIdx]] <- c(pCur,t)
        pathIdx <- pathIdx+1
        hits <- hits+1
      }else{
        queue <- append(queue,list(c(pCur,t)))
      }
    }
  }
  queue[[1]] <- NULL
}
hits/length(paths)

#Do it with MC
testProp <- c()
for(i in 1:100){
  MC.hitdat <- MC.hitmat(Edges, vStart, vGoal, nSteps=10, nSamp=10000)
  testProp <- c(testProp, sum(MC.hitdat$goal=='yes')/nrow(MC.hitdat))
}
MC.hitdat <- MC.hitmat(Edges, vStart, vGoal, nSteps=15, nSamp=10000)
sum(MC.hitdat$goal=='yes')/nrow(MC.hitdat)

# Calculate nr of 'hits' in nSteps of complete random walk while not allowing direct back-track ----
paths   <- list()
queue   <- list(vStart)
prev    <- list(0)
pathIdx <- 1
hits <- 0

while(length(queue) > 0){
  pCur <- queue[[1]]
  if(length(pCur)==nVisit){
    paths[[pathIdx]] <- pCur
    pathIdx <- pathIdx+1
  }else{
    for(t in Edges[[tail(pCur,1)]]){
      if(t==vGoal){
        paths[[pathIdx]] <- c(pCur,t)
        pathIdx <- pathIdx+1
        hits <- hits+1
      }else if(t!=prev[[1]]){
        queue <- append(queue,list(c(pCur,t)))
        prev  <- append(prev, tail(pCur,1))
      }else{
        #this gets skipped
      }
    }
  }
  queue[[1]] <- NULL
  prev[[1]]  <- NULL
}
hits/length(paths)

## Systematically calculate nr of paths from node to goal for different lengths without allowing direct back-track ----
transProbs <- data.frame(V=rep(c(1,4,5,6,7,10,11,12,15),9),stepN=rep(2:10,each=9), propG=0, gPath=0, tPath=0)
for(startV in c(1,4,5,6,7,10,11,12,15)){
  for(stepN in 2:10){
    paths   <- list()
    queue   <- list(startV)
    prev    <- list(0)
    pathIdx <- 1
    hits <- 0
    
    while(length(queue) > 0){
      pCur <- queue[[1]]
      if(length(pCur)==stepN){
        paths[[pathIdx]] <- pCur
        pathIdx <- pathIdx+1
      }else{
        for(t in Edges[[tail(pCur,1)]]){
          if(t==vGoal){
            paths[[pathIdx]] <- c(pCur,t)
            pathIdx <- pathIdx+1
            hits <- hits+1
          }else if(t!=prev[[1]]){
            queue <- append(queue,list(c(pCur,t)))
            prev  <- append(prev, tail(pCur,1))
          }else{
            #this gets skipped
          }
        }
      }
      queue[[1]] <- NULL
      prev[[1]]  <- NULL
    }
    transProbs[transProbs$V==startV & transProbs$stepN==stepN,]$propG <- hits/length(paths)
    transProbs[transProbs$V==startV & transProbs$stepN==stepN,]$gPath <- hits
    transProbs[transProbs$V==startV & transProbs$stepN==stepN,]$tPath <- length(paths)
  }
}

library(ggplot)
transProbs$V <- as.factor(transProbs$V)
ggplot(transProbs, aes(x=stepN,y=propG,col=V)) +
  geom_line()

## Simulate experimental paths without allowing back-track ----
nExp       <- 10000
pGoal      <- rep(0,nExp)
winL       <- c()
for(ex in 1:nExp){
  nTrials    <- 500
  experiment <- list()
  for(t in 1:nTrials){
    trial <- c(vStart, sample(Edges[[vStart]],1))
    for(s in 1:(nVisit-2)){
      if(vGoal %in% trial){
        break
      }
      nextnode <- sample(Edges[[tail(trial,1)]],1)
      #while(nextnode == tail(trial,2)[1]){
      #  nextnode <- sample(Edges[[tail(trial,1)]],1)
      #}
      trial <- c(trial,nextnode)
    }
    experiment <- append(experiment, list(trial))
  }
  pGoal[ex] <- sum(sapply(experiment, function(x){vGoal %in% x}))/nTrials
  winL <- c(winL, sapply(experiment[sapply(experiment, function(x){vGoal %in% x})], length))
}
# Density plot of how often goal is reached
ggGoal <- data.frame(prop=pGoal)
ggplot(ggGoal, aes(x=prop))+
  geom_histogram(aes(y=..density..), color='gray30',fill='white') +
  geom_density() +
  ggtitle('Proportion of trials where goal is reached', subtitle='Over 10000 experiments of 500 trials with maximum 10 transitions')
# Bar plot of how many transitions it takes to reach the goal (goal trials only)
goalStep <- data.frame(table(winL))
ggplot(goalStep, aes(x=winL,y=Freq)) +
  geom_bar(stat='identity') +
  geom_line(aes(x=as.numeric(winL)),color='red')+
  geom_point(aes(x=as.numeric(winL)),color='red')
# Density plot (?) of how many trials it takes to reach the goal
winL.p <- sapply(1:length(table(winL)), function(x){table(winL)[x]/length(winL)})
goalStep <- data.frame(nstep=min(winL):max(winL), prop=winL.p)
ggplot(goalStep, aes(x=nstep,y=prop,label=prop)) +
  geom_bar(stat='identity') +
  geom_line(aes(x=as.numeric(nstep)),color='red')+
  geom_point(aes(x=as.numeric(nstep)),color='red')+
  geom_label(aes(label=round(prop,digits=5)))

## Calculate nr of paths in total, and division of goal-crossing paths ----
vStart  <- c(1,4,5,6,7)
nVisit  <- c(1,2,3,4,5,6,7,8,9,10,11)
hitMat  <- matrix(nrow=length(vStart), ncol=length(nVisit))
rownames(hitMat) <- vStart
colnames(hitMat) <- nVisit
pathMat <- matrix(nrow=length(vStart), ncol=length(nVisit))
rownames(pathMat) <- vStart
colnames(pathMat) <- nVisit
for(vS in vStart){
  for(nV in nVisit){
    paths   <- 0
    queue   <- list(vS)
    hits    <- 0
    
    while(length(queue) > 0){
      pCur <- queue[[1]]
      for(t in Edges[[tail(pCur,1)]]){
        #if(t!= tail(pCur,2)[1] & length(c(pCur,t))<nV){         #No-backtrack
        if(length(c(pCur,t))<nV){                              #Backtrack
          queue <- append(queue,list(c(pCur,t)))
        #}else if(t!= tail(pCur,2)[1] & length(c(pCur,t))==nV){  #No-backtrack
        }else if(length(c(pCur,t))==nV){                       #Backtrack
          paths <- paths+1
          if(vGoal %in% c(pCur,t)){
            hits <- hits+1
          }
        }
      }
      queue[[1]] <- NULL
    }
    hitMat[as.character(vS),as.character(nV)] <- hits
    pathMat[as.character(vS),as.character(nV)] <- paths
  }
  write.csv(hitMat, file='full-T_step-10_hit.csv', row.names=T, col.names=T)
  write.csv(pathMat, file='full-T_step-10_path.csv', row.names=T, col.names=T)
}
read.csv('full-T_step-10_hit.csv', row.names=1)

## Calculate nr of paths with no backtrack conditional of previous node ----
pointMat <- rbind(c(1,2),
                  c(1,4),
                  c(1,5),
                  c(4,1),
                  c(4,6),
                  c(5,1),
                  c(5,15),
                  c(6,4),
                  c(6,7),
                  c(7,6),
                  c(7,9))
nVisit  <- c(1,2,3,4,5,6,7,8,9,10)
hitMat  <- matrix(nrow=nrow(pointMat), ncol=length(nVisit))
rownames(hitMat) <- sapply(1:nrow(pointMat), function(i){paste(pointMat[i,1],'<-',pointMat[i,2],sep='')})
colnames(hitMat) <- nVisit
pathMat <- matrix(nrow=nrow(pointMat), ncol=length(nVisit))
rownames(pathMat) <- sapply(1:nrow(pointMat), function(i){paste(pointMat[i,1],'<-',pointMat[i,2],sep='')})
colnames(pathMat) <- nVisit
for(vS in 1:nrow(pointMat)){
  for(nV in nVisit){
    paths   <- 0
    queue   <- list(c(pointMat[vS,2],pointMat[vS,1]))
    hits    <- 0
    
    while(length(queue) > 0){
      pCur <- queue[[1]]
      for(t in Edges[[tail(pCur,1)]]){
        if(t!= tail(pCur,2)[1] & length(c(pCur,t))<(nV+1)){         #No-backtrack
        #if(length(c(pCur,t))<nV){                              #Backtrack
          queue <- append(queue,list(c(pCur,t)))
        }else if(t!= tail(pCur,2)[1] & length(c(pCur,t))==(nV+1)){  #No-backtrack
        #}else if(length(c(pCur,t))==nV){                       #Backtrack
          paths <- paths+1
          if(vGoal %in% c(pCur,t)){
            hits <- hits+1
          }
        }
      }
      queue[[1]] <- NULL
    }
    hitMat[vS,as.character(nV)] <- hits
    pathMat[vS,as.character(nV)] <- paths
  }
  write.csv(hitMat, file='no-BT_step-9_hit.csv', row.names=T, col.names=T)
  write.csv(pathMat, file='no-BT_step-9_path.csv', row.names=T, col.names=T)
}
## Calculate unique paths ending in goal of different lengths ----
pointMat <- rbind(c(1,2),
                  c(1,4),
                  c(1,5),
                  c(4,1),
                  c(4,6),
                  c(5,1),
                  c(5,15),
                  c(6,4),
                  c(6,7),
                  c(7,6),
                  c(7,9))
nVisit  <- c(2,3,4,5,6,7,8,9,10)
hitMat  <- matrix(nrow=nrow(pointMat), ncol=length(nVisit))
rownames(hitMat) <- sapply(1:nrow(pointMat), function(i){paste(pointMat[i,1],'<-',pointMat[i,2],sep='')})
colnames(hitMat) <- nVisit-1
pathMat <- matrix(nrow=nrow(pointMat), ncol=length(nVisit))
rownames(pathMat) <- sapply(1:nrow(pointMat), function(i){paste(pointMat[i,1],'<-',pointMat[i,2],sep='')})
colnames(pathMat) <- nVisit-1
for(vS in 1:nrow(pointMat)){
  for(nV in nVisit){
    paths   <- 0
    queue   <- list(c(pointMat[vS,2],pointMat[vS,1]))
    hits    <- 0
    
    while(length(queue) > 0){
      pCur <- queue[[1]]
      for(t in Edges[[tail(pCur,1)]]){
        if(t!= tail(pCur,2)[1] & length(c(pCur,t))<(nV+1)){         #No-backtrack
          #if(length(c(pCur,t))<nV){                              #Backtrack
          queue <- append(queue,list(c(pCur,t)))
        }else if(t!= tail(pCur,2)[1] & length(c(pCur,t))==(nV+1)){  #No-backtrack
          #}else if(length(c(pCur,t))==nV){                       #Backtrack
          paths <- paths+1
          if(vGoal == t & !vGoal %in% pCur){
            hits <- hits+1
          }
        }
      }
      queue[[1]] <- NULL
    }
    hitMat[vS,as.character(nV-1)] <- hits
    pathMat[vS,as.character(nV-1)] <- paths
  }
  write.csv(hitMat, file='no-BT_step-9_hit_unique.csv', row.names=T, col.names=T)
}
## Plot graphs of different probabilities of goal-reaches ----
hitMat.FT   <- read.csv('full-T_step-10_hit.csv',row.names=1,check.names=F)
pathMat.FT  <- read.csv('full-T_step-10_path.csv',row.names=1,check.names=F)
hitMat.nBT  <- read.csv('no-BT_step-9_hit.csv',row.names=1,check.names=F)
pathMat.nBT <- read.csv('no-BT_step-9_path.csv',row.names=1,check.names=F)

# Coerce into proper plottable data frame
nBT.data <- merge(melt(data.frame(trans = row.names(hitMat.nBT), hitMat.nBT, check.names=F),id='trans',variable.name='steps',value.name='hits'), 
                  melt(data.frame(trans = row.names(pathMat.nBT), pathMat.nBT, check.names=F),id='trans',variable.name='steps',value.name='paths'))
nBT.data <- merge(nBT.data, melt(data.frame(trans = row.names(pathMat.nBT), (hitMat.nBT/pathMat.nBT), check.names=F),id='trans',variable.name='steps',value.name='prop'))
nBT.data[is.na.data.frame(nBT.data)] <- 0

# Order according to most probable - least probable
nBT.data$trans <- factor(nBT.data$trans, levels=nBT.data[nBT.data$steps==10,][order(-nBT.data[nBT.data$steps==10,]$prop),]$trans, ordered=T)

# Create plot
ggplot(nBT.data, aes(x=steps,y=prop,col=trans)) +
  geom_line(aes(group=trans))

# Coerce into proper plottable data frame
FT.data <- merge(melt(data.frame(trans = row.names(hitMat.FT), hitMat.FT, check.names=F),id='trans',variable.name='steps',value.name='hits'), 
                  melt(data.frame(trans = row.names(pathMat.FT), pathMat.FT, check.names=F),id='trans',variable.name='steps',value.name='paths'))
FT.data <- merge(FT.data, melt(data.frame(trans = row.names(pathMat.FT), (hitMat.FT/pathMat.FT), check.names=F),id='trans',variable.name='steps',value.name='prop'))
FT.data[is.na.data.frame(FT.data)] <- 0

# Order according to most probable - least probable
FT.data$trans <- factor(FT.data$trans, levels=FT.data[FT.data$steps==10,][order(-FT.data[FT.data$steps==10,]$prop),]$trans, ordered=T)

# Create plot
ggplot(FT.data, aes(x=steps,y=prop,col=trans)) +
  geom_line(aes(group=trans))

# Get unique path length steps to goal
propMat   <- t(diff(t(as.matrix(hitMat.nBT))))/max(pathMat.nBT)
propStart <- rep(0,length(1:11))
for(nV in 1:11){
  paths   <- 0
  queue   <- list(c(0,1))

  while(length(queue) > 0){
    pCur <- queue[[1]]
    for(t in Edges[[tail(pCur,1)]]){
      if(t!= tail(pCur,2)[1] & length(c(pCur,t))<(nV+1)){         #No-backtrack
        #if(length(c(pCur,t))<nV){                              #Backtrack
        queue <- append(queue,list(c(pCur,t)))
      }else if(t!= tail(pCur,2)[1] & length(c(pCur,t))==(nV+1)){  #No-backtrack
      #}else if(length(c(pCur,t))==nV){                       #Backtrack
        paths <- paths+1
        if(vGoal %in% c(pCur,t)){
          propStart[nV] <- propStart[nV]+1
        }
      }
    }
    queue[[1]] <- NULL
  }
}
diff(propStart)/paths

# Create stepcosts and goal rewards
gRew  <- 20
sCost <- 1
costVec <- sCost*c(1,2,3,4,5,6,7,8,9)

t(apply(t(t(propMat) * (gRew-costVec)), 1, cumsum)) - t(t(1-(hitMat.nBT/pathMat.nBT)[2:10]) * costVec)
t(t(1-(hitMat.nBT/pathMat.nBT)[2:10]) * costVec)

# Define Edges and computations for Extended Graph 1 ----
vStart <- 1
vGoal  <- 9
# Get 












