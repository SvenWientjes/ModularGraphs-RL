#####################################################################################
####### Get data from first logistical pilot and plot a little bit the choices ######
#####################################################################################
# Load from SQL (laptop local)
library(RMySQL)
dbConnection <- dbConnect(MySQL(), host="mysql.ugent.be", user="s240338", password="PCGbU2wp@-wEGt6E", dbname="lccl")
data <- dbReadTable(dbConnection,"`ArtDealer_PilotLogistic01_data`")

# Load from data file (server)
load('dat/PilotLog01_bigdat')

#################################################################################
## Wrangle database retrieved data into neat data.table
library(data.table)
library(ggplot2)
# Put database into table
PilotLog01 <- as.data.table(data)
save(PilotLog01, file='dat/PilotLog01_bigdat')

# Get randID as factor
PilotLog01$randID <- as.factor(PilotLog01$randID)

# Check which randID has seen the 2 questions and the finishing screen of the experiment (3 indicates o.k.)
fullPPnrs <- PilotLog01[startsWith(stimulus, '<div id="Finish"'), .N, by=randID][,randID]
PilotLog01 <- PilotLog01[randID %in% fullPPnrs,]
levels(PilotLog01$randID) <- c(1:length(levels(PilotLog01$randID)))

#################################################################################
#### Analyze free time screen inspection times ####
# Get goal time data
goaltimedat <- PilotLog01[startsWith(stimulus, 'img/') & endsWith(stimulus, '_goal.png'), .(randID, miniblock, rt)]
goaltimedat$rt <- as.numeric(goaltimedat$rt) # To numeric
names(goaltimedat)[which(names(goaltimedat)=='rt')] <- 'goalrt'
goaltimedat$miniblock <- rep(1:100, 2)
# Get mbend time data
tempRT <- PilotLog01[startsWith(stimulus, '<div id="mbendDiv"') 
                 & !shift(miniblock, 1, type='lag') %in% c('win','lose') 
                 & !shift(miniblock, 2, type='lag') %in% c('win','lose')
                 & (shift(miniblock, 1, type='lag') >= 0
                    | shift(miniblock, 2, type='lag') >= 0),
                list(miniblock=1:100,mbendrt=as.numeric(rt)), by=randID]$mbendrt
goaltimedat$mbendrt <- tempRT
# Get feedback screen time data
tempRT <- PilotLog01[startsWith(stimulus, '<div id="FeedbackDiv"')
                & !shift(miniblock, 2, type='lag') %in% c('win','lose') 
                & !shift(miniblock, 3, type='lag') %in% c('win','lose')
                & (shift(miniblock, 2, type='lag') >= 0
                  | shift(miniblock, 3, type='lag') >= 0),
                list(miniblock=1:100, feedbrt=as.numeric(rt)), by=randID]$feedbrt
goaltimedat$feedbrt <- tempRT
# Align free waiting time complex (feedback of previous -> current goal)
goaltimedat <- goaltimedat[,list(miniblock, goalrt,
                                 mbendrt=shift(mbendrt,1,type='lag'),
                                 feedbrt=shift(feedbrt,1,type='lag')),by=randID]
goaltimedat[is.na(mbendrt),mbendrt:=0]
goaltimedat[is.na(feedbrt),feedbrt:=0]
# Sum waiting time complex
goaltimedat[,totwait:=sum(goalrt,mbendrt,feedbrt),by=.(randID,miniblock)]

# Inspect ordered goal RTs for each participant
cbind(goaltimedat[randID==3, ][order(goalrt),goalrt], goaltimedat[randID==2, ][order(goalrt),goalrt])

# Distribution of 'normal' inspection times
ggplot(goaltimedat[goalrt<7000,], aes(x=goalrt, col=randID))+
  geom_density()

# Line plot of evolution of separate free waiting times throughout experiment progression
ggplot(goaltimedat)+
  geom_line(aes(x=miniblock, y=goalrt),col='blue') +
  geom_line(aes(x=miniblock, y=mbendrt),col='green') +
  geom_line(aes(x=miniblock, y=feedbrt),col='red') +
  facet_grid(.~randID)

# Line plot of evolution of combined free waiting times throughout experiment progression
ggplot(goaltimedat, aes(x=miniblock, y=totwait/1000, col=randID))+
  geom_line()

#################################################################################
#### Get main experiment data ####
expdat <- PilotLog01[randID %in% c(fullPPnrs) & !is.na(node) & !is.na(decision) & miniblock>=0 & !miniblock %in% c('win','lose'),
                    .(randID, miniblock, nSteps, node, goalnode, decision, choiceVar, rt, trBet, totRew)]
expdat$rt <- as.integer(expdat$rt)
expdat$nSteps <- as.numeric(expdat$nSteps)
expdat$miniblock <- as.integer(expdat$miniblock)
expdat$node <- as.integer(expdat$node)
expdat$goalnode <- as.integer(expdat$goalnode)

# Identify nodes as deep or bottleneck
expdat[,nodeType := if(node%in%c(0,4,5,9,10,14)){'btn'}else{'deep'},by=.(randID,miniblock,nSteps)]
# Define starting transitions
expdat[nSteps==0, transType:='start']
# Define bottleneck transitions between clusters
expdat[nSteps!=0 & nodeType=='btn' & shift(nodeType, n=1, type='lag')=='btn', transType:='between']
# Define all transitions that stay within one cluster
expdat[is.na(transType), transType:='within']

# Get normative choices (using older 1-indexed function)
expdat[,opt.choice:=get.opt.choice(miniblock+1, node+1, goalnode+1, (15-nSteps-1), tMat, 1, -1), by=.(randID,miniblock)]
expdat[opt.choice==-1, opt.choice:=0]
# Evaluate current choice as optimal or not
expdat[,was.choice.opt:=choiceVar==opt.choice, by=.(randID, miniblock, nSteps)]
# Bin into groups of 10 miniblocks
expdat[,opt.block.div:=ceiling((miniblock+1)/4),by=.(randID,miniblock,nSteps)]

#################################################################################
#### Analyze display room stimuli reaction times ####
# Difference between RT dist of down and up, for within & between bottleneck
ggplot(expdat[transType!='start' & decision %in% c('down','up'),], aes(x=rt, col=transType))+
  geom_density() +
  geom_vline(data=expdat[transType!='start' & decision %in% c('down','up'),median(rt),by=.(randID,transType,decision)], 
             aes(xintercept=V1))+
  facet_grid(randID~decision)

# RT to main stims over the course of the experiment
ggplot(expdat[!is.na(rt),list(meanrt = mean(rt)),by=.(randID,opt.block.div)], aes(x=opt.block.div, y=meanrt, col=randID))+
  geom_line()

#################################################################################
#### Analyze proportion of normative choices ####
library(matrixcalc)
a <- 0.25
tMat <- rbind(c(0,a,a,a,0,0,0,0,0,0,0,0,0,0,a),
              c(a,0,a,a,a,0,0,0,0,0,0,0,0,0,0),
              c(a,a,0,a,a,0,0,0,0,0,0,0,0,0,0),
              c(a,a,a,0,a,0,0,0,0,0,0,0,0,0,0),
              c(0,a,a,a,0,a,0,0,0,0,0,0,0,0,0),
              c(0,0,0,0,a,0,a,a,a,0,0,0,0,0,0),
              c(0,0,0,0,0,a,0,a,a,a,0,0,0,0,0),
              c(0,0,0,0,0,a,a,0,a,a,0,0,0,0,0),
              c(0,0,0,0,0,a,a,a,0,a,0,0,0,0,0),
              c(0,0,0,0,0,0,a,a,a,0,a,0,0,0,0),
              c(0,0,0,0,0,0,0,0,0,a,0,a,a,a,0),
              c(0,0,0,0,0,0,0,0,0,0,a,0,a,a,a),
              c(0,0,0,0,0,0,0,0,0,0,a,a,0,a,a),
              c(0,0,0,0,0,0,0,0,0,0,a,a,a,0,a),
              c(a,0,0,0,0,0,0,0,0,0,0,a,a,a,0))

# Get percentage optimal choices per bin!
ggplot(expdat[,list(percent.correct=sum(was.choice.opt)/.N),by=.(randID, opt.block.div)], aes(x=opt.block.div, y=percent.correct, col=randID))+
  geom_line()

# Get percentage of up bets per bin
ggplot(expdat[,list(percent.up=sum(choiceVar==1)/.N),by=.(randID, opt.block.div)], aes(x=opt.block.div, y=percent.up, col=randID))+
  geom_line()

