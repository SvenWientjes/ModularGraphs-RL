################################################################################
####### Get data from devtest ArtDealer and plot a little bit the choices ######
################################################################################
# Load from SQL (laptop local)
library(RMySQL)
dbConnection <- dbConnect(MySQL(), host="mysql.ugent.be", user="s240338", password="PCGbU2wp@-wEGt6E", dbname="lccl")
data <- dbReadTable(dbConnection,"`ArtDealer_testdev_data`")

# Load from wrangled / saved data.table (server)
aa

#################################################################################
## Wrangle database retrieved data into neat data.table
library(data.table)
library(ggplot2)
# Put database into table
devtest01 <- as.data.table(data)
# Get randID as factor
devtest01$randID <- as.factor(devtest01$randID)

# Check which randID has seen the 2 questions and the finishing screen of the experiment (3 indicates o.k.)
devtest01[startsWith(stimulus, '<div id="Finish"'), .N, by=randID]

# Select exactly the participant which did 100 trials
devtest01 <- devtest01[randID=='15olh4r11dmxxozswpdvky5pnlj30u13',]
levels(devtest01$randID) <- c(1:length(levels(devtest01$randID)))

# Save only interesting participants and entries
fullpp <- devtest01[randID %in% c(1) & !is.na(node) & !is.na(decision) & miniblock>=0,
                    .(randID, miniblock, nSteps, node, goalnode, decision, choiceVar, rt, trBet, totRew)]
fullpp$rt <- as.integer(fullpp$rt)
fullpp$nSteps <- as.numeric(fullpp$nSteps)
fullpp$miniblock <- as.integer(fullpp$miniblock)
fullpp$node <- as.integer(fullpp$node)
fullpp$goalnode <- as.integer(fullpp$goalnode)

# Identify nodes as deep or bottleneck
fullpp[,nodeType := if(node%in%c(0,4,5,9,10,14)){'btn'}else{'deep'},by=.(randID,miniblock,nSteps)]
# Define starting transitions
fullpp[nSteps==0, transType:='start']
# Define bottleneck transitions between clusters
fullpp[nSteps!=0 & nodeType=='btn' & shift(nodeType, n=1, type='lag')=='btn', transType:='between']
# Define all transitions that stay within one cluster
fullpp[is.na(transType), transType:='within']

#################################################################################
## Analyze RT difference of within and between transitions
ggplot(fullpp[transType!='start' & decision %in% c('down','up'),], aes(x=rt, col=transType))+
  geom_density() +
  geom_vline(data=fullpp[transType!='start' & decision %in% c('down','up'),mean(rt),by=.(transType,decision)], 
             aes(xintercept=V1), color=c('blue', 'red','blue','red'))+
  facet_grid(rows=vars(decision))

#################################################################################
## Analyze proportion of normative choices
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
# Get normative choices (using older 1-indexed function)
fullpp[,opt.choice:=get.opt.choice(miniblock+1, node+1, goalnode+1, (15-nSteps-1), tMat, 1, -1), by=.(randID,miniblock)]
fullpp[opt.choice==-1, opt.choice:=0]
# Evaluate current choice as optimal or not
fullpp[,was.choice.opt:=choiceVar==opt.choice, by=.(randID, miniblock, nSteps)]
# Bin into groups of 10 miniblocks
fullpp[,opt.block.div:=ceiling((miniblock+1)/5),by=.(randID,miniblock,nSteps)]
# Get percentage optimal choices per bin!
plot(1:20,fullpp[,sum(was.choice.opt)/.N,by=opt.block.div]$V1, type='l')
ggplot(fullpp[,list(percent.correct=sum(was.choice.opt)/.N),by=opt.block.div], aes(x=opt.block.div, y=percent.correct))+
  geom_line()

#################################################################################
## Analyze goal inspection times
goaltimedat <- devtest01[startsWith(stimulus, 'img/') & endsWith(stimulus, '_goal.png'), .(randID, miniblock, rt, stimulus)]
goaltimedat$rt <- as.numeric(goaltimedat$rt) # To numeric

# Count nr above 10000
goaltimedat[rt>10000, .N]

ggplot(goaltimedat[rt<10000,], aes(x=rt, col=randID))+
  geom_density()


