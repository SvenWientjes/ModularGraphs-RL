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
## Analyze goal inspection times
goaltimedat <- PilotLog01[startsWith(stimulus, 'img/') & endsWith(stimulus, '_goal.png'), .(randID, miniblock, rt, stimulus)]
goaltimedat$rt <- as.numeric(goaltimedat$rt) # To numeric
goaltimedat$miniblock <- rep(1:100, 2)

# Inspect ordered RTs for each participant
cbind(goaltimedat[randID==3, ][order(rt),rt], goaltimedat[randID==2, ][order(rt),rt])

# Distribution of 'normal' inspection times
ggplot(goaltimedat[rt<2500,], aes(x=rt, col=randID))+
  geom_density()

# Line plot of evolution of inspection times throughout experiment progression
ggplot(goaltimedat, aes(x=miniblock, y=rt, col=randID))+
  geom_line()


