################################################################################
####### Get data from devtest ArtDealer and plot a little bit the choices ######
################################################################################
# Load the two csv's and get them into neato data.table
data1 <- as.data.table(read.csv('dat/Exhaust_Devtest01.csv'))
data2 <- as.data.table(read.csv('dat/Exhaust_DevTest02.csv'))
data1 <- data1[,.(rt, miniblock,nSteps,node,goalnode,choiceVar)]
data2 <- data2[,.(rt, miniblock,nSteps,node,goalnode,choiceVar)]
# Add participant numbers
data1[,pp:=1]
data2[,pp:=2]
data <- rbind(data1, data2)
# Filter for real trials
data <- data[!is.na(miniblock) & rt!='null']
data$rt <- as.numeric(data$rt)

# Append cluster identity
data[node %in% c(0:4), sym:='a']
data[node %in% c(5:9), sym:='b']
data[node %in% c(10:14), sym:='c']
data[goalnode %in% c(0:4), g.sym:='a']
data[goalnode %in% c(5:9), g.sym:='b']
data[goalnode %in% c(10:14), g.sym:='c']
# Append node identity
data[node %in% c(1,2,3,6,7,8,11,12,13), nodeType:='deep']
data[node %in% c(0,4,5,9,10,14), nodeType:='btn']
# Append transition identity in general
data[nodeType=='btn' & shift(nodeType, n=1, type='lag')=='btn', transType:='between', by=pp]
data[is.na(transType), transType:='within', by=pp]
data[nSteps==0, nodeType:='start', by=pp]
data[nodeType=='start', transType:='start', by=pp]
data[is.na(choiceVar), choiceVar:=0, by=pp]
# Append transition identity w.r.t. goal
data[transType=='start',transType.g:='start', by=pp]
data[transType=='between'&sym==g.sym,transType.g:='into', by=pp]
data[transType=='between'&sym!=g.sym&shift(sym,1,type='lag')==g.sym,transType.g:='outof', by=pp]
data[transType=='between'&sym!=g.sym&shift(sym,1,type='lag')!=g.sym,transType.g:='between', by=pp]
data[transType=='within'&sym==g.sym,transType.g:='within', by=pp]
data[transType=='within'&sym!=g.sym,transType.g:='outside', by=pp]

ggplot(data[choiceVar==0], aes(x=log(rt), col=transType.g))+
  geom_density() +
  facet_grid(choiceVar~.)

ggplot(data[transType.g!='start'&choiceVar==0], aes(x=rt, col=transType.g))+
  geom_histogram() +
  facet_grid(choiceVar~transType.g)

# Plot densities by transition w.r.t. goal (can filter for particular response type e.g. 0 or 1)
ggplot(data[transType.g!='start'&choiceVar%in%c(1)],aes(x=rt, col=transType.g))+
  geom_density()

# Get numbers for proportion of choice for particular trial type
data[,sum(choiceVar==-1)/.N, by=.(transType.g,pp)]
data[,sum(choiceVar==0)/.N, by=.(transType.g)]
data[,sum(choiceVar==1)/.N, by=.(transType.g)]
# Plot proportion of choice for particular trial type, with overall choice proportion of all trial types as horizontal line
ggplot(data[transType.g!='start',list(pDown = sum(choiceVar==-1)/.N), by=.(transType.g,pp)], aes(x=transType.g, y=pDown,fill=pp))+
  geom_bar(stat='identity', position='dodge2')+
  geom_hline(yintercept=data[transType.g!='start',sum(choiceVar==-1)/.N])
ggplot(data[transType.g!='start',list(pDown = sum(choiceVar==0)/.N), by=.(transType.g,pp)], aes(x=transType.g, y=pDown,fill=pp))+
  geom_bar(stat='identity', position='dodge2')+
  geom_hline(yintercept=data[transType.g!='start',sum(choiceVar==0)/.N])
ggplot(data[transType.g!='start',list(pDown = sum(choiceVar==1)/.N), by=.(transType.g,pp)], aes(x=transType.g, y=pDown,fill=pp))+
  geom_bar(stat='identity', position='dodge2')+
  geom_hline(yintercept=data[transType.g!='start',sum(choiceVar==1)/.N])

ggplot(data[,list(mRT = mean(rt)),by=miniblock], aes(x=miniblock, y=mRT))+
  geom_line()



