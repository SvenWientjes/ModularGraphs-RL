# Function that can take a hitMat and the full list of edges mapped to identities in the hitMat
#   Returns the optimal stopping step for each vertex or transition (in nBT)

policy.generate.nBT <- function(Edges, EVmat, idmap){

  fullG <- foreach(i = 1:length(Edges), .combine=rbind) %do% {expand.grid(i,Edges[[i]])}
  
  refmat <- foreach(p = EVmat[!duplicated(EVmat[,c('preVertex','Vertex')]),'preVertex'],
                    v = EVmat[!duplicated(EVmat[,c('preVertex','Vertex')]),'Vertex'],
                    .combine = rbind) %do% {
            
    if(!T %in% (EVmat[EVmat$preVertex==p & EVmat$Vertex==v,'EV'] <= 0)){
      stopid <- 0          
    }else if(!F %in% (EVmat[EVmat$preVertex==p & EVmat$Vertex==v,'EV'] <= 0)){
      stopid <- max(EVmat$steps)         
    }else{
      stopid <- max(which(EVmat[EVmat$preVertex == p & EVmat$Vertex == v,'EV'] <= 0))          
    }
    
    idMapped <- expand.grid(idmap[sapply(idmap, function(x){p%in%x})][[1]],
                idmap[sapply(idmap, function(x){v%in%x})][[1]])
    
    idMapped <- idMapped[sapply(1:nrow(idMapped), function(i){  
      T %in% sapply(1:nrow(fullG), function(j){
        identical(as.numeric(idMapped[i,]), as.numeric(fullG[j,]))
      })
    }),]
                                    
    data.frame(preVertex = idMapped[,1], Vertex = idMapped[,2], stopid = stopid)        
  }
  return(refmat)
}

policy.generate <- function(Edges, EVmat, idmap){
  
  fullG <- foreach(i = 1:length(Edges), .combine=rbind) %do% {expand.grid(i,Edges[[i]])}
  
  refmat <- foreach(v = unique(EVmat$vertex), .combine=rbind) %do% {
    
    if(!T %in% (EVmat[EVmat$vertex==v,'EV'] <= 0)){
      stopid <- 0
    }else if(!F %in% (EVmat[EVmat$vertex==v,'EV'] <= 0)){
      stopid <- max(EVmat$steps)
    }else{
      stopid <- max(which(EVmat[EVmat$vertex==v,'EV'] <= 0))
    }
    
    data.frame(vertex=idmap[sapply(1:length(idmap), function(x){v%in%idmap[[x]]})][[1]], stopid=stopid)
  }
  return(refmat)
}
