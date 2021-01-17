EVplot <- function(EVmat, piMat){
  EVmat$vertex <- factor(EVmat$vertex, levels=EVmat[EVmat$steps==max(EVmat$steps),]$vertex[order(-EVmat[EVmat$steps==max(EVmat$steps),]$EV)], ordered=T)
  pl <- ggplot(EVmat, aes(x = steps, y = EV, col=vertex)) +
    geom_line(aes(group=vertex)) +
    scale_color_brewer(palette='Set3') +
    theme_dark()
  
  intVs <- piMat[piMat$stopid!=max(piMat$stopid) & piMat$stopid!=min(piMat$stopid),'vertex'][piMat[piMat$stopid!=max(piMat$stopid) & piMat$stopid!=min(piMat$stopid),'vertex'] %in% EVmat$vertex]
  
  ints <- lapply(intVs, add_intersect, piMat=piMat, EVmat=EVmat)
  
  texts <- lapply(intVs, add_text, piMat=piMat, EVmat=EVmat)
  
  pl + ints + texts
  
  #pl + add_intersect(EVmat, piMat, 2) + add_text(EVmat, piMat, 2)
}

add_intersect <- function(EVmat, piMat, vertex){
  annotate(geom='segment', x=piMat[piMat$vertex==vertex,'stopid'], y=min(EVmat$EV)-1,
           xend=piMat[piMat$vertex==vertex,'stopid'], yend=EVmat[EVmat$vertex==vertex & EVmat$steps==piMat[piMat$vertex==vertex,'stopid'],]$EV,
           color=brewer.pal(n=length(levels(EVmat$vertex)),name='Set3')[which(levels(EVmat$vertex)==vertex)])
}

add_text <- function(EVmat, piMat, vertex){
  annotate(geom='text', x=piMat[piMat$vertex==vertex,'stopid'], y=min(EVmat$EV)-2, label=as.character(piMat[piMat$vertex==vertex,'stopid']),
           color=brewer.pal(n=length(levels(EVmat$vertex)),name='Set3')[which(levels(EVmat$vertex)==vertex)])
}
