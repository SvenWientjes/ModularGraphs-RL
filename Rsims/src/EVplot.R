# Function that plots the EVdat with the first crossing to EV-0 highlighted
EVplot <- function(EVdat){
  ggplot(EVdat, aes(x=sLeft, y=value, color=trans)) +
    geom_line(aes(group=trans)) +
    geom_point()+
    ylim(min(EVdat$value)-0.5, max(EVdat$value+0.5)) +
    annotate(geom='segment', x=max(which(EVdat[EVdat$trans=='1<-5',]$value<0))+0.05, 
             xend=max(which(EVdat[EVdat$trans=='1<-5',]$value<0))+0.05, 
             y=min(EVdat$value)-0.5, 
             yend=EVdat[EVdat$trans=='1<-5',]$value[max(which(EVdat[EVdat$trans=='1<-5',]$value<0))], 
             color=brewer.pal(n=8,name='Set3')[6]) +
    annotate(geom='text',    x=max(which(EVdat[EVdat$trans=='1<-5',]$value<0))+0.05, 
             y=min(EVdat$value)-0.5, 
             label=as.character(max(which(EVdat[EVdat$trans=='1<-5',]$value<0))),
             color=brewer.pal(n=10,name='Set3')[6]) +
    annotate(geom='segment', x=max(which(EVdat[EVdat$trans=='1<-2',]$value<0))+0.05, 
             xend=max(which(EVdat[EVdat$trans=='1<-2',]$value<0))+0.05, 
             y=min(EVdat$value)-0.5, 
             yend=EVdat[EVdat$trans=='1<-2',]$value[max(which(EVdat[EVdat$trans=='1<-2',]$value<0))], 
             color=brewer.pal(n=10,name='Set3')[7]) +
    annotate(geom='text',    x=max(which(EVdat[EVdat$trans=='1<-2',]$value<0))+0.05, 
             y=min(EVdat$value)-0.5, 
             label=as.character(max(which(EVdat[EVdat$trans=='1<-2',]$value<0))),
             color=brewer.pal(n=10,name='Set3')[7]) +
    annotate(geom='segment', x=max(which(EVdat[EVdat$trans=='5<-15',]$value<0))+0.05, 
             xend=max(which(EVdat[EVdat$trans=='5<-15',]$value<0))+0.05, 
             y=min(EVdat$value)-0.5, 
             yend=EVdat[EVdat$trans=='5<-15',]$value[max(which(EVdat[EVdat$trans=='5<-15',]$value<0))], 
             color=brewer.pal(n=10,name='Set3')[8]) +
    annotate(geom='text',    x=max(which(EVdat[EVdat$trans=='5<-15',]$value<0))+0.05, 
             y=min(EVdat$value)-0.5, 
             label=as.character(max(which(EVdat[EVdat$trans=='5<-15',]$value<0))),
             color=brewer.pal(n=10,name='Set3')[8]) +
    annotate(geom='segment', x=max(which(EVdat[EVdat$trans=='5<-1',]$value<0))+0.05, 
             xend=max(which(EVdat[EVdat$trans=='5<-1',]$value<0))+0.05, 
             y=min(EVdat$value)-0.5, 
             yend=EVdat[EVdat$trans=='5<-1',]$value[max(which(EVdat[EVdat$trans=='5<-1',]$value<0))], 
             color=brewer.pal(n=10,name='Set3')[9]) +
    annotate(geom='text',    x=max(which(EVdat[EVdat$trans=='5<-1',]$value<0))+0.05, 
             y=min(EVdat$value)-0.5, 
             label=as.character(max(which(EVdat[EVdat$trans=='5<-1',]$value<0))),
             color=brewer.pal(n=10,name='Set3')[9]) +
    annotate(geom='segment', x=max(which(EVdat[EVdat$trans=='1<-4',]$value<0))+0.05, 
             xend=max(which(EVdat[EVdat$trans=='1<-4',]$value<0))+0.05, 
             y=min(EVdat$value)-0.5, 
             yend=EVdat[EVdat$trans=='1<-4',]$value[max(which(EVdat[EVdat$trans=='1<-4',]$value<0))], 
             color=brewer.pal(n=10,name='Set3')[10]) +
    annotate(geom='text',    x=max(which(EVdat[EVdat$trans=='1<-4',]$value<0))+0.05, 
             y=min(EVdat$value)-0.5, 
             label=as.character(max(which(EVdat[EVdat$trans=='1<-4',]$value<0))),
             color=brewer.pal(n=10,name='Set3')[10]) +
    geom_hline(yintercept=0, color='red') +
    scale_color_brewer(palette='Set3')+
    theme_dark()
}
