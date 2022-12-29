rm(list = ls())

library(tidyverse)

bullet.graph <- function(bg.data){
  
  # compute max and half for the ticks and labels
  max.bg <- max(bg.data$high)
  mid.bg <- max.bg / 2
  
  gg <- ggplot(bg.data) 
  gg <- gg + geom_bar(aes(measure, high),  fill="darkgreen", stat="identity", width=0.5, alpha=0.2) 
  gg <- gg + geom_bar(aes(measure, mean),  fill="goldenrod2", stat="identity", width=0.5, alpha=0.2) 
  gg <- gg + geom_bar(aes(measure, low),   fill="firebrick1", stat="identity", width=0.5, alpha=0.2) 
  gg <- gg + geom_bar(aes(measure, value), fill="black",  stat="identity", width=0.2) 
  gg <- gg + geom_errorbar(aes(x=measure, ymin=target, ymax=target), color="red", width=0.45) 
  gg <- gg + geom_point(aes(measure, target), colour="red", size=2.5) 
  gg <- gg + scale_y_continuous(breaks=seq(0,max.bg,mid.bg))
  gg <- gg + coord_flip()
  gg <- gg + theme(axis.text.x=element_text(size=5),
                   axis.title.x=element_blank(),
                   axis.line.y=element_blank(), 
                   axis.text.y=element_text(hjust=1, color="black"), 
                   axis.ticks.y=element_blank(),
                   axis.title.y=element_blank(),
                   legend.position="none",
                   panel.background=element_blank(), 
                   panel.border=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),
                   plot.background=element_blank())
  
  return(gg)
  
}

incidents <- data.frame(
  measure=c("Total Events (K)", "Security Events (K)"),
  high=c(3200,2000),
  mean=c(2170,1500),
  low=c(1500,500), 
  target=c(2500,1750),
  value=c(2726,1600)
)

bullet.graph(incidents)
