#!/usr/bin/Rscript

distance <- function(x,y){sqrt(x^2+y^2)}
distance(4,3)
distance(5,12)
plotfunction <- function(x){
  y <- sqrt(x)
  y <- y+10
  return(y)
}
png("seqtestplot.png")
plot(seq(1,100,1))
dev.off()
jpeg("functionplot.png")
plot(plotfunction, xlab="X", ylab="Y", main="TITLE", xlim=c(0, 100))
dev.off()
