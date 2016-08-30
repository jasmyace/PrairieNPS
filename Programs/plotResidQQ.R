plotResidQQ <- function(fit,col){
  
  par(mar=c(1,1,0,0))
  qqnorm(resid(fit),pch=19,col=col,bty="n",main="",xaxt="n",yaxt="n",xlab="",ylab="")
  
  y <- resid(fit)
  n <- length(y)
  x <- qnorm(ppoints(n))[order(order(y))]
  xlim <- range(x)
  
  axis(1,at=pretty(xlim),labels=pretty(xlim),cex.axis=0.5,tck=-0.012)                      # x-axis
  axis(2,at=pretty(resid(fit)),labels=pretty(resid(fit)),las=2,cex.axis=0.5,tck=-0.008)    # y-axis
  
  mtext("Theoretical Quantiles",side=1,line=1,cex=0.5,las=1)
  mtext("Sample Quantiles",side=2,line=1,cex=0.5)
  
}