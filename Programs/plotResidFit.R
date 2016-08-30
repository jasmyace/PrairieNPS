plotResidFit <- function(fit,col){
  
  par(mar=c(1,1,0,0))
  plot(fitted(fit),resid(fit),xaxt="n",yaxt="n",pch=19,col=col,bty="n",xlab="",ylab="")
  
  axis(1,at=pretty(fitted(fit)),labels=pretty(fitted(fit)),cex.axis=0.5,tck=-0.012)        # x-axis
  axis(2,at=pretty(resid(fit)),labels=pretty(resid(fit)),las=2,cex.axis=0.5,tck=-0.008)    # y-axis
  
  mtext("Fitted Values",side=1,line=1,cex=0.5)
  mtext("Residuals",side=2,line=1,cex=0.5)
}