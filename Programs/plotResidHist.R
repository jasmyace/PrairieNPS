plotResidHist <- function(fit,col){
  
  par(mar=c(1,1,1,0))
  theHist <- hist(resid(fit),plot=FALSE)
  
  x <- theHist$mids
  y <- theHist$counts
  
  labels <- theHist$counts
  labels[labels == 0] <- NA

  hist(resid(fit),col=col,border="white",bty="n",main="",xaxt="n",yaxt="n",cex=0.25)
  text(x,labels + 1.25,labels,cex=0.35)
  axis(1,at=x,labels=x,cex.axis=0.5,tck=-0.012)                           # x-axis
  #axis(2,at=pretty(y),labels=pretty(y),las=2,cex.axis=0.5,tck=-0.008)    # y-axis
  
  mtext("Residuals",side=1,line=1,cex=0.5)
  
}