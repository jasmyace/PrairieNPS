plotRanEfQQ <- function(group1,fit,col,column){
  
  # group1 <- "LocCode"
  # fit <- fitHu
  # col <- brewer.pal(9,"Paired")[1]
  # column <- 1
  
  y <- ranef(fit)[names(ranef(fit)) == group1][[1]][,column]
  
  par(mar=c(1,1,0,0))
  qqnorm(y,pch=19,col=col,bty="n",main="",xaxt="n",yaxt="n",xlab="",ylab="")

  n <- length(y)
  x <- qnorm(ppoints(n))[order(order(y))]
  xlim <- range(x)
  
  axis(1,at=pretty(xlim),labels=pretty(xlim),cex.axis=0.5,tck=-0.012)             # x-axis
  axis(2,at=pretty(y),labels=pretty(y),las=2,cex.axis=0.5,tck=-0.008)    # y-axis
  
  if(column == 1){
    mtext("Theoretical Quantiles (Intercept)",side=1,line=1,cex=0.5,las=1)
    mtext("Sample Quantiles (Intercept)",side=2,line=1,cex=0.5)
  } else {
    mtext("Theoretical Quantiles (Slope)",side=1,line=1,cex=0.5,las=1)
    mtext("Sample Quantiles (Slope)",side=2,line=1,cex=0.5)
  }
  
}