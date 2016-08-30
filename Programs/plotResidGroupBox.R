plotResidGroupBox <- function(fit,col,group){
  
  # fit <- fitHl
  # col <- brewer.pal(9,"Paired")[1]
  # group <- "LocCode"
  
  par(mar=c(1,1,0,0))
  
  theBox <- boxplot(resid(fit) ~ fit@frame[,group],xlab=deparse(substitute(group)),plot=FALSE)
  y <- c(min(theBox$stats),max(theBox$stats))
  
  #   ---- Actually make the plot.
  boxplot(resid(fit) ~ fit@frame[,group],xlab=deparse(substitute(group)),ylab="Residuals",col=col,main="",xaxt="n",yaxt="n",frame=FALSE)
  axis(1,at=seq(1,length(theBox$names),1),labels=theBox$names,cex.axis=0.5,tck=-0.012,las=2)   # x-axis
  axis(2,at=pretty(y),labels=pretty(y),las=2,cex.axis=0.5,tck=-0.008)                    # y-axis
  
  #   ---- Print empircal variance on the box plot.
  empVar <- getEmpiricalVarComp(fit,group)
  vc <- as.data.frame(VarCorr(fit))
  if(group == "LocCode"){
    modVar <- vc[1,4]
  } else if(group == "Year"){
    modVar <- vc[4,4]
  }
  u <- par("usr")
  text(u[1] + 0.9*(u[2] - u[1]),u[3] + 0.95*(u[4] - u[3]),paste0("EmpVar/ModVar ... EmpSD/ModSD: ",round(empVar,4),"/",round(modVar,4),"  ... ",round(sqrt(empVar),4),"/",round(sqrt(modVar),4)),cex=0.5)
  
  #   ---- Plot cutoffs of empirical standard deviation.  
  abline(2*sqrt(empVar),0,col="yellow")
  abline(1*sqrt(empVar),0,col="orange")
  abline(0,0,col="red")
  abline(-1*sqrt(empVar),0,col="orange")
  abline(-2*sqrt(empVar),0,col="yellow")
}     