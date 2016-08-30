plotAllObj1 <- function(dat,fit,var,metric,tc,c,alpha,bl){
  
  # dat <- dat
  # fit <- fitHl
  # var <- "CoverClass"
  # metric <- "0-10%"
  # tc <- 2010
  # c <- brewer.pal(9,"Paired")[1:2]     # blues.
  # alpha <- 0.10
  # bl <- 2008

  #   ---- Restrict to metric of interest.  
  dat <- dat[dat[,var] == metric,]
  
  #   ---- Count the number of data points. 
  nPoints <- with(dat, aggregate(dat$Year, data.frame(dat$LocCode), length))
  names(nPoints) <- c("LocCode","nPoints")
  dat <- merge(dat,nPoints,by=c("LocCode"),all.x=TRUE)
  
  #   ---- Throw out temporal sequences with less than 2 points.
  dat <- dat[dat$nPoints >= 2,]
  dat <- dat[order(dat$Year),]
  
  #   ---- Identify the type of transformed performed.
  if(substr(colnames(fit@frame)[1],7,7) == "u"){
    outcome <- "pCoveru"
  } else if(substr(colnames(fit@frame)[1],7,7) == "l"){
    outcome <- "pCoverl"
  } else if(substr(colnames(fit@frame)[1],7,7) == "a"){
    outcome <- "pCovera"
  }
 
  #   ---- Identify unique transects.  
  LocCodes <- unique(dat[dat[,outcome] < Inf,]$LocCode)
  nLocCodes <- length(LocCodes)
  
  #   ---- Identify ubiquitous plotting elements. 
  xM <- 2015
  xm <- 2007
  
  #   ---- Adjust the scaling, based on the transform. 
  if(outcome == "pCoveru"){
    yM <- 100
    ym <- 0
  } else if(outcome == "pCoverl"){
    yM <- max(dat[,outcome][dat[,outcome] < Inf]) + 0.5
    ym <- min(dat[,outcome]) - 0.5
  } else if(outcome == "pCovera"){
    yM <- max(dat[,outcome][dat[,outcome] < Inf]) + 0.5
    ym <- min(dat[,outcome]) - 0.5
  } else {
    break
  }

  #   ---- Identify the baseline for this run. 
  datbl <- dat[dat$Year == bl,]
  blNum <- sum(datbl$SegLenM)
  blDen <- sum(datbl$TransectLenM)
  b <- blNum / blDen * 100
  
  #   ---- Loop over each transect.  
  for(i in 1:nLocCodes){
    
    l <- dat[dat$LocCode == LocCodes[i],]
    x <- l$Year 
    y <- l[,outcome]
    
    if(i == 1){

      par(mar=c(0.10,0.25,1.5,0),mgp=c(3,0.25,0))
      #par(new=TRUE)
      plot(1:100,xaxt="n",yaxt="n",xlab="",ylab="",type ="n",bty="n")
      par(new=TRUE)
      rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="grey94",border=NA)
      par(new=TRUE)
      for(j in 1:6){
        abline(h=as.numeric(20*(j - 1)),col="white",lwd=2)
      }
      par(new=TRUE)
      
      #   ---- Identify the cutoffs associated with the Ecological Intensity Rating.
      #   ---- Only makes sense to plot these on the original scale.
      if(substr(colnames(fit@frame)[1],7,7) == "u"){
        abline(h=min(b + 10,100),col="orange",lwd=1)
        abline(h=max(b - 10,0  ),col="orange",lwd=1)
        abline(h=min(b + 30,100),col="red",lwd=1)
        abline(h=max(b - 30,0  ),col="red",lwd=1)
        abline(h=b,col="black",lwd=1)
        par(new=TRUE)
      }
      plot(x,y,xaxt="n",yaxt="n",xlim=c(xm,xM),ylim=c(ym,yM),xlab="",ylab="",col=c[1],pch=19,bty="n")
      axis(1,at=x,labels=x,cex.axis=0.5,tck=-0.012)                                                                     # x-axis labels
      axis(2,at=pretty(seq(ym,yM,length.out=5)),labels=pretty(seq(ym,yM,length.out=5)),las=2,cex.axis=0.5,tck=-0.008)   # y-axis labels
      par(new=TRUE)
      plot(x,y,xaxt="n",yaxt="n",xlim=c(xm,xM),ylim=c(ym,yM),xlab="",ylab="",col=c[1],type='l',bty="n")
    } else {
      par(new=TRUE)
      plot(x,y,xaxt="n",yaxt="n",xlim=c(xm,xM),ylim=c(ym,yM),xlab="",ylab="",col=c[1],pch=19,bty="n")
      par(new=TRUE)
      plot(x,y,xaxt="n",yaxt="n",xlim=c(xm,xM),ylim=c(ym,yM),xlab="",ylab="",col=c[1],type='l',bty="n")
    }
  }
  
  #   ---- Add fixed-effects trend. 
  betas <- coef(summary(fit))
  x <- sort(unique(dat$Year))
  xb <- x - tc
  yb <- betas[1,1] + xb*betas[2,1]
  
  #   ---- Actually make the plot.  Note that we plot on the actual year here, and not the w.
  if(outcome == "pCoveru"){
    par(new=TRUE)
    plot(x,yb,xaxt="n",yaxt="n",xlim=c(xm,xM),ylim=c(ym,yM),xlab="",ylab="",col=c[2],pch=19,cex=2,bty="n")
    par(new=TRUE)
    plot(x,yb,xaxt="n",yaxt="n",xlim=c(xm,xM),ylim=c(ym,yM),xlab="",ylab="",col=c[2],type='l',lwd=2,bty="n")
  } else if(outcome == "pCoverl"){
#     yb <- 1 / (1 + -1*yb)
#     ym <- 1 / (1 + -1*ym)
#     yM <- 1 / (1 + -1*yM)
    par(new=TRUE)
    plot(x,yb,xaxt="n",yaxt="n",xlim=c(xm,xM),ylim=c(ym,yM),xlab="",ylab="",col=c[2],pch=19,cex=2,bty="n")
    par(new=TRUE)
    plot(x,yb,xaxt="n",yaxt="n",xlim=c(xm,xM),ylim=c(ym,yM),xlab="",ylab="",col=c[2],type='l',lwd=2,bty="n")
  } else if(outcome == "pCovera"){
    par(new=TRUE)
    plot(x,yb,xaxt="n",yaxt="n",xlim=c(xm,xM),ylim=c(ym,yM),xlab="",ylab="",col=c[2],pch=19,cex=2,bty="n")
    par(new=TRUE)
    plot(x,yb,xaxt="n",yaxt="n",xlim=c(xm,xM),ylim=c(ym,yM),xlab="",ylab="",col=c[2],type='l',lwd=2,bty="n")
  } else {
    break
  }
  
  #   ---- Put down the linear-effects equation.
  beta0 <- round(betas[1,1],2)
  beta1 <- round(betas[2,1],2)
  par(new=TRUE,xpd=FALSE)
  mtext(bquote(beta[0] + beta[1]*w[j]~"=" ~ .(beta0) ~ "+"~ .(beta1) ~ w[j]),side=3,line=0,cex=0.5)
  
  #   ---- Put down the untransformed beta coefficient for ease in interpretation. 
  if(outcome == "pCoveru"){
    beta1Int <- round(betas[2,1],4)
    beta1Intb <- "NA"
  } else if(outcome == "pCoverl"){
    beta1Int <- round(betas[2,1],4)
    beta1Intb <- ifelse(exp(betas[2,1]) < 1,round(exp(betas[2,1]) - 1,4),round(exp(betas[2,1]),4))
  } else if(outcome == "pCovera"){
    beta1Int <- round(betas[2,1],4)
    beta1Intb <- round(100*(sin(betas[2,1])^2),4)
  } else {
    break
  }
  par(new=TRUE)
  u <- par("usr")
  text(u[1] + 0.9*(u[2] - u[1]),u[3] + 0.95*(u[4] - u[3]),beta1Int)
  text(u[1] + 0.9*(u[2] - u[1]),u[3] + 0.85*(u[4] - u[3]),beta1Intb) 
}