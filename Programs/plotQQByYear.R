plotQQByYear <- function(fit,group1,group2,col,col2,column){
  
  # fit <- fitHl                  
  # group1 <- "LocCode"            
  # group2 <- "Year"
  # col <- col
  # col2 <- col
  # column <- 1
  
  LocCodeYears <- unique(fit@frame[,c('LocCode','Year')])
  levels <- sort(rownames(ranef(fit)[names(ranef(fit)) == group2][[1]]))
  
  xm <- min(ranef(fit)[names(ranef(fit)) == group1][[1]][,1])
  xM <- max(ranef(fit)[names(ranef(fit)) == group1][[1]][,1])
  ym <- min(ranef(fit)[names(ranef(fit)) == group1][[1]][,2])
  yM <- max(ranef(fit)[names(ranef(fit)) == group1][[1]][,2])
  xlim <- c(xm,xM)
  ylim <- c(ym,yM)
  
  helper <- "//lar-file-srv/Data/NPS/Prairie/Analysis/Helper"
  png(paste0(helper,"/QQYear.png"),width=4,height=3,units="in",res=600)
  
  par(mfrow=c(3,3))
  
  for( i in 1:length(levels) ){
    
    thisOne <- levels[i]
    
    #   ---- Get the list of transects that were measured in this year.
    theSample <- LocCodeYears[LocCodeYears[,group2] == thisOne,][,group1]
    
    #   ---- Get the temporally dependent random transect effects.
    theRandEff <- ranef(fit)[names(ranef(fit)) == group1][[1]]
    theRandEff <- theRandEff[rownames(theRandEff) %in% theSample,]
    
    #   ---- At this point, everything is set up.  Now get to work making
    #   ---- the parts of the QQ plot that we need.  
    y <- theRandEff[,column]
    ylim <- range(y)
    n <- length(y)
    x <- qnorm(ppoints(n))[order(order(y))]
    xlim <- range(x)
    
    par(mar=c(0.75,1.5,0.75,0.75))
    plot(x,y,pch=19,cex=0.3,col=col,xaxt="n",yaxt="n",xlim=xlim,ylim=ylim,xlab="",ylab="",mgp=c(1,1,0))#
    axis(1,at=pretty(xlim),labels=NA,tck=-0.012,cex.axis=0.5)  # x-axis
    axis(1,lwd=0,line=-1.3,cex.axis=0.5)          # x-axis
    
    axis(2,at=pretty(ylim),labels=NA,tck=-0.008)  # y-axis
    axis(2,lwd=0,line=-0.8,cex.axis=0.5,las=2)    # y-axis
    
    if(column == 1){
      mtext("Theoretical Quantiles (Intercept)",side=1,line=0.2,cex=0.25)
      mtext("Sample Quantiles (Intercept)",side=2,line=0.8,cex=0.25)
    } else {
      mtext("Theoretical Quantiles (Slope)",side=1,line=0.2,cex=0.25)
      mtext("Sample Quantiles (Slope)",side=2,line=0.8,cex=0.25)
    }
    
    #   ---- Place the year in the plot.  This helps with spacing.
    u <- par("usr")
    text(u[1] + 0.1*(u[2] - u[1]),u[3] + 0.9*(u[4] - u[3]),thisOne,cex=0.5)
    
    #   ---- Add red line of perfection.
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope * x[1L]
    abline(int,slope,col=col2,lwd=2)
    
  }
  
  par(mfrow=c(1,1))
  
  dev.off()
  
  #   ---- Read the image back in for image plotting.  
  par(mar = c(0,0,0,0))
  plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE)
  u <- par("usr")
  img <- readPNG(paste0(helper,"/QQYear.png"))
  rasterImage(img,u[1],u[3],u[2],u[4])
  
  
  
}