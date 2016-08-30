plotRandCorr <- function(fit,group1,group2,col){
  
  # fit <- fitHl                  
  # group1 <- "LocCode"            
  # group2 <- "Year"
  # col <- col

  LocCodeYears <- unique(fit@frame[,c('LocCode','Year')])
  levels <- sort(rownames(ranef(fit)[names(ranef(fit)) == group2][[1]]))
  
  xm <- min(ranef(fit)[names(ranef(fit)) == group1][[1]][,1])
  xM <- max(ranef(fit)[names(ranef(fit)) == group1][[1]][,1])
  ym <- min(ranef(fit)[names(ranef(fit)) == group1][[1]][,2])
  yM <- max(ranef(fit)[names(ranef(fit)) == group1][[1]][,2])
  xlim <- c(xm,xM)
  ylim <- c(ym,yM)
  
  helper <- "//lar-file-srv/Data/NPS/Prairie/Analysis/Helper"
  png(paste0(helper,"/Corr.png"),width=4,height=3,units="in",res=600)
  
  par(mfrow=c(3,3))
  
  for( i in 1:length(levels) ){
  
    thisOne <- levels[i]
    
    #   ---- Get the list of transects that were measured in this year.
    theSample <- LocCodeYears[LocCodeYears[,group2] == thisOne,][,group1]
    
    #   ---- Get the temporally dependent random transect effects.
    theRandEff <- ranef(fit)[names(ranef(fit)) == group1][[1]]
    theRandEff <- theRandEff[rownames(theRandEff) %in% theSample,]
    
    x <- theRandEff[,1]
    y <- theRandEff[,2]
    
    par(mar=c(0.75,1.5,0.75,0.75))
    plot(x,y,pch=19,cex=0.3,col=col,xaxt="n",yaxt="n",xlim=xlim,ylim=ylim,xlab="",ylab="",mgp=c(1,1,0))#
    axis(1,at=pretty(xlim),labels=NA,tck=-0.012,cex.axis=0.5)  # x-axis
    axis(1,lwd=0,line=-1.3,cex.axis=0.5)          # x-axis

    axis(2,at=pretty(ylim),labels=NA,tck=-0.008)  # y-axis
    axis(2,lwd=0,line=-0.8,cex.axis=0.5,las=2)    # y-axis
    
    mtext("Transect Intercepts a[i]",side=1,line=0.2,cex=0.25)
    mtext("Transect Slopes t[i]",side=2,line=0.8,cex=0.25)
    
    #   ---- Place the year in the plot.  This helps with spacing.
    u <- par("usr")
    text(u[1] + 0.9*(u[2] - u[1]),u[3] + 0.9*(u[4] - u[3]),thisOne,cex=0.5)
    
    abline(h=0,col="red",lwd=0.5)
    abline(v=0,col="red",lwd=0.5)
    
  }

  par(mfrow=c(1,1))
  
  dev.off()
  
  #   ---- Read the image back in for image plotting.  
  par(mar = c(0,0,0,0))
  plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE)
  u <- par("usr")
  img <- readPNG(paste0(helper,"/Corr.png"))
  rasterImage(img,u[1],u[3],u[2],u[4])
 
}