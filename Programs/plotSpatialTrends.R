plotSpatialTrends <- function(fit,transects,year,tc,outcome,colRange){
  
  # fit <- fitHu
  # transects <- STransects
  # year <- 2012
  # tc <- 2007
  # outcome <- "pCoveru"
  # colRange <- "Blues"
  
  #   ---- Get appropriate variable value for wj.  
  wj <- year - tc
  
  #   ---- Make crosswalk of rownames and LocCodes for this Year.  
  #   ---- This allows the residuals of the fit, and the shapefile,
  #   ---- to talk to one another.  
  xWalk <- fit@frame[fit@frame$Year == year,]
  
  #   ---- Put the data together and format.  
  transects@data <- merge(transects@data,xWalk,by=c("LocCode"),all.x=TRUE)
  
  #   ---- Make a color gradient for nice plotting.  
  transectsUse <- transects[!is.na(transects@data[,outcome]),]
  plotvar <- transectsUse@data[,outcome]
  nclr <- 5
  plotclr <- brewer.pal(nclr,colRange)
  if( length(unique(plotvar)) > 1 ){
    class <- classIntervals(plotvar,nclr,style="quantile")
    colcode <- findColours(class, plotclr)
  } else {
    colcode <- brewer.pal(3,colRange)[3]
  }

  #   ---- Plot the results, looking for a pattern in the color gradient.
  plot(transectsUse,col=colcode,lwd=5)
  
}