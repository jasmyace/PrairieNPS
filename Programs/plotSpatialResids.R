plotSpatialResids <- function(fit,transects,year,tc,colRange){
  
  # fit <- fitSl
  # transects <- STransects
  # year <- 2012
  # tc <- 2010
  # colRange <- "Greens"
  
  #   ---- Get appropriate variable value for wj.  
  wj <- year - tc
  
  #   ---- Make crosswalk of rownames and LocCodes for this Year.  
  #   ---- This allows the residuals of the fit, and the shapefile,
  #   ---- to talk to one another.  
  xWalk <- fit@frame[fit@frame$Year == year,]
  xWalk$R_ID <- rownames(xWalk)
  
  resids <- data.frame(resid=resid(fit))
  resids$R_ID <- rownames(resids)
  
  #   ---- Put the data together and format.  
  df <- merge(resids,xWalk,by=c('R_ID'),all.y=TRUE)
  df$R_ID <- NULL
  transects@data$R_ID <- seq(1,nrow(transects),1)
  transects@data <- merge(transects@data,df,by=c("LocCode"),all.x=TRUE)
  transects@data <- transects@data[order(transects@data$R_ID),]
  
  #   ---- Make a color gradient for nice plotting.  
  transectsUse <- transects[!is.na(transects@data$resid),]
  plotvar <- transectsUse@data$resid
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