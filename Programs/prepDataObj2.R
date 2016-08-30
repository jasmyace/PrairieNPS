prepDataObj2 <- function(dat){
  
  # dat <- a144
  
  #   ---- Want the proportion of of exotic, given a VegClass.
  datden <- dat[dat$VegType %in% c("H","S","T") & dat$Origin %in% c("E","N"),]
  num <- with(datden, aggregate(datden$SegNumM, data.frame(datden$UnitCode,datden$Year,datden$LocCode,datden$VegType,datden$Origin), sum))
  den <- with(datden, aggregate(datden$SegNumM, data.frame(datden$UnitCode,datden$Year,datden$LocCode,datden$VegType), sum))
  
  colnames(num) <- c('UnitCode','Year','LocCode','VegType','Origin','SegLenM')
  colnames(den) <- c('UnitCode','Year','LocCode','VegType','TransectLenM')
  
  num <- num[order(num$LocCode,num$Year,num$VegType,num$Origin),]
  den <- den[order(den$LocCode,den$Year,den$VegType),]
  
  dat <- merge(num,den,by=c('UnitCode','Year','LocCode','VegType'),all.x=TRUE)
  dat$pCover <- 100*dat$SegLenM / dat$TransectLenM
  
  #   ---- Make sure this is rounded to two decimals.
  dat$pCover <- round(dat$pCover,2)
  
  dat <- dat[dat$VegType != "End",]
  
  #   ---- Need to make sure we have zero percentages where necessary.  Idea is to 
  #   ---- make a 'backbone' of data values for which we expect data, and merge in
  #   ---- what we have against it.  Any blanks must be zero.  
  dat2 <- unique(dat[,c('UnitCode','LocCode','Year')])
  dat2a <- cbind(dat2,VegType=rep("H"),Origin=rep("E"))
  dat2b <- cbind(dat2,VegType=rep("S"),Origin=rep("E"))  
  dat2c <- cbind(dat2,VegType=rep("T"),Origin=rep("E"))
  dat3 <- rbind(dat2a,dat2b,dat2c)
  
  #   ---- Now clean up the data.  Some of this redudant, i.e., this isn't the most
  #   ---- efficient program in the world.  But not much data.  
  dat4 <- merge(dat3,dat,by=c('UnitCode','LocCode','Year','VegType','Origin'),all.x=TRUE)
  names(den)[names(den) == "TransectLenM"] <- "TransectAgain"
  dat5 <- merge(dat4,den,by=c('UnitCode','LocCode','Year','VegType'),all.x=TRUE)
  dat5$TransectLenM <- NULL
  names(dat5)[names(dat5) == "TransectAgain"] <- "TransectLenM"
  dat5[is.na(dat5)] <- 0
  dat <- dat5
  
  dat <- dat[order(dat$UnitCode,dat$LocCode,dat$VegType,dat$Year),]
 
  #   ---- Note that a104 is not called in the function.  Bring in the type of transect.
  dat <- merge(dat,a104[,c('Unit_code','Location_code','Panel_type')],by.x=c('UnitCode','LocCode'),by.y=c('Unit_code','Location_code'))
  dat <- dat[order(dat$UnitCode,dat$LocCode,dat$VegType,dat$Year,dat$Origin),]
  
  #   ---- Make a metric that combines all of Herbaceous, Shrub, and Tree.  
  #   ---- Call this VegType == "A".
  num <- with(dat, aggregate(dat$SegLenM, data.frame(dat$UnitCode,dat$Year,dat$LocCode), sum))
  den <- with(dat, aggregate(dat$TransectLenM, data.frame(dat$UnitCode,dat$Year,dat$LocCode), sum))
  
  colnames(num) <- c('UnitCode','Year','LocCode','SegLenM')
  colnames(den) <- c('UnitCode','Year','LocCode','TransectLenM')
  composite <- merge(den,num,by=c('UnitCode','Year','LocCode'),all.x=TRUE)
  composite$VegType <- "A"    # A is for 'All'
  composite$Origin <- "E"
  composite$pCover <- (composite$SegLenM / composite$TransectLenM) * 100
  composite$Panel_type <- ifelse(substr(composite$LocCode,1,1) == "1","Annual","Alternating")
  composite <- composite[,c('UnitCode','LocCode','Year','VegType','Origin','SegLenM','TransectLenM','pCover','Panel_type')]
  
  dat <- rbind(dat,composite)
  dat <- dat[order(dat$UnitCode,dat$LocCode,dat$VegType,dat$Year,dat$Origin),]
  dat <- dat[,c('UnitCode','Panel_type','LocCode','Origin','VegType','Year','SegLenM','TransectLenM','pCover')]
  
  #   ---- The process of putting in zeros may have created temporal trends for which
  #   ---- ALL observations are zero.  This is bad.  We need to get rid of these only.
  #   ---- Idea is to sum the TransectLenM in a UnitCode, LocCode, VegType. Note that we 
  #   ---- use TransectLenM here, and not pCover -- it is okay to have 0.0% Exotic pCover, IF
  #   ---- all of a non-zero TransectLenM here (100%) was Native.
  zeroCheck <- with(dat, aggregate(dat$TransectLenM, data.frame(dat$UnitCode,dat$LocCode,dat$VegType), sum))
  names(zeroCheck) <- c('UnitCode','LocCode','VegType','TransectLenMSum')
  
  check <- merge(dat,zeroCheck,by=c('UnitCode','LocCode','VegType'),all.x=TRUE)
  dat <- check[check$TransectLenMSum > 0,]
  dat$TransectLenMSum <- NULL
  
  unitCodes <- unique(dat$UnitCode)
  return(list(dat,unitCodes))
  
}