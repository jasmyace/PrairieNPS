prepDataObj3 <- function(dat){
  
  # dat <- a144
  
  #   ---- Want the proportion of exotic class, given Herbaceous Native. 
  datden <- dat[dat$VegType %in% c("H") & dat$Origin %in% c("N"),]
  num <- with(datden, aggregate(datden$SegNumM, data.frame(datden$UnitCode,datden$Year,datden$LocCode,datden$CoverClass), sum))
  den <- with(datden, aggregate(datden$SegNumM, data.frame(datden$UnitCode,datden$Year,datden$LocCode), sum))
  
  colnames(num) <- c('UnitCode','Year','LocCode','CoverClass','SegLenM')
  colnames(den) <- c('UnitCode','Year','LocCode','TransectLenM')
  
  num <- num[order(num$LocCode,num$Year,num$CoverClass),]
  den <- den[order(den$LocCode,den$Year),]
  
  dat <- merge(num,den,by=c('UnitCode','Year','LocCode'),all.x=TRUE)
  dat$pCover <- 100*dat$SegLenM / dat$TransectLenM
  
  #   ---- Make sure this is rounded to two decimals.
  dat$pCover <- round(dat$pCover,2)
  
  #   ---- Need to make sure we have zero percentages where necessary.  Idea is to 
  #   ---- make a 'backbone' of data values for which we expect data, and merge in
  #   ---- what we have against it.  Any blanks must be zero.  
  dat2 <- unique(dat[,c('UnitCode','LocCode','Year')])
  dat2a <- cbind(dat2,CoverClass=rep("0-10%"))
  dat2b <- cbind(dat2,CoverClass=rep("11-49%"))  
  dat2c <- cbind(dat2,CoverClass=rep("50-100%"))
  dat3 <- rbind(dat2a,dat2b,dat2c)
  
  #   ---- Now clean up the data.  Some of this redudant, i.e., this isn't the most
  #   ---- efficient program in the world.  But not much data.  
  dat4 <- merge(dat3,dat,by=c('UnitCode','LocCode','Year','CoverClass'),all.x=TRUE)
  names(den)[names(den) == "TransectLenM"] <- "TransectAgain"
  dat5 <- merge(dat4,den,by=c('UnitCode','LocCode','Year'),all.x=TRUE)
  dat5$TransectLenM <- NULL
  names(dat5)[names(dat5) == "TransectAgain"] <- "TransectLenM"
  dat5[is.na(dat5)] <- 0
  dat <- dat5
  
  dat <- dat[order(dat$UnitCode,dat$LocCode,dat$CoverClass,dat$Year),]
  
  #   ---- Note that a104 is not called in the function.  Get panel types.  
  dat <- merge(dat,a104[,c('Unit_code','Location_code','Panel_type')],by.x=c('UnitCode','LocCode'),by.y=c('Unit_code','Location_code'))
  dat <- dat[order(dat$UnitCode,dat$LocCode,dat$CoverClass,dat$Year),]
  dat <- dat[,c('UnitCode','LocCode','Year','CoverClass','SegLenM','TransectLenM','pCover','Panel_type')]
  
  #   ---- The process of putting in zeros may have created temporal trends for which
  #   ---- ALL observations are zero.  This is bad.  We need to get rid of these only.
  #   ---- Idea is to sum the TransectLenM in a UnitCode, LocCode, VegType. 
  #   ---- For Objective 3, I don't think this does anything. 
  zeroCheck <- with(dat, aggregate(dat$TransectLenM, data.frame(dat$UnitCode,dat$LocCode,dat$CoverClass), sum))
  names(zeroCheck) <- c('UnitCode','LocCode','CoverClass','TransectLenMSum')
  
  check <- merge(dat,zeroCheck,by=c('UnitCode','LocCode','CoverClass'),all.x=TRUE)
  dat <- check[check$TransectLenMSum > 0,]
  dat$TransectLenMSum <- NULL
  
  dat$CoverClass <- as.character(droplevels(dat$CoverClass))
  rownames(dat) <- NULL

  unitCodes <- unique(dat$UnitCode)
  return(list(dat,unitCodes))
  
}