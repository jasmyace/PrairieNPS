prepDataObj1 <- function(dat){
  
  # dat <- a144
  
  num <- with(dat, aggregate(dat$SegNumM, data.frame(dat$UnitCode,dat$Year,dat$LocCode,dat$VegType), sum))
  datden <- dat[dat$VegType %in% c("H","S","T","D","U"),]
  den <- with(datden, aggregate(datden$SegNumM, data.frame(datden$UnitCode,datden$Year,datden$LocCode), sum))
  
  colnames(num) <- c('UnitCode','Year','LocCode','VegType','SegLenM')
  colnames(den) <- c('UnitCode','Year','LocCode','TransectLenM')
  
  num <- num[order(num$LocCode,num$Year,num$VegType),]
  den <- den[order(den$LocCode,den$Year),]
  
  dat <- merge(num,den,by=c('UnitCode','Year','LocCode'),all.x=TRUE)
  dat$pCover <- 100*dat$SegLenM / dat$TransectLenM
  
  #   ---- Make sure this is rounded to two decimals.
  dat$pCover <- round(dat$pCover,2)
  
  dat <- dat[dat$VegType != "End",]
  
  
  #   ---- Need to make sure we have zero percentages where necessary.  Idea is to 
  #   ---- make a 'backbone' of data values for which we expect data, and merge in
  #   ---- what we have against it.  Any blanks must be zero.  Note that I include
  #   ---- VegType here.  
  dat2 <- unique(dat[,c('UnitCode','LocCode','Year')])
  dat2a <- cbind(dat2,VegType=rep("H"))
  dat2b <- cbind(dat2,VegType=rep("S"))  
  dat2c <- cbind(dat2,VegType=rep("T"))
  dat2d <- cbind(dat2,VegType=rep("D"))  
  dat2e <- cbind(dat2,VegType=rep("U"))
  dat3 <- rbind(dat2a,dat2b,dat2c,dat2d,dat2e)
  
  #   ---- Now clean up the data.  Some of this redudant, i.e., this isn't the most
  #   ---- efficient program in the world.  But not much data.  
  dat4 <- merge(dat3,dat,by=c('UnitCode','LocCode','Year','VegType'),all.x=TRUE)
  names(den)[names(den) == "TransectLenM"] <- "TransectAgain"
  dat5 <- merge(dat4,den,by=c('UnitCode','LocCode','Year'),all.x=TRUE)
  dat5$TransectLenM <- NULL
  names(dat5)[names(dat5) == "TransectAgain"] <- "TransectLenM"
  dat5[is.na(dat5)] <- 0
  dat <- dat5
  
  dat <- dat[order(dat$UnitCode,dat$LocCode,dat$VegType,dat$Year),]
  
  #   ---- Note that a104 is not called in the function.  
  dat <- merge(dat,a104[,c('Unit_code','Location_code','Panel_type')],by.x=c('UnitCode','LocCode'),by.y=c('Unit_code','Location_code'))
  dat <- dat[order(dat$UnitCode,dat$LocCode,dat$VegType,dat$Year),]
  dat <- dat[,c('UnitCode','Panel_type','LocCode','VegType','Year','SegLenM','TransectLenM','pCover')]
  
  
  #   ---- The process of putting in zeros may have created temporal trends for which
  #   ---- ALL observations are zero.  This is bad.  We need to get rid of these only.
  #   ---- Idea is to sum the pCover in a UnitCode, LocCode, VegType. 
  zeroCheck <- with(dat, aggregate(dat$pCover, data.frame(dat$UnitCode,dat$LocCode,dat$VegType), sum))
  names(zeroCheck) <- c('UnitCode','LocCode','VegType','pCoverSum')
  
  check <- merge(dat,zeroCheck,by=c('UnitCode','LocCode','VegType'),all.x=TRUE)
  dat <- check[check$pCoverSum > 0,]
  dat$pCoverSum <- NULL
  
  
#   #   ---- Get cover-code meanings.  
#   VegTypeCodes <- read.csv("C:/Users/jmitchell/Desktop/Prairie/Data/Lookups_20160711/tlu_Veg_Type.csv",stringsAsFactors=FALSE)
#   VegTypeCodes[VegTypeCodes$Veg_type == "D",]$Veg_desc <- "Developed"
#   VegTypeCodes[VegTypeCodes$Veg_type == "U",]$Veg_desc <- "Unvegetated"
#   dat <- merge(dat,VegTypeCodes,by.x=c('VegType'),by.y=c('Veg_type'),all.x=TRUE)
#   dat <- dat[order(dat$LocCode,dat$VegType,dat$Year),]
#   
#   dat$Color <- NA
#   dat$Color[dat$VegType == "D"] <- "red"
#   dat$Color[dat$VegType == "H"] <- "orange"
#   dat$Color[dat$VegType == "S"] <- "yellow"
#   dat$Color[dat$VegType == "T"] <- "green"
#   dat$Color[dat$VegType == "U"] <- "blue"
#   rownames(dat) <- NULL
  
  unitCodes <- unique(dat$UnitCode)
  
  #ls(dat,unitCodes)
  return(list(dat,unitCodes))
  
}