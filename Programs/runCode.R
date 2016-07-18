
require(rgdal)

progStem <- "//lar-file-srv/Data/NPS/Prairie/Programs"
dataStem <- "//lar-file-srv/Data/NPS/Prairie/Data/Data_files_20160711"

#progStem <- "C:/Users/jmitchell/Desktop/Prairie/Programs"
#dataStem <- "C:/Users/jmitchell/Desktop/Prairie/Data/Data_files_20160711"

projUTM10 <- "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"



source(paste0(progStem,"/","readData.R"))
source(paste0(progStem,"/","shift.R"))
source(paste0(progStem,"/","makeTransectShp.R"))

#   ---- Read in raw data.  
a104 <- readData(dataStem=dataStem,file="/qs_a104_Location_metadata_20160711_200543.txt",col.names=c("Park_code","Unit_code","Location_code","Location_type","Location_status","Location_name","Panel_name","Panel_type","Firing_order","UTME_public","UTMN_public","Public_offset","Location_desc","Location_notes","Loc_established"))
a114 <- readData(dataStem=dataStem,file="/qs_a114_Event_metadata_20160711_200634.txt",col.names=c("Park_code","Loc_code","Unit_code","Location_code","Calendar_year","Start_date","Start_time","End_date","End_time","Hours_spent","Transect_easting","Transect_northing","GPS_file_name","Transect_notes","Event_notes","Entered_by","Entered_date","Updated_by","Updated_date","Verified_by","Verified_date","Certified_by","Certified_date","Is_excluded","QA_notes"))
a124 <- readData(dataStem=dataStem,file="/qs_a124_Event_observers_20160711_200645.txt",col.names=c("Park_code","Loc_code","Unit_code","Location_code","Calendar_year","Start_date","Contact_ID","Observer_role","Observer_notes"))
a134 <- readData(dataStem=dataStem,file="/qs_a134_Phenology_obs_20160711_200656.txt",col.names=c("Park_code","Loc_code","Unit_code","Location_code","Calendar_year","Start_date","Species_code","Scientific_name","Stage_code","Stage_desc","Avg_height_m","Phenology_notes"))
a144 <- readData(dataStem=dataStem,file="/qs_a144_Transect_obs_20160711_200707.txt",col.names=c("Park_code","Loc_code","Unit_code","Location_code","Calendar_year","Start_date","Observation_ID","Segment_num","Segment_length_m","No_GPS","UTME","UTMN","Field_coord","Veg_type","Origin","Cover_class","Is_grazed","Substrate","Obs_notes","Flag","Office_notes"))


#   ---- See which locations tie to which years.
table3 <- as.data.frame.matrix(table(a114$Location_code,a114$Calendar_year))
table3$LocCode <- rownames(table3)
rownames(table3) <- NULL
table3 <- table3[,c('LocCode','2007','2008','2009','2012','2013','2014','2015')]




# Questions.
# 1.  What is the projection?  I assumed UTM-10N.
# 2.  Query a144 has only American Camp data?
# 3.  Does Table 2 on page 16 correspond with the look-up table?  
# 4.  Flesh out data with 0.0%?
# 5.  What does the use of the word "area" mean in Table 1?
# 6.  Figure 15:  "Nativeness" means "Origin" in query a144?
# 7.  Figure 15:  "Subveg" means "CoverClass" in query a144?


#   ---- Bring in helpful information from other queries.
a144$R_ID <- seq(1,nrow(a144))
a144.1 <- merge(a144,a104[a104$Unit_code == "AC",c('Location_code','Panel_type')],by=c('Location_code'),all.x=TRUE)
a144 <- a144[order(a144$R_ID),]

#   ---- Clean up the dataset.
#a144 <- a144[order(a144$Park_code,a144$Unit_code,a144$Location_code,a144$Calendar_year,a144$Segment_num),]
a144$StartDate <- as.POSIXlt(strptime(a144$Start_date,format="%m/%d/%Y %H:%M:%S",tz="America/Los_Angeles"),tz="America/Los_Angeles")
a144$Obs_notes <- a144$Office_notes <- a144$Field_coord <- NULL

#   ---- Rename variables for nice attribute table variable names.
names(a144)[names(a144) == "Park_code"] <- "ParkCode" 
names(a144)[names(a144) == "Loc_code"] <- "LocID" 
names(a144)[names(a144) == "Unit_code"] <- "UnitCode" 
names(a144)[names(a144) == "Location_code"] <- "LocCode" 
names(a144)[names(a144) == "Calendar_year"] <- "Year" 
names(a144)[names(a144) == "Observation_ID"] <- "ObsID" 
names(a144)[names(a144) == "Segment_num"] <- "SegNum" 
names(a144)[names(a144) == "Segment_length_m"] <- "SegNumM" 
names(a144)[names(a144) == "No_GPS"] <- "NoGPS" 
names(a144)[names(a144) == "Veg_type"] <- "VegType" 
names(a144)[names(a144) == "Cover_class"] <- "CoverClass" 
names(a144)[names(a144) == "Is_grazed"] <- "IsGrazed" 
names(a144)[names(a144) == "Veg_type"] <- "VegType" 
names(a144)[names(a144) == "Cover_class"] <- "CoverClass" 
names(a144)[names(a144) == "Panel_type"] <- "PanelType" 

a144$R_ID <- seq(1,nrow(a144))

#   ---- Calculate the 1-lead for some variables.  
a144$LocCodel1 <- shift(a144$LocCode,1)
a144$Yearl1 <- shift(a144$Year,1)
a144$VegTypel1 <- shift(a144$VegType,1)

#   ---- Calculate indicators for making correct shapefiles.
a144$Transects <- ifelse( a144$VegType == "End" | a144$VegTypel1 == "End",1,0)
a144$End <- ifelse( a144$VegType == "End",1,0)

#   ---- Make transect shapefiles.  
makeTransectShp(a144,type="Walked",proj=projUTM10)     

#   ---- Get historical trends of Table 2.
table2 <- read.csv("//lar-file-srv/Data/NPS/Prairie/Data/Table2.csv",stringsAsFactors=FALSE)

#   Bare Earth             Unvegetated    U
#   Buildings              Developed      D
#   Forest                 Tree           T
#   Managed Grassland      ? Shrub ?      S
#   Prairie                ? Herbaceous ? H
#   Roads                  Developed      D
#   Water 





#   ---- Build data for graphical displays of trend of VegType over time.
num <- with(a144, aggregate(a144$SegNumM, data.frame(a144$UnitCode,a144$Year,a144$LocCode,a144$VegType), sum))
den <- with(a144, aggregate(a144$SegNumM, data.frame(a144$UnitCode,a144$Year,a144$LocCode), sum))

colnames(num) <- c('UnitCode','Year','LocCode','VegType','SegLenM')
colnames(den) <- c('UnitCode','Year','LocCode','TransectLenM')

num <- num[order(num$LocCode,num$Year,num$VegType),]
den <- den[order(den$LocCode,den$Year),]

dat <- merge(num,den,by=c('UnitCode','Year','LocCode'),all.x=TRUE)
dat$pCover <- 100*dat$SegLenM / dat$TransectLenM
dat <- dat[dat$VegType != "End",]

dat <- merge(dat,a104[,c('Unit_code','Location_code','Panel_type')],by.x=c('UnitCode','LocCode'),by.y=c('Unit_code','Location_code'))

#   ---- Get cover-code meanings.  
VegTypeCodes <- read.csv("C:/Users/jmitchell/Desktop/Prairie/Data/Lookups_20160711/tlu_Veg_Type.csv",stringsAsFactors=FALSE)
VegTypeCodes[VegTypeCodes$Veg_type == "D",]$Veg_desc <- "Developed"
VegTypeCodes[VegTypeCodes$Veg_type == "U",]$Veg_desc <- "Unvegetated"
dat <- merge(dat,VegTypeCodes,by.x=c('VegType'),by.y=c('Veg_type'),all.x=TRUE)
dat <- dat[order(dat$LocCode,dat$VegType,dat$Year),]

dat$Color <- NA
dat$Color[dat$VegType == "D"] <- "red"
dat$Color[dat$VegType == "H"] <- "orange"
dat$Color[dat$VegType == "S"] <- "yellow"
dat$Color[dat$VegType == "T"] <- "green"
dat$Color[dat$VegType == "U"] <- "blue"


unitCodes <- unique(dat$UnitCode)
for(h in 1:length(unitCodes)){
  
  unitCode <- as.character(droplevels(unitCodes[h]))
  lilTable2 <- table2[table2$UnitCode == unitCode,]

  locCodes <- unique(dat$LocCode)
  for(i in 1:length(locCodes)){
    
    locCode <- as.character(droplevels(locCodes[i]))
    lilDat <- dat[dat$LocCode == locCode,]
    vegTypes <- unique(lilDat$VegType)
    Panel_type <- lilDat$Panel_type[1]
    
    saveStem <- "//lar-file-srv/Data/NPS/Prairie/Analysis/Trending"
    pngTitle <- paste0(saveStem,"/Temporal Trends -- ",unitCode," -- ",Panel_type," Transect ",locCode,".png")
    
    png(pngTitle,width=11,height=8.5,units="in",res=600)
    
    for(j in 1:length(vegTypes)){
      
      if(unitCode == "AC"){
        niceUnitCode <- "American Camp"
      } else {
        niceUnitCode <- "English Camp"
      }
      
      title <- paste0("Temporal Trends -- ",niceUnitCode,":  ",Panel_type," Transect ",locCode)
      
      x <- lilDat[lilDat$VegType == vegTypes[j],]$Year 
      y <- lilDat[lilDat$VegType == vegTypes[j],]$pCover
      c <- lilDat[lilDat$VegType == vegTypes[j],]$Color
    
      if(j == 1){
        plot(x,y,xlim=c(2007,2015),ylim=c(0,100),xlab="Year",ylab="Percent Cover",col=c,pch=19,main=title)
        par(new=TRUE)
        plot(x,y,xaxt="n",yaxt="n",xlim=c(2007,2015),ylim=c(0,100),xlab="",ylab="",col=c,type='l')
      } else {
        par(new=TRUE)
        plot(x,y,xaxt="n",yaxt="n",xlim=c(2007,2015),ylim=c(0,100),xlab="",ylab="",col=c,pch=19)
        par(new=TRUE)
        plot(x,y,xaxt="n",yaxt="n",xlim=c(2007,2015),ylim=c(0,100),xlab="",ylab="",col=c,type='l')
      }
      
      #    ---- Make a legend specific to this plot. 
      legendDat <- unique(lilDat[,c('Veg_desc','Color')])
      legend("left",legendDat$Veg_desc,col=legendDat$Color,pch=rep(19,nrow(legendDat)),lwd=rep(1,nrow(legendDat)))
    }
    dev.off()
  }
}


















#   ---- Build data for graphical displays of trend of CoverClass over time.

theH <- a144[a144$VegType == "H" & !is.na(a144$VegType),]
theH$CoverClassT <- ifelse(theH$Origin == "N",paste0("E-",theH$CoverClass),
                           ifelse(theH$Origin == "E",paste0("N-",theH$CoverClass),
                                  "Other"))
num <- with(theH, aggregate(theH$SegNumM, data.frame(theH$UnitCode,theH$Year,theH$LocCode,theH$CoverClassT), sum))
den <- with(theH, aggregate(theH$SegNumM, data.frame(theH$UnitCode,theH$Year,theH$LocCode), sum))

colnames(num) <- c('UnitCode','Year','LocCode','CoverClassT','SegLenM')
colnames(den) <- c('UnitCode','Year','LocCode','HTransectLenM')

num <- num[order(num$LocCode,num$Year,num$CoverClassT),]
den <- den[order(den$LocCode,den$Year),]

dat <- merge(num,den,by=c('UnitCode','Year','LocCode'),all.x=TRUE)
dat$pCover <- 100*dat$SegLenM / dat$HTransectLenM
#dat <- dat[dat$VegType != "End",]

dat <- merge(dat,a104[,c('Unit_code','Location_code','Panel_type')],by.x=c('UnitCode','LocCode'),by.y=c('Unit_code','Location_code'))

#   ---- Get cover-code meanings.  
# VegTypeCodes <- read.csv("C:/Users/jmitchell/Desktop/Prairie/Data/Lookups_20160711/tlu_Veg_Type.csv",stringsAsFactors=FALSE)
# VegTypeCodes[VegTypeCodes$Veg_type == "D",]$Veg_desc <- "Developed"
# VegTypeCodes[VegTypeCodes$Veg_type == "U",]$Veg_desc <- "Unvegetated"
# dat <- merge(dat,VegTypeCodes,by.x=c('VegType'),by.y=c('Veg_type'),all.x=TRUE)
dat <- dat[order(dat$LocCode,dat$CoverClassT,dat$Year),]

dat$Color <- NA
dat$Color[dat$CoverClassT == "E-0-10%"] <- "pink"
dat$Color[dat$CoverClassT == "E-11-49%"] <- "red"
dat$Color[dat$CoverClassT == "E-50-100%"] <- "red4"
dat$Color[dat$CoverClassT == "N-0-10%"] <- "lightgreen"
dat$Color[dat$CoverClassT == "N-11-49%"] <- "green"
dat$Color[dat$CoverClassT == "N-50-100%"] <- "darkgreen"


unitCodes <- unique(dat$UnitCode)
for(h in 1:length(unitCodes)){
  
  unitCode <- as.character(droplevels(unitCodes[h]))
  
  locCodes <- unique(dat$LocCode)
  for(i in 1:length(locCodes)){
    
    locCode <- as.character(droplevels(locCodes[i]))
    lilDat <- dat[dat$LocCode == locCode,]
    lilDat$CoverClassT <- as.character(droplevels(lilDat$CoverClassT))
    coverClassTs <- unique(lilDat$CoverClassT)
    Panel_type <- lilDat$Panel_type[1]
    
    saveStem <- "//lar-file-srv/Data/NPS/Prairie/Analysis/Trending"
    pngTitle <- paste0(saveStem,"/Temporal Trends -- Cover Class -- ",unitCode," -- ",Panel_type," Transect ",locCode,".png")
    
    png(pngTitle,width=11,height=8.5,units="in",res=600)
    
    for(j in 1:length(coverClassTs)){
      
      if(unitCode == "AC"){
        niceUnitCode <- "American Camp"
      } else {
        niceUnitCode <- "English Camp"
      }
      
      title <- paste0("Temporal Trends -- Cover Class -- ",niceUnitCode,":  ",Panel_type," Transect ",locCode)
      
      x <- lilDat[lilDat$CoverClassT == coverClassTs[j],]$Year 
      y <- lilDat[lilDat$CoverClassT == coverClassTs[j],]$pCover
      c <- lilDat[lilDat$CoverClassT == coverClassTs[j],]$Color
      
      if(j == 1){
        plot(x,y,xlim=c(2007,2015),ylim=c(0,100),xlab="Year",ylab="Percent Cover",col=c,pch=19,main=title)
        par(new=TRUE)
        plot(x,y,xaxt="n",yaxt="n",xlim=c(2007,2015),ylim=c(0,100),xlab="",ylab="",col=c,type='l')
      } else {
        par(new=TRUE)
        plot(x,y,xaxt="n",yaxt="n",xlim=c(2007,2015),ylim=c(0,100),xlab="",ylab="",col=c,pch=19)
        par(new=TRUE)
        plot(x,y,xaxt="n",yaxt="n",xlim=c(2007,2015),ylim=c(0,100),xlab="",ylab="",col=c,type='l')
      }
      
      #    ---- Make a legend specific to this plot. 
      legendDat <- unique(lilDat[,c('CoverClassT','Color')])
      legend("left",legendDat$CoverClassT,col=legendDat$Color,pch=rep(19,nrow(legendDat)),lwd=rep(1,nrow(legendDat)))
    }
    dev.off()
  }
}


