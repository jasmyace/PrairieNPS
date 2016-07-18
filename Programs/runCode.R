
require(rgdal)

progStem <- "//lar-file-srv/Data/NPS/Prairie/Programs"
dataStem <- "//lar-file-srv/Data/NPS/Prairie/Data/Data_files_20160711"
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
# 2.  Query a144 has only American Camp data.


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




num <- with(a144, aggregate(a144$SegNumM, data.frame(a144$Year,a144$LocCode,a144$VegType), sum))
den <- with(a144, aggregate(a144$SegNumM, data.frame(a144$Year,a144$LocCode), sum))

colnames(num) <- c('Year','LocCode','VegType','SegLenM')
colnames(den) <- c('Year','LocCode','TransectLenM')

num <- num[order(num$LocCode,num$Year,num$VegType),]
den <- den[order(den$LocCode,den$Year),]

dat <- merge(num,den,by=c('Year','LocCode'),all.x=TRUE)
dat$pCover <- 100*dat$SegLenM / dat$TransectLenM
dat <- dat[dat$VegType != "End",]

dat <- dat[order(dat$LocCode,dat$VegType,dat$Year),]

dat$Color <- NA
dat$Color[dat$VegType == "D"] <- "red"
dat$Color[dat$VegType == "H"] <- "orange"
dat$Color[dat$VegType == "S"] <- "yellow"
dat$Color[dat$VegType == "T"] <- "green"
dat$Color[dat$VegType == "U"] <- "blue"

locCodes <- unique(dat$LocCode)
for(i in 1:length(locCodes)){
  
  lilDat <- dat[dat$LocCode == locCodes[i],]
  vegTypes <- unique(lilDat$VegType)
  
  for(j in 1:length(vegTypes)){
    
    x <- lilDat[lilDat$VegType == vegTypes[j],]$Year 
    y <- lilDat[lilDat$VegType == vegTypes[j],]$pCover
    c <- lilDat[lilDat$VegType == vegTypes[j],]$Color
  
    if(j > 1){par(new=TRUE)}
    plot(x,y,xlim=c(2007,2015),ylim=c(0,100),col=c,pch=19)
    par(new=TRUE)
    plot(x,y,xlim=c(2007,2015),ylim=c(0,100),col=c,type='l')
  }
}




