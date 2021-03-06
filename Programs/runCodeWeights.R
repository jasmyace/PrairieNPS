
require(rgdal)
require(RColorBrewer)
require(png)
require(rgeos)
require(jpeg)
require(lme4)
require(pbkrtest)                # Kenwood-Roger (1997) approximation to Satterthwaite's degrees of freedom.
require(classInt)                # function classIntervals in plotSpatialTrends function


progStem <- "//lar-file-srv/Data/NPS/Prairie/Programs"
dataStem <- "//lar-file-srv/Data/NPS/Prairie/Data/Data_files_20160711"

#progStem <- "C:/Users/jmitchell/Desktop/Prairie/Programs"
#dataStem <- "C:/Users/jmitchell/Desktop/Prairie/Data/Data_files_20160711"

projUTM10 <- "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

tc <- 2007

source(paste0(progStem,"/","readData.R"))
source(paste0(progStem,"/","shift.R"))
source(paste0(progStem,"/","makeTransectShp.R"))
source(paste0(progStem,"/","makeVerticalPerfectTransectShp.R"))
source(paste0(progStem,"/","prepDataObj1.R"))
source(paste0(progStem,"/","prepDataObj2.R"))
source(paste0(progStem,"/","prepDataObj3.R"))
source(paste0(progStem,"/","plotAllObj1.R"))
source(paste0(progStem,"/","plotHistObj1.R"))
source(paste0(progStem,"/","plotEmpty.R"))
source(paste0(progStem,"/","plotText.R"))
source(paste0(progStem,"/","plotOneObj1.R"))
source(paste0(progStem,"/","plotTransectMap.R"))
source(paste0(progStem,"/","plotResidFit.R"))
source(paste0(progStem,"/","plotResidQQ.R"))
source(paste0(progStem,"/","plotResidHist.R"))
source(paste0(progStem,"/","plotResidGroupQQ.R"))
source(paste0(progStem,"/","plotResidGroupBox.R"))
source(paste0(progStem,"/","getEmpiricalVarComp.R"))
source(paste0(progStem,"/","plotRandCorr.R"))
source(paste0(progStem,"/","plotQQByYear.R"))
source(paste0(progStem,"/","plotSpatialResids.R"))
source(paste0(progStem,"/","plotSpatialTrends.R"))
source(paste0(progStem,"/","plotRanEfQQ.R"))
source(paste0(progStem,"/","betaTable.R"))
source(paste0(progStem,"/","makeBigPlotTrendsObj1.R"))
source(paste0(progStem,"/","makeBigPlotTrendsObj2.R"))
source(paste0(progStem,"/","makeBigPlotTrendsObj3.R"))
source(paste0(progStem,"/","makeBigPlotAssumptionsObj1.R"))
source(paste0(progStem,"/","makeBigPlotAssumptionsObj2.R"))
source(paste0(progStem,"/","makeBigPlotAssumptionsObj3.R"))




#   ---- Specify display of k decimals.  
#   ---- http://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
specDec <- function(x, k) format(round(x, k), nsmall=k)

#   ---- Read in raw data.  
a104 <- readData(dataStem=dataStem,file="/qs_a104_Location_metadata_20160711_200543.txt",col.names=c("Park_code","Unit_code","Location_code","Location_type","Location_status","Location_name","Panel_name","Panel_type","Firing_order","UTME_public","UTMN_public","Public_offset","Location_desc","Location_notes","Loc_established"))
a114 <- readData(dataStem=dataStem,file="/qs_a114_Event_metadata_20160711_200634.txt",col.names=c("Park_code","Loc_code","Unit_code","Location_code","Calendar_year","Start_date","Start_time","End_date","End_time","Hours_spent","Transect_easting","Transect_northing","GPS_file_name","Transect_notes","Event_notes","Entered_by","Entered_date","Updated_by","Updated_date","Verified_by","Verified_date","Certified_by","Certified_date","Is_excluded","QA_notes"))
a124 <- readData(dataStem=dataStem,file="/qs_a124_Event_observers_20160711_200645.txt",col.names=c("Park_code","Loc_code","Unit_code","Location_code","Calendar_year","Start_date","Contact_ID","Observer_role","Observer_notes"))
a134 <- readData(dataStem=dataStem,file="/qs_a134_Phenology_obs_20160711_200656.txt",col.names=c("Park_code","Loc_code","Unit_code","Location_code","Calendar_year","Start_date","Species_code","Scientific_name","Stage_code","Stage_desc","Avg_height_m","Phenology_notes"))
a144 <- readData(dataStem=dataStem,file="/qs_a144_Transect_obs_20160711_200707.txt",col.names=c("Park_code","Loc_code","Unit_code","Location_code","Calendar_year","Start_date","Observation_ID","Segment_num","Segment_length_m","No_GPS","UTME","UTMN","Field_coord","Veg_type","Origin","Cover_class","Is_grazed","Substrate","Obs_notes","Flag","Office_notes"))

vpTransects <- read.csv(paste0("//lar-file-srv/Data/NPS/Prairie/Data/bTransectCoordinatesUTM10N.csv"),stringsAsFactors=FALSE)
vpTransects$LocCodel1 <- shift(vpTransects$LocCode,1)
vpTransects$R_ID <- seq(1,nrow(vpTransects),1)

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

#   ---- Make walked transect shapefiles.  
makeTransectShp(a144,type="Walked",proj=projUTM10)    

#   ---- Make vertically perfect transect shapefiles.
makeVerticalPerfectTransectShp(vpTransects,type="",proj=projUTM10)

#   ---- Get historical trends of Table 2.
table2 <- read.csv("//lar-file-srv/Data/NPS/Prairie/Data/Table2.csv",stringsAsFactors=FALSE)

#   Bare Earth             Unvegetated    U
#   Buildings              Developed      D
#   Forest                 Tree           T
#   Managed Grassland      ? Shrub ?      S
#   Prairie                ? Herbaceous ? H
#   Roads                  Developed      D
#   Water 









#   ---- OBJECTIVE 1.

#   ---- Build data for graphical displays of trend of VegType over time.
obj1Stuff <- prepDataObj1(a144)
dat <- obj1Stuff[[1]]
unitCodes <- obj1Stuff[[2]]

#   ---- Restrict to Herbaceous, Shrub, Tress, Developed, and Unvegetated.
#   ---- I.e., all groups.  This is a change from a previous restriction.  
dat <- dat[dat$VegType %in% c("H","S","T","D","U"),]

#   ---- Restrict trend analyses for Objective 1 to those from Annual transects.
#dat <- dat[dat$Panel_type == "Annual",]

#   ---- Identify the transects with at least two time points. 
theSet <- unique(data.frame(dat$LocCode,dat$Year))
colnames(theSet) <- c("LocCode","Year")
theSet <- theSet[order(theSet$LocCode,theSet$Year),]
theCounts <- data.frame(counts=tapply(theSet$LocCode,list(theSet$LocCode),length))
theCounts$LocCode <- rownames(theCounts)
dat <- merge(dat,theCounts,by=c("LocCode"),all.x=TRUE)
dat <- dat[dat$counts >= 2,]
dat$counts <- NULL

#   ---- Order in a logical way. 
dat <- dat[order(dat$UnitCode,dat$LocCode,dat$VegType,dat$Year),]

#   ---- Center the data.  
dat$WYear <- dat$Year - tc

#   ---- Preserve the original data values, prior to epsilon manipulaton. 
dat$pCoveru <- dat$pCover

#   ---- Manipulating data.  Warton, D. I. and Hui, F. K. C.  The arcsince is asinine:
#   ---- the analysis of proportions in ecology.  Ecology, 92(1), 2011, pp. 3-10.
datH <- dat[dat$VegType == "H",]
epsilonH <- min(datH[datH$pCover != 0,]$pCover,100 - datH[datH$pCover != 100,]$pCover)
datH$pCover <- ifelse(datH$pCover == 0,epsilonH,
                      ifelse(datH$pCover == 100,100 - epsilonH,datH$pCover))
datS <- dat[dat$VegType == "S",]
epsilonS <- min(datS[datS$pCover != 0,]$pCover,100 - datS[datS$pCover != 100,]$pCover)
datS$pCover <- ifelse(datS$pCover == 0,epsilonS,
                      ifelse(datS$pCover == 100,100 - epsilonS,datS$pCover))
datT <- dat[dat$VegType == "T",]
epsilonT <- min(datT[datT$pCover != 0,]$pCover,100 - datT[datT$pCover != 100,]$pCover)
datT$pCover <- ifelse(datT$pCover == 0,epsilonT,
                      ifelse(datT$pCover == 100,100 - epsilonT,datT$pCover))
datD <- dat[dat$VegType == "D",]
epsilonD <- min(datD[datD$pCover != 0,]$pCover,100 - datD[datD$pCover != 100,]$pCover)
datD$pCover <- ifelse(datD$pCover == 0,epsilonD,
                      ifelse(datD$pCover == 100,100 - epsilonD,datD$pCover))
datU <- dat[dat$VegType == "U",]
epsilonU <- min(datU[datU$pCover != 0,]$pCover,100 - datU[datU$pCover != 100,]$pCover)
datU$pCover <- ifelse(datU$pCover == 0,epsilonU,
                      ifelse(datU$pCover == 100,100 - epsilonU,datU$pCover))
dat <- rbind(datH,datS,datT,datD,datU)

#   ---- Create transformed outcomes. 
#   ---- User pCover for the logistic, to ensure we use the epsilon manpulated values. 
dat$pCoverl <- log(dat$pCover / (100 - dat$pCover))

#   ---- But we use the original pCover (pCoveru) for the arcsine transformation.  
dat$pCovera <- asin(sqrt(dat$pCoveru/100))

#   ---- Rename pCover.  It has served its purpose. 
names(dat)[names(dat) == "pCover"] <- "pCoverEpsilon"

dat$LocCode <- paste0(dat$UnitCode,".",dat$LocCode)
write.csv(dat,"//lar-file-srv/Data/NPS/Prairie/Analysis/Trending/datObjective1.csv",row.names=FALSE)

#   ---- Fit all 15 full models.  
fitHu <- lmer(pCoveru ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "H",]$TransectLenM/1000,data=dat[dat$VegType == "H",])
fitSu <- lmer(pCoveru ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "S",]$TransectLenM/1000,data=dat[dat$VegType == "S",])
fitTu <- lmer(pCoveru ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "T",]$TransectLenM/1000,data=dat[dat$VegType == "T",])
fitDu <- lmer(pCoveru ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "D",]$TransectLenM/1000,data=dat[dat$VegType == "D",])
fitUu <- lmer(pCoveru ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "U",]$TransectLenM/1000,data=dat[dat$VegType == "U",])

fitHl <- lmer(pCoverl ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "H",]$TransectLenM/1000,data=dat[dat$VegType == "H",])
fitSl <- lmer(pCoverl ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "S",]$TransectLenM/1000,data=dat[dat$VegType == "S",])
fitTl <- lmer(pCoverl ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "T",]$TransectLenM/1000,data=dat[dat$VegType == "T",])
fitDl <- lmer(pCoverl ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "D",]$TransectLenM/1000,data=dat[dat$VegType == "D",])
fitUl <- lmer(pCoverl ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "U",]$TransectLenM/1000,data=dat[dat$VegType == "U",])

fitHa <- lmer(pCovera ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "H",]$TransectLenM/1000,data=dat[dat$VegType == "H",])
fitSa <- lmer(pCovera ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "S",]$TransectLenM/1000,data=dat[dat$VegType == "S",])
fitTa <- lmer(pCovera ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "T",]$TransectLenM/1000,data=dat[dat$VegType == "T",])
fitDa <- lmer(pCovera ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "D",]$TransectLenM/1000,data=dat[dat$VegType == "D",])
fitUa <- lmer(pCovera ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "U",]$TransectLenM/1000,data=dat[dat$VegType == "U",])


#   ---- Fit all 15 reduced models.  We do this to test for trend. 
rfitHu <- lmer(pCoveru ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "H",]$TransectLenM/1000,data=dat[dat$VegType == "H",])
rfitSu <- lmer(pCoveru ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "S",]$TransectLenM/1000,data=dat[dat$VegType == "S",])
rfitTu <- lmer(pCoveru ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "T",]$TransectLenM/1000,data=dat[dat$VegType == "T",])
rfitDu <- lmer(pCoveru ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "D",]$TransectLenM/1000,data=dat[dat$VegType == "D",])
rfitUu <- lmer(pCoveru ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "U",]$TransectLenM/1000,data=dat[dat$VegType == "U",])

rfitHl <- lmer(pCoverl ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "H",]$TransectLenM/1000,data=dat[dat$VegType == "H",])
rfitSl <- lmer(pCoverl ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "S",]$TransectLenM/1000,data=dat[dat$VegType == "S",])
rfitTl <- lmer(pCoverl ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "T",]$TransectLenM/1000,data=dat[dat$VegType == "T",])
rfitDl <- lmer(pCoverl ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "D",]$TransectLenM/1000,data=dat[dat$VegType == "D",])
rfitUl <- lmer(pCoverl ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "U",]$TransectLenM/1000,data=dat[dat$VegType == "U",])

rfitHa <- lmer(pCovera ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "H",]$TransectLenM/1000,data=dat[dat$VegType == "H",])
rfitSa <- lmer(pCovera ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "S",]$TransectLenM/1000,data=dat[dat$VegType == "S",])
rfitTa <- lmer(pCovera ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "T",]$TransectLenM/1000,data=dat[dat$VegType == "T",])
rfitDa <- lmer(pCovera ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "D",]$TransectLenM/1000,data=dat[dat$VegType == "D",])
rfitUa <- lmer(pCovera ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "U",]$TransectLenM/1000,data=dat[dat$VegType == "U",])


#   ---- Read in a representative unstraightened shapefile. 
dir <- "//lar-file-srv/Data/NPS/Prairie/Analysis/Shapefiles"
shp <- "AC - 2015 - Walked"
transects <- readOGR(dir,shp)

#   ---- Read in a representative straightened shapefile. 
dir <- "//lar-file-srv/Data/NPS/Prairie/Analysis/Shapefiles"
shp <- "vpTransects"
STransects <- readOGR(dir,shp)

#   ---- Clean up shapefile for use. 
STransects@data$LocCode <- unlist(strsplit(as.character(droplevels(STransects@data$LocCode)),split=".",fixed=TRUE))[c(FALSE,TRUE)]


#   ---- Assumes all necessary objects are in the Global Environment.  
makeBigPlotTrendsObj1(tc)
makeBigPlotAssumptionsObj1(tc)

#   ---- Build table of model statistics.  
betaPCoverHu <- betaTable(fitHu,rfitHu,outcome="pCoverHu")[[1]]
betaPCoverHl <- betaTable(fitHl,rfitHl,outcome="pCoverHl")[[1]]
betaPCoverHa <- betaTable(fitHa,rfitHa,outcome="pCoverHa")[[1]]
betaPCoverSu <- betaTable(fitSu,rfitSu,outcome="pCoverSu")[[1]]
betaPCoverSl <- betaTable(fitSl,rfitSl,outcome="pCoverSl")[[1]]
betaPCoverSa <- betaTable(fitSa,rfitSa,outcome="pCoverSa")[[1]]
betaPCoverTu <- betaTable(fitTu,rfitTu,outcome="pCoverTu")[[1]]
betaPCoverTl <- betaTable(fitTl,rfitTl,outcome="pCoverTl")[[1]]
betaPCoverTa <- betaTable(fitTa,rfitTa,outcome="pCoverTa")[[1]]
betaPCoverDu <- betaTable(fitDu,rfitDu,outcome="pCoverDu")[[1]]
betaPCoverDl <- betaTable(fitDl,rfitDl,outcome="pCoverDl")[[1]]
betaPCoverDa <- betaTable(fitDa,rfitDa,outcome="pCoverDa")[[1]]
betaPCoverUu <- betaTable(fitUu,rfitUu,outcome="pCoverUu")[[1]]
betaPCoverUl <- betaTable(fitUl,rfitUl,outcome="pCoverUl")[[1]]
betaPCoverUa <- betaTable(fitUa,rfitUa,outcome="pCoverUa")[[1]]
parm <- betaTable(fitTa,rfitTa,outcome="pCoverTa")[[2]]

allBetasObj1 <- cbind(parm,betaPCoverHu,betaPCoverHl,betaPCoverHa,betaPCoverSu,betaPCoverSl,betaPCoverSa,betaPCoverTu,betaPCoverTl,betaPCoverTa
                      ,betaPCoverDu,betaPCoverDl,betaPCoverDa,betaPCoverUu,betaPCoverUl,betaPCoverUa)

write.csv(allBetasObj1,"//lar-file-srv/Data/NPS/Prairie/Analysis/Trending/betasObj1.csv")

























#   ---- OBJECTIVE 2.

#   ---- Delete out previous model results, just to be safe.  
rm(list=c("fitHu","fitSu","fitTu","fitDu","fitUu","fitHl","fitSl","fitTl","fitDl","fitUl","fitHa","fitSa","fitTa","fitDa","fitUa",
          "rfitHu","rfitSu","rfitTu","rfitDu","rfitUu","rfitHl","rfitSl","rfitTl","rfitDl","rfitUl","rfitHa","rfitSa","rfitTa","rfitDa","rfitUa"),envir=.GlobalEnv)

#   ---- Build data for graphical displays of trend of VegType over time.
obj2Stuff <- prepDataObj2(a144)
dat <- obj2Stuff[[1]]
unitCodes <- obj2Stuff[[2]]

#   ---- Identify the transects with at least two time points. 
theSet <- unique(data.frame(dat$LocCode,dat$Year))
colnames(theSet) <- c("LocCode","Year")
theSet <- theSet[order(theSet$LocCode,theSet$Year),]
theCounts <- data.frame(counts=tapply(theSet$LocCode,list(theSet$LocCode),length))
theCounts$LocCode <- rownames(theCounts)
dat <- merge(dat,theCounts,by=c("LocCode"),all.x=TRUE)
dat <- dat[dat$counts >= 2,]
dat$counts <- NULL

#   ---- Order in a logical way. 
dat <- dat[order(dat$UnitCode,dat$LocCode,dat$VegType,dat$Year,dat$Origin),]

#   ---- Center the data.  
dat$WYear <- dat$Year - tc

#   ---- Preserve the original data values, prior to epsilon manipulaton. 
dat$pCoveru <- dat$pCover

#   ---- Manipulating data.  Warton, D. I. and Hui, F. K. C.  The arcsince is asinine:
#   ---- the analysis of proportions in ecology.  Ecology, 92(1), 2011, pp. 3-10.
datH <- dat[dat$VegType == "H",]
epsilonH <- min(datH[datH$pCover != 0,]$pCover,100 - datH[datH$pCover != 100,]$pCover)
datH$pCover <- ifelse(datH$pCover == 0,epsilonH,
                      ifelse(datH$pCover == 100,100 - epsilonH,datH$pCover))
datS <- dat[dat$VegType == "S",]
epsilonS <- min(datS[datS$pCover != 0,]$pCover,100 - datS[datS$pCover != 100,]$pCover)
datS$pCover <- ifelse(datS$pCover == 0,epsilonS,
                      ifelse(datS$pCover == 100,100 - epsilonS,datS$pCover))
datT <- dat[dat$VegType == "T",]
epsilonT <- min(datT[datT$pCover != 0,]$pCover,100 - datT[datT$pCover != 100,]$pCover)
datT$pCover <- ifelse(datT$pCover == 0,epsilonT,
                      ifelse(datT$pCover == 100,100 - epsilonT,datT$pCover))
datA <- dat[dat$VegType == "A",]
epsilonA <- min(datA[datA$pCover != 0,]$pCover,100 - datA[datA$pCover != 100,]$pCover)
datA$pCover <- ifelse(datA$pCover == 0,epsilonA,
                      ifelse(datA$pCover == 100,100 - epsilonA,datA$pCover))
dat <- rbind(datH,datS,datT,datA)

#   ---- Create transformed outcomes. 
#   ---- User pCover for the logistic, to ensure we use the epsilon manpulated values. 
dat$pCoverl <- log(dat$pCover / (100 - dat$pCover))

#   ---- But we use the original pCover (pCoveru) for the arcsine transformation.  
dat$pCovera <- asin(sqrt(dat$pCoveru/100))

#   ---- Rename pCover.  It has served its purpose. 
names(dat)[names(dat) == "pCover"] <- "pCoverEpsilon"

dat$LocCode <- paste0(dat$UnitCode,".",dat$LocCode)
write.csv(dat,"//lar-file-srv/Data/NPS/Prairie/Analysis/Trending/datObjective2.csv",row.names=FALSE)

#   ---- Fit all 15 full models.  
fitHu <- lmer(pCoveru ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "H" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "H" & dat$Origin == "E",])
fitSu <- lmer(pCoveru ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "S" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "S" & dat$Origin == "E",])
fitTu <- lmer(pCoveru ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "T" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "T" & dat$Origin == "E",])
fitAu <- lmer(pCoveru ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "A" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "A" & dat$Origin == "E",])

fitHl <- lmer(pCoverl ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "H" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "H" & dat$Origin == "E",])
fitSl <- lmer(pCoverl ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "S" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "S" & dat$Origin == "E",])
fitTl <- lmer(pCoverl ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "T" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "T" & dat$Origin == "E",])
fitAl <- lmer(pCoverl ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "A" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "A" & dat$Origin == "E",])

fitHa <- lmer(pCovera ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "H" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "H" & dat$Origin == "E",])
fitSa <- lmer(pCovera ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "S" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "S" & dat$Origin == "E",])
fitTa <- lmer(pCovera ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "T" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "T" & dat$Origin == "E",])
fitAa <- lmer(pCovera ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "A" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "A" & dat$Origin == "E",])


#   ---- Fit all 15 reduced models.  We do this to test for trend. 
rfitHu <- lmer(pCoveru ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "H" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "H" & dat$Origin == "E",])
rfitSu <- lmer(pCoveru ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "S" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "S" & dat$Origin == "E",])
rfitTu <- lmer(pCoveru ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "T" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "T" & dat$Origin == "E",])
rfitAu <- lmer(pCoveru ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "A" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "A" & dat$Origin == "E",])

rfitHl <- lmer(pCoverl ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "H" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "H" & dat$Origin == "E",])
rfitSl <- lmer(pCoverl ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "S" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "S" & dat$Origin == "E",])
rfitTl <- lmer(pCoverl ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "T" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "T" & dat$Origin == "E",])
rfitAl <- lmer(pCoverl ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "A" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "A" & dat$Origin == "E",])

rfitHa <- lmer(pCovera ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "H" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "H" & dat$Origin == "E",])
rfitSa <- lmer(pCovera ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "S" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "S" & dat$Origin == "E",])
rfitTa <- lmer(pCovera ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "T" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "T" & dat$Origin == "E",])
rfitAa <- lmer(pCovera ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$VegType == "A" & dat$Origin == "E",]$TransectLenM/1000,data=dat[dat$VegType == "A" & dat$Origin == "E",])


#   ---- Read in a representative unstraightened shapefile. 
# dir <- "//lar-file-srv/Data/NPS/Prairie/Analysis/Shapefiles"
# shp <- "AC - 2015 - Walked"
# transects <- readOGR(dir,shp)

#   ---- Read in a representative straightened shapefile. 
dir <- "//lar-file-srv/Data/NPS/Prairie/Analysis/Shapefiles"
shp <- "vpTransects"
STransects <- readOGR(dir,shp)

#   ---- Clean up shapefile for use. 
STransects@data$LocCode <- unlist(strsplit(as.character(droplevels(STransects@data$LocCode)),split=".",fixed=TRUE))[c(FALSE,TRUE)]


#   ---- Assumes all necessary objects are in the Global Environment.  
makeBigPlotTrendsObj2(tc)
makeBigPlotAssumptionsObj2(tc)

#   ---- Build table of model statistics.  
betaPCoverHu <- betaTable(fitHu,rfitHu,outcome="pCoverHu")[[1]]
betaPCoverHl <- betaTable(fitHl,rfitHl,outcome="pCoverHl")[[1]]
betaPCoverHa <- betaTable(fitHa,rfitHa,outcome="pCoverHa")[[1]]
betaPCoverSu <- betaTable(fitSu,rfitSu,outcome="pCoverSu")[[1]]
betaPCoverSl <- betaTable(fitSl,rfitSl,outcome="pCoverSl")[[1]]
betaPCoverSa <- betaTable(fitSa,rfitSa,outcome="pCoverSa")[[1]]
betaPCoverTu <- betaTable(fitTu,rfitTu,outcome="pCoverTu")[[1]]
betaPCoverTl <- betaTable(fitTl,rfitTl,outcome="pCoverTl")[[1]]
betaPCoverTa <- betaTable(fitTa,rfitTa,outcome="pCoverTa")[[1]]
betaPCoverAu <- betaTable(fitAu,rfitAu,outcome="pCoverAu")[[1]]
betaPCoverAl <- betaTable(fitAl,rfitAl,outcome="pCoverAl")[[1]]
betaPCoverAa <- betaTable(fitAa,rfitAa,outcome="pCoverAa")[[1]]
parm <- betaTable(fitTa,rfitTa,outcome="pCoverTa")[[2]]

allBetasObj2 <- cbind(parm,betaPCoverHu,betaPCoverHl,betaPCoverHa,betaPCoverSu,betaPCoverSl,betaPCoverSa,betaPCoverTu,betaPCoverTl,betaPCoverTa,betaPCoverAu,betaPCoverAl,betaPCoverAa)

write.csv(allBetasObj2,"//lar-file-srv/Data/NPS/Prairie/Analysis/Trending/betasObj2.csv")



































#   ---- OBJECTIVE 3.

#   ---- Delete out previous model results, just to be safe.  
rm(list=c("fitHu","fitSu","fitTu","fitAu","fitHl","fitSl","fitTl","fitAl","fitHa","fitSa","fitTa","fitAa",
          "rfitHu","rfitSu","rfitTu","rfitAu","rfitHl","rfitSl","rfitTl","rfitAl","rfitHa","rfitSa","rfitTa","rfitAa"),envir=.GlobalEnv)

#   ---- Build data for graphical displays of trend of VegType over time.
obj3Stuff <- prepDataObj3(a144)
dat <- obj3Stuff[[1]]
unitCodes <- obj3Stuff[[2]]

#   ---- Identify the transects with at least two time points. 
theSet <- unique(data.frame(dat$LocCode,dat$Year))
colnames(theSet) <- c("LocCode","Year")
theSet <- theSet[order(theSet$LocCode,theSet$Year),]
theCounts <- data.frame(counts=tapply(theSet$LocCode,list(theSet$LocCode),length))
theCounts$LocCode <- rownames(theCounts)
dat <- merge(dat,theCounts,by=c("LocCode"),all.x=TRUE)
dat <- dat[dat$counts >= 2,]
dat$counts <- NULL

#   ---- Order in a logical way. 
dat <- dat[order(dat$UnitCode,dat$LocCode,dat$CoverClass,dat$Year),]

#   ---- Center the data.  
dat$WYear <- dat$Year - tc

#   ---- Preserve the original data values, prior to epsilon manipulaton. 
dat$pCoveru <- dat$pCover

#   ---- Manipulating data.  Warton, D. I. and Hui, F. K. C.  The arcsince is asinine:
#   ---- the analysis of proportions in ecology.  Ecology, 92(1), 2011, pp. 3-10.
dat0.10 <- dat[dat$CoverClass == "0-10%",]
epsilon0.10 <- min(dat0.10[dat0.10$pCover != 0,]$pCover,100 - dat0.10[dat0.10$pCover != 100,]$pCover)
dat0.10$pCover <- ifelse(dat0.10$pCover == 0,epsilon0.10,
                         ifelse(dat0.10$pCover == 100,100 - epsilon0.10,dat0.10$pCover))
dat11.49 <- dat[dat$CoverClass == "11-49%",]
epsilon11.49 <- min(dat11.49[dat11.49$pCover != 0,]$pCover,100 - dat11.49[dat11.49$pCover != 100,]$pCover)
dat11.49$pCover <- ifelse(dat11.49$pCover == 0,epsilon11.49,
                          ifelse(dat11.49$pCover == 100,100 - epsilon11.49,dat11.49$pCover))
dat50.100 <- dat[dat$CoverClass == "50-100%",]
epsilon50.100 <- min(dat50.100[dat50.100$pCover != 0,]$pCover,100 - dat50.100[dat50.100$pCover != 100,]$pCover)
dat50.100$pCover <- ifelse(dat50.100$pCover == 0,epsilon50.100,
                           ifelse(dat50.100$pCover == 100,100 - epsilon50.100,dat50.100$pCover))

dat <- rbind(dat0.10,dat11.49,dat50.100)

#   ---- Create transformed outcomes. 
#   ---- User pCover for the logistic, to ensure we use the epsilon manpulated values. 
dat$pCoverl <- log(dat$pCover / (100 - dat$pCover))

#   ---- But we use the original pCover (pCoveru) for the arcsine transformation.  
dat$pCovera <- asin(sqrt(dat$pCoveru/100))

#   ---- Rename pCover.  It has served its purpose. 
names(dat)[names(dat) == "pCover"] <- "pCoverEpsilon"

dat$LocCode <- paste0(dat$UnitCode,".",dat$LocCode)
write.csv(dat,"//lar-file-srv/Data/NPS/Prairie/Analysis/Trending/datObjective3.csv",row.names=FALSE)

#   ---- Fit all 9 full models.  
fit0.10u   <- lmer(pCoveru ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "0-10%",]$TransectLenM/1000,data=dat[dat$CoverClass == "0-10%",])
fit11.49u  <- lmer(pCoveru ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "11-49%",]$TransectLenM/1000,data=dat[dat$CoverClass == "11-49%",])
fit50.100u <- lmer(pCoveru ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "50-100%",]$TransectLenM/1000,data=dat[dat$CoverClass == "50-100%",])

fit0.10l   <- lmer(pCoverl ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "0-10%",]$TransectLenM/1000,data=dat[dat$CoverClass == "0-10%",])
fit11.49l  <- lmer(pCoverl ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "11-49%",]$TransectLenM/1000,data=dat[dat$CoverClass == "11-49%",])
fit50.100l <- lmer(pCoverl ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "50-100%",]$TransectLenM/1000,data=dat[dat$CoverClass == "50-100%",])

fit0.10a   <- lmer(pCovera ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "0-10%",]$TransectLenM/1000,data=dat[dat$CoverClass == "0-10%",])
fit11.49a  <- lmer(pCovera ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "11-49%",]$TransectLenM/1000,data=dat[dat$CoverClass == "11-49%",])
fit50.100a <- lmer(pCovera ~ 1 + WYear + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "50-100%",]$TransectLenM/1000,data=dat[dat$CoverClass == "50-100%",])


#   ---- Fit all 9 reduced models.  We do this to test for trend. 
rfit0.10u   <- lmer(pCoveru ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "0-10%",]$TransectLenM/1000,data=dat[dat$CoverClass == "0-10%",])
rfit11.49u  <- lmer(pCoveru ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "11-49%",]$TransectLenM/1000,data=dat[dat$CoverClass == "11-49%",])
rfit50.100u <- lmer(pCoveru ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "50-100%",]$TransectLenM/1000,data=dat[dat$CoverClass == "50-100%",])

rfit0.10l   <- lmer(pCoverl ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "0-10%",]$TransectLenM/1000,data=dat[dat$CoverClass == "0-10%",])
rfit11.49l  <- lmer(pCoverl ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "11-49%",]$TransectLenM/1000,data=dat[dat$CoverClass == "11-49%",])
rfit50.100l <- lmer(pCoverl ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "50-100%",]$TransectLenM/1000,data=dat[dat$CoverClass == "50-100%",])

rfit0.10a   <- lmer(pCovera ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "0-10%",]$TransectLenM/1000,data=dat[dat$CoverClass == "0-10%",])
rfit11.49a  <- lmer(pCovera ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "11-49%",]$TransectLenM/1000,data=dat[dat$CoverClass == "11-49%",])
rfit50.100a <- lmer(pCovera ~ 1 + (1 + WYear | LocCode) + (1|Year),weight=dat[dat$CoverClass == "50-100%",]$TransectLenM/1000,data=dat[dat$CoverClass == "50-100%",])


#   ---- Read in a representative unstraightened shapefile. 
# dir <- "//lar-file-srv/Data/NPS/Prairie/Analysis/Shapefiles"
# shp <- "AC - 2015 - Walked"
# transects <- readOGR(dir,shp)

#   ---- Read in a representative straightened shapefile. 
dir <- "//lar-file-srv/Data/NPS/Prairie/Analysis/Shapefiles"
shp <- "vpTransects"
STransects <- readOGR(dir,shp)

#   ---- Clean up shapefile for use. 
STransects@data$LocCode <- unlist(strsplit(as.character(droplevels(STransects@data$LocCode)),split=".",fixed=TRUE))[c(FALSE,TRUE)]


#   ---- Assumes all necessary objects are in the Global Environment.  
makeBigPlotTrendsObj3(tc)
makeBigPlotAssumptionsObj3(tc)

#   ---- Build table of model statistics.  
betaPCover0.10u <- betaTable(fit0.10u,rfit0.10u,outcome="pCover0.10u")[[1]]
betaPCover0.10l <- betaTable(fit0.10l,rfit0.10l,outcome="pCover0.10l")[[1]]
betaPCover0.10a <- betaTable(fit0.10a,rfit0.10a,outcome="pCover0.10a")[[1]]
betaPCover11.49u <- betaTable(fit11.49u,rfit11.49u,outcome="pCover11.49u")[[1]]
betaPCover11.49l <- betaTable(fit11.49l,rfit11.49l,outcome="pCover11.49l")[[1]]
betaPCover11.49a <- betaTable(fit11.49a,rfit11.49a,outcome="pCover11.49a")[[1]]
betaPCover50.100u <- betaTable(fit50.100u,rfit50.100u,outcome="pCover50.100u")[[1]]
betaPCover50.100l <- betaTable(fit50.100l,rfit50.100l,outcome="pCover50.100l")[[1]]
betaPCover50.100a <- betaTable(fit50.100a,rfit50.100a,outcome="pCover50.100a")[[1]]
parm <- betaTable(fit0.10a,rfit0.10a,outcome="pCover0.10a")[[2]]

allBetasObj3 <- cbind(parm,betaPCover0.10u,betaPCover0.10l,betaPCover0.10a,betaPCover11.49u,betaPCover11.49l,betaPCover11.49a,betaPCover50.100u,betaPCover50.100l,betaPCover50.100a)

write.csv(allBetasObj3,"//lar-file-srv/Data/NPS/Prairie/Analysis/Trending/betasObj3.csv")

