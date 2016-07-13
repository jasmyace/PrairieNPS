
#   ---- Read-in raw data. 

dataStem <- "//lar-file-srv/Data/NPS/Prairie/Data/Data_files_20160711"

a104 <- read.table(file=paste0(dataStem,"/qs_a104_Location_metadata_20160711_200543.txt"),
                   header=TRUE,
                   sep=",",
                   col.names=c("Park_code","Unit_code","Location_code","Location_type","Location_status","Location_name","Panel_name","Panel_type","Firing_order","UTME_public","UTMN_public","Public_offset","Location_desc","Location_notes","Loc_established"
))

a114 <- read.table(file=paste0(dataStem,"/qs_a114_Event_metadata_20160711_200634.txt"),
                   header=TRUE,
                   sep=",",
                   col.names=c("Park_code","Loc_code","Unit_code","Location_code","Calendar_year","Start_date","Start_time","End_date","End_time","Hours_spent","Transect_easting","Transect_northing","GPS_file_name","Transect_notes","Event_notes","Entered_by","Entered_date","Updated_by","Updated_date","Verified_by","Verified_date","Certified_by","Certified_date","Is_excluded","QA_notes"
                   ))

a124 <- read.table(file=paste0(dataStem,"/qs_a124_Event_observers_20160711_200645.txt"),
                   header=TRUE,
                   sep=",",
                   col.names=c("Park_code","Loc_code","Unit_code","Location_code","Calendar_year","Start_date","Contact_ID","Observer_role","Observer_notes"
                   ))

a134 <- read.table(file=paste0(dataStem,"/qs_a134_Phenology_obs_20160711_200656.txt"),
                   header=TRUE,
                   sep=",",
                   col.names=c("Park_code","Loc_code","Unit_code","Location_code","Calendar_year","Start_date","Species_code","Scientific_name","Stage_code","Stage_desc","Avg_height_m","Phenology_notes"
                   ))

a144 <- read.table(file=paste0(dataStem,"/qs_a144_Transect_obs_20160711_200707.txt"),
                   header=TRUE,
                   sep=",",
                   col.names=c("Park_code","Loc_code","Unit_code","Location_code","Calendar_year","Start_date","Observation_ID","Segment_num","Segment_length_m","No_GPS","UTME","UTMN","Field_coord","Veg_type","Origin","Cover_class","Is_grazed","Substrate","Obs_notes","Flag","Office_notes"
                   ))
