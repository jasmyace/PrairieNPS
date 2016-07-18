
makeTransectShp <- function(df,type,proj){
  
  # df <- a144[a144$End == 0,]
  # type <- "Walked"
  # proj <- projUTM10
  
  records <- nrow(df) - 1
  transect.list <- vector("list",records)
  
  for(i in 1:records){
    if(df$LocCode[i] == df$LocCodel1[i] & df$Year[i] == df$Yearl1[i]){
      transect.list[[i]] <- Lines(list(Line(df[c(i,i + 1),c('UTME','UTMN')])), as.character(df$R_ID[i]))
    }
  }
  
  #   ---- Convert to a SpatialLinesDataFrame
  transect.list <- transect.list[!sapply(transect.list,is.null)]
  line2 <- as(SpatialLines(transect.list,proj4string=CRS(proj)),"SpatialLinesDataFrame")
  
  #   ---- Put together the data frame for use in the shapefile.
  R_ID <- data.frame(R_ID=sapply(line2@lines,function(x) x@ID))
  R_ID$R_ID <- as.numeric(levels(R_ID$R_ID))[R_ID$R_ID]
  df2 <- merge(R_ID,df,by=c('R_ID'),all.x=TRUE)
  df2 <- df2[order(df2$R_ID),]
  rownames(df2) <- df2$R_ID
  line2@data <- df2
  
  #   ---- Clean up the attribute table.
  line2@data$LocCodel1 <- line2@data$Yearl1 <- line2@data$R_ID <- NULL
  
  #   ---- Export the year-based shapefiles.  
  years <- unique(line2@data$Year)
  for(i in 1:length(years)){
    year <- years[i]
    string <- paste0("AC"," - ",year," - ",type)
    assign(string,line2[line2@data$Year == year,])
    writeOGR(get(string),"//lar-file-srv/Data/NPS/Prairie/Analysis/Shapefiles",string,overwrite_layer=TRUE,driver="ESRI Shapefile")
  }
}