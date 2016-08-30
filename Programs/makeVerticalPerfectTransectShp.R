
makeVerticalPerfectTransectShp <- function(df,type,proj){
  
  # df <- vpTransects
  # type <- ""
  # proj <- projUTM10
  
  records <- nrow(df) - 1
  transect.list <- vector("list",records)
  
  for(i in 1:records){
    if(df$LocCode[i] == df$LocCodel1[i]){
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
  line2@data$LocCodel1 <- line2@data$R_ID <- line2@data$UTME <- line2@data$UTMN <- line2@data$Start_End <- line2@data$Source <- NULL
  
  #   ---- Export all vertically perfect shapefiles.  
  writeOGR(line2,"//lar-file-srv/Data/NPS/Prairie/Analysis/Shapefiles","vpTransects",overwrite_layer=TRUE,driver="ESRI Shapefile")
}