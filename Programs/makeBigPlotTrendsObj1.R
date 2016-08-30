makeBigPlotTrendsObj1 <- function(tc){
  
 
    
  png(paste0("//lar-file-srv/Data/NPS/Prairie/Analysis/Trending/Objective1-Trends.png"),width=36,height=90,units="in",res=300)
  
  #   ---- Bigger, with 5.
  string <- c(rep(1,34),rep(2,34),seq(3,6),rep(7,5),8,rep(9,5),10,rep(11,5),12,rep(13,5),14,rep(15,5),16,rep(17,34),seq(18,21),rep(22,5),23,rep(24,5),25,rep(26,5),27,rep(28,5),29,rep(30,5),31,rep(32,34),seq(33,66,1),rep(67,34),seq(68,101,1),rep(102,34),seq(103,136,1),rep(137,34),seq(138,171,1),rep(172,34),seq(173,206,1),rep(207,34),seq(208,241,1),rep(242,34),seq(243,276,1),rep(277,34),seq(278,311,1),rep(312,34),seq(313,346,1),rep(347,34),seq(348,381,1),rep(382,34),seq(383,416,1),rep(417,34),seq(418,451,1),rep(452,34),seq(453,486,1),rep(487,34),seq(488,521,1),rep(522,34),seq(523,556,1),rep(557,34),seq(558,591,1),rep(592,34),seq(593,626,1),rep(627,34),seq(628,661,1),rep(662,34),seq(663,696,1),rep(697,34),seq(698,731,1),rep(732,34),seq(733,766,1),rep(767,34),seq(768,801,1),rep(802,34),seq(803,836,1),rep(837,34),seq(838,871,1),rep(872,34),seq(873,906,1),rep(907,34),seq(908,941,1),rep(942,34),seq(943,976,1),rep(977,34),seq(978,1011,1),rep(1012,34),seq(1013,1046,1),rep(1047,34),seq(1048,1081,1),rep(1082,34),seq(1083,1116,1),rep(1117,34),seq(1118,1151,1),rep(1152,34),seq(1153,1186,1),rep(1187,34),seq(1188,1221,1),rep(1222,34),seq(1223,1256,1),rep(1257,34),seq(1258,1291,1),rep(1292,34),seq(1293,1326,1),rep(1327,34),seq(1328,1361,1),rep(1362,34),seq(1363,1396,1),rep(1397,34),seq(1398,1431,1),rep(1432,34),seq(1433,1466,1),rep(1467,34),seq(1468,1501,1),rep(1048,34))
  widths <- c(0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005)
  heights <- c(0.02,0.005,0.01,0.005,0.04,0.005,0.01,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.01)
  a <- layout(matrix(string,90,34,byrow=TRUE),widths=widths,heights=heights)
  layout.show(a)
 
  #   ---- Row 1
  plotText(paste0("Objective 1 Analytics"),5)
  
  #   ---- Row 2
  plotEmpty()
  
  #   ---- Row 3
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotText("Herbaceous",3.5)
  plotEmpty()
  plotText("Shrubs",3.5)
  plotEmpty()
  plotText("Trees",3.5)
  plotEmpty()
  plotText("Developed",3.5)
  plotEmpty()
  plotText("Unvegetated",3.5)
  plotEmpty()

  #   ---- Row 4
  plotEmpty()
  
  #   ---- Row 5
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotSpatialTrends(fitHu,STransects,2012,tc,"pCoveru","Blues")
  plotEmpty()
  plotSpatialTrends(fitSu,STransects,2012,tc,"pCoveru","Oranges")
  plotEmpty()
  plotSpatialTrends(fitTu,STransects,2012,tc,"pCoveru","Greens")
  plotEmpty()
  plotSpatialTrends(fitDu,STransects,2012,tc,"pCoveru","Reds")
  plotEmpty()
  plotSpatialTrends(fitUu,STransects,2012,tc,"pCoveru","Purples")
  plotEmpty()
  
  #   ---- Row 6
  plotEmpty()
  
  #   ---- Row 7
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotText("Untransformed",2.5)
  plotEmpty()
  plotText("Logit",2.5)
  plotEmpty()
  plotText("Arc-Sine",2.5)
  plotEmpty()
  plotText("Untransformed",2.5)
  plotEmpty()
  plotText("Logit",2.5)
  plotEmpty()
  plotText("Arc-Sine",2.5)
  plotEmpty()
  plotText("Untransformed",2.5)
  plotEmpty()
  plotText("Logit",2.5)
  plotEmpty()
  plotText("Arc-Sine",2.5)
  plotEmpty()
  plotText("Untransformed",2.5)
  plotEmpty()
  plotText("Logit",2.5)
  plotEmpty()
  plotText("Arc-Sine",2.5)
  plotEmpty()
  plotText("Untransformed",2.5)
  plotEmpty()
  plotText("Logit",2.5)
  plotEmpty()
  plotText("Arc-Sine",2.5)
  plotEmpty() 

  #   ---- Row 8
  plotEmpty()
  
  #   ---- Row 9
  plotText("Piepho & Ogutu\nPlot of Fixed Effects",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotAllObj1(dat,fitHu,"VegType","H",2010,brewer.pal(9,"Paired")[1:2],0.10,tc)    # blue
  plotEmpty()
  plotAllObj1(dat,fitHl,"VegType","H",2010,brewer.pal(9,"Paired")[1:2],0.10,tc)    # blue
  plotEmpty()
  plotAllObj1(dat,fitHa,"VegType","H",2010,brewer.pal(9,"Paired")[1:2],0.10,tc)    # blue
  plotEmpty()
  plotAllObj1(dat,fitSu,"VegType","S",2010,brewer.pal(9,"Paired")[7:8],0.10,tc)    # orange
  plotEmpty()
  plotAllObj1(dat,fitSl,"VegType","S",2010,brewer.pal(9,"Paired")[7:8],0.10,tc)    # orange
  plotEmpty()
  plotAllObj1(dat,fitSa,"VegType","S",2010,brewer.pal(9,"Paired")[7:8],0.10,tc)    # orange
  plotEmpty()
  plotAllObj1(dat,fitTu,"VegType","T",2010,brewer.pal(9,"Paired")[3:4],0.10,tc)    # green
  plotEmpty()
  plotAllObj1(dat,fitTl,"VegType","T",2010,brewer.pal(9,"Paired")[3:4],0.10,tc)    # green
  plotEmpty()
  plotAllObj1(dat,fitTa,"VegType","T",2010,brewer.pal(9,"Paired")[3:4],0.10,tc)    # green
  plotEmpty()
  plotAllObj1(dat,fitDu,"VegType","D",2010,brewer.pal(9,"Paired")[5:6],0.10,tc)    # orange
  plotEmpty()
  plotAllObj1(dat,fitDl,"VegType","D",2010,brewer.pal(9,"Paired")[5:6],0.10,tc)    # orange
  plotEmpty()
  plotAllObj1(dat,fitDa,"VegType","D",2010,brewer.pal(9,"Paired")[5:6],0.10,tc)    # orange
  plotEmpty()
  plotAllObj1(dat,fitUu,"VegType","U",2010,brewer.pal(9,"Paired")[9:10],0.10,tc)    # green
  plotEmpty()
  plotAllObj1(dat,fitUl,"VegType","U",2010,brewer.pal(9,"Paired")[9:10],0.10,tc)    # green
  plotEmpty()
  plotAllObj1(dat,fitUa,"VegType","U",2010,brewer.pal(9,"Paired")[9:10],0.10,tc)    # green
  plotEmpty()
  
  #   ---- Row 10
  plotEmpty()
  
  #   ---- Row 11
  plotText("Histogram of\nOutcomes",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotHistObj1(dat,"VegType","H",5,"pCoveru")
  plotEmpty()
  plotHistObj1(dat,"VegType","H",5,"pCoverl")
  plotEmpty()
  plotHistObj1(dat,"VegType","H",5,"pCovera")
  plotEmpty()
  plotHistObj1(dat,"VegType","S",5,"pCoveru")
  plotEmpty()
  plotHistObj1(dat,"VegType","S",5,"pCoverl")
  plotEmpty()
  plotHistObj1(dat,"VegType","S",5,"pCovera")
  plotEmpty()
  plotHistObj1(dat,"VegType","T",95,"pCoveru")
  plotEmpty()
  plotHistObj1(dat,"VegType","T",95,"pCoverl")
  plotEmpty()
  plotHistObj1(dat,"VegType","T",95,"pCovera")
  plotEmpty()
  plotHistObj1(dat,"VegType","D",5,"pCoveru")
  plotEmpty()
  plotHistObj1(dat,"VegType","D",5,"pCoverl")
  plotEmpty()
  plotHistObj1(dat,"VegType","D",5,"pCovera")
  plotEmpty()
  plotHistObj1(dat,"VegType","U",95,"pCoveru")
  plotEmpty()
  plotHistObj1(dat,"VegType","U",95,"pCoverl")
  plotEmpty()
  plotHistObj1(dat,"VegType","U",95,"pCovera")
  plotEmpty()
  
  #   ---- Row 12
  plotEmpty()
  
  
  #   ---- Now, plot each individual transect one-by-one.  
  
  LocCodes <- unique(dat$LocCode)
  nLocCodes <- length(LocCodes)
  
  for(k in 1:nLocCodes){
    
    locCode <- LocCodes[k]
    
    #   ---- Row j + 12
    plotText(paste0("Transect\n",locCode),1.5)
    plotEmpty()
    plotTransectMap(locCode)
    plotEmpty()
    plotOneObj1(dat,fitHu,"VegType","H",2010,brewer.pal(9,"Paired")[1:2],locCode,0.10,tc)    # blue
    plotEmpty()
    plotOneObj1(dat,fitHl,"VegType","H",2010,brewer.pal(9,"Paired")[1:2],locCode,0.10,tc)    # blue
    plotEmpty()
    plotOneObj1(dat,fitHa,"VegType","H",2010,brewer.pal(9,"Paired")[1:2],locCode,0.10,tc)    # blue
    plotEmpty()
    plotOneObj1(dat,fitSu,"VegType","S",2010,brewer.pal(9,"Paired")[7:8],locCode,0.10,tc)    # orange
    plotEmpty()
    plotOneObj1(dat,fitSl,"VegType","S",2010,brewer.pal(9,"Paired")[7:8],locCode,0.10,tc)    # orange
    plotEmpty()
    plotOneObj1(dat,fitSa,"VegType","S",2010,brewer.pal(9,"Paired")[7:8],locCode,0.10,tc)    # orange
    plotEmpty()
    plotOneObj1(dat,fitTu,"VegType","T",2010,brewer.pal(9,"Paired")[3:4],locCode,0.10,tc)    # green
    plotEmpty()
    plotOneObj1(dat,fitTl,"VegType","T",2010,brewer.pal(9,"Paired")[3:4],locCode,0.10,tc)    # green
    plotEmpty()
    plotOneObj1(dat,fitTa,"VegType","T",2010,brewer.pal(9,"Paired")[3:4],locCode,0.10,tc)    # green
    plotEmpty()
    plotOneObj1(dat,fitDu,"VegType","D",2010,brewer.pal(9,"Paired")[5:6],locCode,0.10,tc)    # orange
    plotEmpty()
    plotOneObj1(dat,fitDl,"VegType","D",2010,brewer.pal(9,"Paired")[5:6],locCode,0.10,tc)    # orange
    plotEmpty()
    plotOneObj1(dat,fitDa,"VegType","D",2010,brewer.pal(9,"Paired")[5:6],locCode,0.10,tc)    # orange
    plotEmpty()
    plotOneObj1(dat,fitUu,"VegType","U",2010,brewer.pal(9,"Paired")[9:10],locCode,0.10,tc)    # green
    plotEmpty()
    plotOneObj1(dat,fitUl,"VegType","U",2010,brewer.pal(9,"Paired")[9:10],locCode,0.10,tc)    # green
    plotEmpty()
    plotOneObj1(dat,fitUa,"VegType","U",2010,brewer.pal(9,"Paired")[9:10],locCode,0.10,tc)    # green
    plotEmpty()
    
    #   ---- Row (j + 1) + 9
    plotEmpty()
    
  }
  
  #   ---- Row End
  plotText("This is going to be a boring set of footnotes.",0.5)
  
  dev.off()
  
}


