makeBigPlotTrends <- function(label){
  
  if( label == "Objective1" ){
    
    png(paste0("//lar-file-srv/Data/NPS/Prairie/Analysis/Trending/",label,"-Trends.png"),width=36,height=90,units="in",res=300)
  
    #   ---- Bigger, with 5.
    string <- c(rep(1,34),rep(2,34),seq(3,6),rep(7,5),8,rep(9,5),10,rep(11,5),12,rep(13,5),14,rep(15,5),16,rep(17,34),seq(18,21),rep(22,5),23,rep(24,5),25,rep(26,5),27,rep(28,5),29,rep(30,5),31,rep(32,34),seq(33,66,1),rep(67,34),seq(68,101,1),rep(102,34),seq(103,136,1),rep(137,34),seq(138,171,1),rep(172,34),seq(173,206,1),rep(207,34),seq(208,241,1),rep(242,34),seq(243,276,1),rep(277,34),seq(278,311,1),rep(312,34),seq(313,346,1),rep(347,34),seq(348,381,1),rep(382,34),seq(383,416,1),rep(417,34),seq(418,451,1),rep(452,34),seq(453,486,1),rep(487,34),seq(488,521,1),rep(522,34),seq(523,556,1),rep(557,34),seq(558,591,1),rep(592,34),seq(593,626,1),rep(627,34),seq(628,661,1),rep(662,34),seq(663,696,1),rep(697,34),seq(698,731,1),rep(732,34),seq(733,766,1),rep(767,34),seq(768,801,1),rep(802,34),seq(803,836,1),rep(837,34),seq(838,871,1),rep(872,34),seq(873,906,1),rep(907,34),seq(908,941,1),rep(942,34),seq(943,976,1),rep(977,34),seq(978,1011,1),rep(1012,34),seq(1013,1046,1),rep(1047,34),seq(1048,1081,1),rep(1082,34),seq(1083,1116,1),rep(1117,34),seq(1118,1151,1),rep(1152,34),seq(1153,1186,1),rep(1187,34),seq(1188,1221,1),rep(1222,34),seq(1223,1256,1),rep(1257,34),seq(1258,1291,1),rep(1292,34),seq(1293,1326,1),rep(1327,34),seq(1328,1361,1),rep(1362,34),seq(1363,1396,1),rep(1397,34),seq(1398,1431,1),rep(1432,34),seq(1433,1466,1),rep(1467,34),seq(1468,1501,1),rep(1048,34))
    widths <- c(0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005,0.05382,0.005)
    heights <- c(0.02,0.005,0.01,0.005,0.04,0.005,0.01,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.01)
    a <- layout(matrix(string,90,34,byrow=TRUE),widths=widths,heights=heights)
    layout.show(a)
  } else {
    
    png(paste0("//lar-file-srv/Data/NPS/Prairie/Analysis/Trending/",label,"-Trends.png"),width=24,height=90,units="in",res=300)
    
    #   ---- Smaller, with 3.
    string <- c(rep(1,22),rep(2,22),seq(3,6),rep(7,5),8,rep(9,5),10,rep(11,5),12,rep(13,22),seq(14,17),rep(18,5),19,rep(20,5),21,rep(22,5),23,rep(24,22),seq(25,46),rep(47,22),seq(48,69),rep(70,22),seq(71,92),rep(93,22),seq(94,115),rep(116,22),seq(117,138),rep(139,22),seq(140,161),rep(162,22),seq(163,184),rep(185,22),seq(186,207),rep(208,22),seq(209,230),rep(231,22),seq(232,253),rep(254,22),seq(255,276),rep(277,22),seq(278,299),rep(300,22),seq(301,322),rep(323,22),seq(324,345),rep(346,22),seq(347,368),rep(369,22),seq(370,391),rep(392,22),seq(393,414),rep(415,22),seq(416,437),rep(438,22),seq(439,460),rep(461,22),seq(462,483),rep(484,22),seq(485,506),rep(507,22),seq(508,529),rep(530,22),seq(531,552),rep(553,22),seq(554,575),rep(576,22),seq(577,598),rep(599,22),seq(600,621),rep(622,22),seq(623,644),rep(645,22),seq(646,667),rep(668,22),seq(669,690),rep(691,22),seq(692,713),rep(714,22),seq(715,736),rep(737,22),seq(738,759),rep(760,22),seq(761,782),rep(783,22),seq(784,805),rep(806,22),seq(807,828),rep(829,22),seq(830,851),rep(852,22),seq(853,874),rep(875,22),seq(876,897),rep(898,22),seq(899,920),rep(921,22),seq(922,943),rep(944,22),seq(945,966),rep(967,22),seq(968,989),rep(990,22))
    widths <- c(0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005)
    heights <- c(0.02,0.005,0.01,0.005,0.04,0.005,0.01,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.01)
    a <- layout(matrix(string,90,22,byrow=TRUE),widths=widths,heights=heights)
    layout.show(a)
  }
  
  #   ---- Row 1
  plotText(paste0(label," Analytics."),5)
  
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
  if( label == "Objective1" ){
    plotText("Developed",3.5)
    plotEmpty()
    plotText("Unvegetated",3.5)
    plotEmpty()
  }
  
  #   ---- Row 4
  plotEmpty()
  
  #   ---- Row 5
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotSpatialTrends(fitHu,"H",STransects,2012,2010,"pCoveru","Blues")
  plotEmpty()
  plotSpatialTrends(fitSu,"S",STransects,2012,2010,"pCoveru","Oranges")
  plotEmpty()
  plotSpatialTrends(fitTu,"T",STransects,2012,2010,"pCoveru","Greens")
  plotEmpty()
  if( label == "Objective1" ){
    plotSpatialTrends(fitDu,"D",STransects,2012,2010,"pCoveru","Reds")
    plotEmpty()
    plotSpatialTrends(fitUu,"U",STransects,2012,2010,"pCoveru","Purples")
    plotEmpty()
  }
  
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
  if( label == "Objective1" ){
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
  }
  
  #   ---- Row 8
  plotEmpty()
  
  #   ---- Row 9
  plotText("Piepho & Ogutu\nPlot of Fixed Effects",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotAllObj1(dat,fitHu,"H",2010,brewer.pal(9,"Paired")[1:2],0.10,2008)    # blue
  plotEmpty()
  plotAllObj1(dat,fitHl,"H",2010,brewer.pal(9,"Paired")[1:2],0.10,2008)    # blue
  plotEmpty()
  plotAllObj1(dat,fitHa,"H",2010,brewer.pal(9,"Paired")[1:2],0.10,2008)    # blue
  plotEmpty()
  plotAllObj1(dat,fitSu,"S",2010,brewer.pal(9,"Paired")[7:8],0.10,2008)    # orange
  plotEmpty()
  plotAllObj1(dat,fitSl,"S",2010,brewer.pal(9,"Paired")[7:8],0.10,2008)    # orange
  plotEmpty()
  plotAllObj1(dat,fitSa,"S",2010,brewer.pal(9,"Paired")[7:8],0.10,2008)    # orange
  plotEmpty()
  plotAllObj1(dat,fitTu,"T",2010,brewer.pal(9,"Paired")[3:4],0.10,2008)    # green
  plotEmpty()
  plotAllObj1(dat,fitTl,"T",2010,brewer.pal(9,"Paired")[3:4],0.10,2008)    # green
  plotEmpty()
  plotAllObj1(dat,fitTa,"T",2010,brewer.pal(9,"Paired")[3:4],0.10,2008)    # green
  plotEmpty()
  if(label == "Objective1" ){
    plotAllObj1(dat,fitDu,"D",2010,brewer.pal(9,"Paired")[5:6],0.10,2008)    # orange
    plotEmpty()
    plotAllObj1(dat,fitDl,"D",2010,brewer.pal(9,"Paired")[5:6],0.10,2008)    # orange
    plotEmpty()
    plotAllObj1(dat,fitDa,"D",2010,brewer.pal(9,"Paired")[5:6],0.10,2008)    # orange
    plotEmpty()
    plotAllObj1(dat,fitUu,"U",2010,brewer.pal(9,"Paired")[9:10],0.10,2008)    # green
    plotEmpty()
    plotAllObj1(dat,fitUl,"U",2010,brewer.pal(9,"Paired")[9:10],0.10,2008)    # green
    plotEmpty()
    plotAllObj1(dat,fitUa,"U",2010,brewer.pal(9,"Paired")[9:10],0.10,2008)    # green
    plotEmpty()
  }
  
  #   ---- Row 10
  plotEmpty()
  
  #   ---- Row 11
  plotText("Histogram of\nOutcomes",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotHistObj1(dat,"H",5,"pCoveru")
  plotEmpty()
  plotHistObj1(dat,"H",5,"pCoverl")
  plotEmpty()
  plotHistObj1(dat,"H",5,"pCovera")
  plotEmpty()
  plotHistObj1(dat,"S",5,"pCoveru")
  plotEmpty()
  plotHistObj1(dat,"S",5,"pCoverl")
  plotEmpty()
  plotHistObj1(dat,"S",5,"pCovera")
  plotEmpty()
  plotHistObj1(dat,"T",95,"pCoveru")
  plotEmpty()
  plotHistObj1(dat,"T",95,"pCoverl")
  plotEmpty()
  plotHistObj1(dat,"T",95,"pCovera")
  plotEmpty()
  if( label == "Objective1" ){
    plotHistObj1(dat,"D",5,"pCoveru")
    plotEmpty()
    plotHistObj1(dat,"D",5,"pCoverl")
    plotEmpty()
    plotHistObj1(dat,"D",5,"pCovera")
    plotEmpty()
    plotHistObj1(dat,"U",95,"pCoveru")
    plotEmpty()
    plotHistObj1(dat,"U",95,"pCoverl")
    plotEmpty()
    plotHistObj1(dat,"U",95,"pCovera")
    plotEmpty()
  }
  
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
    plotOneObj1(dat,fitHu,"H",2010,brewer.pal(9,"Paired")[1:2],locCode,0.10,2008)    # blue
    plotEmpty()
    plotOneObj1(dat,fitHl,"H",2010,brewer.pal(9,"Paired")[1:2],locCode,0.10,2008)    # blue
    plotEmpty()
    plotOneObj1(dat,fitHa,"H",2010,brewer.pal(9,"Paired")[1:2],locCode,0.10,2008)    # blue
    plotEmpty()
    plotOneObj1(dat,fitSu,"S",2010,brewer.pal(9,"Paired")[7:8],locCode,0.10,2008)    # orange
    plotEmpty()
    plotOneObj1(dat,fitSl,"S",2010,brewer.pal(9,"Paired")[7:8],locCode,0.10,2008)    # orange
    plotEmpty()
    plotOneObj1(dat,fitSa,"S",2010,brewer.pal(9,"Paired")[7:8],locCode,0.10,2008)    # orange
    plotEmpty()
    plotOneObj1(dat,fitTu,"T",2010,brewer.pal(9,"Paired")[3:4],locCode,0.10,2008)    # green
    plotEmpty()
    plotOneObj1(dat,fitTl,"T",2010,brewer.pal(9,"Paired")[3:4],locCode,0.10,2008)    # green
    plotEmpty()
    plotOneObj1(dat,fitTa,"T",2010,brewer.pal(9,"Paired")[3:4],locCode,0.10,2008)    # green
    plotEmpty()
    if( label == "Objective1" ){
      plotOneObj1(dat,fitDu,"D",2010,brewer.pal(9,"Paired")[5:6],locCode,0.10,2008)    # orange
      plotEmpty()
      plotOneObj1(dat,fitDl,"D",2010,brewer.pal(9,"Paired")[5:6],locCode,0.10,2008)    # orange
      plotEmpty()
      plotOneObj1(dat,fitDa,"D",2010,brewer.pal(9,"Paired")[5:6],locCode,0.10,2008)    # orange
      plotEmpty()
      plotOneObj1(dat,fitUu,"U",2010,brewer.pal(9,"Paired")[9:10],locCode,0.10,2008)    # green
      plotEmpty()
      plotOneObj1(dat,fitUl,"U",2010,brewer.pal(9,"Paired")[9:10],locCode,0.10,2008)    # green
      plotEmpty()
      plotOneObj1(dat,fitUa,"U",2010,brewer.pal(9,"Paired")[9:10],locCode,0.10,2008)    # green
      plotEmpty()
    }
    
    #   ---- Row (j + 1) + 9
    plotEmpty()
    
  }
  
  #   ---- Row End
  plotText("This is going to be a boring set of footnotes.",0.5)
  
  dev.off()
  
}


