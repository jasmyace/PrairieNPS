makeBigPlotTrendsObj2 <- function(tc){
  
  
  png(paste0("//lar-file-srv/Data/NPS/Prairie/Analysis/Trending/Objective2-Trends.png"),width=36,height=90,units="in",res=300)
  
  #   ---- Medium, with 4.
  string <- c(rep(1,28),rep(2,28),seq(3,6),rep(7,5),8,rep(9,5),10,rep(11,5),12,rep(13,5),14,rep(15,28),seq(16,19),rep(20,5),21,rep(22,5),23,rep(24,5),25,rep(26,5),27,rep(28,28),seq(29,56,1),rep(57,28),seq(58,85,1),rep(86,28),seq(87,114,1),rep(115,28),seq(116,143,1),rep(144,28),seq(145,172,1),rep(173,28),seq(174,201,1),rep(202,28),seq(203,230,1),rep(231,28),seq(232,259,1),rep(260,28),seq(261,288,1),rep(289,28),seq(290,317,1),rep(318,28),seq(319,346,1),rep(347,28),seq(348,375,1),rep(376,28),seq(377,404,1),rep(405,28),seq(406,433,1),rep(434,28),seq(435,462,1),rep(463,28),seq(464,491,1),rep(492,28),seq(493,520,1),rep(521,28),seq(522,549,1),rep(550,28),seq(551,578,1),rep(579,28),seq(580,607,1),rep(608,28),seq(609,636,1),rep(637,28),seq(638,665,1),rep(666,28),seq(667,694,1),rep(695,28),seq(696,723,1),rep(724,28),seq(725,752,1),rep(753,28),seq(754,781,1),rep(782,28),seq(783,810,1),rep(811,28),seq(812,839,1),rep(840,28),seq(841,868,1),rep(869,28),seq(870,897,1),rep(898,28),seq(899,926,1),rep(927,28),seq(928,955,1),rep(956,28),seq(957,984,1),rep(985,28),seq(986,1013,1),rep(1014,28),seq(1015,1042,1),rep(1043,28),seq(1044,1071,1),rep(1072,28),seq(1073,1100,1),rep(1101,28),seq(1102,1129,1),rep(1130,28),seq(1131,1158,1),rep(1159,28),seq(1160,1187,1),rep(1188,28),seq(1189,1216,1),rep(1217,28),seq(1218,1245,1),rep(1246,28))
  widths <- c(0.06643,0.005,0.06643,0.005,0.06643,0.005,0.06643,0.005,0.06643,0.005,0.06643,0.005,0.06643,0.005,0.06643,0.005,0.06643,0.005,0.06643,0.005,0.06643,0.005,0.06643,0.005,0.06643,0.005,0.06643,0.005)
  heights <- c(0.02,0.005,0.01,0.005,0.04,0.005,0.01,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.01)
  a <- layout(matrix(string,90,28,byrow=TRUE),widths=widths,heights=heights)
  layout.show(a)
 
  #   ---- Row 1
  plotText(paste0("Objective 2 Analytics"),5)
  
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
  plotText("Composite",3.5)
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
  plotSpatialTrends(fitAu,STransects,2012,tc,"pCoveru","Reds")
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

  #   ---- Row 8
  plotEmpty()
  
  #   ---- Row 9
  plotText("Piepho & Ogutu\nPlot of Fixed Effects",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotAllObj1(dat,fitHu,"VegType","H",tc,brewer.pal(9,"Paired")[1:2],0.10,2008)    # blue
  plotEmpty()
  plotAllObj1(dat,fitHl,"VegType","H",tc,brewer.pal(9,"Paired")[1:2],0.10,2008)    # blue
  plotEmpty()
  plotAllObj1(dat,fitHa,"VegType","H",tc,brewer.pal(9,"Paired")[1:2],0.10,2008)    # blue
  plotEmpty()
  plotAllObj1(dat,fitSu,"VegType","S",tc,brewer.pal(9,"Paired")[7:8],0.10,2008)    # orange
  plotEmpty()
  plotAllObj1(dat,fitSl,"VegType","S",tc,brewer.pal(9,"Paired")[7:8],0.10,2008)    # orange
  plotEmpty()
  plotAllObj1(dat,fitSa,"VegType","S",tc,brewer.pal(9,"Paired")[7:8],0.10,2008)    # orange
  plotEmpty()
  plotAllObj1(dat,fitTu,"VegType","T",tc,brewer.pal(9,"Paired")[3:4],0.10,2008)    # green
  plotEmpty()
  plotAllObj1(dat,fitTl,"VegType","T",tc,brewer.pal(9,"Paired")[3:4],0.10,2008)    # green
  plotEmpty()
  plotAllObj1(dat,fitTa,"VegType","T",tc,brewer.pal(9,"Paired")[3:4],0.10,2008)    # green
  plotEmpty()
  plotAllObj1(dat,fitAu,"VegType","A",tc,brewer.pal(9,"Paired")[5:6],0.10,2008)    # orange
  plotEmpty()
  plotAllObj1(dat,fitAl,"VegType","A",tc,brewer.pal(9,"Paired")[5:6],0.10,2008)    # orange
  plotEmpty()
  plotAllObj1(dat,fitAa,"VegType","A",tc,brewer.pal(9,"Paired")[5:6],0.10,2008)    # orange
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
  plotHistObj1(dat,"VegType","A",5,"pCoveru")
  plotEmpty()
  plotHistObj1(dat,"VegType","A",5,"pCoverl")
  plotEmpty()
  plotHistObj1(dat,"VegType","A",5,"pCovera")
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
    plotOneObj1(dat,fitHu,"VegType","H",tc,brewer.pal(9,"Paired")[1:2],locCode,0.10,2008)    # blue
    plotEmpty()
    plotOneObj1(dat,fitHl,"VegType","H",tc,brewer.pal(9,"Paired")[1:2],locCode,0.10,2008)    # blue
    plotEmpty()
    plotOneObj1(dat,fitHa,"VegType","H",tc,brewer.pal(9,"Paired")[1:2],locCode,0.10,2008)    # blue
    plotEmpty()
    plotOneObj1(dat,fitSu,"VegType","S",tc,brewer.pal(9,"Paired")[7:8],locCode,0.10,2008)    # orange
    plotEmpty()
    plotOneObj1(dat,fitSl,"VegType","S",tc,brewer.pal(9,"Paired")[7:8],locCode,0.10,2008)    # orange
    plotEmpty()
    plotOneObj1(dat,fitSa,"VegType","S",tc,brewer.pal(9,"Paired")[7:8],locCode,0.10,2008)    # orange
    plotEmpty()
    plotOneObj1(dat,fitTu,"VegType","T",tc,brewer.pal(9,"Paired")[3:4],locCode,0.10,2008)    # green
    plotEmpty()
    plotOneObj1(dat,fitTl,"VegType","T",tc,brewer.pal(9,"Paired")[3:4],locCode,0.10,2008)    # green
    plotEmpty()
    plotOneObj1(dat,fitTa,"VegType","T",tc,brewer.pal(9,"Paired")[3:4],locCode,0.10,2008)    # green
    plotEmpty()
    plotOneObj1(dat,fitAu,"VegType","A",tc,brewer.pal(9,"Paired")[5:6],locCode,0.10,2008)    # orange
    plotEmpty()
    plotOneObj1(dat,fitAl,"VegType","A",tc,brewer.pal(9,"Paired")[5:6],locCode,0.10,2008)    # orange
    plotEmpty()
    plotOneObj1(dat,fitAa,"VegType","A",tc,brewer.pal(9,"Paired")[5:6],locCode,0.10,2008)    # orange
    plotEmpty()
    
    #   ---- Row (j + 1) + 9
    plotEmpty()
    
  }
  
  #   ---- Row End
  plotText("This is going to be a boring set of footnotes.",0.5)
  
  dev.off()
  
}


