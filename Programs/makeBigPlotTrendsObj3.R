makeBigPlotTrendsObj3 <- function(tc){
  
  
  png(paste0("//lar-file-srv/Data/NPS/Prairie/Analysis/Trending/Objective3-Trends.png"),width=24,height=90,units="in",res=300)
    
  #   ---- Smaller, with 3.
  string <- c(rep(1,22),rep(2,22),seq(3,6),rep(7,5),8,rep(9,5),10,rep(11,5),12,rep(13,22),seq(14,17),rep(18,5),19,rep(20,5),21,rep(22,5),23,rep(24,22),seq(25,46),rep(47,22),seq(48,69),rep(70,22),seq(71,92),rep(93,22),seq(94,115),rep(116,22),seq(117,138),rep(139,22),seq(140,161),rep(162,22),seq(163,184),rep(185,22),seq(186,207),rep(208,22),seq(209,230),rep(231,22),seq(232,253),rep(254,22),seq(255,276),rep(277,22),seq(278,299),rep(300,22),seq(301,322),rep(323,22),seq(324,345),rep(346,22),seq(347,368),rep(369,22),seq(370,391),rep(392,22),seq(393,414),rep(415,22),seq(416,437),rep(438,22),seq(439,460),rep(461,22),seq(462,483),rep(484,22),seq(485,506),rep(507,22),seq(508,529),rep(530,22),seq(531,552),rep(553,22),seq(554,575),rep(576,22),seq(577,598),rep(599,22),seq(600,621),rep(622,22),seq(623,644),rep(645,22),seq(646,667),rep(668,22),seq(669,690),rep(691,22),seq(692,713),rep(714,22),seq(715,736),rep(737,22),seq(738,759),rep(760,22),seq(761,782),rep(783,22),seq(784,805),rep(806,22),seq(807,828),rep(829,22),seq(830,851),rep(852,22),seq(853,874),rep(875,22),seq(876,897),rep(898,22),seq(899,920),rep(921,22),seq(922,943),rep(944,22),seq(945,966),rep(967,22),seq(968,989),rep(990,22))
  widths <- c(0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005,0.0859,0.005)
  heights <- c(0.02,0.005,0.01,0.005,0.04,0.005,0.01,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.005,0.01683,0.01)
  a <- layout(matrix(string,90,22,byrow=TRUE),widths=widths,heights=heights)
  layout.show(a)
 
  
  #   ---- Row 1
  plotText(paste0("Objective 3 Analytics"),5)
  
  #   ---- Row 2
  plotEmpty()
  
  #   ---- Row 3
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotText("High-Qual Herbaceous (0-10%)",3.5)
  plotEmpty()
  plotText("Med-Qual Herbaceous (11-49%)",3.5)
  plotEmpty()
  plotText("Low-Qual Herbaceous (50-100%)",3.5)
  plotEmpty()
  
  #   ---- Row 4
  plotEmpty()
  
  #   ---- Row 5
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotSpatialTrends(fit0.10u,STransects,2012,tc,"pCoveru","Blues")
  plotEmpty()
  plotSpatialTrends(fit11.49u,STransects,2012,tc,"pCoveru","Oranges")
  plotEmpty()
  plotSpatialTrends(fit50.100u,STransects,2012,tc,"pCoveru","Greens")
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
  
  #   ---- Row 8
  plotEmpty()
  
  #   ---- Row 9
  plotText("Piepho & Ogutu\nPlot of Fixed Effects",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotAllObj1(dat,fit0.10u,"CoverClass","0-10%",tc,brewer.pal(9,"Paired")[1:2],0.10,2008)    # blue
  plotEmpty()
  plotAllObj1(dat,fit0.10l,"CoverClass","0-10%",tc,brewer.pal(9,"Paired")[1:2],0.10,2008)    # blue
  plotEmpty()
  plotAllObj1(dat,fit0.10a,"CoverClass","0-10%",tc,brewer.pal(9,"Paired")[1:2],0.10,2008)    # blue
  plotEmpty()
  plotAllObj1(dat,fit11.49u,"CoverClass","11-49%",tc,brewer.pal(9,"Paired")[7:8],0.10,2008)    # orange
  plotEmpty()
  plotAllObj1(dat,fit11.49l,"CoverClass","11-49%",tc,brewer.pal(9,"Paired")[7:8],0.10,2008)    # orange
  plotEmpty()
  plotAllObj1(dat,fit11.49a,"CoverClass","11-49%",tc,brewer.pal(9,"Paired")[7:8],0.10,2008)    # orange
  plotEmpty()
  plotAllObj1(dat,fit50.100u,"CoverClass","50-100%",tc,brewer.pal(9,"Paired")[3:4],0.10,2008)    # green
  plotEmpty()
  plotAllObj1(dat,fit50.100l,"CoverClass","50-100%",tc,brewer.pal(9,"Paired")[3:4],0.10,2008)    # green
  plotEmpty()
  plotAllObj1(dat,fit50.100a,"CoverClass","50-100%",tc,brewer.pal(9,"Paired")[3:4],0.10,2008)    # green
  plotEmpty()
  
  #   ---- Row 10
  plotEmpty()
  
  #   ---- Row 11
  plotText("Histogram of\nOutcomes",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotHistObj1(dat,"CoverClass","0-10%",5,"pCoveru")
  plotEmpty()
  plotHistObj1(dat,"CoverClass","0-10%",5,"pCoverl")
  plotEmpty()
  plotHistObj1(dat,"CoverClass","0-10%",5,"pCovera")
  plotEmpty()
  plotHistObj1(dat,"CoverClass","11-49%",5,"pCoveru")
  plotEmpty()
  plotHistObj1(dat,"CoverClass","11-49%",5,"pCoverl")
  plotEmpty()
  plotHistObj1(dat,"CoverClass","11-49%",5,"pCovera")
  plotEmpty()
  plotHistObj1(dat,"CoverClass","50-100%",95,"pCoveru")
  plotEmpty()
  plotHistObj1(dat,"CoverClass","50-100%",95,"pCoverl")
  plotEmpty()
  plotHistObj1(dat,"CoverClass","50-100%",95,"pCovera")
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
    plotOneObj1(dat,fit0.10u,"CoverClass","0-10%",tc,brewer.pal(9,"Paired")[1:2],locCode,0.10,2008)    # blue
    plotEmpty()
    plotOneObj1(dat,fit0.10l,"CoverClass","0-10%",tc,brewer.pal(9,"Paired")[1:2],locCode,0.10,2008)    # blue
    plotEmpty()
    plotOneObj1(dat,fit0.10a,"CoverClass","0-10%",tc,brewer.pal(9,"Paired")[1:2],locCode,0.10,2008)    # blue
    plotEmpty()
    plotOneObj1(dat,fit11.49u,"CoverClass","11-49%",tc,brewer.pal(9,"Paired")[7:8],locCode,0.10,2008)    # orange
    plotEmpty()
    plotOneObj1(dat,fit11.49l,"CoverClass","11-49%",tc,brewer.pal(9,"Paired")[7:8],locCode,0.10,2008)    # orange
    plotEmpty()
    plotOneObj1(dat,fit11.49a,"CoverClass","11-49%",tc,brewer.pal(9,"Paired")[7:8],locCode,0.10,2008)    # orange
    plotEmpty()
    plotOneObj1(dat,fit50.100u,"CoverClass","50-100%",tc,brewer.pal(9,"Paired")[3:4],locCode,0.10,2008)    # green
    plotEmpty()
    plotOneObj1(dat,fit50.100l,"CoverClass","50-100%",tc,brewer.pal(9,"Paired")[3:4],locCode,0.10,2008)    # green
    plotEmpty()
    plotOneObj1(dat,fit50.100a,"CoverClass","50-100%",tc,brewer.pal(9,"Paired")[3:4],locCode,0.10,2008)    # green
    plotEmpty()
    
    #   ---- Row (j + 1) + 9
    plotEmpty()
    
  }
  
  #   ---- Row End
  plotText("This is going to be a boring set of footnotes.",0.5)
  
  dev.off()
  
}


