makeBigPlotAssumptionsObj2 <- function(tc){
 
  
  
  png(paste0("//lar-file-srv/Data/NPS/Prairie/Analysis/Trending/Objective2-Assumptions.png"),width=24,height=90,units="in",res=300)
    
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
  
  
  #   ---- Row 13
  plotText("Residuals vs.\nFitted Values",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotResidFit(fitHu,brewer.pal(9,"Paired")[1])
  plotEmpty()
  plotResidFit(fitHl,brewer.pal(9,"Paired")[1])
  plotEmpty()
  plotResidFit(fitHa,brewer.pal(9,"Paired")[1])
  plotEmpty()
  plotResidFit(fitSu,brewer.pal(9,"Paired")[7])
  plotEmpty()
  plotResidFit(fitSl,brewer.pal(9,"Paired")[7])
  plotEmpty()
  plotResidFit(fitSa,brewer.pal(9,"Paired")[7])
  plotEmpty()
  plotResidFit(fitTu,brewer.pal(9,"Paired")[3])
  plotEmpty()
  plotResidFit(fitTl,brewer.pal(9,"Paired")[3])
  plotEmpty()
  plotResidFit(fitTa,brewer.pal(9,"Paired")[3])
  plotEmpty()
  plotResidFit(fitAu,brewer.pal(9,"Paired")[5])
  plotEmpty()
  plotResidFit(fitAl,brewer.pal(9,"Paired")[5])
  plotEmpty()
  plotResidFit(fitAa,brewer.pal(9,"Paired")[5])
  plotEmpty() 
  
  #   ---- Row 14
  plotEmpty()
  
  #   ---- Row 15
  plotText("QQ Plot of\nResiduals",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotResidQQ(fitHu,brewer.pal(9,"Paired")[1])
  plotEmpty()
  plotResidQQ(fitHl,brewer.pal(9,"Paired")[1])
  plotEmpty()
  plotResidQQ(fitHa,brewer.pal(9,"Paired")[1])
  plotEmpty()
  plotResidQQ(fitSu,brewer.pal(9,"Paired")[7])
  plotEmpty()
  plotResidQQ(fitSl,brewer.pal(9,"Paired")[7])
  plotEmpty()
  plotResidQQ(fitSa,brewer.pal(9,"Paired")[7])
  plotEmpty()
  plotResidQQ(fitTu,brewer.pal(9,"Paired")[3])
  plotEmpty()
  plotResidQQ(fitTl,brewer.pal(9,"Paired")[3])
  plotEmpty()
  plotResidQQ(fitTa,brewer.pal(9,"Paired")[3])
  plotEmpty()
  plotResidQQ(fitAu,brewer.pal(9,"Paired")[5])
  plotEmpty()
  plotResidQQ(fitAl,brewer.pal(9,"Paired")[5])
  plotEmpty()
  plotResidQQ(fitAa,brewer.pal(9,"Paired")[5])
  plotEmpty()
  
  #   ---- Row 16
  plotEmpty()
  
  #   ---- Row 17
  plotText("Histogram of\nResiduals",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotResidHist(fitHu,brewer.pal(9,"Paired")[1])
  plotEmpty()
  plotResidHist(fitHl,brewer.pal(9,"Paired")[1])
  plotEmpty()
  plotResidHist(fitHa,brewer.pal(9,"Paired")[1])
  plotEmpty()
  plotResidHist(fitSu,brewer.pal(9,"Paired")[7])
  plotEmpty()
  plotResidHist(fitSl,brewer.pal(9,"Paired")[7])
  plotEmpty()
  plotResidHist(fitSa,brewer.pal(9,"Paired")[7])
  plotEmpty()
  plotResidHist(fitTu,brewer.pal(9,"Paired")[3])
  plotEmpty()
  plotResidHist(fitTl,brewer.pal(9,"Paired")[3])
  plotEmpty()
  plotResidHist(fitTa,brewer.pal(9,"Paired")[3])
  plotEmpty()
  plotResidHist(fitAu,brewer.pal(9,"Paired")[5])
  plotEmpty()
  plotResidHist(fitAl,brewer.pal(9,"Paired")[5])
  plotEmpty()
  plotResidHist(fitAa,brewer.pal(9,"Paired")[5])
  plotEmpty()
  
  #   ---- Row 18
  plotEmpty()
  
  #   ---- Row 19
  plotText("Histograms of\nResiduals\nTransects",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotResidGroupQQ(fitHu,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"LocCode",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitHl,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"LocCode",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitHa,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"LocCode",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitSu,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"LocCode",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitSl,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"LocCode",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitSa,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"LocCode",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitTu,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"LocCode",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitTl,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"LocCode",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitTa,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"LocCode",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitAu,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"LocCode",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitAl,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"LocCode",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitAa,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"LocCode",type="Residuals")
  plotEmpty()
  
  #   ---- Row 20
  plotEmpty()
  
  #   ---- Row 21
  plotText("Box Plots of\nResiduals\nTransects",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotResidGroupBox(fitHu,brewer.pal(9,"Paired")[1],"LocCode")
  plotEmpty()
  plotResidGroupBox(fitHl,brewer.pal(9,"Paired")[1],"LocCode")
  plotEmpty()
  plotResidGroupBox(fitHa,brewer.pal(9,"Paired")[1],"LocCode")
  plotEmpty()
  plotResidGroupBox(fitSu,brewer.pal(9,"Paired")[7],"LocCode")
  plotEmpty()
  plotResidGroupBox(fitSl,brewer.pal(9,"Paired")[7],"LocCode")
  plotEmpty()
  plotResidGroupBox(fitSa,brewer.pal(9,"Paired")[7],"LocCode")
  plotEmpty()
  plotResidGroupBox(fitTu,brewer.pal(9,"Paired")[3],"LocCode")
  plotEmpty()
  plotResidGroupBox(fitTl,brewer.pal(9,"Paired")[3],"LocCode")
  plotEmpty()
  plotResidGroupBox(fitTa,brewer.pal(9,"Paired")[3],"LocCode")
  plotEmpty()
  plotResidGroupBox(fitAu,brewer.pal(9,"Paired")[5],"LocCode")
  plotEmpty()
  plotResidGroupBox(fitAl,brewer.pal(9,"Paired")[5],"LocCode")
  plotEmpty()
  plotResidGroupBox(fitAa,brewer.pal(9,"Paired")[5],"LocCode")
  plotEmpty()

  #   ---- Row 22
  plotEmpty()
  
  #   ---- Row 23
  plotText("Histogram Plots of\nResiduals\nYears",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotResidGroupQQ(fitHu,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"Year",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitHl,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"Year",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitHa,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"Year",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitSu,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"Year",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitSl,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"Year",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitSa,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"Year",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitTu,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"Year",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitTl,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"Year",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitTa,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"Year",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitAu,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"Year",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitAl,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"Year",type="Residuals")
  plotEmpty()
  plotResidGroupQQ(fitAa,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"Year",type="Residuals")
  plotEmpty()
  
  #   ---- Row 24
  plotEmpty()
  
  #   ---- Row 25
  plotText("Box Plots of\nResiduals\nYears",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotResidGroupBox(fitHu,brewer.pal(9,"Paired")[1],"Year")
  plotEmpty()
  plotResidGroupBox(fitHl,brewer.pal(9,"Paired")[1],"Year")
  plotEmpty()
  plotResidGroupBox(fitHa,brewer.pal(9,"Paired")[1],"Year")
  plotEmpty()
  plotResidGroupBox(fitSu,brewer.pal(9,"Paired")[7],"Year")
  plotEmpty()
  plotResidGroupBox(fitSl,brewer.pal(9,"Paired")[7],"Year")
  plotEmpty()
  plotResidGroupBox(fitSa,brewer.pal(9,"Paired")[7],"Year")
  plotEmpty()
  plotResidGroupBox(fitTu,brewer.pal(9,"Paired")[3],"Year")
  plotEmpty()
  plotResidGroupBox(fitTl,brewer.pal(9,"Paired")[3],"Year")
  plotEmpty()
  plotResidGroupBox(fitTa,brewer.pal(9,"Paired")[3],"Year")
  plotEmpty()
  plotResidGroupBox(fitAu,brewer.pal(9,"Paired")[5],"Year")
  plotEmpty()
  plotResidGroupBox(fitAl,brewer.pal(9,"Paired")[5],"Year")
  plotEmpty()
  plotResidGroupBox(fitAa,brewer.pal(9,"Paired")[5],"Year")
  plotEmpty()
  
  #   ---- Row 26
  plotEmpty()
  
  #   ---- Row 27
  plotText("QQ Plots of\nRandom Effects\nTransect Intercepts",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotResidGroupQQ(fitHu,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"LocCode",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitHl,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"LocCode",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitHa,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"LocCode",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitSu,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"LocCode",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitSl,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"LocCode",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitSa,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"LocCode",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitTu,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"LocCode",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitTl,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"LocCode",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitTa,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"LocCode",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitAu,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"LocCode",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitAl,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"LocCode",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitAa,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"LocCode",type="RandomEffects")
  plotEmpty()
  
  #   ---- Row 26
  plotEmpty()
  
  #   ---- Row 27
  plotText("QQ Plots of\nRandom Effects\nTransect Slopes",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotResidGroupQQ(fitHu,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"LocCode",type="RandomEffects",column=2)
  plotEmpty()
  plotResidGroupQQ(fitHl,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"LocCode",type="RandomEffects",column=2)
  plotEmpty()
  plotResidGroupQQ(fitHa,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"LocCode",type="RandomEffects",column=2)
  plotEmpty()
  plotResidGroupQQ(fitSu,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"LocCode",type="RandomEffects",column=2)
  plotEmpty()
  plotResidGroupQQ(fitSl,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"LocCode",type="RandomEffects",column=2)
  plotEmpty()
  plotResidGroupQQ(fitSa,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"LocCode",type="RandomEffects",column=2)
  plotEmpty()
  plotResidGroupQQ(fitTu,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"LocCode",type="RandomEffects",column=2)
  plotEmpty()
  plotResidGroupQQ(fitTl,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"LocCode",type="RandomEffects",column=2)
  plotEmpty()
  plotResidGroupQQ(fitTa,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"LocCode",type="RandomEffects",column=2)
  plotEmpty()
  plotResidGroupQQ(fitAu,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"LocCode",type="RandomEffects",column=2)
  plotEmpty()
  plotResidGroupQQ(fitAl,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"LocCode",type="RandomEffects",column=2)
  plotEmpty()
  plotResidGroupQQ(fitAa,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"LocCode",type="RandomEffects",column=2)
  plotEmpty()
  
  #   ---- Row 28
  plotEmpty()
  
  #   ---- Row 29
  plotText("QQ Plots of\nRandom Effects\nYear Intercepts",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotResidGroupQQ(fitHu,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"Year",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitHl,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"Year",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitHa,brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],"Year",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitSu,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"Year",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitSl,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"Year",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitSa,brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],"Year",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitTu,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"Year",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitTl,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"Year",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitTa,brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],"Year",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitAu,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"Year",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitAl,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"Year",type="RandomEffects")
  plotEmpty()
  plotResidGroupQQ(fitAa,brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],"Year",type="RandomEffects")
  plotEmpty()
  
  #   ---- Row 30
  plotEmpty()
  
  #   ---- Row 31
  plotText("Paired Plots of\nTransect Intercepts\n& Slope Random Effects",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotRandCorr(fitHu,"LocCode","Year",brewer.pal(9,"Paired")[1])
  plotEmpty()
  plotRandCorr(fitHl,"LocCode","Year",brewer.pal(9,"Paired")[1])
  plotEmpty()
  plotRandCorr(fitHa,"LocCode","Year",brewer.pal(9,"Paired")[1])
  plotEmpty()
  plotRandCorr(fitSu,"LocCode","Year",brewer.pal(9,"Paired")[7])
  plotEmpty()
  plotRandCorr(fitSl,"LocCode","Year",brewer.pal(9,"Paired")[7])
  plotEmpty()
  plotRandCorr(fitSa,"LocCode","Year",brewer.pal(9,"Paired")[7])
  plotEmpty()
  plotRandCorr(fitTu,"LocCode","Year",brewer.pal(9,"Paired")[3])
  plotEmpty()
  plotRandCorr(fitTl,"LocCode","Year",brewer.pal(9,"Paired")[3])
  plotEmpty()
  plotRandCorr(fitTa,"LocCode","Year",brewer.pal(9,"Paired")[3])
  plotEmpty()
  plotRandCorr(fitAu,"LocCode","Year",brewer.pal(9,"Paired")[5])
  plotEmpty()
  plotRandCorr(fitAl,"LocCode","Year",brewer.pal(9,"Paired")[5])
  plotEmpty()
  plotRandCorr(fitAa,"LocCode","Year",brewer.pal(9,"Paired")[5])
  plotEmpty()
  
  #   ---- Row 30
  plotEmpty()
  
  #   ---- Row 31
  plotText("QQ Plots of\nTransect Intercepts\nby Year",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotQQByYear(fitHu,"LocCode","Year",brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],column=1)
  plotEmpty()
  plotQQByYear(fitHl,"LocCode","Year",brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],column=1)
  plotEmpty()
  plotQQByYear(fitHa,"LocCode","Year",brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],column=1)
  plotEmpty()
  plotQQByYear(fitSu,"LocCode","Year",brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],column=1)
  plotEmpty()
  plotQQByYear(fitSl,"LocCode","Year",brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],column=1)
  plotEmpty()
  plotQQByYear(fitSa,"LocCode","Year",brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],column=1)
  plotEmpty()
  plotQQByYear(fitTu,"LocCode","Year",brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],column=1)
  plotEmpty()
  plotQQByYear(fitTl,"LocCode","Year",brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],column=1)
  plotEmpty()
  plotQQByYear(fitTa,"LocCode","Year",brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],column=1)
  plotEmpty()
  plotQQByYear(fitAu,"LocCode","Year",brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],column=1)
  plotEmpty()
  plotQQByYear(fitAl,"LocCode","Year",brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],column=1)
  plotEmpty()
  plotQQByYear(fitAa,"LocCode","Year",brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],column=1)
  plotEmpty()
  
  #   ---- Row 32
  plotEmpty()
  
  #   ---- Row 33
  plotText("QQ Plots of\nTransect Slopes\nby Year",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotQQByYear(fitHu,"LocCode","Year",brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],column=2)
  plotEmpty()
  plotQQByYear(fitHl,"LocCode","Year",brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],column=2)
  plotEmpty()
  plotQQByYear(fitHa,"LocCode","Year",brewer.pal(9,"Paired")[1],brewer.pal(9,"Paired")[2],column=2)
  plotEmpty()
  plotQQByYear(fitSu,"LocCode","Year",brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],column=2)
  plotEmpty()
  plotQQByYear(fitSl,"LocCode","Year",brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],column=2)
  plotEmpty()
  plotQQByYear(fitSa,"LocCode","Year",brewer.pal(9,"Paired")[7],brewer.pal(9,"Paired")[8],column=2)
  plotEmpty()
  plotQQByYear(fitTu,"LocCode","Year",brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],column=2)
  plotEmpty()
  plotQQByYear(fitTl,"LocCode","Year",brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],column=2)
  plotEmpty()
  plotQQByYear(fitTa,"LocCode","Year",brewer.pal(9,"Paired")[3],brewer.pal(9,"Paired")[4],column=2)
  plotEmpty()
  plotQQByYear(fitAu,"LocCode","Year",brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],column=2)
  plotEmpty()
  plotQQByYear(fitAl,"LocCode","Year",brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],column=2)
  plotEmpty()
  plotQQByYear(fitAa,"LocCode","Year",brewer.pal(9,"Paired")[5],brewer.pal(9,"Paired")[6],column=2)
  plotEmpty()
  
  #   ---- Row 34
  plotEmpty()
  
  #   ---- Row 35
  plotText("QQ Plots of\nTransect Intercepts",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotRanEfQQ("LocCode",fitHu,brewer.pal(9,"Paired")[1],column=1)
  plotEmpty()
  plotRanEfQQ("LocCode",fitHl,brewer.pal(9,"Paired")[1],column=1)
  plotEmpty()
  plotRanEfQQ("LocCode",fitHa,brewer.pal(9,"Paired")[1],column=1)
  plotEmpty()
  plotRanEfQQ("LocCode",fitSu,brewer.pal(9,"Paired")[7],column=1)
  plotEmpty()
  plotRanEfQQ("LocCode",fitSl,brewer.pal(9,"Paired")[7],column=1)
  plotEmpty()
  plotRanEfQQ("LocCode",fitSa,brewer.pal(9,"Paired")[7],column=1)
  plotEmpty()
  plotRanEfQQ("LocCode",fitTu,brewer.pal(9,"Paired")[3],column=1)
  plotEmpty()
  plotRanEfQQ("LocCode",fitTl,brewer.pal(9,"Paired")[3],column=1)
  plotEmpty()
  plotRanEfQQ("LocCode",fitTa,brewer.pal(9,"Paired")[3],column=1)
  plotEmpty()
  plotRanEfQQ("LocCode",fitAu,brewer.pal(9,"Paired")[5],column=1)
  plotEmpty()
  plotRanEfQQ("LocCode",fitAl,brewer.pal(9,"Paired")[5],column=1)
  plotEmpty()
  plotRanEfQQ("LocCode",fitAa,brewer.pal(9,"Paired")[5],column=1)
  plotEmpty()
  
  #   ---- Row 36
  plotEmpty()
  
  #   ---- Row 37
  plotText("QQ Plots of\nTransect Slopes",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotRanEfQQ("LocCode",fitHu,brewer.pal(9,"Paired")[1],column=2)
  plotEmpty()
  plotRanEfQQ("LocCode",fitHl,brewer.pal(9,"Paired")[1],column=2)
  plotEmpty()
  plotRanEfQQ("LocCode",fitHa,brewer.pal(9,"Paired")[1],column=2)
  plotEmpty()
  plotRanEfQQ("LocCode",fitSu,brewer.pal(9,"Paired")[7],column=2)
  plotEmpty()
  plotRanEfQQ("LocCode",fitSl,brewer.pal(9,"Paired")[7],column=2)
  plotEmpty()
  plotRanEfQQ("LocCode",fitSa,brewer.pal(9,"Paired")[7],column=2)
  plotEmpty()
  plotRanEfQQ("LocCode",fitTu,brewer.pal(9,"Paired")[3],column=2)
  plotEmpty()
  plotRanEfQQ("LocCode",fitTl,brewer.pal(9,"Paired")[3],column=2)
  plotEmpty()
  plotRanEfQQ("LocCode",fitTa,brewer.pal(9,"Paired")[3],column=2)
  plotEmpty()
  plotRanEfQQ("LocCode",fitAu,brewer.pal(9,"Paired")[5],column=2)
  plotEmpty()
  plotRanEfQQ("LocCode",fitAl,brewer.pal(9,"Paired")[5],column=2)
  plotEmpty()
  plotRanEfQQ("LocCode",fitAa,brewer.pal(9,"Paired")[5],column=2)
  plotEmpty()
  
  #   ---- Row 38
  plotEmpty()
  
  #   ---- Row 39
  plotText("Spatial Plots of\nResiduals",1.5)
  plotEmpty()
  plotEmpty()
  plotEmpty()
  plotSpatialResids(fitHu,STransects,2012,tc,"Blues")
  plotEmpty()
  plotSpatialResids(fitHl,STransects,2012,tc,"Blues")
  plotEmpty()
  plotSpatialResids(fitHa,STransects,2012,tc,"Blues")
  plotEmpty()
  plotSpatialResids(fitSu,STransects,2012,tc,"Oranges")
  plotEmpty()
  plotSpatialResids(fitSl,STransects,2012,tc,"Oranges")
  plotEmpty()
  plotSpatialResids(fitSa,STransects,2012,tc,"Oranges")
  plotEmpty()
  plotSpatialResids(fitTu,STransects,2012,tc,"Greens")
  plotEmpty()
  plotSpatialResids(fitTl,STransects,2012,tc,"Greens")
  plotEmpty()
  plotSpatialResids(fitTa,STransects,2012,tc,"Greens")
  plotEmpty()
  plotSpatialResids(fitAu,STransects,2012,tc,"Reds")
  plotEmpty()
  plotSpatialResids(fitAl,STransects,2012,tc,"Reds")
  plotEmpty()
  plotSpatialResids(fitAa,STransects,2012,tc,"Reds")
  plotEmpty()
  
  dev.off()
  
  
}