plotHistObj1 <- function(dat,var,metric,xlabelCoord,outcome){
  
  # dat <- dat
  # var <- "VegType"
  # metric <- "T"
  # xlabelCoord <- 95
  # outcome <- "pCoverl"
  
  if(metric == "H" | metric == "0-10%"){
    col <- brewer.pal(10,"Paired")[2]
  } else if(metric == "S" | metric == "11-49%"){
    col <- brewer.pal(10,"Paired")[8]
  } else if(metric == "T" | metric == "50-100%"){
    col <- brewer.pal(10,"Paired")[4]
  } else if(metric == "D"){
    col <- brewer.pal(10,"Paired")[6]
  } else if(metric == "U"){
    col <- brewer.pal(10,"Paired")[10]
  }

  years <- sort(unique(dat$Year))
  nYears <- length(years)
  
  if(outcome == "pCoveru"){
    bM <- 100
    bm <- 0
    by <- 10
  } else if(outcome == "pCoverl"){
    bM <- max(dat[,outcome][dat[,outcome] < Inf]) + 1
    bm <- min(dat[,outcome]) - 1
    by <- 1
  } else if(outcome == "pCovera"){
    bM <- max(dat[,outcome][dat[,outcome] < Inf]) + 0.5
    bm <- min(dat[,outcome]) - 0.5
    by <- 0.20
  } else {
    break
  }
  
  helper <- "//lar-file-srv/Data/NPS/Prairie/Analysis/Helper"
  metric2 <- gsub("%","p",metric,fixed=TRUE)
  jpeg(paste0(helper,"/Histo ",metric2,".jpeg"),width=4,height=3,units="in",res=300)
  
  #   ---- Find out the counts of each year.  Find the max of these to ensure appropriate 
  #   ---- year-to-year comparisons. 
  maxVec <- rep(NA,nYears)
  for(i in 1:nYears){
    maxVec[i] <- max(hist(dat[dat[,var] == metric & dat$Year == years[i],outcome],breaks=seq(bm,bM,by),plot=FALSE)$counts)
  }
  
  #   ---- Make the plot. 
  par(mfrow=c(nYears,1))
  
  for(i in 1:nYears){
    par(mar=c(0.5,0,0.1,0))
    obj <- hist(dat[dat[,var] == metric & dat$Year == years[i],outcome],breaks=seq(bm,bM,by),plot=FALSE)
    labels <- obj$counts
    labels[labels == 0] <- NA
    hist(dat[dat[,var] == metric & dat$Year == years[i],outcome],breaks=seq(bm,bM,by),main="",xlab="",ylab="",ylim=c(0,max(maxVec + 1.5)),axes=FALSE,col=col,labels=as.character(labels))
    par(new=TRUE)
    text(xlabelCoord,5,years[i],cex=2.0,col=col)
    if(i == nYears){
      axis(1,at=obj$mids,labels=obj$mids)
    }
  }
  
  par(mfrow=c(1,1))
  
  dev.off()
  
  #   ---- Read the image back in for image plotting.  
  par(mar = c(0,0,0,0))
  plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE)
  u <- par("usr")
  img <- readJPEG(paste0(helper,"/Histo ",metric2,".jpeg"))
  rasterImage(img,u[1],u[3],u[2],u[4])
  
}