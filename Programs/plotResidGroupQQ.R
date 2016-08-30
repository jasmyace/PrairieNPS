plotResidGroupQQ <- function(fit,col,col2,group,type,column=1){
  
  # fit <- fitSl
  # col <- brewer.pal(9,"Paired")[1]
  # col2 <- brewer.pal(9,"Paired")[2]
  # group <- "LocCode"
  # type <- "RandomEffects"
  # column <- 1
  
  #   ---- Identify the unique levels in the group for which we want to 
  #   ---- plot a qq curve.  
  
  if( type == "Residuals" ){
    levels <- unique(fit@frame[,group])
    y <- resid(fit)
  } else if( type == "RandomEffects" ){
    levels <- 1#rownames(ranef(fit)[names(ranef(fit)) == group][[1]])
    y <- ranef(fit)[names(ranef(fit)) == group][[1]][,column]
    names(y) <- rownames(ranef(fit)[names(ranef(fit)) == group][[1]])
  }
  
  #   ---- We need to the xlim and ylim values of all the data.  This makes 
  #   ---- plotting different levels work -- we need the same scaling for each
  #   ---- plotted level.  Dig into the qqnorm.default function and get the 
  #   ---- to make the qqnorm plot for all data.  This way, we get the min 
  #   ---- max values we need for each of x and y.  

  ylim <- range(y)
  n <- length(y)
  x <- qnorm(ppoints(n))[order(order(y))]
  xlim <- range(x)
  
  #   ---- Commented out:  these two statements, after running the above, should 
  #   ---- produce the same graph (outside of ancillary stuff, e.g., title).
  # plot(x,y,xlab="Theoretical Quantities",ylab="Sample Quantiles",ylim=ylim)
  # qqnorm(resid(fit))
  
  for(i in 1:length(levels)){
    
    par(mar=c(1,1,0,0))
    
    #   ---- Pick out the residuals for the ith level of the group. 
    group_i <- levels[i]
    
    if( type == "Residuals" ){
      keepers <- rownames(fit@frame[fit@frame[,group] == group_i,])
    } else if( type == "RandomEffects" ) {
      keepers <- rownames(ranef(fit)[names(ranef(fit)) == group][[1]])
    }
    residKeepers <- sort(y[names(y) %in% keepers])
    
    if(i == 1){
      qqnorm(residKeepers,type="l",col=col,xlab="",ylab="",xlim=xlim,ylim=ylim,main="",xaxt="n",yaxt="n",bty="n")
      axis(1,at=pretty(x),labels=pretty(x),cex.axis=0.5,tck=-0.012)         # x-axis
      axis(2,at=pretty(y),labels=pretty(y),cex.axis=0.5,tck=-0.008)         # y-axis
      mtext("Theoretical Quantiles",side=1,line=1,cex=0.5,las=1)
      mtext("Sample Quantiles",side=2,line=1,cex=0.5)
      par(new=TRUE)
      qqnorm(residKeepers,type="p",col=col,xlab="",ylab="",xlim=xlim,ylim=ylim,main="",xaxt="n",yaxt="n",pch=19,cex=0.5,bty="n")
      
      #   ---- Add red line of perfection.
      slope <- diff(y)/diff(x)
      int <- y[1L] - slope * x[1L]
      abline(int,slope,col=col2,lwd=2)
    } else {
      par(new=TRUE)
      qqnorm(residKeepers,type="l",col=col,xlab="",ylab="",xlim=xlim,ylim=ylim,main="",xaxt="n",yaxt="n",bty="n")
      par(new=TRUE)
      qqnorm(residKeepers,type="p",col=col,xlab="",ylab="",xlim=xlim,ylim=ylim,main="",xaxt="n",yaxt="n",pch=19,cex=0.5,bty="n")
    }
#     absResidKeepers <- residKeepers
#     if(max(absResidKeepers - mean(absResidKeepers) / sd(absResidKeepers)) > 1.645 & length(absResidKeepers) > 1){
#       text(head(jitter(c(-1,1)),1),max(residKeepers),group_i,cex=0.5)
#       par(new=TRUE)
#       qqnorm(residKeepers,type="l",col="red",xlab="",ylab="",xlim=xlim,ylim=ylim,main="",xaxt="n",yaxt="n",bty="n")
#       par(new=TRUE)
#       qqnorm(residKeepers,type="p",col="red",xlab="",ylab="",xlim=xlim,ylim=ylim,main="",xaxt="n",yaxt="n",pch=19,cex=0.5,bty="n")
#     }
  }

}