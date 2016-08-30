plotText <- function(text,cex){
  
  # text <- "Testing String"
  # cex <- 1.75
  
  par(mar = c(0,0,0,0))
  plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE)
  u <- par("usr")
  text(1,u[1] + 1.5*(u[2]-u[1])/3,text,cex=cex)                                                                                                        # place text in indicated spot
  
}