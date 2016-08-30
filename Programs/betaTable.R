betaTable <- function(full,reduced,outcome){
  
  # full <- fitHu
  # reduced <- rfitHu
  # outcome <- "Untransformed"
  
  options(scipen=10)
  
  #   ---- Get main model components. 
  vc <- as.data.frame(VarCorr(full))
  
  #   ---- Get standard deviations of variance components.
  sigma_b  <- format(round(vc[4,5],2),nsmall=2)
  sigma_a  <- format(round(vc[1,5],2),nsmall=2)
  sigma_t  <- format(round(vc[2,5],2),nsmall=2)
  rho_at <- format(round(vc[3,5],2),nsmall=2)  # this is the correlation.
  sigma_e  <- format(round(vc[5,5],2),nsmall=2)
  
  #   ---- Keep in mind that sigma_ab = rho * sqrt( sigma_a^2 * sigma_b^2 )
  
  #   ---- Calculate intraclass correlation estimates.
  #   ---- I don't think these are appropriate with the random ints and slopes correlation.  
  
  #   ---- Get fixed effects stuff.  
  beta0    <- format(round(coef(summary(full))[1,1],2),nsmall=2)
  se_beta0 <- format(round(coef(summary(full))[1,2],2),nsmall=2)
  beta1    <- format(round(coef(summary(full))[2,1],2),nsmall=2)
  se_beta1 <- format(round(coef(summary(full))[2,2],2),nsmall=2)
  tStat    <- format(round(coef(summary(full))[2,3],2),nsmall=2)
  
  #   ---- Get appropriate degrees of freedom to conduct a test for trend.
  varBeta <- vcov(full)[2,2]       # Get estimated variance of the beta1 parameter (trend).
  DDF      <- format(round(KRmodcomp(full,reduced)$stats$ddf,2),nsmall=2)   # Kenwood-Roger (1997) estimate of degrees of freedom. 
  p        <- format(round(2*pt(abs(coef(summary(full))[2,3]),KRmodcomp(full,reduced)$stats$ddf,lower=FALSE),4),nsmall=4)
  logLik   <- format(round(logLik(full),2),nsmall=2)
  
  #   ---- Form a data row. 
  df <- data.frame(rbind(sigma_b,sigma_a,sigma_t,rho_at,sigma_e,beta0,se_beta0,beta1,se_beta1,tStat,DDF,p,logLik))
  colnames(df) <- outcome
  parm <- rownames(df)
  rownames(df) <- NULL
  
  return(list(df,parm))
}