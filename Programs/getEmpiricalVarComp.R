getEmpiricalVarComp <- function(fit,group){
  
  # fit <- fitHl
  # group <- "LocCode"
  
  #   ---- Get individual level standard deviations, excluding NAs (from n=1 most likely).
#   varEsts <- tapply(fit@frame[,"pCoverl"],fit@frame[,group],mean)
#   varRange <- range(varEsts[!is.na(varEsts)])
#   varMean <- var(varEsts[!is.na(varEsts)])
  
  varEsts <- tapply(resid(fit),fit@frame[,group],stats::var)
  varRange <- range(varEsts[!is.na(varEsts)])
  varMean <- mean(varEsts[!is.na(varEsts)])
  
  return(varMean)
  
}


