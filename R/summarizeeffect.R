summarizeeffect <-
function(table, method="DL", confidencelevel=95){
  ## given a table of a fixed effects model with effect sizes and their variances
  ## returns the random effects model meta-analytic summary
  
  ## defaults: ## method="DL" : DerSimonian & Laird method (1996)
  
  ## requires package 'metafor' to use the function rma()

  ## to avoid R CMD CHECK NOTE: "no visible binding for global variable"
  yi <- NULL
  
  randmodel <- rma(yi, vi, data=table, method=method, level=confidencelevel)  
  
  m <- randmodel$b[1]
  m.se <- randmodel$se[1]
  m.lcl <- randmodel$ci.lb[1]  
  m.ucl <- randmodel$ci.ub[1]
  
  expm <- exp(m) 
  expm.lcl <- exp(randmodel$ci.lb[1]) 
  expm.ucl <- exp(randmodel$ci.ub[1]) 

  tau2 <- randmodel$tau2[1]
  
  out <- as.list(c(m, m.se, m.lcl, m.ucl, expm.lcl, expm, expm.ucl,tau2))
  names(out) <- c("m","m.se","m.lcl","m.ucl", "exp.m.lcl","exp.m","exp.m.ucl","tau2")
  return(out)
}
