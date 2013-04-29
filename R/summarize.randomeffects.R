summarize.randomeffects <-
function(table, method="DL", confidencelevel=95, sigdigits=3,...){
  ## Input: Table of Hedges g and their variances under a fixed effects model
  ## Output: Random effects model meta-analytic summary
    ## Default setting: method="DL" for the DerSimonian & Laird method (1996)
  ## Requires: package 'metafor' to use the function rma()

  ## to avoid R CMD CHECK NOTE: "no visible binding for global variable"
  yi <- NULL
  
  ## The ... in this function acts as a 'garbage collector' for runaway parameters upstream.

  randmodel <- rma(yi, vi, data=table, method=method, level=confidencelevel, digits=sigdigits)  
  
  ## binary: summary log RR, or summary log OR
  m <- randmodel$b[1]  
  m.se <- randmodel$se[1]
  m.lcl <- randmodel$ci.lb[1]  
  m.ucl <- randmodel$ci.ub[1]
  
  ## binary: summary RR, or summary OR
  expm <- exp(m) 
  expm.lcl <- exp(randmodel$ci.lb[1]) 
  expm.ucl <- exp(randmodel$ci.ub[1]) 

  ## measures of heterogeneity
  tau2  <- randmodel$tau2[1]
  Q     <- randmodel$QE[1]  
  Qpval <- randmodel$QEp[1]
  
  out <- as.list(c(m, m.se, m.lcl, m.ucl, expm.lcl, expm, expm.ucl,tau2, Q, Qpval))
  names(out) <- c("m","m.se","m.lcl","m.ucl", "exp.m.lcl","exp.m","exp.m.ucl","tau2","Q","Qpval")
  return(out)
}
