convertmeans2smd <-
function(table, measure="SMD"){
  ## given table of continuous data (mean,sd,n) of both ctrl and expt arms
  ## return standardized mean difference and variance
  
  ## requires package "metafor" to use function escalc()
  
  fixedmodel <- escalc(measure=measure,
                       m1i=table$expt.mean, sd1i=table$expt.sd, n1i=table$expt.n,
                       m2i=table$ctrl.mean, sd2i=table$ctrl.sd, n2i=table$ctrl.n,
                       data=table, append=TRUE)  
  return(fixedmodel)
}
