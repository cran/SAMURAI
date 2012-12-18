convertbin2effectsize <-
function(table, measure="RR"){
  ## given table of binary count data of both ctrl and expt arms
  ## return effect size (such as log RR) and variance (for a fixed effects model)
  
  ## requires package "metafor" to use function escalc()
  
  fixedmodel <- escalc(measure=measure, 
                       ai=table$expt.events, n1i=table$expt.n, 
                       ci=table$ctrl.events, n2i=table$ctrl.n, 
                       data=table, append=TRUE)
  
  return(fixedmodel)
}
