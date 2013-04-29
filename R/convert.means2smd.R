convert.means2smd <-
function(table, measure="SMD"){
  ## In: Table of continuous data (mean,sd,n) of both ctrl and expt arms
  ## Out: Adjusted SMD/Hedges g and variance under a fixed effects model 
  ## Calls: package "metafor" to use function escalc()
  ## Callers: contintuousforest(), binary.table()
  fixedmodel <- escalc(measure=measure, data=table, append=TRUE,
    m1i=table$expt.mean, sd1i=table$expt.sd, n1i=table$expt.n,
    m2i=table$ctrl.mean, sd2i=table$ctrl.sd, n2i=table$ctrl.n)  
  return(fixedmodel)
}
