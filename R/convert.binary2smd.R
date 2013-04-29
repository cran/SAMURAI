convert.binary2smd <-
function(table, measure="RR"){
  ## In: Table of binary count data of both ctrl and expt arms.
  ## Out: Adjusted SMD/Hedges g and its variance under a fixed effects model.
  ## Default: log risk ratio
  ## Calls: package "metafor" to use function escalc()
  ## Callers: binaryforest(), binary.table()
  fixedmodel <- escalc(measure=measure, data=table, append=TRUE,
    ai=table$expt.events, n1i=table$expt.n, 
    ci=table$ctrl.events, n2i=table$ctrl.n)
  return(fixedmodel)
}
