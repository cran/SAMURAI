impute.smd.v <-
function(table, matchedgroups=FALSE){
  ## In: Table with ctrl.n, expt.n, and Hedge's g (sample estimate of the SMD)
  ## Out: estimates variance of Hedge's g using a "very good" approximation (Borenstein)
  
  ## Main Reference: Michael Borenstein, "Effect Sizes for Continuous Data", page 226,
  ## Chapter 12 in Cooper, Hedges, and Valentine, Handbook of Research Synthesis and Meta-analysis
  
  ## testing
  #   table <- table4
  
  table$flagmissing <- as.numeric(is.na(table$vi))
  
  n1 <- table$ctrl.n
  n2 <- table$expt.n
  
  df <- n1+n2-2
  j <- 1 - 3/(4*df-1)  # correction factor between Cohen's d and Hedge's g

  ## convert to Cohen's d
    hedgesg <- table$yi
    cohensd <- hedgesg/j
  ## find variance of Cohen's d; convert to variance of Hedges' g
    cohensd.v <- (n1+n2)/(n1*n2) + cohensd^2/(2*(n1+n2))
    table$hedgesg.v <- j^2 * cohensd.v
  ##
    table[table$flagmissing==1,]$vi <- table[table$flagmissing==1,]$hedgesg.v  
  ## drop columns
    table$flagmissing <- NULL
    table$hedgesg.v <- NULL
  return(table)
}
