impute.smd.v <-
function(table, matchedgroups=FALSE){
  ## given ctrl.n, expt.n, and Hedge's g (sample estimate of the SMD)
  ## returns a "very good" approximation of the variance of Hedge's g (according to Borenstein)
  
  ## Main Reference: 
  ## Michael Borenstein, "Effect Sizes for Continuous Data", page 226
  ##  Chapter 12 in Cooper, Hedges, and Valentine, 
  ##  Handbook of Research Synthesis and Meta-analysis
  
  ## testing
  #   table <- table4
  
  table$flagmissing <- as.numeric(is.na(table$vi))
  
  n1 <- table$ctrl.n
  n2 <- table$expt.n
  
  df <- n1+n2-2
  j <- 1 - 3/(4*df-1)  # correction factor between Cohen's d and Hedge's g
  
  hedgesg <- table$yi
  cohensd <- hedgesg/j
  
  cohensd.v <- (n1+n2)/(n1*n2) + cohensd^2/(2*(n1+n2))
  table$hedgesg.v <- j^2 * cohensd.v
  
  table[table$flagmissing==1,]$vi <- table[table$flagmissing==1,]$hedgesg.v  
  
  ## drop columns
  table$flagmissing <- NULL
  table$hedgesg.v <- NULL
  
  return(table)
}
