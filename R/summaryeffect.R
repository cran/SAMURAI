summaryeffect <-
function(table, measure="RR", method="DL", confidencelevel=95, exp=TRUE){
  summ <- summarizeeffect(table, method=method, confidencelevel=confidencelevel)
  if(exp==TRUE){
    effectsize <- summ$exp.m
    ci <- c(summ$exp.m.lcl, summ$exp.m.ucl)  
  }
  else{
    effectsize <- summ$m
    ci <- c(summ$m.lcl, summ$m.ucl)
  }
  tau2 <- summ$tau2
  out <- as.list(c(effectsize, ci, tau2))
  names(out) <- c("effect", "lcl", "ucl", "tau2")
  return(out)  
}
