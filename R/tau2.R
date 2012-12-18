tau2 <-
function(table, measure="RR", method="DL", 
                            confidencelevel=95){
  ## given a table, 
  
  ## outputs the tau^2 for each of the following subsets:
  ## published; unpublished; all (published & unpublished)
  
  ## for testing
#   table=table1; confidencelevel=95; measure="RR"; method="DL"
  
  # published
  pub <- table[which(table$outlook=="published"),] 
  if(nrow(pub)>0){
    pub.agg <- summarizeeffect(pub, confidencelevel=confidencelevel)
    pub.tau2 <- pub.agg$tau2
  } else { pub.agg <- NA}
  
  # unpublished
  unpub <- table[which(table$outlook != "published"),]
  if(nrow(unpub)>0){
    unpub.agg <- summarizeeffect(unpub, confidencelevel=confidencelevel)  
    unpub.tau2 <- unpub.agg$tau2
  } else { unpub.agg <- NA }
  
  all.agg <- summarizeeffect(table=table, confidencelevel=confidencelevel)
  all.tau2 <- all.agg$tau2
  
  out <- as.list(c(pub.tau2,unpub.tau2,all.tau2))
  names(out) <- c("pub","unpub","all")
  return(out)  
}
