aggeffects.asis <-
function(table, measure="RR", method="DL", 
                            confidencelevel=95){
  ## given a table,  
  ## outputs the aggregate estimates for each of the following subsets:
  ## published; unpublished; all (published & unpublished)
  
  ## output is formatted for the addpoly() function in the metafor package
  
  # published
  pub <- table[which(table$outlook=="published"),]
  if(nrow(pub)>0){
    pub.agg <- summarize.randomeffects(pub, confidencelevel=confidencelevel)
  } else { pub.agg <- NA}
  
  # summary effect for unpublished studies only
  unpub <- table[which(table$outlook != "published"),]
  if(nrow(unpub)>0){
    unpub.agg <- summarize.randomeffects(unpub, confidencelevel=confidencelevel)  
  } else { unpub.agg <- NA }

  # overall summary effect
  all.agg <- summarize.randomeffects(table=table, confidencelevel=confidencelevel)
  
  out <- as.data.frame(rbind(pub.agg,unpub.agg,all.agg))
  return(out)  
}
