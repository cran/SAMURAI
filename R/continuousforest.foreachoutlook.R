continuousforest.foreachoutlook <-
function(table,
  meanssd=FALSE,
  confidencelevel=95,
  event.is.good=FALSE,
  smd.vpos=NA, smd.pos=NA, smd.neg=NA, smd.vneg=NA,
  ...){
    
  outlooks <- c("very positive", "positive", "no effect", "negative", "very negative", 
                "very positive CL", "positive CL", "current effect", "negative CL", "very negative CL")
  n <- length(outlooks)
  for (i in 1:n){
    outlook <- outlooks[i]
    continuousforest(table,
                     meanssd=meanssd,
                     confidencelevel=confidencelevel,
                     event.is.good=event.is.good,
                     smd.vpos=NA, smd.pos=NA, smd.neg=NA, smd.vneg=NA,                 
                     unpub.oneoutlook=outlook,
                     ...)
    
  }  
}
