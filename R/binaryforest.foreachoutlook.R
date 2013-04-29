binaryforest.foreachoutlook <-
function(table, 
  confidencelevel=95, event.is.good=FALSE, 
  rr.vpos=NA, rr.pos=NA, rr.neg=NA, rr.vneg=NA,
  simsperstudy=1,
  ...){  
  outlooks <- c("very positive", "positive", "no effect", "negative", "very negative", 
                "very positive CL", "positive CL", "current effect", "negative CL", "very negative CL")
  n <- length(outlooks)
  for (i in 1:n){
    outlook <- outlooks[i]
    binaryforest(table,
                 confidencelevel=confidencelevel,
                 event.is.good=event.is.good,
                 rr.vpos=rr.vpos, 
                 rr.pos=rr.pos, 
                 rr.neg=rr.neg, 
                 rr.vneg=rr.vneg,
                 simsperstudy=simsperstudy,
                 unpub.oneoutlook=outlook,
                 ...)
    
  }  
}
