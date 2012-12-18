binaryforest.foreachoutlook <-
function(
  table, 
  confidencelevel=95,
  event.is.good=FALSE, rr.vpos=NA, rr.pos=NA, rr.neg=NA, rr.vneg=NA,
  random.number.seed=NA, simsperstudy=10,
  plot.title="Random Effects Forest Plot", scale=1){
  
  outlooks <- c("very positive", "positive", "negative", "very negative", 
                "very positive CL", "positive CL", "negative CL", "very negative CL",
                "current effect", "no effect")
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
                 random.number.seed=random.number.seed, 
                 simsperstudy=simsperstudy,
                 plot.title=plot.title,
                 scale=scale,
                 unpub.oneoutlook=outlook)
    
  }  
}
