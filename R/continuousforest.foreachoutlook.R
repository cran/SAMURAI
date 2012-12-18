continuousforest.foreachoutlook <-
function(table,
                                            meanssd=FALSE,
                                            confidencelevel=95,                      
                                            event.is.good=FALSE,
                                            smd.vpos=NA, smd.pos=NA, smd.neg=NA, smd.vneg=NA,
                                            random.number.seed=NA, 
                                            plot.title="Random Effects Forest Plot",
                                            scale=1
){
    
  outlooks <- c("very positive", "positive", "negative", "very negative", 
                "very positive CL", "positive CL", "negative CL", "very negative CL",
                "current effect", "no effect")
  n <- length(outlooks)
  for (i in 1:n){
    outlook <- outlooks[i]
    continuousforest(table,
                     meanssd=meanssd,
                     confidencelevel=confidencelevel,
                     event.is.good=event.is.good,
                     smd.vpos=NA, smd.pos=NA, smd.neg=NA, smd.vneg=NA,                 
                     random.number.seed=random.number.seed, 
                     plot.title=plot.title,
                     scale=scale,
                     unpub.oneoutlook=outlook)
    
  }  
}
