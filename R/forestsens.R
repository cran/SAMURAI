forestsens <-
function(table, binaryoutcome = TRUE, meanssd = FALSE, event.is.good = FALSE,  
             unpub.oneoutlook = NA, foralloutlooks = FALSE,  
             random.number.seed = NA, simsperstudy = 10, 
             sigdigits = 3, confidencelevel = 95, 
                       
             binarymeasure = "RR",  method = "DL",
             rr.vpos = NA, rr.pos = NA, rr.neg = NA, rr.vneg = NA, 
                       
             smd.vpos = NA, smd.pos = NA, smd.neg = NA, smd.vneg = NA,
                       
             plot.title = "Random Effects Forest Plot", scale = 1
){
  if(binaryoutcome==TRUE){ # for binary outcomes
    if(foralloutlooks==TRUE){ # for all outlooks
      binaryforest.foreachoutlook(table,
                                  confidencelevel=confidencelevel, event.is.good=event.is.good, 
                                  rr.vpos=rr.vpos, rr.pos=rr.pos, rr.neg=rr.neg, rr.vneg=rr.vneg,
                                  random.number.seed=random.number.seed, 
                                  simsperstudy=simsperstudy,
                                  plot.title=plot.title, scale=scale)  
      summarize.eachoutlook(table,
                             binaryoutcome=TRUE,
                             confidencelevel=confidencelevel, event.is.good=event.is.good, 
                             rr.vpos=rr.vpos, rr.pos=rr.pos, rr.neg=rr.neg, rr.vneg=rr.vneg,
                             random.number.seed=random.number.seed, simsperstudy=simsperstudy,
                             measure=binarymeasure, method=method, sigdigits=sigdigits)
    }
    else{ # for one outlook
      binaryforest(table, 
                   confidencelevel=confidencelevel, event.is.good=event.is.good, 
                   rr.vpos=rr.vpos, rr.pos=rr.pos, rr.neg=rr.neg, rr.vneg=rr.vneg,
                   random.number.seed=random.number.seed, 
                   simsperstudy=simsperstudy,
                   plot.title=plot.title, scale=scale, 
                   unpub.oneoutlook=unpub.oneoutlook)        
    }
  }

  else{ # for continuous outcomes
    if(foralloutlooks==TRUE){ # for all outlooks
      continuousforest.foreachoutlook(table, 
                                      meanssd=meanssd,
                                      confidencelevel=confidencelevel, event.is.good=event.is.good, 
                                      smd.vpos=smd.vpos, smd.pos=smd.pos, smd.neg=smd.neg, smd.vneg=smd.vneg,
                                      random.number.seed=random.number.seed, 
                                      plot.title=plot.title, scale=scale)
      
      summarize.eachoutlook(table, 
                             binaryoutcome=FALSE, meanssd=meanssd, 
                             confidencelevel=confidencelevel, event.is.good=event.is.good,
                             smd.vpos=smd.vpos, smd.pos=smd.pos, smd.neg=smd.neg, smd.vneg=smd.vneg,
                             random.number.seed=random.number.seed, 
                             method=method, sigdigits=sigdigits)
        
    }
    else{ # for one outlook
      continuousforest(table, 
                       meanssd=meanssd,
                       confidencelevel=confidencelevel, event.is.good=event.is.good, 
                       smd.vpos=smd.vpos, smd.pos=smd.pos, smd.neg=smd.neg, smd.vneg=smd.vneg,
                       random.number.seed=random.number.seed, 
                       plot.title=plot.title, scale=scale,
                       unpub.oneoutlook=unpub.oneoutlook)      
    }
  }
  
}
