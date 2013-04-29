forestsens <-
function(table, 
  binaryoutcome = TRUE, # outcomes = c("binary","continuous"),
  meanssd = FALSE, 
  binarymeasure = "RR",

  event.is.good = FALSE,  

  rr.vpos = NA, rr.pos = NA, rr.neg = NA, rr.vneg = NA,
  smd.vpos = NA, smd.pos = NA, smd.neg = NA, smd.vneg = NA,

  unpub.oneoutlook = NA, foralloutlooks = FALSE,  

  random.number.seed = NA, 
  smd.noise=0.01,
  simsperstudy = 10, 
 
  method = "DL",
 
  sigdigits = 3, 
  confidencelevel = 95, 
 
  plot.title = "", 
  scale = 1
){
  
  ## Set random.number.seed
  ## Stating it first relieves us of the need to specify it within each function call.
  if(is.na(random.number.seed) != T) {set.seed(random.number.seed)} 
  
  ## for binary outcomes
  if(binaryoutcome==TRUE){ 
    ## for all outlooks
    if(foralloutlooks==TRUE){ 
      
      binaryforest.foreachoutlook(table,
        event.is.good=event.is.good, 
        rr.vpos=rr.vpos, rr.pos=rr.pos, rr.neg=rr.neg, rr.vneg=rr.vneg,
        smd.noise=smd.noise, simsperstudy=simsperstudy,
        confidencelevel=confidencelevel, sigdigits=sigdigits,
        title=plot.title, scale=scale, 
        )  
      
      summarize.eachoutlook(table,
        binaryoutcome=TRUE, measure=binarymeasure,
        method=method,

        event.is.good=event.is.good, 
        rr.vpos=rr.vpos, rr.pos=rr.pos, rr.neg=rr.neg, rr.vneg=rr.vneg,
        smd.noise=smd.noise, simsperstudy=simsperstudy,
        confidencelevel=confidencelevel, sigdigits=sigdigits,
        title=plot.title, scale=scale
        )
    }
    else{ # for one outlook
      binaryforest(table, 
        unpub.oneoutlook=unpub.oneoutlook,

        event.is.good=event.is.good, 
        rr.vpos=rr.vpos, rr.pos=rr.pos, rr.neg=rr.neg, rr.vneg=rr.vneg,
        smd.noise=smd.noise, simsperstudy=simsperstudy,
        confidencelevel=confidencelevel, sigdigits=sigdigits,
        title=plot.title, scale=scale
        )
    }
  }
  
  else{ # for continuous outcomes
    if(foralloutlooks==TRUE){ # for all outlooks
      continuousforest.foreachoutlook(table, 
        meanssd=meanssd,

        event.is.good=event.is.good, 
        smd.vpos=smd.vpos, smd.pos=smd.pos, smd.neg=smd.neg, smd.vneg=smd.vneg,
        smd.noise=smd.noise,
        confidencelevel=confidencelevel, 
        title=plot.title, scale=scale
        )
        
      
      summarize.eachoutlook(table, 
                            binaryoutcome=FALSE, meanssd=meanssd, 
                            confidencelevel=confidencelevel, event.is.good=event.is.good,
                            smd.vpos=smd.vpos, smd.pos=smd.pos, smd.neg=smd.neg, smd.vneg=smd.vneg,
                            method=method, sigdigits=sigdigits,
                            smd.noise=smd.noise,
                            exp=FALSE)
      
    }
    else{ # for one outlook
      continuousforest(table, 
                       meanssd=meanssd,
                       confidencelevel=confidencelevel, event.is.good=event.is.good, 
                       smd.vpos=smd.vpos, smd.pos=smd.pos, smd.neg=smd.neg, smd.vneg=smd.vneg,
                       title=plot.title, 
                       unpub.oneoutlook=unpub.oneoutlook,
                       scale=scale,
                       smd.noise=smd.noise)
    }
  }
  
}
