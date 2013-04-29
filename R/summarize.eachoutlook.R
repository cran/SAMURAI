summarize.eachoutlook <-
function(table, 
  binaryoutcome=TRUE,
  measure="RR", 
  meanssd=TRUE, 

  event.is.good=FALSE,  

  rr.vpos=NA, rr.pos=NA, rr.neg=NA, rr.vneg=NA,
  smd.vpos=NA, smd.pos=NA, smd.neg=NA, smd.vneg=NA,
  
  simsperstudy=10, 

  method="DL",
  sigdigits=3, confidencelevel=95,  
  
  exp=TRUE,

  ... 
)
{
  ##
  
  outlooks <- c("very positive", "positive", "no effect", "negative", "very negative", 
                "very positive CL", "positive CL", "current effect", "negative CL", "very negative CL")
  n <- length(outlooks)
  effect <- rep(NA,n)
  lcl  <- rep(NA,n)
  ucl  <- rep(NA,n)
  tau2 <- rep(NA,n)
  
  for (i in 1:n){
    outlook <- outlooks[i]
    
    if(binaryoutcome==TRUE){
      table0 <- binary.table(table, 
        event.is.good=event.is.good,
        rr.vpos=rr.vpos, rr.pos=rr.pos, rr.neg=rr.neg, rr.vneg=rr.vneg,
        simsperstudy=simsperstudy,
        confidencelevel=confidencelevel,
        unpub.oneoutlook=outlook,
        ...)      
      out <- summarize.randomeffects(table0)
    }     
    else{
      table0 <- continuous.table(table,
        meanssd=meanssd,
        event.is.good=event.is.good,
        smd.vpos=NA, smd.pos=NA, smd.neg=NA, smd.vneg=NA,                 
        confidencelevel=confidencelevel,
        unpub.oneoutlook=outlook, 
        ...)      
      out <- summarize.randomeffects(table0)
    }  

    # "m","m.se","m.lcl","m.ucl", "exp.m.lcl","exp.m","exp.m.ucl"
  
    tau2[i] <- out$tau2

    if(exp==FALSE){
      effect[i] <- out$m
      lcl[i] <- out$m.lcl
      ucl[i] <- out$m.ucl  
    }
    else{
      effect[i] <- out$exp.m
      lcl[i] <- out$exp.m.lcl
      ucl[i] <- out$exp.m.ucl
    }
  }  
  
  ## round numbers
  effect <- round(effect,sigdigits)
  lcl <- round(lcl,sigdigits)
  ucl <- round(ucl,sigdigits)
  tau2 <- round(tau2,sigdigits)
  out <- as.data.frame(cbind(outlooks,effect,lcl,ucl,tau2))
  return(out)
}
