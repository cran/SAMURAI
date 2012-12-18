summarize.eachoutlook <-
function(table, 
  confidencelevel=95,  
  event.is.good=FALSE, 
  random.number.seed=NA, 
  method="DL",
  sigdigits=3,
  binaryoutcome=TRUE,
  measure="RR", simsperstudy=10, rr.vpos=NA, rr.pos=NA, rr.neg=NA, rr.vneg=NA,
  meanssd=TRUE, smd.vpos=NA, smd.pos=NA, smd.neg=NA, smd.vneg=NA
)
{
  outlooks <- c("very positive", "positive", "negative", "very negative", 
                "very positive CL", "positive CL", "negative CL", "very negative CL",
                "current effect", "no effect")
  n <- length(outlooks)
  effect <- rep(NA,n)
  lcl  <- rep(NA,n)
  ucl  <- rep(NA,n)
  tau2 <- rep(NA,n)
  
  for (i in 1:n){
    outlook <- outlooks[i]
    
    if(binaryoutcome==TRUE){
      table0 <- binary.table(table, 
                             confidencelevel=confidencelevel,
                             event.is.good=event.is.good,
                             rr.vpos=rr.vpos, rr.pos=rr.pos, rr.neg=rr.neg, rr.vneg=rr.vneg,
                             random.number.seed=random.number.seed, 
                             simsperstudy=simsperstudy,
                             unpub.oneoutlook=outlook)      
    }     
    else{
      table0 <- continuous.table(table,
      							 meanssd=meanssd,
                                 confidencelevel=confidencelevel,
                                 event.is.good=event.is.good,
                                 smd.vpos=NA, smd.pos=NA, smd.neg=NA, smd.vneg=NA,                 
                                 random.number.seed=random.number.seed, 
                                 unpub.oneoutlook=outlook)      
    }  
  
    effect[i] <- summaryeffect(table0)$effect
    lcl[i] <- summaryeffect(table0)$lcl
    ucl[i] <- summaryeffect(table0)$ucl
    tau2[i] <- summaryeffect(table0)$tau2
  }  
  
  ## round numbers
  effect <- round(effect,sigdigits)
  lcl <- round(lcl,sigdigits)
  ucl <- round(ucl,sigdigits)
  tau2 <- round(tau2,sigdigits)
  out <- as.data.frame(cbind(outlooks,effect,lcl,ucl,tau2))
  return(out)
}
