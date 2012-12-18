continuous.summary.foreachoutlook <-
function(
  table,
  meanssd=FALSE,
  confidencelevel=95,
  event.is.good=FALSE, smd.vpos=NA, smd.pos=NA, smd.neg=NA, smd.vneg=NA,
  random.number.seed=NA, 
  method="DL",
  sigdigits=3
){
  
  ## testing
#   if(testing==TRUE){
#     filename="BHHR2009p88plus2.csv"
#     meanssd=TRUE
#     semicolon.csv=FALSE
#     confidencelevel=95
#     event.is.good=FALSE
#     smd.vpos=NA
#     smd.pos=NA 
#     smd.neg=NA
#     smd.vneg=NA
#     random.number.seed=NA
#   }
  
  outlooks <- c("very positive", "positive", "negative", "very negative", 
                "very positive CL", "positive CL", "negative CL", "very negative CL",
                "current effect", "no effect")
  n <- length(outlooks)
  effect <- rep(NA,n)
  lcl    <- rep(NA,n)
  ucl    <- rep(NA,n)
  
  for (i in 1:n){
    outlook <- outlooks[i]
    
    table0 <- continuous.table(table,
                     meanssd=meanssd,
                     confidencelevel=confidencelevel,
                     event.is.good=event.is.good,
                     smd.vpos=NA, smd.pos=NA, smd.neg=NA, smd.vneg=NA,                 
                     random.number.seed=random.number.seed, 
                     unpub.oneoutlook=outlook)
    effect[i] <- summaryeffect(table0,exp=FALSE)[[1]]
    lcl[i] <- summaryeffect(table0,exp=FALSE)[[2]]
    ucl[i] <- summaryeffect(table0,exp=FALSE)[[3]]
  }  
  effect <- round(effect,sigdigits)
  lcl <- round(lcl,sigdigits)
  ucl <- round(ucl,sigdigits)
  out <- as.data.frame(cbind(outlooks,effect,lcl,ucl))
  return(out)
}
