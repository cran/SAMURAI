tablesens <-
function(table, 
                       binaryoutcome = TRUE, outcome = c("binary","continuous"),
                       binarymeasure = "RR",
                       rr.vpos = NA, rr.pos = NA, rr.neg = NA, rr.vneg = NA,
                       meanssd = FALSE, 
                       smd.vpos = NA, smd.pos = NA, smd.neg = NA, smd.vneg = NA,
                       event.is.good = FALSE,  
                       simsperstudy = 10, 
                       confidencelevel = 95, 
                       method = "DL", 
                       sigdigits = 3,
                       
                       scale = 1,
                       
                       random.number.seed = NA, 
                       smd.noise=0.01
){
  
  ## Resolve redundancies among parameters
#   if(outcome=="binary") {binaryoutcome=TRUE}
#   if(outcome=="continuous") {binaryoutcome=FALSE}
  
  ## Set random.number.seed
  if(is.na(random.number.seed) != T) {set.seed(random.number.seed)} 
  
  if(binaryoutcome==TRUE){ # for binary outcomes
      summarize.eachoutlook(table,
                            binaryoutcome=TRUE,
                            confidencelevel=confidencelevel, event.is.good=event.is.good, 
                            rr.vpos=rr.vpos, rr.pos=rr.pos, rr.neg=rr.neg, rr.vneg=rr.vneg,
                            simsperstudy=simsperstudy,
                            measure=binarymeasure, method=method, 
                            sigdigits=sigdigits,
                            smd.noise=smd.noise)
    }
    else{ 
      summarize.eachoutlook(table, 
                            binaryoutcome=FALSE, meanssd=meanssd, 
                            confidencelevel=confidencelevel, event.is.good=event.is.good,
                            smd.vpos=smd.vpos, smd.pos=smd.pos, smd.neg=smd.neg, smd.vneg=smd.vneg,
                            method=method, sigdigits=sigdigits,
                            smd.noise=smd.noise,
                            exp=FALSE)
      
    }
  
  
}
