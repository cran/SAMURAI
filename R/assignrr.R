assignrr <-
function(pubrr, pubrr.lcl, pubrr.ucl, event.is.good=FALSE,
                     rr.vpos=NA, rr.pos=NA, rr.neg=NA, rr.vneg=NA,
                     log=TRUE){  
  ## given table of published and unpublished studies with binary outcome data
  ## assigns risk ratios of unpublished studies 
  ## based on CI of risk ratio of published studies
  
  ## for testing
  #   pubrr=pubsummary$exp.m;  pubrr.lcl=pubsummary$exp.m.lcl
  #   pubrr.ucl=pubsummary$exp.m.ucl;  event.is.good=TRUE
  
  if(is.na(rr.vpos)) { if (event.is.good==FALSE) {rr.vpos <- 0.33} else {rr.vpos <- 3} }
  if(is.na(rr.pos)) { if (event.is.good==FALSE) {rr.pos <- 0.5} else {rr.pos <- 2} }
  if(is.na(rr.neg)) { if (event.is.good==FALSE) {rr.neg <- 2} else {rr.neg <- 0.5} }
  if(is.na(rr.vneg)) { if (event.is.good==FALSE) {rr.vneg <- 3} else {rr.vneg <- 0.33} }
  
  if(log==TRUE){
    loghalfup   <- 0.5*(log(pubrr)+log(pubrr.ucl))
    loghalfdown <- 0.5*(log(pubrr)+log(pubrr.lcl))
    
    halfup <- exp(loghalfup)
    halfdown <- exp(loghalfdown)
  } else {
    halfup   <- 0.5*(pubrr+pubrr.ucl)
    halfdown <- 0.5*(pubrr+pubrr.lcl)
  }
  
  if(event.is.good == FALSE){
    vnegcl <- pubrr.ucl
    negcl  <- halfup
    poscl  <- halfdown
    vposcl  <- pubrr.lcl
  }
  if(event.is.good == TRUE){
    vnegcl <- pubrr.lcl
    negcl  <- halfdown
    poscl  <- halfup
    vposcl  <- pubrr.ucl
  }
  current <- pubrr  
  
  rr <- as.list(c(rr.vpos, rr.pos, rr.neg, rr.vneg, current, 
                  vposcl, poscl, negcl, vnegcl))
  names(rr) <- c("vpos", "pos", "neg", "vneg", "current", 
                 "vposcl", "poscl", "negcl", "vnegcl")
  return(rr)
}
