assignsmd <-
function(event.is.good=FALSE, pubsmd, pubsmd.lcl, pubsmd.ucl,
                      smd.vpos=NA, smd.pos=NA, smd.neg=NA, smd.vneg=NA){  
  ## given table of published and unpublished studies with continuous outcome data
  ## assigns SMD (standardized mean difference) of unpublished studies 
  ## based on CI of risk ratio of published studies
  
  ## for testing
  #     pubsmd=pubsummary$exp.m;  pubsmd.lcl=pubsummary$exp.m.lcl
  #     pubsmd.ucl=pubsummary$exp.m.ucl
  
  halfup   <- 0.5*(pubsmd+pubsmd.ucl)
  halfdown <- 0.5*(pubsmd+pubsmd.lcl)
  
  if(event.is.good == FALSE){
    vnegcl <- pubsmd.ucl
    negcl  <- halfup
    poscl  <- halfdown
    vposcl  <- pubsmd.lcl
  }
  if(event.is.good == TRUE){
    vnegcl <- pubsmd.lcl
    negcl  <- halfdown
    poscl  <- halfup
    vposcl  <- pubsmd.ucl
  }
  current <- pubsmd  
  
  smd <- as.list(c(smd.vpos, smd.pos, smd.neg, smd.vneg, current, 
                   vposcl, poscl, negcl, vnegcl))
  names(smd) <- c("vpos", "pos", "neg", "vneg", "current", 
                  "vposcl", "poscl", "negcl", "vnegcl")
  return(smd)
}
