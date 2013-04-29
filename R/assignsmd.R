assignsmd <-
function(event.is.good=FALSE, pubsmd, pubsmd.lcl, pubsmd.ucl,
                      smd.vpos=NA, smd.pos=NA, smd.neg=NA, smd.vneg=NA){  
  ## In: Table of published and unpublished studies with continuous outcome data.
  ## Out: Assigns adjusted SMD (Hedges g) of unpublished studies, based on CI of summary across published studies
  ## Callers: continuousforest()
    
  ## cSMD's that do NOT depend on the summary effect across published studies
  if(is.na(smd.vpos)) { smd.vpos <- 0.8 }
  if(is.na(smd.pos))  { smd.pos  <- 0.3 }
  if(is.na(smd.neg))  { smd.neg  <- -0.3 }
  if(is.na(smd.vneg)) { smd.vneg <- -0.8 }        
  
  smd.indy <- c(smd.vpos,smd.pos,smd.neg,smd.vneg)
  
  if(event.is.good==TRUE){   } 
  else if (event.is.good==FALSE){
    smd.indy <- smd.indy*(-1)
  }
    
  ## cSMD's that DO depend on the summary effect across published studies
  current <- pubsmd  
  halfup   <- 0.5*(pubsmd+pubsmd.ucl)
  halfdown <- 0.5*(pubsmd+pubsmd.lcl)
  if(event.is.good == TRUE){
    vposcl <- pubsmd.ucl; poscl <- halfup; negcl <- halfdown; vnegcl <- pubsmd.lcl
  }
  else {
    vnegcl <- pubsmd.ucl; negcl <- halfup; poscl <- halfdown; vposcl <- pubsmd.lcl
  }
  
  smd <- as.list(c(smd.indy, current, vposcl, poscl, negcl, vnegcl))
  names(smd) <- c("vpos", "pos", "neg", "vneg", "current", "vposcl", "poscl", "negcl", "vnegcl")
  return(smd)
}
