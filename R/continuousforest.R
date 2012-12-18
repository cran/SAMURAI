continuousforest <-
function(
  table,
  meanssd=FALSE,
  confidencelevel=95,
  event.is.good=TRUE, smd.vpos=NA, smd.pos=NA, smd.neg=NA, smd.vneg=NA,
  unpub.oneoutlook=NA, 
  plot.title="Random Effects Forest Plot", scale=1,
  random.number.seed=NA){
  
  ## assumptions
  ## - both control and experimental arms have the same sample size
  ## - variance within control and within expt arms are the same
  
  ###########
  
	table0 <- table
  
  ## rename columns  
  names(table0)[names(table0)=="smd"] <- "yi"
  names(table0)[names(table0)=="smd.v"] <- "vi"
  
  ###########
  ## Part 0.1 : if data has proportion for each arm, convert to SMD

  ## convert means/sd data to SMD
  if(meanssd==TRUE){
    table0 <- convertmeans2smd(table0)
  }
  
  ###########
  ## Part 0.2 (optional) : assign same outlook to all unpublished studies
  
  ## If all unpublished studies are to have the same outlook:
  if(is.na(unpub.oneoutlook)==FALSE){
    
    ## Add outlook to factor levels. 
    ## (When importing a table, only outlooks in the table will become factor levels.)
    if( (unpub.oneoutlook %in% levels(table0$outlook))==FALSE ){
      levels(table0$outlook) <- c(levels(table0$outlook), unpub.oneoutlook) 
    }
    ## Assign outlook of all unpublished studies.
    table0$outlook[table0$outlook != "published"] <- unpub.oneoutlook
    ## Append outlook as subtitle to forest plot. 
    plot.title <- paste(plot.title,": unpublished studies with", unpub.oneoutlook,"outlooks")
  }
  
  ###########
  ## Part 1 : extract published studies
  pub0 <- table0[which(table0$outlook=="published"),]
    
  ## calculate SMD over all published studies
  pubsummary <- summarizeeffect(pub0, confidencelevel=confidencelevel) 
  #   exp(as.numeric(pubsummary))
  pubsmd    <- pubsummary$m
  pubsmd.se <- pubsummary$m.se
  pubsmd.v  <- pubsmd.se^2
  pubsmd.lcl <- pubsummary$m.lcl
  pubsmd.ucl <- pubsummary$m.ucl
  pub.tau2 <- pubsummary$tau2
  
  smd.pub <- as.list(c(pubsmd, pubsmd.lcl, pubsmd.ucl))
  names(smd.pub) <- c("m","m.lcl","m.ucl")
  
  ###########
  ## Part 3 : assign SMD to unpublished studies, acc to outlooks
  
  if(is.na(smd.vpos)) { smd.vpos <- 0.8 }
  if(is.na(smd.pos))  { smd.pos  <- 0.3 }
  if(is.na(smd.neg))  { smd.neg  <- -0.3 }
  if(is.na(smd.vneg)) { smd.vneg <- -0.8 }        
  
  smd <- c(smd.vpos, smd.pos, smd.neg, smd.vneg)
  
  if(event.is.good==TRUE){ 
    smd <- smd 
  } else if (event.is.good==FALSE){
    smd <- smd*(-1)
  }
  
  smd <- as.list(smd)
  names(smd) <- c("vpos", "pos", "neg", "vneg")    
  
  
  assignedsmd <- assignsmd(event.is.good, pubsmd, pubsmd.lcl, pubsmd.ucl, 
                           smd.vpos=smd$vpos, 
                           smd.pos=smd$pos, 
                           smd.neg=smd$neg, 
                           smd.vneg=smd$vneg)  
  
  ###########
  ## Part 4 : impute SMD and its variance
  
  table4a <- impute.smd(table=table0, assignedsmd=assignedsmd) 
  table4b <- impute.smd.v(table=table4a)
  
  ###########
  ## Part 5 : calculate the random effects model summary SMD 
  allsummary <- summarizeeffect(table4b, method="DL", confidencelevel=confidencelevel) 
  
  #  aggeffects.asis(table, confidencelevel=(1-alpha)*100)
  
  ###########
  ## Part 6 : graph forest plot and add aggregate results
  
  ## extract list of authors and years
  studylabel <- paste(table4b$study,table4b$year, sep=", ")
  
  graphcontinuousforest(table=table4b, 
                        alpha=(100-confidencelevel)/100, 
                        event.desired=event.is.good, 
                        title=plot.title,
                        scale=scale
  )
  
}
