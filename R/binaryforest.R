binaryforest <-
function(table,
                         confidencelevel=95,                      
                         event.is.good=FALSE,
                         rr.vpos=NA, rr.pos=NA, rr.neg=NA, rr.vneg=NA,
                         random.number.seed=NA, 
                         simsperstudy=10,
                         plot.title="Random Effects Forest Plot",
                         scale=1,
                         unpub.oneoutlook=NA){
  ## given a CSV file
  ## return a forest plot
  
  ## This function is essentially plotbinaryforestfromtable()
  ## with a Part 0 (to import CSV file).
  
  ## to avoid R CMD CHECK NOTE: "no visible binding for global variable"
  expt.events <- expt.n <- ctrl.events <- ctrl.n <- NULL  
  
  ###########
  ## Part 0.5 (optional) : assign same outlook to all unpublished studies
  
  table0 <- table
  
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
    plot.title <- paste(plot.title,":", unpub.oneoutlook)
  }
  
  ###########
  ## Part 1 : extract published studies
  pub0 <- table0[which(table0$outlook=="published"),]
  
  ## calculate log rr and its variance for each study
  pub1 <- convertbin2effectsize(pub0, measure="RR") 
  
  ## calculate log rr over all published studies
  pubsummary <- summarizeeffect(pub1, confidencelevel=confidencelevel) 
  #   exp(as.numeric(pubsummary))
  pubrr <- pubsummary$exp.m
  pubrr.lcl <- pubsummary$exp.m.lcl
  pubrr.ucl <- pubsummary$exp.m.ucl
  
  ###########
  ## Part 2 : impute events in control arms
  table2 <- impute.ctrl.events(table0) 
  
  ###########
  ## Part 3 : assign RR to unpublished studies, acc to outlooks
  
  rr <- assignrr(pubrr, pubrr.lcl, pubrr.ucl, event.is.good=event.is.good,
                 rr.vpos=rr.vpos, rr.pos=rr.pos, rr.neg=rr.neg, rr.vneg=rr.vneg)
  
  ###########
  ## Part 4 : impute events in intervention arms, with random variation
  ## set random number seed
  if(is.na(random.number.seed) != T) {set.seed(random.number.seed)} 
  table4 <- impute.expt.events(table=table2, rr, simsperstudy=simsperstudy) 
  
  ###########
  ## Part 5 : fit random effects model
  
  ## calculate log risk ratio for each study and its standard error
  table5 <- escalc(measure="RR", ai=expt.events, n1i=expt.n, ci=ctrl.events, n2i=ctrl.n, data=table4, append=TRUE) 
  
  ## calculate the risk ratio of aggregated studies
  #   aggregates <- summarizeeffect(table5, alpha=0.05) 
  
  ###########
  ## Part 6 : graph forest plot and add aggregate results
  
  ## extract list of authors and years
  studylabel <- paste(table5$study,table5$year, sep=", ")
  
  ## the forest() function in the metafor package is a quick way to generate a forest plot
  #   mixedmodel <- rma(yi,vi,data=table5)
  #   forest(mixedmodel, slab=studylabel, atransf=exp, cex=0.75)
  
  graphbinaryforest(table=table5, 
                    alpha=(100-confidencelevel)/100,
                    rr.vpos=rr$vpos, rr.pos=rr$pos, rr.neg=rr$neg, rr.vneg=rr$vneg, rr.cur=rr$current,
                    event.desired=event.is.good, 
                    title=plot.title,
                    scale=scale)
  
}
