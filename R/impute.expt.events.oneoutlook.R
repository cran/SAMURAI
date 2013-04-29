impute.expt.events.oneoutlook <-
function(table, whichoutlook, assignedrr,simsperstudy=1){
  ## In: Table of (pub & unpub) studies with binary outcomes
  ## Out: Imputes events in the intervention arms of unpublished studies with a specific outlook.
  ## Callers: impute.expt.events()
  
  ## extract subset
    subtable <- as.data.frame(table[which(table$outlook==whichoutlook),])
  ## exit clause: stop function if subset is empty
    if(nrow(subtable) == 0) { return(table) }
  ## proceed only if subset is not empty
    sub.nst <- nrow(subtable)      ## number of studies in subset
    sub.cn <- subtable$ctrl.n      ## sample size of control arm
    sub.c1 <- subtable$ctrl.events ## number of events in control arm
    sub.xn <- subtable$expt.n      ## sample size of expt arm
  ## impute expt.events
    sub.xp <- assignedrr * (sub.c1/sub.cn)  ## estimated probability of event in expt group
  ## remove unpub studies for which the estimated prob of event in the expt arm exceeds 1
    if(max(sub.xp)>1) {
        cat("Note: The imputed proportion of events in the experimental group of some unpublished studies 
            with the outlook '", whichoutlook,"' exceeds 1. 
            Those studies will be removed from the meta-analysis.")
        subtable <- subtable[-(sub.xp>1),] 
    }
  ## remove unpub studies for which the estimated prob of event in the expt arm is below 0
    if(min(sub.xp)<0) {
        cat("Note: The imputed proportion of events in the experimental group 
            of some unpublished studies with the outlook '", whichoutlook,"' is less than 0. 
            Those studies will be removed from the meta-analysis.")
        subtable <- subtable[-(sub.xp<0),]
    }
  ## exit clause: stop function if subset is empty
    if(nrow(subtable) == 0) { return(table) }
  ## add some random variation to expt.events in each study     
  ## generate one or more vectors of random numbers and take the average
    sum <- 0 
    for(i in 1:simsperstudy){
      sum <- sum + rbinom(n=sub.nst, size=sub.xn, prob=sub.xp)
    }
    avg <- sum/simsperstudy
    sub.x1 <- avg
  ## round events to nearest integer
    subtable$expt.events <- round(sub.x1)
    subtable$ctrl.events <- round(subtable$ctrl.events)
  ## replace rows in table
    table[which(table$outlook==whichoutlook),] <- subtable  
  return(table)
}
