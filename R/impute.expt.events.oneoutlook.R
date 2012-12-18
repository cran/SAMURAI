impute.expt.events.oneoutlook <-
function(table, whichoutlook, assignedrr,
                                          simsperstudy=10){
  ## a function called by impute.expt.events()
  
  ## for testing
  #   table=table2; whichoutlook="negative CL"; assignedrr=rr$negcl
  
  # extract subset
  subtable <- as.data.frame(table[which(table$outlook==whichoutlook),])
  # proceed only if subset is not empty
  if(nrow(subtable) != 0) {
    sub.nst <- nrow(subtable)  # number of studies in subset
    sub.cn <- subtable$ctrl.n
    sub.xn <- subtable$expt.n
    sub.c1 <- subtable$ctrl.events
    
    # impute expt.events
    sub.xp <- assignedrr * (sub.c1/sub.cn)  # estimated probability of event in subset control group
    # add some random variation to expt.events in each study : normally distributed random variable
    #     mu.normal <- sub.xn*sub.xp  # E(X)=np
    #     se <- sqrt(sub.xp*(1-sub.xp)) / sqrt(sub.xn)  # standard error of a binomial r.v. (CLT approximation)
    #     se <- sqrt(sub.xp*(1-sub.xp)) * sqrt(sub.xn)
    #     cat(se,",")
    #     sub.x1 <- rnorm(sub.nst, mean=mu.normal, sd=se)
    phat.binomial <- sub.xp
    
    sum <- 0 
    for(i in 1:simsperstudy){
      sum <- sum + rbinom(n=sub.nst, size=sub.xn, prob=sub.xp)
      avg <- sum/simsperstudy
    }
    sub.x1 <- avg
    
    #     sub.x1 <- rbinom(n=sub.nst, size=sub.xn, prob=sub.xp)
    
    # round events to nearest integer
    subtable$expt.events <- round(sub.x1)
    subtable$ctrl.events <- round(subtable$ctrl.events)
    ## replace rows in table
    table[which(table$outlook==whichoutlook),] <- subtable  
  }
  return(table)
}
