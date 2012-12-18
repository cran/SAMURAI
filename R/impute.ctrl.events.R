impute.ctrl.events <-
function(table, roundtable=FALSE){
  ## given table of published and unpublished studies with binary outcome data
  ## returns imputed control arm events for unpublished studies 
  ## based on the rate of control arm events in published studies
  
  ## requires the following colunm headers: ctrl.n, ctrl.events, expt.n, expt.events
  
  num.studies <- nrow(table)  # number of rows in table
  
  ## Published studies: Determine risk ratio ##
  
  # subset of published studies only; c-control, x-experimental, n-sample size, v-# events
  pub <- table[which(table$outlook=="published"),]
  pub.cn <- pub$ctrl.n
  pub.xn <- pub$expt.n
  pub.cv <- pub$ctrl.events
  pub.xv <- pub$expt.events
  # column totals for published studies only; s-sum
  pub.scn <- sum(pub.cn)
  pub.sxn <- sum(pub.xn)
  pub.scv <- sum(pub.cv)
  pub.sxv <- sum(pub.xv)
  
  pub.xp <- pub.sxv/pub.sxn  # estimated proportion of treatment group having event  
  pub.cp <- pub.scv/pub.scn  # estimated proportion of control group having event
  pub.rr <- pub.xp/pub.cp  # risk ratio 
  
  # calculate ctrl.events based on rate of effect in published studies
  # don't round off figures until later
  table[which(table$outlook!='published'),]$ctrl.events <- 
    table[which(table$outlook!='published'),]$ctrl.n * pub.cp
  
  # round events to nearest integer if ROUND=TRUE
  table$ctrl.events <- if(roundtable==TRUE) {round(table$ctrl.events)} else {table$ctrl.events}
  return(table)
}
