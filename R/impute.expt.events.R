impute.expt.events <-
function(table, rr, simsperstudy=10){
  ## impute number of events in intervention treatment group
  ## requires the following colunm headers: ctrl.n, ctrl.events, expt.n, expt.events
  
  ##   order of risk ratios: see assignrr() under part 3
  
  ## impute intervention events in unpublished studies with positive outlook
  sims <- simsperstudy
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="very positive", assignedrr=rr$vpos, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="positive", assignedrr=rr$pos, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="negative", assignedrr=rr$neg, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="very negative", assignedrr=rr$vneg, simsperstudy=sims)
  
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="current effect", assignedrr=rr$current, simsperstudy=sims)  
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="no effect", assignedrr=1, simsperstudy=sims)
  
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="very positive CL", assignedrr=rr$vposcl, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="positive CL", assignedrr=rr$poscl, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="negative CL", assignedrr=rr$negcl, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="very negative CL", assignedrr=rr$vnegcl, simsperstudy=sims)
  
  return(table)
}
