impute.expt.events <-
function(table, rr, simsperstudy=1){
  ## In: Table of (pub & unpub) studies with binary outcomes
  ## Out: Imputes events in the intervention arms of unpublished studies.
  ## Calls: impute.expt.events.oneoutlook()
  ## Notes: 
  ## - Table must include the following columns: ctrl.n, ctrl.events, expt.n, expt.events
  ## - Order of risk ratios is in the vector "rr", which is the output of assignrr().
  sims <- simsperstudy
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="very positive", assignedrr=rr$vpos, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="positive", assignedrr=rr$pos, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="no effect", assignedrr=1, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="negative", assignedrr=rr$neg, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="very negative", assignedrr=rr$vneg, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="very positive CL", assignedrr=rr$vposcl, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="positive CL", assignedrr=rr$poscl, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="current effect", assignedrr=rr$current, simsperstudy=sims)  
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="negative CL", assignedrr=rr$negcl, simsperstudy=sims)
  table <- impute.expt.events.oneoutlook(table=table, whichoutlook="very negative CL", assignedrr=rr$vnegcl, simsperstudy=sims)
  return(table)
}
