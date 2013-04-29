impute.smd <-
function(table, assignedsmd,...){
  ## In: Table that includes unpublished studies
  ## Out: Impute intervention events in unpublished studies with positive outlook
  ## Calls: impute.smd.oneoutlook(smd.noise)
  ## Callers: continuousforest(), continuous.table()
  
  table <- impute.smd.oneoutlook(table=table, whichoutlook="very positive", smdassigned=assignedsmd$vpos,...)
  table <- impute.smd.oneoutlook(table=table, whichoutlook="positive", smdassigned=assignedsmd$pos,...)
  table <- impute.smd.oneoutlook(table=table, whichoutlook="no effect", smdassigned=0,...)
  table <- impute.smd.oneoutlook(table=table, whichoutlook="negative", smdassigned=assignedsmd$neg,...)
  table <- impute.smd.oneoutlook(table=table, whichoutlook="very negative", smdassigned=assignedsmd$vneg,...)
  table <- impute.smd.oneoutlook(table=table, whichoutlook="very positive CL", smdassigned=assignedsmd$vposcl,...)
  table <- impute.smd.oneoutlook(table=table, whichoutlook="positive CL", smdassigned=assignedsmd$poscl,...)
  table <- impute.smd.oneoutlook(table=table, whichoutlook="current effect", smdassigned=assignedsmd$current,...)  
  table <- impute.smd.oneoutlook(table=table, whichoutlook="negative CL", smdassigned=assignedsmd$negcl,...)
  table <- impute.smd.oneoutlook(table=table, whichoutlook="very negative CL", smdassigned=assignedsmd$vnegcl,...)
  return(table)
}
