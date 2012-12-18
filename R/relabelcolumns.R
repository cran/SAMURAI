relabelcolumns <-
function(table, 
                           ctrl.n=NA, ctrl.events=NA, ctrl.mean=NA, ctrl.sd=NA,
                           expt.n=NA, expt.events=NA, expt.mean=NA, expt.sd=NA,
                           smd="smd", smd.v="smd.v"){
  
  if(is.na(ctrl.n)==FALSE)      {names(table)[which(names(table)==ctrl.n)] <- "ctrl.n"}
  if(is.na(ctrl.events)==FALSE) {names(table)[which(names(table)==ctrl.events)] <- "ctrl.events"}
  if(is.na(ctrl.mean)==FALSE)   {names(table)[which(names(table)==ctrl.mean)] <- "ctrl.mean"}
  if(is.na(ctrl.sd)==FALSE)     {names(table)[which(names(table)==ctrl.sd)] <- "ctrl.sd"}
  
  if(is.na(expt.n)==FALSE)      {names(table)[which(names(table)==expt.n)] <- "expt.n"}
  if(is.na(expt.events)==FALSE) {names(table)[which(names(table)==expt.events)] <- "expt.events"}
  if(is.na(expt.mean)==FALSE)   {names(table)[which(names(table)==expt.mean)] <- "expt.mean"}
  if(is.na(expt.sd)==FALSE)     {names(table)[which(names(table)==expt.sd)] <- "expt.sd"}
  
  if(is.na(smd)==FALSE)     {names(table)[which(names(table)==smd)] <- "yi"}
  if(is.na(smd.v)==FALSE)     {names(table)[which(names(table)==smd.v)] <- "vi"}
  
  return(table)
}
