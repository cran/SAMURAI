impute.smd.oneoutlook <-
function(table, whichoutlook, smdassigned){
  ## function called by impute.smd()
  
  ## testing
  #   table <- table0; whichoutlook="positive"
  #   table$yi[which(table$outlook=="positive")][1] <- 0.2
  
  ## extract subset
  subtable <- as.data.frame(table[which(table$outlook==whichoutlook),])
  ## proceed only if subset is not empty
  if(nrow(subtable) != 0) {      
    # impute smd only if not already imputed
    subtable[is.na(subtable$yi),]$yi <- smdassigned 
    ## replace rows in table
    table[which(table$outlook==whichoutlook),] <- subtable  
  }
  return(table)
}
