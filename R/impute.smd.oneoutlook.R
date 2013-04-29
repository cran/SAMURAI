impute.smd.oneoutlook <-
function(table, whichoutlook, smdassigned, smd.noise,...){
  ## Callers: impute.smd()
    
  ## extract subset
  subtable <- as.data.frame(table[which(table$outlook==whichoutlook),])
  ## proceed only if subset is not empty
  if(nrow(subtable) != 0) {      
    numstudies <- nrow(subtable) 
    
    ## add random noise
    smd <- rnorm(numstudies, mean=smdassigned, sd=smd.noise)
    
    subtable[is.na(subtable$yi),]$yi <- smd 
    ## replace rows in table
    table[which(table$outlook==whichoutlook),] <- subtable  
  }
  return(table)
}
