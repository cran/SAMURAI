importtable <-
function(filename,head=TRUE,semicolon.csv.param=FALSE){
  ## import CSV file as an R data frame
  ## on German computer systems, set semicolon.csv.param=TRUE 
  
  if(semicolon.csv.param==TRUE){
    table <- as.data.frame(read.csv2(file=filename,header=head))  # sep=";", dec=","
  } else{ 
    table <- as.data.frame(read.csv(file=filename,header=head))  # sep=",", dec="."
  }
  return(table)
}
