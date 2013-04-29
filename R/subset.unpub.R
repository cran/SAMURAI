subset.unpub <-
function(table){
    unpub <- table[which(table$outlook!="published"),]
    return(unpub)
}
