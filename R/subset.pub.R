subset.pub <-
function(table){
    pub <- table[which(table$outlook=="published"),]
    return(pub)
}
