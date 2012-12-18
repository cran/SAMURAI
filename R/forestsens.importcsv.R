forestsens.importcsv <-
function(filename,german.windows=TRUE,...){
	table <- importtable(filename,head=TRUE,semicolon.csv.param=german.windows)
	forestsens(table,...)
}
