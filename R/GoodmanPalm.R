#' @title GoodmanPalm
#' @description Function to estimate AGB of palms using family level diameter based equation from Goodman et al. 2013.
#' @param data Object returned by \code{mergefp}.
#' @param dbh Name of column containing diameter data. Default is "D4".
#' @return Vector with estimated AGB for each row in /code{data}.
#' @author Martin Sullivan

#' @export

GoodmanPalm<-function(data,dbh="D4"){
	ln.AGB<--3.3488+(2.7483*log(data[,dbh]/10))
	return((exp(ln.AGB))/1000)
}


	