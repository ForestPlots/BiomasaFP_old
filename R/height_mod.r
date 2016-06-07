#' @title height.mod
#' @description Function to estimate height from height-diameter model parameters
#' @param dbh Vector of diameters.
#' @param a a parameter in Weibull or log-log model
#' @param b b parameter in Weibull or log-log model
#' @param c c parameter in Weibull model. If NA and other parameters are not, the function assumes the model is a log-log model
#' @return Vector with estimated heights.
#' @author Martin Sullivan

#' @export

height.mod<-function(dbh,a,b,c=NA){
	h<-ifelse(is.na(c) & !is.na(a),exp(a+b*log(dbh/10)),a*(1-exp(-b*(dbh/10)^c)))
	h
}

