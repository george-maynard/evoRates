################################################################################
## Darwin Calculator
################################################################################
#' The evolutionary rate of change between two groups in darwins
#' 
#' @param x1 a vector of trait measurements for population one
#' @param x2 a vector of trait measurements for population two
#' @param t the time (in millions of years) between two populations
#' @return z the evolutionary rate of change between two populations in darwins
#' @examples
#' darwin(pop1, pop2, 0.5) ## the difference between two populations over 500,000 years
################################################################################
darwin = function(x1, x2, t){
	if(is.vector(x1)==FALSE || length(x1)<=1){
		stop("x1 must be a vector with length > 1")
		}
	if(is.vector(x2)==FALSE || length(x2)<=1){
		stop("x2 must be a vector with length > 1")
		}
	z = (log(mean(x2)) - log(mean(x1)))/t
	cat(print("The rate of evolutionary change from x1 to x2 is"),paste(z,
		"Darwins",sep=" "),"\n")
}