################################################################################
## Haldane Calculator
################################################################################
#' The evolutionary rate of change between two groups in haldanes
#' 
#' @param x1 a vector of trait measurements for population one
#' @param x2 a vector of trait measurements for population two
#' @param g the time (in # of generations) between the two populations
#' @param log whether the trait values should be log transformed (TRUE/FALSE)
#' @return z the evolutionary rate of change between two populations in haldanes
#' @examples
#' haldane(pop1, pop2, 5, log=FALSE) ## The difference between two populations over 5 generations
################################################################################
haldane = function(x1, x2, g, log){
	if(is.vector(x1)==FALSE || length(x1)<=1){
		stop("x1 must be a vector with length > 1")
		}
	if(is.vector(x2)==FALSE || length(x2)<=1){
		stop("x2 must be a vector with length > 1")
		}
	s = sqrt((((length(x1)-1)*sd(x1)^2)+((length(x2)-1)*sd(x2)^2))/(length(x1)+length(x2)-2))
	if(log == FALSE){
		z = ((mean(x2)/s)-(mean(x1)/s))/g
	} else {
		z = ((mean(log(x2))/s)-(mean(log(x1))/s))/g
	}
	cat(print("The rate of evolutionary change from x1 to x2 is"),paste(z,
		"Haldanes",sep=" "),"\n")
}
