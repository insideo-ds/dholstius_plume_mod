#'Modified from: 
#'David Holstius (2012). plume: Gaussian dispersion modeling. 
#'R package version 0.1. y David Holstius (2012). An Introduction to Steady-State 
#'Gaussian Dispersion Modeling in R. 
#'From: https://rdrr.io/github/holstius/plume/f/inst/doc/plume-intro.pdf
#'
#' Briggs
#' 
#' Briggs neutral curves for urban areas
#' 
#' @param stability Pasquill stability class
#' @return a function sigma(x) returning a list with y and z components
#' @references Briggs, G.A., 1973. Diffusion estimation for small emissions. ATDL contribution, File No. 79. Air Resources Atmospheric Turbulence and Diffusion Laboratory. NOAA, Oakridge, Tennessee, 59pp.
#' @export
Briggs <- function(stability='D') {
	if (stability == 'A'|stability == 'B'){
	  sigma <- function(x) {
	    list(
	      y = 0.32 * x * (1 + 0.0004 * x) ^ (-1/2),
	      z = 0.24 * x * (1 + 0.001 * x) ^ (1/2)
	    )
	  }
	} else if (stability == 'C'){
	  sigma <- function(x) {
	    list(
	      y = 0.22 * x * (1 + 0.0004 * x) ^ (-1/2),
	      z = 0.20 * x
	    )
	  }
	} else if (stability == 'D') {
	  sigma <- function(x) {
	    list(
	      y = 0.16 * x * (1 + 0.0004 * x) ^ (-1/2),
	      z = 0.14 * x * (1 + 0.0003 * x) ^ (-1/2)
	    )
	  }
	}else if (stability == 'E'|stability == 'F'){
	  sigma <- function(x) {
	    list(
	      y = 0.11 * x * (1 + 0.0004 * x) ^ (-1/2),
	      z = 0.08 * x * (1 + 0.0015 * x) ^ (-1/2)
	    )
	  }
	} else {
	  stop("Unimplemented for stability class ", stability)
	}
	return(sigma)
}