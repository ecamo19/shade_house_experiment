# Packages ----------------------------------------------------------------
library(docstring)


# Function RGR ------------------------------------------------------------

rgr <- function(height, days){
	#' This funtion calculates the Relative growth rate 
	#' (log(Final Height) - log(Inital height)) /
	#'           time final - time initial  
	
	#Select the first measure
	initial_heigth <- height[days == 0]
	
	#Calculate rgr
	rgr <- ( log(height) - log(initial_heigth)) / days
	return(rgr)
}


# Function AGR ------------------------------------------------------------

agr <- function(height, days){
	#' This funtion calculates the absulate growth rate 
	#'         (Final Height - Inital height) /
	#'           time final - time initial  
	
	#Select the first mesaure
	initial_heigth <- height[days == 0]
	
	#Calculate rgr
	agr <- ( height - initial_heigth) / days
	return(agr)
}

























