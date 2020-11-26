# Packages ----------------------------------------------------------------
library(docstring)


# Function RGR ------------------------------------------------------------

perc_diff <- 
	function(trait,
			 treatment_reference = "ambientrain") {
		
	#' This funtion calculates the percentage difference
	#' between 2 values 
	#' ((y2 - y1)/ y1)*100
	
	#Select the treatment that you want use as reference, the
	#default is ambient rain treatment
	
	trait_value_reference <-
		filter(treatment ==  "ambientrain")
	
	#trait_value <- trait[treatment_reference !=  "ambientrain"] 
	
	#Calculate <- function(x) ((x-y)/y)*100
	
	perc_diff <- 
		trait 
		#(trait_value_reference ) 
		 #/trait_value_reference) * 100
	
	return(perc_diff)
}




# Prueba 2 ----------------------------------------------------------------


rgr <- function(rmf, treatment){
	#' This funtion calculates the Relative growth rate 
	#' (log(Final Height) - log(Inital height)) /
	#'           time final - time initial  
	
	#Select the first measure
	initial_heigth <- rmf[treatment == "ambientrain"]
	
	#Calculate rgr
	rgr <- rmf - initial_heigth
	return(rgr)
}
















