# Packages ----------------------------------------------------------------
library(docstring)


# Function RGR ------------------------------------------------------------

perc_diff <- 
	function(treatment_to_compare, treatment_reference = 'ambientrain' ){
	#' This funtion calculates the percentage difference
	#' between 2 values 
	#' ((y2 - y1)/ y1)*100
	
	#Select the treatment that you want use as reference, the
	#default is ambient rain treatment
	treatment_reference <- trait[treatment == treatment_reference]
	
	#Calculate rgr
	perc_diff <- ( treatment_to_compare - treatment_reference
				   / treatment_reference) * 100
	return(perc_diff)
}





















