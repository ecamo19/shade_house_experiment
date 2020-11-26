perc_difference <- function(inicial,cambio){
	perc_diff <- ((cambio-inicial)/inicial)*100
	return(perc_diff)
}

perc_difference(inicial = 21.722, cambio = 37.209)
