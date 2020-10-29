masomenos_plot <- function(data,xvar,yvar,tvar,lcl,ucl,color,palette){
	xvar <- enquo(xvar)
	yvar <- enquo(yvar)
	tvar <- enquo(tvar)
	lcl  <- enquo(lcl)
	ucl  <- enquo(ucl)
	color <- enquo(color)
	
	
	plot <- ggplot(aes(x = !! xvar, y = !! yvar, 
					   group = !! tvar, colour = !! color ), data = data)+
		geom_point(position = position_dodge(width=0.5))+
		geom_line(linetype = "dashed", position=position_dodge(width=0.5))+
		theme_bw() +
		scale_colour_manual(values = palette)+
		geom_errorbar(aes(
			ymin = !! lcl, 
			ymax = !! ucl),
			width=.2,
			position=position_dodge(0.5)) +
		xlab("Levels of nfixer") + 
		ylab("Linear prediction") +
	
	#Harvestatthebegging = Black ("#000000")
	#ambientrain = Yellow ("#F0E442")
	#ambientrain_nutrients = Green ("#009E73") 
	#ambientrain_water = Light blue ("#56B4E9")
	#ambientrain_water_nutrients = Dark blue ("#0072B2")
	    scale_colour_manual(values = c("#F0E442",
	    							   "#009E73","#56B4E9","#0072B2"))+
		theme_bw() 
	
	return(plot)
}
