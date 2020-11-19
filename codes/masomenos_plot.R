
# Function: Plot with lines -----------------------------------------------
masomenos_plot <- function(data,xvar,yvar,tvar,lcl,ucl,color){
	xvar <- enquo(xvar)
	yvar <- enquo(yvar)
	tvar <- enquo(tvar)
	lcl  <- enquo(lcl)
	ucl  <- enquo(ucl)
	color <- enquo(color)
		
		
	plot <- ggplot(aes(x = !! xvar, y = !! yvar,
					   group = !! tvar, colour = !! color ), 
				   data = data)+
		geom_point(position = position_dodge(width=0.5))+
		geom_line(linetype = "dashed", position=position_dodge(width=0.5))+
		theme_classic() +
			
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
		scale_colour_manual(values = c("#000000","#F0E442",
									   "#009E73","#56B4E9",
									   "#0072B2"))+
		theme_classic() +
		theme(legend.position = "right") + 
		guides(col = guide_legend(ncol = 1,title.position = "top",))
		
		return(plot)
	}
	


# Function: Plot no lines -------------------------------------------------
masomenos_plot_no_lines <- function(data,xvar,yvar,tvar,lcl,ucl,color){
	xvar <- enquo(xvar)
	yvar <- enquo(yvar)
	tvar <- enquo(tvar)
	lcl  <- enquo(lcl)
	ucl  <- enquo(ucl)
	color <- enquo(color)
		
	plot_nolines <- ggplot(aes(x = !! xvar, y = !! yvar,
							   group = !! tvar, colour = !! color ), 
						   data = data) +
		geom_point(position = position_dodge(width=0.5))+
		#geom_line(linetype = "dashed", position=position_dodge(width=0.5))+
		theme_bw() +
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
			
   		scale_colour_manual(values = c("#000000","#F0E442",
										   "#009E73","#56B4E9",
										   "#0072B2"))+
		theme_classic() +
		theme(legend.position = "right") +
		guides(col = guide_legend(ncol = 1,title.position = "top",))
		
		return(plot_nolines)
}
	
	

# Boxplots ----------------------------------------------------------------

masomenos_boxplot <- function(data,xvar,yvar,color, n_treat){
	xvar <- enquo(xvar)
	yvar <- enquo(yvar)
	color <- enquo(color)
	
	if (n_treat == 4 ) {
		boxplot <- ggplot(aes(x = !! xvar, y = !! yvar, 
							  #group = !! tvar, 
							  colour = !! color ), data = data)+
			geom_boxplot(position = position_dodge(width = .93 ))+
			
			theme_bw() +
			xlab("Levels of nfixer") + 
			ylab("") +
			
			#ambientrain = Yellow ("#F0E442")
			#ambientrain_nutrients = Green ("#009E73") 
			#ambientrain_water = Light blue ("#56B4E9")
			#ambientrain_water_nutrients = Dark blue ("#0072B2")
			scale_colour_manual(values = c("#F0E442","#009E73",
										   "#56B4E9","#0072B2"))+
			theme_classic() +
			theme(legend.position = "right") + 
			guides(col = guide_legend(ncol = 1 ,title.position = "top",))
		
		return(boxplot)} else {
		
		boxplot <- ggplot(aes(x = !! xvar, y = !! yvar,colour = !! color ), 
						  data = data) +
			geom_boxplot(position = position_dodge(width = .93 ))+
			theme_bw() +
			xlab("Levels of nfixer") +
			ylab("") +
				
			#Harvestatthebegging = Black ("#000000")
			#ambientrain = Yellow ("#F0E442")
			#ambientrain_nutrients = Green ("#009E73") 
			#ambientrain_water = Light blue ("#56B4E9")
			#ambientrain_water_nutrients = Dark blue ("#0072B2")
			scale_colour_manual(values = c("#000000","#F0E442",
										   "#009E73","#56B4E9","#0072B2"))+
			theme_classic() +
			theme(legend.position = "right") + 
			guides(col = guide_legend(ncol = 1 ,title.position = "top",))
			
			return(boxplot)} 
	
}
	
		



	

