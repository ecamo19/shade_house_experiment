# Objective ----------------------------------------------------------------

#This script join all the data sets with the objective of adding the 
# harvested at the beginning treatment to the data
# 
# This code creates the data file called data_for_biomass_leaftraits_models.csv

# Packages ----------------------------------------------------------------

library(dplyr)
library(knitr)
library(ggplot2)
library(tidyr)
library(ggpubr) 
library(janitor)
library(textshape)

#Load data ---------------------------------------------------------------


# Biomass data ------------------------------------------------------------

data_biomass <- 
	read.csv("/home/ecamo19/documents/projects/shade_house_exp/old_stuff/raw_data/mass_fractions/6_plant_dry_weights_data.csv", header = T)


#Clean total biomass

data_totalbiom <- 
	data_biomass %>%
	
	#Total biomass
	mutate(total_biomass = root_dry_weight + stem_dry_weight + 
		   	whole_leaf_dry_weight) %>%
	
	# % Above ground biomass
	mutate(above_biom = 
		   	(stem_dry_weight + whole_leaf_dry_weight / total_biomass)) %>% 
	
	# % Below ground biomass
	mutate(below_biom = (root_dry_weight/ total_biomass)) %>% 
	
	# Calculate mass fractions
	mutate(rmf = (root_dry_weight / total_biomass)*100) %>% 
	mutate(smf = (stem_dry_weight / total_biomass)*100) %>% 
	mutate(lmf = (whole_leaf_dry_weight / total_biomass)*100) %>% 
	
	#filter( !treatment %in% 'Harvestatthebegging') %>%
	#dplyr::select(-c(root_dry_weight, stem_dry_weight)) %>% 
	dplyr::select(id, spcode,treatment, rmf, smf,lmf,
				  whole_leaf_dry_weight, everything())


# Data ecophys ------------------------------------------------------------


########################################
# No ecophys data because it was not   #
# measured in the harvest at beginning #
########################################


# Leaf trait data ---------------------------------------------------------

data_traits <- read.csv("/home/ecamo19/documents/projects/shade_house_exp/old_stuff/raw_data/aboveground_data/2_leaf_trait_data.csv", header = T) 

# Clean leaf traits data
data_leaf_traits <- 
	data_traits %>%
	clean_names() %>% 
	dplyr::select(-c (family))


# Isotopes data -----------------------------------------------------------

data_isotopes <- read.csv("/home/ecamo19/documents/projects/shade_house_exp/old_stuff/raw_data/aboveground_data/4_isotopes_data.csv", 
						  header = T) 

# Clean Isotopes data
data_nitrogen_carbon_d13c <- 
	data_isotopes %>% 
	clean_names() %>% 
	dplyr::select(-c (d15n, family))


# Initial height data -----------------------------------------------------

data_initheight <- read.csv("/home/ecamo19/documents/projects/shade_house_exp/old_stuff/raw_data/aboveground_data/data_heights.csv", 
						  header = T) 

#Clean initial height data
data_initheight <- 
	data_initheight	%>% 
	dplyr::select(1:5) %>% 
	rename(init_height = X20150831)


# Join data sets ----------------------------------------------------------
data_complete <- 
	
	#Join biomass and leaf traits  
	right_join(data_totalbiom, data_leaf_traits, by = c("id", "spcode"))  %>% 
	dplyr::select(-c(treatment.y)) %>% 
	rename(treatment = treatment.x) %>% 
	
	#Add leaf carbon and nitrogen 
	right_join(., data_nitrogen_carbon_d13c , by = c("id", "spcode"))  %>% 
	dplyr::select(-c(treatment.y)) %>% 
	rename(treatment = treatment.x) %>% 
	
	#Add plant initial height
	right_join(., data_initheight , by = c("id", "spcode")) %>% 
	dplyr::select(-c(treatment.y)) %>% 
	rename(treatment = treatment.x) %>% 
	clean_names() %>% 
	
	#I deleted 	leaf_density and leaf thickness because this data was not 
	#available for harvested at beginning plants
	
	dplyr::select(-c(leaf_density,lt,lma)) %>% 
	rename(totalbiom = total_biomass)  


# Convert data to the right units -----------------------------------------

data_complete <- 
	
	data_complete %>% 
	#Transform sla to cm2
	mutate(sla_cm2_g = sla*10000) %>% 
	dplyr::select(-sla)
	

# Calculate Nmass and Narea -----------------------------------------------

data_complete_final <- 
	data_complete %>%
	mutate(CN = perc_c/perc_n,
		   N_g = leaf_dry_weight*(perc_n/100),
		   N_mg = (leaf_dry_weight*(perc_n/100))*1000,
		   Narea_g_m2 = N_g/la,
		   Nmass_mg_g = N_mg/leaf_dry_weight,
		   
		   #Whole plant traits
		   
		   #Whole plant leaf area in m2
		   #whole_leafarea_m2 = (whole_leaf_dry_weight*sla_cm2_g)/10000,
		   
		   #Whole leaf mass multiplied by Nmass
		   totalleafmass_Nmass = whole_leaf_dry_weight*Nmass_mg_g) %>% 
		   
		   #Whole plant leaf area in m2 multiplied by Narea 
		   #whole_leafarea_Narea_m2 = whole_leafarea_m2 * Narea_g_m2) %>%

	dplyr::select(-c(leaf_fresh_weight,leaf_dry_weight,
					 perc_n,perc_c,ratio_c_n,N_g,N_mg)) %>% 
	mutate(nfixer = ifelse(spcode == "ec" |
						   	spcode == "dr" |
						   	spcode == "gs","fixer", "nonfixer")) %>%
	dplyr::select(id,spcode,treatment,family,nfixer,init_height,
				  everything()) %>% 
	
	drop_na()



# Order the factor treatments ---------------------------------------------

data_complete_final$treatment <- factor(data_complete_final$treatment,
					levels = c("Harvestatthebegging",
							   "ambientrain", 
							   "ambientrain_nutrients",
							   "ambientrain_water",
							   "ambientrain_water_nutrients"
							   )
					)

# Save file as .csv  ------------------------------------------------------
#write.csv(data_complete_final,"~/documents/projects/shade_house_exp/exploratory_figures_and_models/data/data_for_biomass_leaftraits_models.csv")





