# Load packages -----------------------------------------------------------
library(tidyverse)
library(janitor)


# Load nodule and height data ----------------------------------------------

data_nodules <- 
	read.csv("~/Documents/projects/shade_house_experiment/data/raw_data/1_nodule_data.csv", header = T) %>%
	clean_names()

data_init_height <- 
	read.csv("~/Documents/projects/shade_house_experiment/data/raw_data/data_heights.csv", header = T) %>%
	clean_names() %>% 
	
	#remove unused columns
	dplyr::select(1:5)


# Transform to factor spcode,id and treatment -------------------------

data_nodules <- 
	data_nodules %>% 
	mutate(id = factor(id),
		   spcode = factor(spcode),
		   treatment = factor(treatment))
	


#Order factors 
data_nodules$treatment <-
	factor(data_nodules$treatment,
		   levels = c(
		   	"ambientrain", 
		   	"ambientrain_nutrients",
		   	"ambientrain_water",
		   	"ambientrain_water_nutrients"
		   )
	)


# Clean data and join initial height ---------------------------------------

#Select nfixer species

data_init_height_nfixer <- 
	data_init_height  %>% 
	filter(family == "Legume" & spcode != "hc") %>% 
	
	#Remove unused columns 
	dplyr::select(-c(family)) %>% 
	
	#Remove Harvestatthebegging level
	filter(!treatment == "Harvestatthebegging") %>% 
	
	#Transform columns to a factor
	mutate(spcode = factor(spcode),
		   treatment = factor(treatment),
		   id = factor(id)) %>% 
		
	rename(init_height = "x20150831")
	 
	
data_nodules_cleaned <-
	data_nodules %>%
	
	dplyr::select(id,spcode,treatment, everything())
	

#Join data sets
#left_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

data_nodules_cleaned <- 
	left_join(data_nodules_cleaned,data_init_height_nfixer , by = c('id','treatment',
																'spcode')) 




# Remove unused data sets --------------------------------------------------
rm(data_nodules)
rm(data_init_height)