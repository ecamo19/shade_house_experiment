# Load packages -----------------------------------------------------------
library(tidyverse)

# Load Biomass data -------------------------------------------------------
data_mycorrhizal_colonization <- 
	read.csv("~/Documents/projects/shade_house_experiment/data/raw_data/2_micorryzhal_colonization_data.csv", header = T)

# Clean data --------------------------------------------------------------

data_mycorrhizal_colonization_cleaned <- 
	data_mycorrhizal_colonization %>% 
	select(-family) %>% 
	
	#There are some values greater that 100%. Values greater than 100% were
	#treated as 100%
	mutate(percentage_upto_100 = if_else(percentage > 100, 100, percentage)) %>% 
	
	#Create nfixer column
	mutate(nfixer = ifelse(spcode == "ec" |
						   	spcode == "dr" |
						   	spcode == "gs","fixer", "nonfixer")) %>% 
	
	#Order the columns
	select(id,spcode,treatment,nfixer, everything()) %>% 
	
	#Group data this is done because there are 2 sub samples per 
	#plant so what I am doing is get the mean of the sub-samples
	group_by(id,spcode,treatment,nfixer) %>%
	 
	#log only the values greater than 0
	mutate(log_perc = if_else(percentage > 0, log(percentage),0)) %>% 
	mutate(log_perc_upto_100 = 
		   	if_else(percentage_upto_100  > 0, log(percentage_upto_100 ),0)) %>%
	

	#Create a value for every single plant
	summarise_if(is.numeric, funs(mean))  %>% 
	
	arrange(nfixer) 

# Transform to factor class spcode,family and treatment -------------------

#spcode

data_mycorrhizal_colonization_cleaned$spcode <- 
	as.factor(data_mycorrhizal_colonization_cleaned$spcode)

#nfixer
data_mycorrhizal_colonization_cleaned$nfixer <- 
	as.factor(data_mycorrhizal_colonization_cleaned$nfixer)

#Treatment
data_mycorrhizal_colonization_cleaned$treatment <- 
	as.factor(data_mycorrhizal_colonization_cleaned$treatment)

#id
#Treatment
data_mycorrhizal_colonization_cleaned$id <- 
	as.factor(data_mycorrhizal_colonization_cleaned$id)

#Order factors 
data_mycorrhizal_colonization_cleaned$treatment <-
	factor(data_mycorrhizal_colonization_cleaned$treatment,
		   levels = c(
		   	"ambientrain", 
		   	"ambientrain_nutrients",
		   	"ambientrain_water",
		   	"ambientrain_water_nutrients"
		   )
	)

# Remove uncleaned data set -----------------------------------------------
rm(data_mycorrhizal_colonization)
