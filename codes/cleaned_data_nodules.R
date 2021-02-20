# Load packages -----------------------------------------------------------
library(tidyverse)
library(janitor)


# Load Biomass data -------------------------------------------------------

data_nodules <- 
	read.csv("~/Documents/projects/shade_house_experiment/data/raw_data/1_nodule_data.csv", header = T) %>%
	clean_names()


# Transform to factor spcode,id and treatment -------------------------

#id
data_nodules$id <- 
	as.factor(data_nodules$id)

#spcode
data_nodules$spcode <- 
	as.factor(data_nodules$spcode)

#Treatment
data_nodules$treatment <- 
	as.factor(data_nodules$treatment)

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

# Clean data --------------------------------------------------------------

data_nodules_cleaned <-
	data_nodules %>%
	
	select(id,spcode,treatment, everything())
	
	#Group data this is done because there are 2 sub samples per 
	#plant so what I am doing is get the mean of the sub-samples

str(data_nodules_cleaned)

