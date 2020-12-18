rm(list = ls())

# Load packages -----------------------------------------------------------
library(tidyverse)
library(janitor)

# Load Biomass data -------------------------------------------------------
data_soil_moisture <- 
	read.csv("~/documents/projects/shade_house_exp/exploratory_figures_and_models/data/raw_data/soil_moisture_data.csv", header = T)


# Transform to factor class spcode,family and treatment -------------------

#spcode
data_soil_moisture$spcode <- 
	as.factor(data_soil_moisture$spcode)

#Treatment
data_soil_moisture$Treatment <- 
	as.factor(data_soil_moisture$Treatment)

#Date
data_soil_moisture$Date <- 
	as.factor(data_soil_moisture$Date)


# Recode factors ----------------------------------------------------------

#Replace the + in the treatments levels for _
data_soil_moisture$Treatment <- 
	recode(data_soil_moisture$Treatment,
		   `ambient_rain` =	                "ambientrain",			   
		   `ambient_rain+nutrients`=        "ambientrain_nutrients",
		   `ambient_rain+water` =           "ambientrain_water",
		   `ambient_rain+water+nutrients` = "ambientrain_water_nutrients"
		   )


#Replace dt for dr 
data_soil_moisture$spcode <- 
	recode(data_soil_moisture$spcode,
		   `dt`= "dr"
	)

# Clean data --------------------------------------------------------------

data_soil_moisture_cleaned <- 
	data_soil_moisture %>% 
	clean_names() %>% 
	#Create nfixer column
	mutate(nfixer = as.factor(ifelse(spcode == "ec" |
						   	  spcode == "dr" |
						   	  spcode == "gs","fixer", "nonfixer"))) %>% 
	
	#Order the columns
	select(spcode,treatment,nfixer,date, everything()) %>% 
	arrange(nfixer) 

# Order factors -----------------------------------------------------------
data_soil_moisture_cleaned$treatment <-
		factor(data_soil_moisture_cleaned$treatment,
			   levels = c(
			   	"ambientrain", 
			   	"ambientrain_nutrients",
			   	"ambientrain_water",
			   	"ambientrain_water_nutrients"
			   )
		)
	


str(data_soil_moisture_cleaned)

# Remove uncleaned data set -----------------------------------------------
rm(data_soil_moisture)
