# Load packages -----------------------------------------------------------
library(tidyverse)
library(janitor)

# Load Biomass data -------------------------------------------------------
data_soil_moisture <- 
	read.csv("~/documents/projects/shade_house_exp/exploratory_figures_and_models/data/raw_data/soil_moisture_data_complete.csv", header = T, na.strings = "-")

# Recode factors ----------------------------------------------------------
unique(data_soil_moisture$Treatment)

#Replace the + in the treatments levels for _
data_soil_moisture$Treatment <- 
	recode(data_soil_moisture$Treatment,
		   `ambient rain` =	                "ambientrain",			   
		   `ambient rain + nutrients`=        "ambientrain_nutrients",
		   `ambient rain + water` =           "ambientrain_water",
		   `ambient rain + water + nutrients` = "ambientrain_water_nutrients"
		   )

#Replace dt for dr 
data_soil_moisture$sppcode <- 
	recode(data_soil_moisture$sppcode,
		   `dt`= "dr"
	)

# Clean data --------------------------------------------------------------

data_soil_moisture_cleaned <- 
	data_soil_moisture %>% 
	clean_names() %>% 
	drop_na() %>% 
	
	#Rename columns
	rename(spcode = sppcode,
		   sm_before_watering = soil_moisture_meausrements_water_content_before_watering,
		   sm_after_watering  = soil_moisture_meausrements_water_content_after_watering) %>% 
	 
	#Transform to factor class spcode,family and treatment
	mutate( spcode = as.factor(spcode),
			date_day_month = as.factor(date_day_month),
		    treatment = as.factor(treatment)) %>% 

	#Create nfixer column
	mutate(nfixer = as.factor(ifelse(spcode == "ec" |
						   	  spcode == "dr" |
						   	  spcode == "gs","fixer", "nonfixer"))) %>% 
	
	#Pivot soil moisture before and soil moisture after in one column
	pivot_longer(c(sm_before_watering,sm_after_watering), 
				 names_to = "sm_measured", values_to = "soil_moisture") %>% 
	
	mutate(sm_measured = as.factor(sm_measured)) %>% 

	#Order the columns
	select(id,spcode,treatment,nfixer,date_day_month,sm_measured,everything()) %>% 
	select(- height_cm) %>% 
	arrange(nfixer) 


# Recode new factors ------------------------------------------------------
	
data_soil_moisture_cleaned$sm_measured <- 
	recode(data_soil_moisture_cleaned$sm_measured,
		   `sm_before_watering` = "before_watering",
		   `sm_after_watering`  = "after_watering"
	)


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

data_soil_moisture_cleaned$date_day_month <-
	factor(data_soil_moisture_cleaned$date_day_month,
		   levels = c(
		   	"31_08", 
		   	"19_09",
		   	"4_10",
		   	"17_10",
		   	"31_10",
		   	"15_11"
		   )
	)

data_soil_moisture_cleaned$sm_measured <-
	factor(data_soil_moisture_cleaned$sm_measured,
		   levels = c(
		   	"before_watering", 
		   	"after_watering"
		   )
	)

	
str(data_soil_moisture_cleaned)

# Remove uncleaned data set -----------------------------------------------
rm(data_soil_moisture)

