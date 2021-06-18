# Load packages  ----------------------------------------------------------------
library(janitor)
library(ggpubr)
library(nlme)
library(car)
library(emmeans)
library(lme4)
library(cowplot)
library(tidyverse)
library(ggsci)
library(knitr)
library(performance)


#  Load data --------------------------------------------------------------------

# Biomass Variables
data_complete_final <- 
	read.csv("~/Documents/projects/shade_house_experiment/data/data_for_biomass_leaftraits_models.csv", header = T)


# Clean Biomass Variables
data_complete_final$treatment <- factor(data_complete_final$treatment,
										levels = c(
											"ambientrain", 
											"ambientrain_nutrients",
											"ambientrain_water",
											"ambientrain_water_nutrients"))


data_biomass_variables <-
	data_complete_final %>% 
	
	#Order the columns  
	dplyr::select(id,spcode,family,treatment,nfixer,init_height,
				  everything()) %>% 
	clean_names()   %>%
	dplyr::select(-c(la,ldmc,d13c,sla_cm2_g,cn,narea_g_m2,nmass_mg_g,
					 totalleafmass_nmass, whole_leaf_dry_weight,root_dry_weight,
					 stem_dry_weight,rmf,smf,lmf,family)) %>% 
	mutate(ratio_below_above = below_biom /above_biom)
						


data_biomass_variables$id <- as.factor(data_biomass_variables$id)
data_biomass_variables$spcode <- as.factor(data_biomass_variables$spcode)
data_biomass_variables$treatment <- as.factor(data_biomass_variables$treatment)


# Number of nodules data 
# Source cleaned data
source("~/Documents/projects/shade_house_experiment/codes/cleaned_data_nodules.R")

# Delete unused variables
data_nodules_cleaned <-
	data_nodules_cleaned %>%
	dplyr::select(-c("init_height")) %>% 
	dplyr::select(id,spcode,treatment, everything())
	#				 "nodule_mass_in_the_lab","average_nodule_weight",
	#				 "estimated_nodule_mass_per_plant")) %>%

data_nodules_cleaned$id <- as.factor(data_nodules_cleaned$id)
data_nodules_cleaned$spcode <- as.factor(data_nodules_cleaned$spcode)
data_nodules_cleaned$treatment <- as.factor(data_nodules_cleaned$treatment)


# Join data ----------------------------------------------------------------
data_test <- left_join(data_biomass_variables,data_nodules_cleaned, by = c("id","spcode","treatment"))

data_test_2 <- data_test %>% 
	mutate(log_nodules = log(number_of_root_nodulation))
data_test_2

m1 <- lm(totalbiom ~ treatment*log_nodules + init_height, data = data_test_2)
Anova(m1, type = "III", test = "F")
plot(totalbiom ~ number_of_root_nodulation,data = data_test)

m2 <- lm(totalbiom ~ ratio_below_above*treatment + init_height, data = data_biomass_variables)
plot(totalbiom ~ ratio_below_above,data = data_biomass_variables)
Anova(m2, type = "III", test = "F")

coplot(totalbiom ~ ratio_below_above)



