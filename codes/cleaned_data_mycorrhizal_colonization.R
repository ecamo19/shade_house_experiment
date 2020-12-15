
# Load Biomass data -------------------------------------------------------

data_mycorrhizal_colonization <- 
	read.csv("~/documents/projects/shade_house_exp/exploratory_figures_and_models/data/raw_data/2_micorryzhal_colonization_data.csv", header = T)



# Clean data --------------------------------------------------------------

data_mycorrhizal_colonization_cleaned <- 
	data_mycorrhizal_colonization %>% 
	
	#Merge id and sub-sample
	#unite("id",id,sub_sample,remove = TRUE) %>% 
	
	#Create nfixer column
	mutate(nfixer = ifelse(spcode == "ec" |
						   	spcode == "dr" |
						   	spcode == "gs","fixer", "nonfixer")) %>% 
	
	#log transform percentage variable
	mutate(log_perc = log(percentage)) %>% 
	select(-family) %>% 
	select(id,spcode,treatment,nfixer, everything()) %>% 
	
	#Group data this is done because there are 2 sub samples per 
	#plant so what I am doing is get the mean of the sub-samples
	group_by(id,spcode,treatment,nfixer) %>%
	
	#log only the values greather than 0
	mutate(log_perc = if_else(percentage > 0, log(percentage),0)) %>% 
	summarise_if(is.numeric, funs(mean)) %>% 
	arrange(nfixer) 



# Transform to factor spcode,family and treatment -------------------------

#spcode

data_mycorrhizal_colonization_cleaned$spcode <- 
	as.factor(data_mycorrhizal_colonization_cleaned$spcode)

#nfixer
data_mycorrhizal_colonization_cleaned$nfixer <- 
	as.factor(data_mycorrhizal_colonization_cleaned$nfixer)

#Treatment
data_mycorrhizal_colonization_cleaned$treatment <- 
	as.factor(data_mycorrhizal_colonization_cleaned$treatment)

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

