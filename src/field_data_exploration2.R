###Script to data manage the NRCS basal hit field data to obtain a crust data matrix

###Need to install libraries needed
library(tidyverse)

###First need to set Working Directory
setwd("~/Documents/NRCS_biocrust_field_data/NRCS_biocrust_outputs_2026-04-13")

###Read in the field data
data <- read.csv('surface_hit_frequency_summary_2026-04-13.csv')
data

###Reading in the refined hierarchical crust categories
coldat <- read.csv('col_names_refined2.csv')
coldat

###filter to just biocrust categories
selectedvar <- coldat %>% filter(biocrust=="x") %>% pull(all_categories)

###subset the data frame by columns using the select function
#filter will subset a data matrix by rows while select subsets it by columns, we also are going to include the plot ID to the selectedvar list using the concatenation function c
temp <- data %>% select(all_of(c("PlotID", selectedvar)))
View(temp)
#Get some additional metadata hidden in the PlotID column 
temp2 <- temp %>% separate_wider_delim(PlotID, delim = "_", names = c("Desert", "Substrate", "SiteNr"), cols_remove = FALSE)
View(temp2)
unique(temp2$Desert)
#has a NA row for a site without a ID

#summary stats
temp2 %>% group_by(Desert) %>% summarise(across(selectedvar, ~mean(.x,na.rm=T)))
temp3 <- temp2 %>% group_by(Desert) %>% summarise(across(selectedvar, ~mean(.x,na.rm=T)))
View(temp3)
gdat <- temp3 %>% 
  pivot_longer(!Desert, names_to = "all_categories", values_to = "values") %>%
  left_join(.,coldat, by = "all_categories")

#graph the fun
ggplot(gdat %>% filter(!is.na(Desert), biocrust_category_coarse!="")) + 
  geom_boxplot(aes(x=Desert, y=values, fill=biocrust_category_coarse)) +
  facet_wrap(~biocrust_category_coarse, scales="free") +
  theme_bw()

gdat %>% filter(biocrust_category_coarse=="")
