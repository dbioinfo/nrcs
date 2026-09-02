library(tidyverse)
library(readxl)

setwd('~/WorkForaging/Academia/Nicole/plfa/')

data <- read_xlsx('data/Biological_Results_Master_QCed.xlsx')
meta <- read_xlsx('data/MetaData_PLFA_MM.xlsx')

data <- data %>% select(SampleNumber, 
                        "Total Microbial Biomass","Protists Biomass","Percent Protists",
                        "Fungi to Bacteria Ratio","Undifferentiated Biomass",
                        "Percent Undifferentiated",
                        "Other Gram Pos Biomass",
                        "Percent Other Gram Pos",
                        "Saprophytes Biomass",
                        "Percent Saprophytes",
                        "Arbuscular Mycorrhizal Fungal Biomass"="Arbuscular Mycorrhizal Fungal  Biomass",
                        "Percent Arbuscular Mycorrhizal Fungi",
                        "Total Fungal Biomass",
                        "Percent Fungal Biomass",
                        "Rhizobia Biomass",
                        "Percent Rhizobia",
                        "Other Gram Neg Biomass",
                        "Percent Other Gram Neg",
                        "Actinobacteria Biomass",
                        "Percent Actinobacteria",
                        "Total Bacteria Biomass",
                        "Percent Bacteria",
                        "Shannon Diversity") %>% 
  pivot_longer(!SampleNumber, names_to = 'variable', values_to = 'value') %>% 
  mutate(variable = gsub(' ','_',variable),
         value = case_when(value=='< 0.01'~0, .default=as.numeric(value))) %>% 
  pivot_wider(id_cols = SampleNumber, names_from = "variable", values_from = "value")  %>% 
  left_join(., meta, by="SampleNumber")


write_csv(data, 'data/cleaned_crust_data.csv')



