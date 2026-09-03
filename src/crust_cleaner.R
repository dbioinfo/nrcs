library(tidyverse)
library(readxl)

setwd('~/WorkForaging/Academia/Nicole/nrcs/')

#read inputs 
tdat <- read_csv("data/PLFA_merged.csv")
meta <- read_xlsx('data/MetaData_PLFA_MM.xlsx') %>% rename("SampleID"="Sample ID 2")

#make data a bit more readable down the line
data <- tdat %>% select(SampleID,
                        "Total Microbial Biomass" = "Total.Living.Microbial.Biomass.ng.g",
                        "Protists Biomass" = "Protozoan.ng.g",
                        "Percent Protists" = "Protozoan.ng.g.perc.Biomass",
                        "Fungi to Bacteria Ratio" = "Fungi.Bacteria.ng.g",
                        "Undifferentiated Biomass" = "Undifferentiated.ng.g",
                        "Percent Undifferentiated" = "Undifferentiated.ng.g.perc.Biomass",
                        "Other Gram Pos Biomass" = "Gram.pos.ng.g",
                        "Percent Other Gram Pos" = "Gram.pos.ng.g.perc.Biomass",
                        "Saprophytes Biomass" = "Saprophytes.ng.g",
                        "Percent Saprophytes" = "Saprophytes.ng.g.perc.Biomass",
                        "Arbuscular Mycorrhizal Fungal Biomass"="Arbuscular.Mycorrhizal.ng.g",
                        "Percent Arbuscular Mycorrhizal Fungi"="Arbuscular.Mycorrhizal.ng.g.perc.Biomass",
                        "Total Fungal Biomass" = "Total.Fungi.ng.g",
                        "Percent Fungal Biomass" = "Total.Fungi.ng.g.perc.Biomass",
                        "Rhizobia Biomass" = "Rhizobia.ng.g",
                        "Percent Rhizobia" = "Rhizobia.ng.g.perc.Biomass",
                        "Other Gram Neg Biomass" ="Gram.neg.ng.g",
                        "Percent Other Gram Neg"="Gram.neg.ng.g.perc.Biomass",
                        "Actinobacteria Biomass"="Actinomycetes.ng.g",
                        "Percent Actinobacteria" = "Actinomycetes.ng.g.perc.Biomass",
                        "Total Bacteria Biomass" = "Total.Bacteria.ng.g",
                        "Percent Bacteria" = "Total.Bacteria.ng.g.perc.Biomass",
                        "Shannon Diversity" = "Functional.Group.Diversity.Index.ng.g",
                        "Predator Prey Ratio" = "Predator.Prey.ng.g") %>% 
  pivot_longer(!SampleID, names_to = 'variable', values_to = 'value') %>% 
  mutate(variable = gsub(' ','_',variable),
         value = case_when(value=='< 0.01'~0, .default=as.numeric(value))) %>% 
  pivot_wider(id_cols = SampleID, names_from = "variable", values_from = "value")  %>% 
  left_join(., meta, by="SampleID")

#write to file
write_csv(data, 'data/cleaned_crust_data.csv')
