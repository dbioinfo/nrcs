library(tidyverse)

setwd('~/WorkForaging/Academia/Nicole/nrcs/')

#input meta
meta <- read_xlsx('data/20260904_MetaData_PLFA.xlsx') %>% 
  rename("Sample.ID.2"="Sample ID 2")  %>% 
  mutate(Sample.ID.2 = gsub('_','-',Sample.ID.2))

#define directory path where .csv files live
plfa_dir <- "data/PLFAData/"
fs <- list.files(plfa_dir)

#assumptions that must not be violated, or else new code required
## all groups have exact same column headers (except PLFA.Wt which is only in 2 sample blocks and always empty)
tdat <- c()
for (f in fs){
  idat <- read.csv(paste0(plfa_dir,f) )
  if ( ("PLFA.Wt" %in% colnames(idat)) ) {idat <- idat %>% select(-PLFA.Wt)}
  fc <- str_extract(f,"[0-9].*(?=\\.csv)")
  idat <- idat %>% mutate(batch_ID=fc)
  
  tdat <- rbind(tdat,idat)
}

#!!!danger step: manual colname adjustment 
colnames(tdat) <- c("Sample.Type","Customer.No",
                    "Name","Company","Address.1","Address.2","City","State","Zip",
                    "Date.Received","Date.Reported","Lab.No", "Results.For",
                    "Sample.ID.1","Sample.ID.2","Sample.ID.3", "Begin.Depth", "End.Depth",
                    "Crop","Past.Crop","Total.Living.Microbial.Biomass.ng.g","Protozoan.ng.g",
                    "Protozoan.ng.g.perc.Biomass", "Cyclo.19.0.ng.g", "PolyUnsaturated.ng.g",
                    "Cyclo.17.0.ng.g", "MonoUnsaturated.ng.g", "Saturated.ng.g", "Unsaturated.ng.g",
                    "Pre.18.1w7c.cy19.0.ng.g","Pre.18.1.w7c.ng.g","Pre.16.1w7c.cy17.0.ng.g",
                    "Pre.16.1.w7c.ng.g","Monounsaturated.Polyunsaturated.ng.g","Saturated.Unsaturated.ng.g",
                    "Gram.posneg.ratio.ng.g","Predator.Prey.ng.g","Fungi.Bacteria.ng.g",
                    "Undifferentiated.ng.g","Undifferentiated.ng.g.perc.Biomass","Gram.pos.ng.g",
                    "Gram.pos.ng.g.perc.Biomass", "Saprophytes.ng.g","Saprophytes.ng.g.perc.Biomass",
                    "Arbuscular.Mycorrhizal.ng.g","Arbuscular.Mycorrhizal.ng.g.perc.Biomass","Total.Fungi.ng.g",
                    "Total.Fungi.ng.g.perc.Biomass","Rhizobia.ng.g", "Rhizobia.ng.g.perc.Biomass","Gram.neg.ng.g",
                    "Gram.neg.ng.g.perc.Biomass", "Actinomycetes.ng.g", "Actinomycetes.ng.g.perc.Biomass", 
                    "Total.Bacteria.ng.g", "Total.Bacteria.ng.g.perc.Biomass", 
                    "Functional.Group.Diversity.Index.ng.g", "batch_ID")

#Sample.ID.2 is the unique identifier column, but the formatting is awful
#here we standardize the using the metadata (does nicole want me to automate that too?)

tdat <- tdat %>% 
  mutate(Sample.ID.2 = gsub('_','-',Sample.ID.2))

#QC checks -- Sample comparison one-way
ndiff <- length(setdiff(tdat$Sample.ID.2,meta$Sample.ID.2))
if (ndiff>0){
  print(paste0("Warning -- ", ndiff, " samples excluded found in data but not metadata"))
  print(setdiff(tdat$Sample.ID.2,meta$Sample.ID.2))
} else {
  print("All samples with data found in metadata")
}

tdat <- tdat %>% filter(Sample.ID.2 %in% meta$Sample.ID.2) %>% 
  left_join(., meta %>% select(Sample.ID.2, Crust_Nr), by="Sample.ID.2") %>% 
  rename("SampleID"="Crust_Nr")

#remove problematic samples with no data (Total.Living.Microbial.Biomass.ng.g==1) 5 samples only
#across all columns with "<0.01", change that value to 0 to help with downstream computation
charcols <- c("Gram.neg.ng.g", "Gram.pos.ng.g" ,"Gram.posneg.ratio.ng.g","Predator.Prey.ng.g","Rhizobia.ng.g",
              "Saprophytes.ng.g", "Cyclo.19.0.ng.g","PolyUnsaturated.ng.g","Cyclo.17.0.ng.g",
              "Pre.18.1w7c.cy19.0.ng.g", "Pre.18.1.w7c.ng.g", "Pre.16.1w7c.cy17.0.ng.g", "Pre.16.1.w7c.ng.g",
              "Arbuscular.Mycorrhizal.ng.g", "Actinomycetes.ng.g")
idat <- tdat %>% 
  filter(Total.Living.Microbial.Biomass.ng.g!=1) %>% 
  mutate(across(all_of(charcols), ~if_else(str_detect(.x,"<"), 0, as.numeric(.x) )))


#write to file
write_csv(idat, "data/PLFA_merged.csv")

#graph-ready data 
data <- idat %>% select(SampleID,
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
  left_join(., meta %>% rename('SampleID'='Crust_Nr'), by="SampleID")

#write to file
write_csv(data, 'data/cleaned_crust_data.csv')

