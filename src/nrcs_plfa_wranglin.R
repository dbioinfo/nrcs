library(tidyverse)

setwd('~/WorkForaging/Academia/Nicole/nrcs/')


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
  
  tdat <- rbind(idat, tdat)
}

#!!!danger step: manual colname adjustment 
#SampleID refers to Sample ID 2 and must be a unique identifier
colnames(tdat) <- c("Sample.Type","Customer.No",
                    "Name","Company","Address.1","Address.2","City","State","Zip",
                    "Date.Received","Date.Reported","Lab.No", "Results.For",
                    "Sample.ID.1","SampleID","Sample.ID.3", "Begin.Depth", "End.Depth",
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


#remove problematic samples with no data (Total.Living.Microbial.Biomass.ng.g==1)
#across all columns with "<0.01", change that value to 0 to help with downstream computation
charcols <- c("Gram.neg.ng.g", "Gram.pos.ng.g" ,"Gram.posneg.ratio.ng.g","Predator.Prey.ng.g","Rhizobia.ng.g",
              "Saprophytes.ng.g", "Cyclo.19.0.ng.g","PolyUnsaturated.ng.g","Cyclo.17.0.ng.g",
              "Pre.18.1w7c.cy19.0.ng.g", "Pre.18.1.w7c.ng.g", "Pre.16.1w7c.cy17.0.ng.g", "Pre.16.1.w7c.ng.g",
              "Arbuscular.Mycorrhizal.ng.g", "Actinomycetes.ng.g")
idat <- tdat %>% 
  filter(Total.Living.Microbial.Biomass.ng.g!=1) %>% 
  mutate(SampleNumber=row_number()) %>% 
  mutate(across(all_of(charcols), ~if_else(str_detect(.x,"<"), 0, as.numeric(.x) )))


#write to file
write_csv(idat, "data/PLFA_merged.csv")


