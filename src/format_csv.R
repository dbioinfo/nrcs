library(tidyverse)
library(xlsx)
setwd('~/WorkForaging/Academia/Nicole/nrcs')

maindat <- read.xlsx('data/PLFA75.xlsx', sheetIndex = 1)
maindat <- maindat[, !grepl("NA", colnames(maindat))] %>% filter(!is.na(SampleID)) #remove NA rows and cols

set1 <- read.csv('data/Biological-Results-1514279.csv') %>% rename(SampleID=Sample.ID.2)
set2 <- read.csv('data/Biological-Results-1511100.csv') %>% rename(SampleID=Sample.ID.2)
set3 <- read.csv('data/Biological-Results-1507396.csv') %>% rename(SampleID=Sample.ID.2) %>% 
  mutate(SampleID = str_remove(SampleID, "^[^ ]* "))  %>% #set3 has something weird in SampleID column
  mutate(SampleID=str_replace(SampleID, "_(?=[^_]*$)", "")) %>% #something very weird
  mutate(SampleID=str_replace(SampleID, "_(?=[^_]*$)", "-")) #too weird to explain really
    
outdat <- left_join(maindat, rbind(set1, set2, set3), by="SampleID")

write.csv(outdat, file = 'data/PLFA75.merged.csv')

indat <- read.csv('data/PLFA75.mergedNP.csv') %>% 
  filter(!is.na(Arbuscular.Mycorrhizal.ng.g)) %>% 
  filter(Arbuscular.Mycorrhizal.ng.g!="") %>% 
  mutate(Arbuscular.Mycorrhizal.ng.g=as.numeric(Arbuscular.Mycorrhizal.ng.g),
         Biocrust_Type=factor(Biocrust_Type, levels=c('LCC/PC', 'LCC/CC', 
                                                      'LCC', 'DCC/PC', 'DCC/CC',
                                                      'DCC', 'CLAV/PC', 'CLAV/CC',
                                                      'CLAV', 'COLLE2/PC', 
                                                      'COLLE2/CC', 'COLLE2')))


ggplot(indat)+
  geom_violin(aes(x=Biocrust_Type, y=Arbuscular.Mycorrhizal.ng.g, fill=Biocrust_Type))+
  geom_jitter(aes(x=Biocrust_Type, y=Arbuscular.Mycorrhizal.ng.g), height = 0, width=0.3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  guides(fill='none')
