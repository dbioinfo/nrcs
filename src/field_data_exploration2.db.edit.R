###Script to data manage the NRCS basal hit field data to obtain a crust data matrix

###Need to install libraries needed
library(tidyverse)

###First need to set Working Directory
setwd("~/Documents/NRCS_biocrust_field_data/NRCS_biocrust_outputs_2026-04-13")

###Read in the field data
data <- read.csv('surface_hit_frequency_summary_2026-04-13.csv') %>% filter(!is.na(PlotID)) #this filters the bad samp
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
temp2 %>% group_by(Desert) %>% summarise(across(all_of(selectedvar), ~mean(.x,na.rm=T)))
temp3 <- temp2 %>% group_by(Desert) %>% summarise(across(all_of(selectedvar), ~mean(.x,na.rm=T)))
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


#Fine nmds
library(vegan)
cnames <- coldat %>% filter(biocrust=='x') %>% pull(all_categories) #filter which individual variables to use
nmds_dat <- data[,cnames] #subset data for nmds
rownames(nmds_dat) <- data$PlotID #make sure to track IDs

out <- metaMDS(nmds_dat, #run nmds
               k=3,
               wascores = T,
               weakties=T,
               try=50,
               trymax=100,
               parallel=8,
               maxit=300)


svec <- envfit(out, nmds_dat) #fit the vectors to the previous nmds
selvecs <- names(svec$vectors$r[svec$vectors$r>.35]) #choose the top correlated vectors
sdat <-  as.data.frame(scores(out, display = "species")) %>% 
  rownames_to_column('biocrust_category_fine') %>% 
  filter(biocrust_category_fine %in% selvecs)


gdat <- out$points %>% #preprocess for graphing
  as.data.frame() %>% 
  rownames_to_column("PlotID") %>% 
  separate_wider_delim(PlotID, delim = '_', names = c("Desert","Substrate","PlotNr"))

g1a <- ggplot(gdat)+ #MDS 1 v 2
  geom_point(aes(x=MDS1, y=MDS2, fill=Desert), shape=21, size=3)+
  geom_segment(data=sdat, mapping = aes(x=0,y=0, 
                                        xend=NMDS1, yend=NMDS2, 
                                        color=biocrust_category_fine), 
               arrow = arrow())+
  scale_fill_manual(values = c("#F88299","#DDB71F","#A37BA7"))+
  scale_color_manual(values = c("#F8A582","#DE9CDA","#B892E7","#696969","#9ED080","#54B06C","#8FC0C7"),name="Biocrust")+
  theme_bw()+
  ggtitle("Fine-Scale NMDS (MDS1 v MDS2)")

g1b <- ggplot(gdat)+ #MDS 2 v 3
  geom_point(aes(x=MDS2, y=MDS3, fill=Desert), shape=21, size=3)+
  geom_segment(data=sdat, mapping = aes(x=0,y=0, 
                                        xend=NMDS1, yend=NMDS2, 
                                        color=biocrust_category_fine), 
               arrow = arrow())+
  scale_fill_manual(values = c("#F88299","#DDB71F","#A37BA7"))+
  scale_color_manual(values = c("#F8A582","#DE9CDA","#B892E7","#696969","#9ED080","#54B06C","#8FC0C7"),name="Biocrust")+
  theme_bw()+
  ggtitle("Fine-Scale NMDS (MDS2 v MDS3)")

g2a <- ggplot(gdat)+ #MDS 1 v 2
  geom_point(aes(x=MDS1, y=MDS2, fill=Substrate), shape=21, size=3)+
  geom_segment(data=sdat, mapping = aes(x=0,y=0, 
                                        xend=NMDS1, yend=NMDS2, 
                                        color=biocrust_category_fine), 
               arrow = arrow())+
  scale_fill_manual(values = c("#DDB71F","#AE91E8"))+
  scale_color_manual(values = c("#F8A582","#DE9CDA","#B892E7","#696969","#9ED080","#54B06C","#8FC0C7"),name="Biocrust")+
  theme_bw()+
  ggtitle("Fine-Scale NMDS (MDS1 v MDS2)")

g2b <- ggplot(gdat)+ #MDS 2 v 3
  geom_point(aes(x=MDS2, y=MDS3, fill=Substrate), shape=21, size=3)+
  geom_segment(data=sdat, mapping = aes(x=0,y=0, 
                                        xend=NMDS1, yend=NMDS2, 
                                        color=biocrust_category_fine), 
               arrow = arrow())+
  scale_fill_manual(values = c("#DDB71F","#AE91E8"))+
  scale_color_manual(values = c("#F8A582","#DE9CDA","#B892E7","#696969","#9ED080","#54B06C","#8FC0C7"),name="Biocrust")+
  theme_bw()+
  ggtitle("Fine-Scale NMDS (MDS2 v MDS3)")

library(patchwork)
pdf("figs/field_data.nmds.fine.pdf", width = 12, height = 7)
(g1a | g1b) / 
  (g2a | g2b)
dev.off()


#features ordination
vecdat <- nmds_dat %>% 
  rownames_to_column("PlotID") %>% 
  pivot_longer(!PlotID, names_to = 'all_categories', values_to = 'value') %>% 
  left_join(., coldat, by="all_categories") %>% 
  group_by(PlotID,biocrust_category_coarse) %>% 
  summarize(value=sum(value)) %>% 
  pivot_wider(., names_from = biocrust_category_coarse, values_from = value)
svec <- envfit(out, vecdat)
sdat <- svec$vectors$arrows %>% as.data.frame() %>% rownames_to_column('biocrust_category_coarse')


gdat <- as.data.frame(scores(out, display = "species")) %>% #plot features not samples
  rownames_to_column('all_categories') %>% 
  left_join(., coldat, by="all_categories")

g1a <- ggplot(gdat)+ #MDS 1 v 2
  geom_point(aes(x=NMDS1, y=NMDS2, fill=biocrust_category_coarse), shape=21, size=3)+
  geom_segment(data=sdat, mapping = aes(x=0,y=0, 
                                        xend=NMDS1, yend=NMDS2, 
                                        color=biocrust_category_coarse), 
               arrow = arrow())+
  geom_text_repel(mapping = aes(x=NMDS1,y=NMDS2,label=all_categories, color=biocrust_category_coarse), size=2.5)+
  scale_fill_manual(values = c("#F8A582","#8FC0C7","#54B06C","#696969","#DE9CDA","#9ED080","#DAC46C"), name="Biocrust")+
  scale_color_manual(values = c("#F8A582","#8FC0C7","#54B06C","#696969","#DE9CDA","#9ED080","#DAC46C"), name="Biocrust")+
  theme_bw()+
  ggtitle("NMDS Feature Embedding (MDS1 v MDS2)")
g1a
ggsave('figs/field_data.feature_embedding.png')



#coarse
cnames <- coldat %>% filter(biocrust_category_coarse!="") %>% pull(all_categories)
nmds_dat <- data[,cnames] 
rownames(nmds_dat) <- data$PlotID

out <- metaMDS(nmds_dat,
               k=3,
               wascores = T,
               weakties=T,
               try=50,
               trymax=100,
               parallel=8,
               maxit=300)

vecdat <- nmds_dat %>% 
  rownames_to_column("PlotID") %>% 
  pivot_longer(!PlotID, names_to = 'all_categories', values_to = 'value') %>% 
  left_join(., coldat, by="all_categories") %>% 
  group_by(PlotID,biocrust_category_coarse) %>% 
  summarize(value=sum(value)) %>% 
  pivot_wider(., names_from = biocrust_category_coarse, values_from = value)
svec <- envfit(out, vecdat)
sdat <- svec$vectors$arrows %>% as.data.frame() %>% rownames_to_column('biocrust_category_coarse')

gdat <- out$points %>% 
  as.data.frame() %>% 
  rownames_to_column("PlotID") %>% 
  separate_wider_delim(PlotID, delim = '_', names = c("Desert","Substrate","PlotNr"))


g1a <- ggplot(gdat)+
  geom_point(aes(x=MDS1, y=MDS2, fill=Desert), shape=21, size=3)+
  geom_segment(data=sdat, mapping = aes(x=0,y=0, 
                                        xend=NMDS1, yend=NMDS2, 
                                        color=biocrust_category_coarse), 
               arrow = arrow())+
  scale_fill_manual(values = c("#F88299","#DDB71F","#A37BA7"))+
  scale_color_manual(values = c("#F8A582","#8FC0C7","#54B06C","#696969","#DE9CDA","#9ED080","#DAC46C"),name="Biocrust")+
  theme_bw()+
  ggtitle("Coarse-Scale NMDS (MDS1 v MDS2)")

g1b <- ggplot(gdat)+
  geom_point(aes(x=MDS2, y=MDS3, fill=Desert), shape=21, size=3)+
  geom_segment(data=sdat, mapping = aes(x=0,y=0, 
                                        xend=NMDS1, yend=NMDS2, 
                                        color=biocrust_category_coarse), 
               arrow = arrow())+
  scale_fill_manual(values = c("#F88299","#DDB71F","#A37BA7"))+
  scale_color_manual(values = c("#F8A582","#8FC0C7","#54B06C","#696969","#DE9CDA","#9ED080","#DAC46C"),name="Biocrust")+
  theme_bw()+
  ggtitle("Coarse-Scale NMDS (MDS2 v MDS3)")

g2a <- ggplot(gdat)+
  geom_point(aes(x=MDS1, y=MDS2, fill=Substrate), shape=21, size=3)+
  geom_segment(data=sdat, mapping = aes(x=0,y=0, 
                                        xend=NMDS1, yend=NMDS2, 
                                        color=biocrust_category_coarse), 
               arrow = arrow())+
  scale_fill_manual(values = c("#DDB71F","#AE91E8"))+
  scale_color_manual(values = c("#F8A582","#8FC0C7","#54B06C","#696969","#DE9CDA","#9ED080","#DAC46C"),name="Biocrust")+
  theme_bw()+
  ggtitle("Coarse-Scale NMDS (MDS1 v MDS2)")

g2b <- ggplot(gdat)+
  geom_point(aes(x=MDS2, y=MDS3, fill=Substrate), shape=21, size=3)+
  geom_segment(data=sdat, mapping = aes(x=0,y=0, 
                                        xend=NMDS1, yend=NMDS2, 
                                        color=biocrust_category_coarse), 
               arrow = arrow())+
  scale_fill_manual(values = c("#DDB71F","#AE91E8"))+
  scale_color_manual(values = c("#F8A582","#8FC0C7","#54B06C","#696969","#DE9CDA","#9ED080","#DAC46C"),name="Biocrust")+
  theme_bw()+
  ggtitle("Coarse-Scale NMDS (MDS2 v MDS3)")

library(patchwork)
pdf("figs/field_data.nmds.coarse.pdf", width = 12, height = 7)
(g1a | g1b) / 
  (g2a | g2b)
dev.off()


