library(tidyverse)
library(readxl)

setwd('~/WorkForaging/Academia/Nicole/nrcs/')

### Import data
meta <- read_xlsx('data/20260904_MetaData_PLFA.xlsx') %>%
  rename("SampleID"="Crust_Nr") %>% 
  mutate(SampleID=as.character(SampleID), 
         Biocrust_Type_Coarse=factor(Biocrust_Type_Coarse, levels=c("Light Cyanobacteria Crust","Dark Cyanobacteria Crust",
                                                                    "Cyanolichen Crust","Chlorolichen Crust","Moss Crust"))) 
data <- read_csv('data/cleaned_crust_data.csv') %>%  #run src/nrcs_plfa_wranglin.R first
  filter((SampleID %in% meta$SampleID))

### Color palettes
crust_pal_coarse <- c("#7FE067",'#54B06C','#80D0B2','#F8A582',"#D0B280")

#Panel A
tmp <- data %>% 
  select(SampleID,Protists_Biomass,Total_Fungal_Biomass,Total_Bacteria_Biomass,Undifferentiated_Biomass) %>% 
  mutate(SampleID=as.character(SampleID)) %>% 
  pivot_longer(!SampleID, names_to = "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") 

g1 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=SampleID, y=value, fill=variable), stat='identity',width=1)+
  facet_wrap(~Biocrust_Type_Coarse, nrow = 1, scales='free_x')+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299","#F8E082","#D0AC80","#949494"))+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank(),panel.grid = element_blank())+
  ylab("Total Biomass ng/g")+
  xlab("Sample")+
  ggtitle("Total Biomass Per Crust")
g1

#Panel A1
tmp <- data %>% 
  select(SampleID,Protists_Biomass,
         Saprophytes_Biomass,Arbuscular_Mycorrhizal_Fungal_Biomass,
         Actinobacteria_Biomass,Other_Gram_Pos_Biomass,Other_Gram_Neg_Biomass,Rhizobia_Biomass,
         Undifferentiated_Biomass) %>% 
  mutate(SampleID=as.character(SampleID)) %>% 
  pivot_longer(!SampleID, names_to = "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") %>% 
  mutate(variable=factor(variable, levels=c("Protists_Biomass",
                                            'Actinobacteria_Biomass','Other_Gram_Pos_Biomass','Other_Gram_Neg_Biomass','Rhizobia_Biomass',
                                            'Saprophytes_Biomass','Arbuscular_Mycorrhizal_Fungal_Biomass',
                                            'Undifferentiated_Biomass')))

g1a <- ggplot(tmp)+
  geom_bar(mapping=aes(x=SampleID, y=value, fill=variable), stat='identity',width=1)+
  facet_wrap(~Biocrust_Type_Coarse, nrow = 1, scales='free_x')+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299",
                               "#9F851E","#EECE4D","#F8E082","#FFF4C6",
                               "#D0AC80","#9C7D54",
                               "#949494"))+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank())+
  ylab("Total Biomass ng/g")+
  xlab("Sample")+
  ggtitle("Total Biomass Per Crust")
g1a


#Panel B
tmp <- data %>% #coarse
  select(SampleID,Protists_Biomass,Total_Fungal_Biomass,Total_Bacteria_Biomass,Undifferentiated_Biomass) %>%
  mutate(across(ends_with("Biomass"), as.numeric)) %>% 
  mutate(Total_Biomass=rowSums(across(ends_with("Biomass"))),
         SampleID=as.character(SampleID) ) %>% 
  mutate(Percent_Protists = Protists_Biomass/Total_Biomass, 
         Percent_Fungi= Total_Fungal_Biomass/Total_Biomass, 
         Percent_Bacteria= Total_Bacteria_Biomass/Total_Biomass, 
         Percent_Undifferentiated= Undifferentiated_Biomass/Total_Biomass) %>% 
  select(SampleID,Percent_Protists,Percent_Fungi,Percent_Bacteria,Percent_Undifferentiated) %>% 
  pivot_longer(!SampleID, names_to= "variable", values_to = 'percent') %>% 
  mutate(variable=gsub("Percent_","",variable))%>% 
  left_join(., meta, by="SampleID") %>% 
  mutate(variable=factor(variable, levels=c("Protists","Bacteria","Fungi","Undifferentiated")))

g2 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=SampleID, y=percent, fill=variable), stat='identity',width=1)+
  facet_wrap(~Biocrust_Type_Coarse, nrow = 1, scales='free_x')+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299","#F8E082","#D0AC80","#949494"))+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        panel.grid= element_blank())+
  ylab("Relative Biomass")+
  xlab("")+
  ggtitle("Relative Biomass Per Crust")
#g2

#Panel B1
tmp <- data %>%  #fine
  select(SampleID,Protists_Biomass,
         Saprophytes_Biomass,Arbuscular_Mycorrhizal_Fungal_Biomass,
         Actinobacteria_Biomass,Other_Gram_Pos_Biomass,Other_Gram_Neg_Biomass,Rhizobia_Biomass,
         Undifferentiated_Biomass) %>%
  mutate(across(ends_with("Biomass"), as.numeric)) %>% 
  mutate(Total_Biomass=rowSums(across(ends_with("Biomass"))),
         SampleID=as.character(SampleID) ) %>% 
  mutate(Percent_Protists = Protists_Biomass/Total_Biomass, 
         Percent_Saprophytes= Saprophytes_Biomass/Total_Biomass,
         Percent_Arbuscular_Mycorrhizal = Arbuscular_Mycorrhizal_Fungal_Biomass/Total_Biomass, 
         Percent_Actinobacteria = Actinobacteria_Biomass/Total_Biomass, 
         Percent_Other_Gram_Pos= Other_Gram_Pos_Biomass/Total_Biomass, 
         Percent_Other_Gram_Neg= Other_Gram_Neg_Biomass/Total_Biomass, 
         Percent_Rhizobia= Rhizobia_Biomass/Total_Biomass, 
         Percent_Undifferentiated = Undifferentiated_Biomass/Total_Biomass) %>% 
  select(SampleID,Percent_Protists,
         Percent_Saprophytes,Percent_Arbuscular_Mycorrhizal,
         Percent_Actinobacteria,Percent_Other_Gram_Pos,Percent_Other_Gram_Neg,Percent_Rhizobia,
         Percent_Undifferentiated) %>% 
  pivot_longer(!SampleID, names_to= "variable", values_to = 'percent') %>% 
  mutate(variable=gsub("Percent_","",variable))%>% 
  left_join(., meta, by="SampleID") %>% 
  mutate(variable=factor(variable, levels=c("Protists",
                                            'Actinobacteria','Other_Gram_Pos','Other_Gram_Neg','Rhizobia',
                                            'Saprophytes','Arbuscular_Mycorrhizal',
                                            'Undifferentiated')))



g2a <- ggplot(tmp)+
  geom_bar(mapping=aes(x=SampleID, y=percent, fill=variable), stat='identity',width=1)+
  facet_wrap(~Biocrust_Type_Coarse, nrow = 1, scales='free_x')+
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299",
                               "#9F851E","#EECE4D","#F8E082","#FFF4C6",
                               "#D0AC80","#9C7D54",
                               "#949494"))+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank())+
  ylab("Relative Biomass")+
  xlab("")+
  ggtitle("Relative Biomass Per Crust")
g2a




#Panel C
library(vegan)
mat <- data %>% select(Protists_Biomass, Undifferentiated_Biomass,
                       Saprophytes_Biomass,Arbuscular_Mycorrhizal_Fungal_Biomass,
                       Actinobacteria_Biomass,Other_Gram_Neg_Biomass,Other_Gram_Pos_Biomass, Rhizobia_Biomass) %>% 
  mutate(across(everything(), ~replace_na(., 0))) %>% 
  as.data.frame()


rownames(mat) <- as.character(data$SampleID)

mat <- mat %>% 
  filter(rowSums(across(everything()), na.rm = TRUE) > 0) 

X <- vegdist(mat, method='bray', na.rm = T)
out <- metaMDS(X,
               k=3,
               wascores = T,
               weakties=T,
               try=50,
               trymax=100,
               parallel=8,
               maxit=300)
gdat <- data.frame(out$points)
gdat$goodness <- goodness(out)
gdat$SampleID <- rownames(gdat)
gdat <- left_join(gdat, meta , by='SampleID')

centroids <- gdat %>% group_by(Biocrust_Type_Coarse) %>% summarize(MDS1 = mean(MDS1),MDS2 = mean(MDS2))
g3 <- ggplot(gdat)+
  geom_point(mapping=aes(x=MDS1,y=MDS2, fill=Biocrust_Type_Coarse), shape=21, stroke=0.2, size=2)+
  stat_ellipse(aes(x=MDS1, y=MDS2, group=Biocrust_Type_Coarse, color=Biocrust_Type_Coarse))+
  geom_point(data=centroids, aes(x=MDS1, y=MDS2, color=Biocrust_Type_Coarse), size=6, alpha=0.8, shape=4, stroke=2)+
  scale_color_manual(values=crust_pal_coarse)+
  scale_fill_manual(values=crust_pal_coarse)+
  theme_bw()+
  labs(fill="Coarse Biocrust Type", color="Coarse Biocrust Type")+
  ggtitle("PLFA NMDS")
g3


#Panel D
tmp <- data %>% 
  select(SampleID,
         Shannon_Diversity, Fungi_to_Bacteria_Ratio) %>% 
  mutate(SampleID=as.character(SampleID),
         LFC_Fungi_Bacteria_Ratio = log2(Fungi_to_Bacteria_Ratio)) %>% 
  pivot_longer(!c(SampleID, Fungi_to_Bacteria_Ratio), names_to= "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") %>% 
  mutate(SampleID=as.character(SampleID)) 

g4 <- ggplot(tmp)+
  geom_hline(yintercept=0, linewidth=0.1)+
  geom_bar(mapping=aes(x=SampleID, y=value, fill=Biocrust_Type_Coarse), stat='identity',width=1)+
  facet_grid(variable~Biocrust_Type_Coarse, scales='free')+
  scale_fill_manual(values=crust_pal_coarse)+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank())+
  guides(fill='none')+
  xlab("Sample")+
  ylab("")
g4

#Bonus Plots
tmp <- data %>% 
  select(SampleID,
         Percent_Saprophytes, Percent_Arbuscular_Mycorrhizal_Fungi) %>% 
  mutate(SampleID=as.character(SampleID)) %>% 
  pivot_longer(!SampleID, names_to= "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") %>% 
  mutate(variable=gsub("Percent_","",variable))

g5 <- ggplot(tmp)+
  geom_violin(mapping=aes(x=Biocrust_Type_Coarse, y=value, fill=Biocrust_Type_Coarse),alpha=0.8)+
  geom_jitter(mapping=aes(x=Biocrust_Type_Coarse,y=value, shape=Substrate, color=Substrate), 
              height=0, width=0.2, size=2,stroke=.5)+
  scale_fill_manual(values=c(crust_pal_coarse))+
  scale_color_manual(values =  c('#7397EF',"#AE8A14"))+
  scale_shape_manual(values=c(0,1))+
  facet_wrap(~variable, nrow=1, scales='free')+
  theme_bw()+
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank())+
  guides(fill = guide_legend(order = 1, title = "Coarse Biocrust Type"),shape = guide_legend(order = 2),color = guide_legend(order=2))+
  xlab("")+
  ylab("Percent Biomass")
g5


tmp <- data %>% 
  select(SampleID,
         Percent_Actinobacteria, Percent_Rhizobia, Percent_Other_Gram_Pos, Percent_Other_Gram_Neg) %>% 
  mutate(SampleID=as.character(SampleID)) %>% 
  pivot_longer(!SampleID, names_to= "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") %>% 
  mutate(variable=gsub("Percent_","",variable))

g6 <- ggplot(tmp)+
  geom_violin(mapping=aes(x=Biocrust_Type_Coarse, y=value, fill=Biocrust_Type_Coarse),alpha=0.8)+
  geom_jitter(mapping=aes(x=Biocrust_Type_Coarse,y=value,shape=Substrate, color=Substrate), 
              height=0, width=0.2, alpha=0.7, stroke=.5)+
  scale_fill_manual(values=crust_pal_coarse)+
  scale_color_manual(values = c('#7397EF',"#AE8A14"))+
  scale_shape_manual(values=c(0,1))+
  facet_wrap(~variable, nrow=1, scales='free')+
  theme_bw()+
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank())+
  guides(fill = guide_legend(order = 1, title = "Coarse Biocrust Type"),shape = guide_legend(order = 2),color = guide_legend(order=2))+
  xlab("")+
  ylab("Percent Biomass")
g6
 

##8 panels biomass by metavars

#biocrust
tmp <- data %>% 
  select(SampleID,Protists_Biomass,Total_Fungal_Biomass,Total_Bacteria_Biomass,Undifferentiated_Biomass) %>% 
  mutate(SampleID=as.character(SampleID)) %>% 
  pivot_longer(!SampleID, names_to = "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") %>% 
  group_by(Biocrust_Type_Coarse, variable) %>% 
  summarize(value=median(value, na.rm = T))

#abs
g7 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Biocrust_Type_Coarse, y=value, fill=variable), stat='identity')+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299","#F8E082","#D0AC80","#949494"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Total Biomass ng/g")+
  xlab("Biocrust")+
  ggtitle("Median Total Biomass Per Crust")
g7

#rel
tmp <- data %>% #coarse
  select(SampleID,Protists_Biomass,Total_Fungal_Biomass,Total_Bacteria_Biomass,Undifferentiated_Biomass) %>%
  mutate(across(ends_with("Biomass"), as.numeric)) %>% 
  mutate(SampleID=as.character(SampleID) ) %>% 
  pivot_longer(!SampleID, names_to= "variable", values_to = 'value') %>%
  left_join(., meta, by="SampleID") %>% 
  group_by(Biocrust_Type_Coarse, variable) %>% 
  summarize(Biomass=sum(value, na.rm = T)) %>% ungroup() %>% 
  group_by(Biocrust_Type_Coarse) %>% 
  mutate(Group_Total = sum(Biomass),
         percent = Biomass / Group_Total) %>% ungroup () %>% 
  mutate(variable=case_when(variable=="Protists_Biomass"~"Protists",
                            variable=="Total_Bacteria_Biomass"~"Bacteria",
                            variable=="Total_Fungal_Biomass"~"Fungi",
                            variable=="Undifferentiated_Biomass"~"Undifferentiated")) %>% 
  mutate(variable=factor(variable, levels=c("Protists","Bacteria","Fungi","Undifferentiated")))

g8 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Biocrust_Type_Coarse, y=percent, fill=variable), stat='identity',width=.9)+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299","#F8E082","#D0AC80","#949494"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Mean Relative Biomass %")+
  xlab("Biocrust")+
  ggtitle("Mean Relative Biomass Per Crust")
g8

#desert & substrate
tmp <- data %>% 
  select(SampleID,Protists_Biomass,Total_Fungal_Biomass,Total_Bacteria_Biomass,Undifferentiated_Biomass) %>% 
  mutate(SampleID=as.character(SampleID)) %>% 
  pivot_longer(!SampleID, names_to = "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") %>% 
  group_by(Desert, Substrate, variable) %>% 
  summarize(value=median(value, na.rm = T))

g9 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Desert, y=value, fill=variable), stat='identity',width=.9)+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299","#F8E082","#D0AC80","#949494"))+
  facet_wrap(~Substrate, nrow=1, scales='free')+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Median Total Biomass ng/g")+
  xlab("Desert")+
  ggtitle("Median Total Biomass Per Desert and Substrate")
g9

#rel
tmp <- data %>% #coarse
  select(SampleID,Protists_Biomass,Total_Fungal_Biomass,Total_Bacteria_Biomass,Undifferentiated_Biomass) %>%
  mutate(across(ends_with("Biomass"), as.numeric)) %>% 
  mutate(SampleID=as.character(SampleID) ) %>% 
  pivot_longer(!SampleID, names_to= "variable", values_to = 'value') %>%
  left_join(., meta, by="SampleID") %>% 
  group_by(Desert, Substrate, variable) %>% 
  summarize(Biomass=sum(value, na.rm = T)) %>% ungroup() %>% 
  group_by(Desert, Substrate) %>% 
  mutate(Group_Total = sum(Biomass),
         percent = Biomass / Group_Total) %>% ungroup () %>% 
  mutate(variable=case_when(variable=="Protists_Biomass"~"Protists",
                            variable=="Total_Bacteria_Biomass"~"Bacteria",
                            variable=="Total_Fungal_Biomass"~"Fungi",
                            variable=="Undifferentiated_Biomass"~"Undifferentiated")) %>% 
  mutate(variable=factor(variable, levels=c("Protists","Bacteria","Fungi","Undifferentiated")))

g10 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Desert, y=percent, fill=variable), stat='identity',width=.9)+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299","#F8E082","#D0AC80","#949494"))+
  facet_wrap(~Substrate, scales='free')+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Mean Relative Biomass %")+
  xlab("Desert")+
  ggtitle("Mean Relative Biomass Per Desert and Substrate")
g10

#desert only 
tmp <- data %>% 
  select(SampleID,Protists_Biomass,Total_Fungal_Biomass,Total_Bacteria_Biomass,Undifferentiated_Biomass) %>% 
  mutate(SampleID=as.character(SampleID)) %>% 
  pivot_longer(!SampleID, names_to = "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") %>% 
  group_by(Desert, variable) %>% 
  summarize(value=median(value, na.rm = T))

g11 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Desert, y=value, fill=variable), stat='identity',width=0.7)+
  scale_x_discrete(expand = c(.15,.15))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299","#F8E082","#D0AC80","#949494"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Median Total Biomass ng/g")+
  xlab("Desert")+
  ggtitle("Median Total Biomass Per Desert")
g11

#rel
tmp <- data %>% #coarse
  select(SampleID,Protists_Biomass,Total_Fungal_Biomass,Total_Bacteria_Biomass,Undifferentiated_Biomass) %>%
  mutate(across(ends_with("Biomass"), as.numeric)) %>% 
  mutate(SampleID=as.character(SampleID) ) %>% 
  pivot_longer(!SampleID, names_to= "variable", values_to = 'value') %>%
  left_join(., meta, by="SampleID") %>% 
  group_by(Desert, variable) %>% 
  summarize(Biomass=sum(value, na.rm = T)) %>% ungroup() %>% 
  group_by(Desert) %>% 
  mutate(Group_Total = sum(Biomass),
         percent = Biomass / Group_Total) %>% ungroup () %>% 
  mutate(variable=case_when(variable=="Protists_Biomass"~"Protists",
                            variable=="Total_Bacteria_Biomass"~"Bacteria",
                            variable=="Total_Fungal_Biomass"~"Fungi",
                            variable=="Undifferentiated_Biomass"~"Undifferentiated")) %>% 
  mutate(variable=factor(variable, levels=c("Protists","Bacteria","Fungi","Undifferentiated")))

g12 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Desert, y=percent, fill=variable), stat='identity',width=0.7)+
  scale_x_discrete(expand = c(.15,.15))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299","#F8E082","#D0AC80","#949494"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Mean Relative Biomass %")+
  xlab("Desert")+
  ggtitle("Mean Relative Biomass Per Desert")
g12

#Substrate only
tmp <- data %>% 
  select(SampleID,Protists_Biomass,Total_Fungal_Biomass,Total_Bacteria_Biomass,Undifferentiated_Biomass) %>% 
  mutate(SampleID=as.character(SampleID)) %>% 
  pivot_longer(!SampleID, names_to = "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") %>% 
  group_by(Substrate, variable) %>% 
  summarize(value=median(value, na.rm = T))

g13 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Substrate, y=value, fill=variable), stat='identity', width=0.9)+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299","#F8E082","#D0AC80","#949494"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Median Total Biomass ng/g")+
  xlab("Substrate")+
  ggtitle("Median Total Biomass Per Substrate")
g13

#rel
tmp <- data %>% #coarse
  select(SampleID,Protists_Biomass,Total_Fungal_Biomass,Total_Bacteria_Biomass,Undifferentiated_Biomass) %>%
  mutate(across(ends_with("Biomass"), as.numeric)) %>% 
  mutate(SampleID=as.character(SampleID) ) %>% 
  pivot_longer(!SampleID, names_to= "variable", values_to = 'value') %>%
  left_join(., meta, by="SampleID") %>% 
  group_by(Substrate, variable) %>% 
  summarize(Biomass=sum(value, na.rm = T)) %>% ungroup() %>% 
  group_by(Substrate) %>% 
  mutate(Group_Total = sum(Biomass),
         percent = Biomass / Group_Total) %>% ungroup () %>% 
  mutate(variable=case_when(variable=="Protists_Biomass"~"Protists",
                            variable=="Total_Bacteria_Biomass"~"Bacteria",
                            variable=="Total_Fungal_Biomass"~"Fungi",
                            variable=="Undifferentiated_Biomass"~"Undifferentiated")) %>% 
  mutate(variable=factor(variable, levels=c("Protists","Bacteria","Fungi","Undifferentiated")))

g14 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Substrate, y=percent, fill=variable), stat='identity',width=0.8)+
  scale_x_discrete(expand = c(.25,.25))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299","#F8E082","#D0AC80","#949494"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Mean Relative Biomass %")+
  xlab("Substrate")+
  ggtitle("Mean Relative Biomass Per Substrate")
g14


### 8 panels but with fine categories
#biocrust
tmp <- data %>% 
  select(SampleID,Protists_Biomass,
         Saprophytes_Biomass,Arbuscular_Mycorrhizal_Fungal_Biomass,
         Actinobacteria_Biomass,Other_Gram_Pos_Biomass,Other_Gram_Neg_Biomass,Rhizobia_Biomass,
         Undifferentiated_Biomass) %>% 
  mutate(SampleID=as.character(SampleID)) %>% 
  pivot_longer(!SampleID, names_to = "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") %>% 
  mutate(variable=factor(variable, levels=c("Protists_Biomass",
                                            'Actinobacteria_Biomass','Other_Gram_Pos_Biomass','Other_Gram_Neg_Biomass','Rhizobia_Biomass',
                                            'Saprophytes_Biomass','Arbuscular_Mycorrhizal_Fungal_Biomass',
                                            'Undifferentiated_Biomass'))) %>%  
  group_by(Biocrust_Type_Coarse, variable) %>% 
  summarize(value=median(value, na.rm = T))

#abs
g15 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Biocrust_Type_Coarse, y=value, fill=variable), stat='identity')+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299","#9F851E","#EECE4D","#F8E082","#FFF4C6", "#D0AC80","#9C7D54","#949494"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Total Biomass ng/g")+
  xlab("Biocrust")+
  ggtitle("Median Total Biomass Per Crust")
g15

#rel
tmp <- data %>% #ffine
  select(SampleID,Protists_Biomass,
         Saprophytes_Biomass,Arbuscular_Mycorrhizal_Fungal_Biomass,
         Actinobacteria_Biomass,Other_Gram_Pos_Biomass,Other_Gram_Neg_Biomass,Rhizobia_Biomass,
         Undifferentiated_Biomass) %>%
  mutate(across(ends_with("Biomass"), as.numeric)) %>% 
  mutate(SampleID=as.character(SampleID) ) %>% 
  pivot_longer(!SampleID, names_to= "variable", values_to = 'value') %>%
  left_join(., meta, by="SampleID") %>% 
  group_by(Biocrust_Type_Coarse, variable) %>% 
  summarize(Biomass=sum(value, na.rm = T)) %>% ungroup() %>% 
  group_by(Biocrust_Type_Coarse) %>% 
  mutate(Group_Total = sum(Biomass),
         percent = Biomass / Group_Total) %>% ungroup () %>% 
  mutate(variable=gsub("_Biomass","",variable)) %>% 
  mutate(variable=factor(variable, levels=c("Protists",
                                            'Actinobacteria','Other_Gram_Pos','Other_Gram_Neg','Rhizobia',
                                            'Saprophytes','Arbuscular_Mycorrhizal_Fungal',
                                            'Undifferentiated')))
  
  
  
  
g16 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Biocrust_Type_Coarse, y=percent, fill=variable), stat='identity')+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299", "#9F851E","#EECE4D","#F8E082","#FFF4C6","#D0AC80","#9C7D54","#949494"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Mean Relative Biomass %")+
  xlab("Biocrust")+
  ggtitle("Mean Relative Biomass Per Crust")
g16

#desert & substrate
tmp <- data %>% 
  select(SampleID,Protists_Biomass,
         Saprophytes_Biomass,Arbuscular_Mycorrhizal_Fungal_Biomass,
         Actinobacteria_Biomass,Other_Gram_Pos_Biomass,Other_Gram_Neg_Biomass,Rhizobia_Biomass,
         Undifferentiated_Biomass) %>% 
  mutate(SampleID=as.character(SampleID)) %>% 
  pivot_longer(!SampleID, names_to = "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") %>% 
  mutate(variable=factor(variable, levels=c("Protists_Biomass",
                                            'Actinobacteria_Biomass','Other_Gram_Pos_Biomass','Other_Gram_Neg_Biomass','Rhizobia_Biomass',
                                            'Saprophytes_Biomass','Arbuscular_Mycorrhizal_Fungal_Biomass',
                                            'Undifferentiated_Biomass'))) %>%  
  group_by(Desert, Substrate, variable) %>% 
  summarize(value=median(value, na.rm = T))

g17 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Desert, y=value, fill=variable), stat='identity')+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299", "#9F851E","#EECE4D","#F8E082","#FFF4C6", "#D0AC80","#9C7D54", "#949494"))+
  facet_wrap(~Substrate, nrow=1, scales='free')+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Median Total Biomass ng/g")+
  xlab("Desert")+
  ggtitle("Median Total Biomass Per Desert and Substrate")
g17

#rel
tmp <- data %>% #ffine
  select(SampleID,Protists_Biomass,
         Saprophytes_Biomass,Arbuscular_Mycorrhizal_Fungal_Biomass,
         Actinobacteria_Biomass,Other_Gram_Pos_Biomass,Other_Gram_Neg_Biomass,Rhizobia_Biomass,
         Undifferentiated_Biomass) %>%
  mutate(across(ends_with("Biomass"), as.numeric)) %>% 
  mutate(SampleID=as.character(SampleID) ) %>% 
  pivot_longer(!SampleID, names_to= "variable", values_to = 'value') %>%
  left_join(., meta, by="SampleID") %>% 
  group_by(Desert, Substrate, variable) %>% 
  summarize(Biomass=sum(value, na.rm = T)) %>% ungroup() %>% 
  group_by(Desert, Substrate) %>% 
  mutate(Group_Total = sum(Biomass),
         percent = Biomass / Group_Total) %>% ungroup () %>% 
  mutate(variable=gsub("_Biomass","",variable)) %>% 
  mutate(variable=factor(variable, levels=c("Protists",
                                            'Actinobacteria','Other_Gram_Pos','Other_Gram_Neg','Rhizobia',
                                            'Saprophytes','Arbuscular_Mycorrhizal_Fungal',
                                            'Undifferentiated')))

g18 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Desert, y=percent, fill=variable), stat='identity')+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299","#9F851E","#EECE4D","#F8E082","#FFF4C6", "#D0AC80","#9C7D54","#949494"))+
  facet_wrap(~Substrate, scales='free')+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Mean Relative Biomass %")+
  xlab("Desert")+
  ggtitle("Mean Relative Biomass Per Desert and Substrate")
g18

#desert only 
tmp <- data %>% 
  select(SampleID,Protists_Biomass,
         Saprophytes_Biomass,Arbuscular_Mycorrhizal_Fungal_Biomass,
         Actinobacteria_Biomass,Other_Gram_Pos_Biomass,Other_Gram_Neg_Biomass,Rhizobia_Biomass,
         Undifferentiated_Biomass) %>% 
  mutate(SampleID=as.character(SampleID)) %>% 
  pivot_longer(!SampleID, names_to = "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") %>% 
  mutate(variable=factor(variable, levels=c("Protists_Biomass",
                                            'Actinobacteria_Biomass','Other_Gram_Pos_Biomass','Other_Gram_Neg_Biomass','Rhizobia_Biomass',
                                            'Saprophytes_Biomass','Arbuscular_Mycorrhizal_Fungal_Biomass',
                                            'Undifferentiated_Biomass'))) %>% 
  group_by(Desert, variable) %>% 
  summarize(value=median(value, na.rm = T))

g19 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Desert, y=value, fill=variable), stat='identity')+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299", "#9F851E","#EECE4D","#F8E082","#FFF4C6","#D0AC80","#9C7D54",                                "#949494"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Median Total Biomass ng/g")+
  xlab("Desert")+
  ggtitle("Median Total Biomass Per Desert")
g19

#rel
tmp <- data %>% #ffine
  select(SampleID,Protists_Biomass,
         Saprophytes_Biomass,Arbuscular_Mycorrhizal_Fungal_Biomass,
         Actinobacteria_Biomass,Other_Gram_Pos_Biomass,Other_Gram_Neg_Biomass,Rhizobia_Biomass,
         Undifferentiated_Biomass) %>%
  mutate(across(ends_with("Biomass"), as.numeric)) %>% 
  mutate(SampleID=as.character(SampleID) ) %>% 
  pivot_longer(!SampleID, names_to= "variable", values_to = 'value') %>%
  left_join(., meta, by="SampleID") %>% 
  group_by(Desert, variable) %>% 
  summarize(Biomass=sum(value, na.rm = T)) %>% ungroup() %>% 
  group_by(Desert) %>% 
  mutate(Group_Total = sum(Biomass),
         percent = Biomass / Group_Total) %>% ungroup () %>% 
  mutate(variable=gsub("_Biomass","",variable)) %>% 
  mutate(variable=factor(variable, levels=c("Protists",
                                            'Actinobacteria','Other_Gram_Pos','Other_Gram_Neg','Rhizobia',
                                            'Saprophytes','Arbuscular_Mycorrhizal_Fungal',
                                            'Undifferentiated')))

g20 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Desert, y=percent, fill=variable), stat='identity')+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299","#9F851E","#EECE4D","#F8E082","#FFF4C6", "#D0AC80","#9C7D54",                                "#949494"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Mean Relative Biomass %")+
  xlab("Desert")+
  ggtitle("Mean Relative Biomass Per Desert")
g20

#Substrate only
tmp <- data %>% 
  select(SampleID,Protists_Biomass,
         Saprophytes_Biomass,Arbuscular_Mycorrhizal_Fungal_Biomass,
         Actinobacteria_Biomass,Other_Gram_Pos_Biomass,Other_Gram_Neg_Biomass,Rhizobia_Biomass,
         Undifferentiated_Biomass) %>% 
  mutate(SampleID=as.character(SampleID)) %>% 
  pivot_longer(!SampleID, names_to = "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") %>% 
  mutate(variable=factor(variable, levels=c("Protists_Biomass",
                                            'Actinobacteria_Biomass','Other_Gram_Pos_Biomass','Other_Gram_Neg_Biomass','Rhizobia_Biomass',
                                            'Saprophytes_Biomass','Arbuscular_Mycorrhizal_Fungal_Biomass',
                                            'Undifferentiated_Biomass'))) %>%  
  group_by(Substrate, variable) %>% 
  summarize(value=median(value, na.rm = T))

g21 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Substrate, y=value, fill=variable), stat='identity')+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299", "#9F851E","#EECE4D","#F8E082","#FFF4C6", "#D0AC80","#9C7D54",                                "#949494"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Median Total Biomass ng/g")+
  xlab("Substrate")+
  ggtitle("Median Total Biomass Per Substrate")
g21

#rel
tmp <- data %>% #ffine
  select(SampleID,Protists_Biomass,
         Saprophytes_Biomass,Arbuscular_Mycorrhizal_Fungal_Biomass,
         Actinobacteria_Biomass,Other_Gram_Pos_Biomass,Other_Gram_Neg_Biomass,Rhizobia_Biomass,
         Undifferentiated_Biomass) %>%
  mutate(across(ends_with("Biomass"), as.numeric)) %>% 
  mutate(SampleID=as.character(SampleID) ) %>% 
  pivot_longer(!SampleID, names_to= "variable", values_to = 'value') %>%
  left_join(., meta, by="SampleID") %>% 
  group_by(Substrate, variable) %>% 
  summarize(Biomass=sum(value, na.rm = T)) %>% ungroup() %>% 
  group_by(Substrate) %>% 
  mutate(Group_Total = sum(Biomass),
         percent = Biomass / Group_Total) %>% ungroup () %>% 
  mutate(variable=gsub("_Biomass","",variable)) %>% 
  mutate(variable=factor(variable, levels=c("Protists",
                                            'Actinobacteria','Other_Gram_Pos','Other_Gram_Neg','Rhizobia',
                                            'Saprophytes','Arbuscular_Mycorrhizal_Fungal',
                                            'Undifferentiated')))

g22 <- ggplot(tmp)+
  geom_bar(mapping=aes(x=Substrate, y=percent, fill=variable), stat='identity')+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#F88299", "#9F851E","#EECE4D","#F8E082","#FFF4C6","#D0AC80","#9C7D54", "#949494"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, vjust=.45),
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  ylab("Mean Relative Biomass %")+
  xlab("Substrate")+
  ggtitle("Mean Relative Biomass Per Substrate")
g22


## violin by metavar
tmp <- data %>%  #fine
  select(SampleID,Protists_Biomass,
         Saprophytes_Biomass,Arbuscular_Mycorrhizal_Fungal_Biomass,
         Actinobacteria_Biomass,Other_Gram_Pos_Biomass,Other_Gram_Neg_Biomass,Rhizobia_Biomass,
         Undifferentiated_Biomass) %>%
  mutate(across(ends_with("Biomass"), as.numeric)) %>% 
  mutate(Total_Biomass=rowSums(across(ends_with("Biomass"))),
         SampleID=as.character(SampleID) ) %>% 
  mutate(Percent_Protists = Protists_Biomass/Total_Biomass, 
         Percent_Saprophytes= Saprophytes_Biomass/Total_Biomass,
         Percent_Arbuscular_Mycorrhizal = Arbuscular_Mycorrhizal_Fungal_Biomass/Total_Biomass, 
         Percent_Actinobacteria = Actinobacteria_Biomass/Total_Biomass, 
         Percent_Other_Gram_Pos= Other_Gram_Pos_Biomass/Total_Biomass, 
         Percent_Other_Gram_Neg= Other_Gram_Neg_Biomass/Total_Biomass, 
         Percent_Rhizobia= Rhizobia_Biomass/Total_Biomass, 
         Percent_Undifferentiated = Undifferentiated_Biomass/Total_Biomass) %>% 
  select(SampleID,Percent_Protists,
         Percent_Saprophytes,Percent_Arbuscular_Mycorrhizal,
         Percent_Actinobacteria,Percent_Other_Gram_Pos,Percent_Other_Gram_Neg,Percent_Rhizobia,
         Percent_Undifferentiated) %>% 
  pivot_longer(!SampleID, names_to= "variable", values_to = 'percent') %>% 
  mutate(variable=gsub("Percent_","",variable))%>% 
  left_join(., meta, by="SampleID") %>% 
  mutate(variable=factor(variable, levels=c("Protists",
                                            'Actinobacteria','Other_Gram_Pos','Other_Gram_Neg','Rhizobia',
                                            'Saprophytes','Arbuscular_Mycorrhizal',
                                            'Undifferentiated')))

g23 <- ggplot(tmp)+
  geom_violin(mapping=aes(x=Biocrust_Type_Coarse, y=percent, fill=Biocrust_Type_Coarse))+
  geom_jitter(mapping=aes(x=Biocrust_Type_Coarse,y=percent, color=Substrate, shape=Substrate), 
              height=0, width=0.25, alpha=0.7,stroke=.5)+
  scale_fill_manual(values=crust_pal_coarse, name="Coarse Biocrust Type")+
  scale_color_manual(values =  c('#7397EF',"#AE8A14"))+
  scale_shape_manual(values=c(0,1))+
  facet_grid(variable~Desert, scales='free')+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  xlab("")+
  ylab("Percent Biomass")
g23


g24 <- ggplot(tmp)+
  geom_violin(mapping=aes(x=Biocrust_Type_Coarse, y=percent, fill=Biocrust_Type_Coarse))+
  geom_jitter(mapping=aes(x=Biocrust_Type_Coarse,y=percent, color=Substrate, shape=Substrate), 
              height=0, width=0.2, alpha=0.2, stroke=.5)+
  scale_fill_manual(values=crust_pal_coarse, name="Coarse Biocrust Type")+
  scale_color_manual(values =  c('#7397EF',"#AE8A14"))+
  scale_shape_manual(values=c(0,1))+
  facet_grid(variable~Substrate, scales = 'free')+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  xlab("")+
  ylab("Percent Biomass")
g24

g25 <- ggplot(tmp)+
  geom_violin(mapping=aes(x=Biocrust_Type_Coarse, y=percent, fill=Biocrust_Type_Coarse))+
  geom_jitter(mapping=aes(x=Biocrust_Type_Coarse,y=percent, color=Substrate, shape=Substrate), 
              height=0, width=0.2, alpha=0.3, stroke=.5)+
  scale_fill_manual(values=crust_pal_coarse, name="Coarse Biocrust Type")+
  scale_color_manual(values =  c('#7397EF',"#AE8A14"))+
  scale_shape_manual(values=c(0,1))+
  facet_grid(variable~Desert+Substrate, scales='free')+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  xlab("")+
  ylab("Percent Biomass")
g25

## mean F/B & Shannon per metavar
tmp <- data %>% 
  select(SampleID,
         Shannon_Diversity, Fungi_to_Bacteria_Ratio) %>% 
  mutate(SampleID=as.character(SampleID),
         LFC_Fungi_Bacteria_Ratio = log2(Fungi_to_Bacteria_Ratio)) %>% 
  pivot_longer(!c(SampleID, Fungi_to_Bacteria_Ratio), names_to= "variable", values_to = 'value') %>% 
  left_join(., meta, by="SampleID") %>% 
  mutate(SampleID=as.character(SampleID)) 

g26 <- ggplot(tmp)+
  geom_violin(mapping=aes(x=Biocrust_Type_Coarse, y=value, fill=Biocrust_Type_Coarse))+
  geom_jitter(mapping=aes(x=Biocrust_Type_Coarse, y=value, color=Substrate, shape=Substrate), 
              height = 0, width=0.2, alpha=0.5, stroke=.5)+
  facet_grid(variable~Desert, scales='free')+
  scale_fill_manual(values=crust_pal_coarse, name="Coarse Biocrust Type")+
  scale_color_manual(values =  c('#7397EF',"#AE8A14"))+
  scale_shape_manual(values=c(0,1))+
  scale_y_continuous(expand=c(0,0))+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  xlab("")+
  ylab("")
g26


g27 <- ggplot(tmp)+
  geom_violin(mapping=aes(x=Biocrust_Type_Coarse, y=value, fill=Biocrust_Type_Coarse))+
  geom_jitter(mapping=aes(x=Biocrust_Type_Coarse, y=value, color=Substrate, shape=Substrate), 
              height = 0, width=0.2, alpha=0.5, stroke=.5)+
  facet_grid(variable~Substrate, scales='free')+
  scale_fill_manual(values=crust_pal_coarse, name="Coarse Biocrust Type")+
  scale_color_manual(values =  c('#7397EF',"#AE8A14"))+
  scale_shape_manual(values=c(0,1))+
  scale_y_continuous(expand=c(0,0))+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  xlab("")+
  ylab("")
g27

g28 <- ggplot(tmp)+
  geom_violin(mapping=aes(x=Biocrust_Type_Coarse, y=value, fill=Biocrust_Type_Coarse))+
  geom_jitter(mapping=aes(x=Biocrust_Type_Coarse, y=value, color=Substrate, shape=Substrate), 
              height = 0, width=0.2, alpha=0.5, stroke=.5)+  
  facet_grid(variable~Desert+Substrate, scales='free')+
  scale_fill_manual(values=crust_pal_coarse, name="Coarse Biocrust Type")+
  scale_color_manual(values =  c('#7397EF',"#AE8A14"))+
  scale_shape_manual(values=c(0,1))+
  scale_y_continuous(expand=c(0,0))+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank())+
  xlab("")+
  ylab("")
g28


#put it all together in a report pdf
library(patchwork)
pdf("figs/plfa_eda.v1.2.pdf", width=11, height=11)

(g1) /
  (g2)

(g1a) /
  (g2a)

(g3) /
  (g4)

(g5) /
  (g6)

(g7 | g8) /
  (g9 | g10) /
  (g11 | g12) /
  (g13 | g14)

(g15 | g16) /
  (g17 | g18) /
  (g19 | g20) /
  (g21 | g22)

(g23)

(g24)

(g25)


(g26) /
  (g27) /
  (g28)

dev.off()




