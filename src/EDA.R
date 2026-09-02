library(tidyverse)
library(readxl)

setwd('~/WorkForaging/Academia/Nicole/nrcs/')

#surface roughness
surf <- read_xlsx("data/SurfaceRoughnessData.xlsx")
surf <- surf %>% 
    filter(!is.na(Staff_Name_Data_Entry)) %>%  #filter so that only rows with data entered remain
    mutate(across(c("CL1RI", "CL2RI", "CL3RI", "CL4RI","CL1PR","CL2PR","CL3PR","CL4PR"),.fns=~as.numeric(.x)))
surf <- surf %>% 
  mutate(RI_mean=rowMeans(across(c(CL1RI, CL2RI, CL3RI, CL4RI)),na.rm=T),
         PR_mean=rowMeans(across(c(CL1PR,CL2PR, CL3PR, CL4PR)),na.rm=T))
surf %>% 
  summarize(across(c("CL1RI","CL2RI","CL3RI","CL4RI","RI_mean",
                     "CL1PR","CL2PR","CL3PR","CL4PR","PR_mean"), 
                   .fns=~mean(as.numeric(.x), na.rm=T), .names = "{.col}_avg"))

write_csv(surf, "data/SurfaceRoughnessData.csv")


#stability
stab <- read_xlsx('data/AgreggateStabilityData.xlsx')
stab <- stab %>% 
  filter(!is.na(Staff_Name_Data_Entry))
surf 
