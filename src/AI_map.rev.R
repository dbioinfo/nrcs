library(tidyverse)
library(raster)
library(USA.state.boundaries)
library(tigris)
library(terra)
library(ggspatial)
library(sf)
library(ggnewscale)
library(sbtools)

setwd('~/WorkForaging/Academia/Nicole/nrcs/')

#optional download files
if (!file.exists('data/ppt.nc')) {
  download.file(url = 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_ppt_2023.nc',
                destfile = 'data/ppt.nc')
}
if (!file.exists('data/pet.nc')) {
  download.file(url = 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_pet_2019.nc',
                destfile = 'data/pet.nc')
}
if (!file.exists('data/USGS_SGMC_Shapefiles.zip')){
  id_sgmc <- '5888bf4fe4b05ccb964bab9d'
  item_file_download(sb_id = id_sgmc, names = "USGS_SGMC_Shapefiles.zip", destinations = "data/USGS_SGMC_Shapefiles.zip" )
  item_file_download(sb_id = id_sgmc, names = "USGS_SGMC_Tables_CSV.zip", destinations = "data/USGS_SGMC_Tables_CSV.zip" )
  
}



# Read Precipitation
ppt <- stack(x = 'data/ppt.nc')

# Read Evapotranspiration
pet <- stack(x = 'data/pet.nc')

# Read geology
lith <- st_read("data/USGS_SGMC_Tables_Shapefiles/SGMC_Geology.shp")
ltab <- read_csv("data/USGS_SGMC_Tables_CSV/SGMC_Lithology.csv")


# Read metadata
meta <- read_csv("data/meta_MM.csv") 
meta$parent_mat <- factor(meta$parent_mat, levels = c("Gypsum","Granite","Both"))

# Mean Precipitation
ppt_mean_large <- calc(ppt, # RasterStack object
                 fun = mean, # Function to apply across the layers
                 na.rm = TRUE)

# Mean Evapotranspiration
pet_mean_large <- calc(pet,
                 fun = mean, 
                 na.rm = TRUE)

# Cut off all values outside California ## 8.20.25 changed to incorporate larger area
ext <- extent(c(xmin = -130, xmax = -95, 
                ymin = 30, ymax = 42))
ppt_mean <- crop(x = ppt_mean_large, 
                     y = ext)
pet_mean <- crop(x = pet_mean_large, 
                     y = ext)

# Calculate aridity index 
# Precipitation (ppt) / Evapotranspiration (pet)
aridity_index <- overlay(x = ppt_mean, # Raster object 1
                         y = pet_mean, # Raster object 2
                         fun = function(x, y){return(x / y)}) # Function to apply

#--- Convert raster to a matrix ---#
aridity_index_matrix <- rasterToPoints(aridity_index)


# prepare gypsum and granite layers
igyp <- ltab %>% filter(LITH4=="Gypsum") %>% pull(UNIT_LINK)
igran <- ltab %>% filter(LITH4=="Granite") %>% pull(UNIT_LINK)

lith_gyp <- lith %>% filter(UNIT_LINK %in% igyp) %>% mutate(Substrate="Gypsum")
lith_gran <- lith %>% filter(UNIT_LINK %in% igran) %>% mutate(Substrate="Granite")

ilith <- rbind(lith_gran, lith_gyp)
ilith <-  st_crop(x=ilith, y=c(xmin = -125, xmax =-109, 
                                     ymin =  31, ymax = 39))

#gypworld kml
gyp1 <- st_read("data/GypWorld_1.kml")
gyp2 <- st_read("data/GypWorld_2.kml")

gypworld <- rbind(gyp1, gyp2)

#--- Convert to the matrix to a dataframe ---#
aridity_index_df <- as.data.frame(aridity_index_matrix)
aridity_index_df$layer_AI = log10(aridity_index_df$layer + 1)

#--- Set up some map properties
#state lines
states <- states(cb = TRUE)  # 'cb = TRUE' gives simplified geometries
ca_geom <- states[states$NAME == "California", ]
nv_geom <- states[states$NAME == "Nevada", ]
az_geom <- states[states$NAME == "Arizona", ]
nm_geom <- states[states$NAME == "New Mexico", ]
ut_geom <- states[states$NAME == "Utah", ]
co_geom <- states[states$NAME == "Colorado",]
tx_geom <- states[states$NAME == "Texas",]
ka_geom <- states[states$NAME == "Kansas",]
wy_geom <- states[states$NAME == "Wyoming",]
ne_geom <- states[states$NAME == "Nebraska",]



#color gradient
num_colors <- 10000 #how many discrete levels #5
#palette <- colorRampPalette(c("beige","#B8E17A","#93CC3E","#51A951", "#0A8F0A","#006400"))(num_colors)
palette <- colorRampPalette(c("#84332E","#F8AF82","beige","#93CC3E","#006400"))(num_colors)
palette <- colorRampPalette(c("#84332E","#F8AF82","beige","#93CC3E"))(num_colors)
palette <- c("#FF2429","#FF933F","#FED15D","#FCFFB3","#C0C0C0")
substrate_pal <- c('#7397EF',"#AE8A14")

##add in AI cats 
#regions are categorized as hyperarid (P/ET < 0.05), arid (between 0.05 and 0.20), 
#semiarid (between 0.20 and 0.50), and subhumid (between 0.50 and 0.75). 
aridity_index_df <- aridity_index_df %>% 
                      mutate(AI_cat = case_when(layer <= 0.05 ~ "Hyperarid",
                                      (layer>=0.05)&(layer<0.20) ~ "Arid",
                                      (layer>=0.20)&(layer<0.5) ~"Semiarid",
                                      (layer>=0.5)&(layer<0.75) ~ "Subhumid", 
                                      layer >= 0.75 ~ "Humid") ) %>% 
                      mutate(AI_cat = factor(AI_cat, levels=c("Hyperarid", "Arid",
                                                              "Semiarid", "Subhumid",
                                                              "Humid")))

#assemble background plot
p <- ggplot() +
  geom_raster(data = aridity_index_df,
              aes(y = y, x = x, fill = AI_cat)) +
  scale_fill_manual(values = palette)+  #, limits=c(0,2), oob = scales::squish) + #also manual/gradientn
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_blank(),
        panel.grid.major = element_line(linetype = 2, 
                                        linewidth = 0.5,
                                        colour = 'lightblue'),
        panel.grid.minor = element_blank(),
        # Set background color to blue
        panel.background = element_rect(fill = "lightblue")) + 
  geom_sf(data = ca_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = nv_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = az_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = nm_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = ut_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = co_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = tx_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = ka_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = wy_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = ne_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = lith_gyp, aes(), alpha=.8, color="#AE8A14", linewidth=0.2)+
  geom_sf(data = lith_gran, aes(), alpha=.8, color='#7397EF', linewidth=0.2)+
  coord_sf(ylim = c(31, 41.5),
           xlim = c(-124.5, -97))
p


# add in annotations
p <- p +
  ggspatial::annotation_scale(
    location = "bl",
    #plot_unit="km",
    bar_cols = c("black", "white")
    #text_family = "ArcherPro Book" #removed because ggplot did not recognize
  ) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_orienteering(
      fill = c("black", "white"),
      line_col = "black",
    )
  ) + 
  geom_point(data=meta, aes(x=long, y=lat, shape=parent_mat), size=4, stroke=1.2) +
  scale_shape_manual(values=c(3,4,8))+
  guides(fill=guide_legend(order=1),
         shape=guide_legend(order=2))
  #guides(shape="none")+
  #scale_color_manual(values= c("gold","cornflowerblue","forestgreen"))
p
ggsave("figs/AI_site_map_category.png", p)


#crop that 

#assemble background plot
p <- ggplot() +
  geom_raster(data = aridity_index_df,
              aes(y = y, x = x, fill = AI_cat)) +
  scale_fill_manual(values = palette)+  #, limits=c(0,2), oob = scales::squish) + #also manual/gradientn
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_blank(),
        panel.grid.major = element_line(linetype = 2, 
                                        linewidth = 0.5,
                                        colour = 'lightblue'),
        panel.grid.minor = element_blank(),
        # Set background color to blue
        panel.background = element_rect(fill = "lightblue")) + 
  geom_sf(data = ca_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = nv_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = az_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = nm_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = ut_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = co_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = tx_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = ka_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = wy_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = ne_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = gyp1, aes(), alpha=0, color="purple", linewidth=0.6)+
  geom_sf(data = gyp2, aes(), alpha=0, color="purple", linewidth=0.6)+
  new_scale_fill()+
  geom_sf(data = ilith, aes(fill=Substrate), alpha=.8, color="grey25", linewidth=0.03)+
  scale_fill_manual(values = c("#1B378D","#2FBA21"))+
  coord_sf(ylim = c(31, 39),
           xlim = c(-123, -109))+
  ggspatial::annotation_scale(
    location = "bl",
    #plot_unit="km",
    bar_cols = c("black", "white")
    #text_family = "ArcherPro Book" #removed because ggplot did not recognize
  ) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_orienteering(
      fill = c("black", "white"),
      line_col = "black",
    )
  ) + 
  geom_point(data=meta, aes(x=long, y=lat, shape=parent_mat), size=4, stroke=1.2) +
  scale_shape_manual(values=c(3,4,8))+
  guides(fill=guide_legend(order=1),
         shape=guide_legend(order=3))
p
ggsave("figs/AI_site_map_category_cropped.substrate.png", p,dpi=500)



#export granite and gypsum as kml
#export AI index as kml
st_write(ilith, "data/lith_sw_crop.kml", driver = "KML", )
