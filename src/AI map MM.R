# AI map for moss microbiome grant
# run Map_arid_index.Rmd to line 159 first

# start with these
# data frame with lat, long, and AI as columns
aridity_index_df <- as.data.frame(aridity_index_matrix) 
# make new column for log-normalized AI to make it relative aridity
aridity_index_df$layer_relAI = log10(aridity_index_df$layer + 1)

##obtain shapefiles for states
install.packages("tigris")
library(tigris)
states <- states(cb = TRUE)  # 'cb = TRUE' gives simplified geometries
ca_geom <- states[states$NAME == "California", ]
nv_geom <- states[states$NAME == "Nevada", ]
az_geom <- states[states$NAME == "Arizona", ]
nm_geom <- states[states$NAME == "New Mexico", ]
ut_geom <- states[states$NAME == "Utah", ]

## set colors

# Define the number of colors you want in the palette
num_colors <- 10000

# Generate the palette from white to red
palette <- colorRampPalette(c("beige", "forestgreen", "darkgreen"))(num_colors)

## make plot
p <- ggplot() +
  geom_raster(data = aridity_index_df,
              aes(y = y, x = x, fill = layer)) +
  scale_fill_gradientn("Relative aridity", colours = palette) +
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_blank(),
        panel.grid.major = element_line(linetype = 2, 
                                        size = 0.5,
                                        colour = 'lightblue'),
        panel.grid.minor = element_blank(),
        # Set background color to blue
        panel.background = element_rect(fill = "lightblue")) + 
  geom_sf(data = ca_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = nv_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = az_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  #geom_sf(data = nm_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  geom_sf(data = ut_geom, aes(), alpha = 0, colour = "black", linewidth = 0.6) +
  coord_sf(ylim = c(31, 37.5),
           xlim = c(-124, -111)) + 
  guides(fill = guide_colourbar(reverse = TRUE, title = "Aridity Index",
                                title.position = "top",
                                title.theme = element_text(size = 12)),
         colour = guide_legend(reverse = TRUE, title = "Aridity Index", title.position = "top")) 

p

## Plot map with compass, scale, and metadata

# Import excel sheet with X and Y coordinates for Lat and Long for sites to plot coordinates. 
# use ggspatial to add a compass and scalebar.

meta <- read_csv("./meta_MM.csv") #

meta$parent_mat <- factor(meta$parent_mat, levels = c("Both", "Gypsum","Granite"))


p +
  ggspatial::annotation_scale(
    location = "bl",
    unit="km",
    bar_cols = c("black", "white")
    #text_family = "ArcherPro Book" #removed because ggplot did not recognize
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_orienteering(
      fill = c("black", "white"),
      line_col = "black",
    )
  ) + 
  geom_point(data=meta, aes(x=long, y=lat, color=parent_mat)) +
  scale_color_manual(values= c("purple","orange","hotpink")) 

####



