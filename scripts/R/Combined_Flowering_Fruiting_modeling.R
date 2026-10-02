#Script 7 
#For comparing/exploring Flowering and fruiting BART models and Graphing outputs used in publication
#PWM 8/24/2026

rm(list=ls())  # Clears memory of all objects -- useful for debugging! But doesn't kill packages.



library("tidyverse")
library("embarcadero")
library("raster")
library("sf")
library("cowplot")
library("ggplot2")
library("dplyr")
library(purrr)
library(broom)
library(RColorBrewer)
library(terra)
library(stringr)
library(ggnewscale)



states <- read_sf(dsn = "data/spatial/10m_cultural/ne_10m_admin_1_states_provinces", lay= "ne_10m_admin_1_states_provinces")
coast <- read_sf("data/spatial/10m_physical/ne_10m_coastline", "ne_10m_coastline")
range <- read_sf("data/yucca/Jotr_SDM2023_range.shp")

view(range)

#fruit model cut off Cutoff =  0.2622647

flow <- read.csv("output/fruiting_obs_climate_subsp.csv") # flowering/fruiting, gridded and annualized
flow2 <- flow %>% filter(!(year==2018.5 & flr==TRUE), year>=2008) %>% mutate(year=floor(year)) # drop the late-flowering anomaly

yubr <- filter(flow2, type=="YUBR")
glimpse(yubr) # 1,840 observations for Y brevifolia
yuja <- filter(flow2, type=="YUJA")
glimpse(yuja) # 1,152 for Y jaegeriana

#flower 
flowerflow <- read.csv("output/flowering_obs_climate_subsp.csv")
flowerflow2 <- flowerflow %>% filter(!(year==2018.5 & flr==TRUE), year>=2008) %>% mutate(year=floor(year)) # drop the late-flowering anomaly

floweryubr <- filter(flowerflow2, type=="YUBR")
glimpse(yubr) # 1,840 observations for Y brevifolia
floweryuja <- filter(flowerflow2, type=="YUJA")
glimpse(yuja) # 1,152 for Y jaegeriana


jotr.fruit.mod <- read_rds(file="output/BART/bart.fruitNTO.model.Jotr.rds")
jotr.flower.mod <-  read_rds(file="output/BART/flowerNTO.bart.model.Jotr.rds")

summary(jotr.fruit.mod) #cutoff 0.4620259  auc 0.7512791 
summary(jotr.flower.mod) #cutoff 0.2007121  auc 0.8017465    


####_Map of Observations used to Generate Figure 2 


flowerflow_ag <- flowerflow %>%
  group_by(lon, lat) %>%
  summarise(total_obs = sum(total_obs, na.rm = TRUE)) %>%
  ungroup()



library(ggplot2)
library(sf)

#validation points to add on top
#vobs.all <- read.csv("data/Validation data/allvalidationdata.csv")
field.vobs
#have to rasterize these too actually
prism_temp_rast <- raster("data/PRISM/annual/ppt_Mojave_2010Q1.bil")

vobs_raster <- rasterize(
  field.vobs[, c("lon", "lat")],
  prism_temp_rast,
  fun = "count",
  background = 0
)

occupied_tiles <- rasterToPoints(vobs_raster)

occupied_tiles <- as.data.frame(occupied_tiles)
names(occupied_tiles) <- c("lon", "lat", "count")

occupied_tiles <- subset(occupied_tiles, count > 0)

occupied_tiles <- occupied_tiles[
  !rownames(occupied_tiles) %in% c("16167", "15823", "15495", "14989", "14820", "14822","17007", "16659"),]

p <- ggplot() + 
  
  geom_sf(data = coast, fill = alpha("blue", 0.4), color = "black", size = 0.3) +
  geom_sf(data = states, fill = alpha("#f5f5f5", 0.6), color = "black", size = 0.7) +
  geom_sf(data = range, fill = alpha("#8B4513", 0.4), color = "transparent", size = 0.1) +
  
  geom_tile(
    data = flowerflow_ag,
    aes(x = lon, y = lat, fill = total_obs),
    color = NA,
    alpha = 1
  ) +
  
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log10",
    begin = .3,
    breaks = c(1, 10, 100, 500),
    labels = c("1", "10", "100", "500"),
    name = "iNaturalist Observations",
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(size = 12, family = "sans"),
      label.position = "bottom",
      label.theme = element_text(size = 11, family = "sans"),
      barwidth = unit(4, "cm"),
      barheight = unit(0.4, "cm")
    )
  ) +
  
  annotate("text", x = -113.5, y = 37.7, label = "UT", size = 5.5, fontface = "bold") +
  annotate("text", x = -113.7, y = 35.5, label = "AZ", size = 5.5, fontface = "bold") +
  annotate("text", x = -118.5, y = 37.0, label = "CA", size = 5.5, fontface = "bold") +
  annotate("text", x = -116.0, y = 37.7, label = "NV", size = 5.5, fontface = "bold") +
  
  coord_sf(
    xlim = c(-119, -112.7),
    ylim = c(33.6, 38),
    expand = FALSE
  ) +
  
  geom_tile(
    data = occupied_tiles,
    aes(x = lon, y = lat),
    fill = "white",
    alpha = .3,
    color = "black",
    linewidth = .4,     
    width = res(prism_temp_rast)[1],
    height = res(prism_temp_rast)[2]
  ) +
  
  geom_point(
    data = data.frame(lon = NA, lat = NA, validation = "Field Validation Sites"),
    aes(x = lon, y = lat, shape = validation),
    color = "black",
    size = 4
  ) +
  
  scale_shape_manual(
    name = "Validation sites",  # now the visible TITLE, on top
    values = c("Validation sites" = 0),
    labels = "",                 # blank out the label row underneath
    guide = guide_legend(
      direction = "horizontal",
      title.position = "top",
      title.theme = element_text(size = 12, family = "sans"),  # matches colorbar title size
      label.position = "bottom",
      override.aes = list(
        shape = 0,
        size = 5,
        color = "black",
        stroke = 1.8   # bolder outline on the legend key itself
      )
    )
  ) +
  
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    text = element_text(size = 12, family = "sans"),
    
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(size = 14),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.box.just = "top",     # both legends now anchor from the title row down
    legend.spacing.x = unit(0.6, "cm"),
    
    legend.title = element_text(size = 12, hjust = 0.5),
    legend.text = element_text(size = 11),
    
    legend.key.width = unit(0.6, "cm"),
    legend.key.height = unit(0.3, "cm"),
    
    panel.grid = element_blank()
  )

print(p)

##Bar plot for panel 2
flow2_summary_total <- flowerflow %>%
  group_by(year) %>%
  summarise(total_obs = sum(total_obs, na.rm = TRUE))

flow2_summary_total <- flow2_summary_total %>% 
  filter(year >= 2008) %>% 
  mutate(year = floor(year)) %>%
  group_by(year) %>%
  summarise(total_obs = sum(total_obs), .groups = "drop")

p_bar_total <- ggplot(flow2_summary_total,
                      aes(x = factor(year),
                          y = total_obs,
                          fill = total_obs)) +
  geom_col(color = "black", linewidth = 0.4) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log10",
    begin = 0,
    breaks = c(1, 10, 100, 500)
  ) +
  labs(
    x = "Year",
    y = "Total Observations",
    fill = "Observations"
  ) +
  guides(fill = "none")+
  theme( text = element_text(size = 12, family = "sans", angle = 45, hjust = 1), 
         plot.title = element_text(size = 14, hjust = 0.5), panel.grid = element_blank(), 
         axis.title.x = element_text(angle = 0, hjust = 0.5), axis.title.y = element_text(angle = 90, hjust = 0.5),
         axis.text.y = element_text(angle = 0),   panel.background = element_rect(fill = "white", color = NA),
         plot.background  = element_rect(fill = "white", color = NA),
         
         )

print(p_bar_total)

# Okay now put them together
# Okay now put them together

# Strip the legend off the map plot
p_map_only <- p + theme(legend.position = "none",
                        plot.margin = margin(0, 0, 0, 0))

# Extract the legend as its own grob (robust across cowplot versions)
map_legend <- tryCatch(
  cowplot::get_legend(p),
  error = function(e) NULL
)
if (is.null(map_legend)) {
  map_legend <- cowplot::get_plot_component(p, "guide-box", return_all = TRUE)[[1]]
}

# Build a small custom "legend" for Field Validation Sites: black square + label
validation_legend_plot <- ggplot() +
  geom_point(aes(x = 1, y = 1), shape = 22, size = 5, fill = "white", color = "black", stroke = 2) +
  annotate("text", x = 1.3, y = 1, label = "Field Validation Sites",
           hjust = 0, size = 4) +
  xlim(0.8, 3) +
  ylim(0.5, 1.5) +
  theme_void()

# Combine map legend + validation legend side by side
legend_row <- plot_grid(
  map_legend,
  validation_legend_plot,
  ncol = 2,
  rel_widths = c(1, 1)   # adjust ratio if one side needs more/less room
)

# Give the bar chart a bit of left margin so its y-axis doesn't collide with label "B"
p_bar_total <- p_bar_total + theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 15))

# Set explicit sizes: map large, legend row just tall enough, bar chart smaller
map_height_in    <- 6      # big map
legend_height_in <- 0.6    # adjust to taste
bar_height_in    <- 2.5    # bar chart shorter than the map
total_width       <- 8     # overall width in inches -- adjust to your map's aspect ratio

# Rebuild the layout with three rows: map / legend row / bar chart
combined_plot <- plot_grid(
  p_map_only,
  legend_row,
  p_bar_total,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(map_height_in, legend_height_in, bar_height_in),
  labels = c("A", "", "B"),
  label_size = 16,
  label_fontface = "bold",
  label_x = 0.008,   # nudge label left, off the panel
  hjust = 0
)

total_height <- map_height_in + legend_height_in + bar_height_in

ggsave("C:/Users/13073/Desktop/Masters/Thesis/Jotr_phenology-main/Jotr_phenology-main/scripts/output/plots/Map_Obsbyyear.pdf",
       combined_plot, width = total_width, height = total_height, dpi = 1200)
#______________________________
#making combined variable predictor selection plot  
varflowerNTO <- readRDS("output/BART/bart.flowerNTO.varimp.Jotr.rds")
varflower <- readRDS("output/BART/bart.flower.varimp.Jotr.rds")
varfruitNTO <- readRDS("output/BART/bart.fruitNTO.varimp.Jotr.rds")
varfruit <- readRDS("output/BART/bart.fruit.varimp.Jotr.rds")


#1
levels(varflowerNTO$data$names)

varflowerNTO$data <- varflowerNTO$data |> mutate(trees = factor(trees, c(10,20,50,100,150,200)))

levels(varflowerNTO$data$names) <- c("PPT[Y1-2]", "Min*Temp[Y0-1]", "PPT[Y0-1]", "PPT[Y0Q3]", "Min*VPD[Y0Q3]", "Min*Temp[Y0Q4]", "Min*VPD[Y0Q3]", "Max*VPD[Y0-1]", "PPT[Y0Q1]", 
                                     "Min*VPD[Y0]", "PPT[Y2]", "Min*Temp[Y0Q4]", "Min*VPD[Y0-1]", "Max*VPD[Y0Q3]", "Min*VPD[Y0Q4]", "Max*Temp[Y0Q4]", "PPT[Y0Q4]", "Max*VPD[Y0]", 
                                     "Max*Temp[Y0-1]", "Max*Temp[Y0Q1]", "Max*VPD[Y0Q1]", "Min*Temp[Y0Q3]", "Min*Temp[Y0]", "PPT[Y0Q4]", "Min*VPD[Y0Q1]", "Max*VPD[Y0Q4]", "Max*VPD[Y0Q4]", 
                                     "PPT[Y0]", "Min*Temp[Y0Q1]", "Max*Temp[Y0Q3]", "Max*Temp[Y0]", "Max*Temp[Y0Q4]")

varflowerNTO$labels$group <- "Trees"
varflowerNTO$labels$colour <- "Trees"

label_parse <- function(breaks){ parse(text=breaks) } # need this, for reasons
flowerplotnto<- varflowerNTO + scale_x_discrete(label=label_parse) + theme(legend.position=c(0.8, 0.7), axis.text=element_text(size=13), legend.text=element_text(size=12), legend.title=element_text(size=13))

#2
levels(varflower$data$names)

varflower$data <- varflower$data |> mutate(trees = factor(trees, c(10,20,50,100,150,200)))

levels(varflower$data$names) <- c("Total*Obs", "PPT[Y1-2]", "Min*Temp[Y0-1]", "PPT[Y0-1]", "Min*VPD[Y0Q3]", "Min*Temp[Y0Q4]", "PPT[Y0Q4]", "PPT[Y0Q3]", "PPT[Y0Q1]",
                                  "PPT[Y2]", "Min*VPD[Y0-1]", "Min*VPD[Y0Q4]", "Max*VPD[Y0-1]", "Min*VPD[Y0]", "Min*VPD[Y0Q4]", "Min*Temp[Y0Q4]", "Max*Temp[Y0Q1]", "Max*Temp[Y0Q4]",
                                  "Min*Temp[Y0]", "Min*VPD[Y0Q1]", "Max*Temp[Y0-1]", "PPT[Y0Q4]", "Max*VPD[Y0Q4]", "PPT[Y0]", "Max*VPD[Y0Q1]", "Min*Temp[Y0Q3]", "Max*VPD[Y0Q4]",
                                  "Max*VPD[Y0]", "Max*Temp[Y0Q3]", "Max*VPD[Y0Q3]", "Max*Temp[Y0]", "Min*Temp[Y0Q1]", "Max*Temp[Y0Q4]")

varflower$labels$group <- "Trees"
varflower$labels$colour <- "Trees"

label_parse <- function(breaks){ parse(text=breaks) } # need this, for reasons
flowerplot<- varflower + scale_x_discrete(label=label_parse) + theme(legend.position=c(0.8, 0.7), axis.text=element_text(size=13), legend.text=element_text(size=12), legend.title=element_text(size=13))

#3
levels(varfruitNTO$data$names)

varfruitNTO$data <- varfruitNTO$data |> mutate(trees = factor(trees, c(10,20,50,100,150,200)))

levels(varfruitNTO$data$names) <- c("Min*VPD[Y0Q3]", "Delta[Y0-1]*Max*Temp", "Min*VPD[Y1]", "PPT[Y0Q3]", "Max*VPD[Y0Q4]", "Max*Temp[Y0Q4]", "Min*VPD[Y0Q4]", "PPT[Y0Q4]", "Delta[Y0-1]*Max*VPD",
                                    "Delta[Y0-1]*Min*VPD", "Delta[Y1-2]*PPT", "PPT[Y1]", "Delta[Y0-1]*PPT", "Delta[Y0-1]*Min*Temp", "Min*Temp[Y0]", "Min*Temp[Y0Q3]", "Max*Temp[Y0Q1]", "Min*VPD[Y0Q1]",
                                    "Min*Temp[Y1]", "Min*Temp[Y0Q1]", "PPT[Y0]", "Min*Temp[Y0Q4]", "Max*VPD[Y0Q1]", "Min*VPD[Y0]", "PPT[Y2]", "Max*VPD[Y1]", "PPT[Y0Q1]",
                                    "Max*Temp[Y0Q3]", "Max*Temp[Y0]", "Max*Temp[Y1]", "Max*VPD[Y0]", "Max*VPD[Y0Q3]")
                                    

varfruitNTO$labels$group <- "Trees"
varfruitNTO$labels$colour <- "Trees"

label_parse <- function(breaks){ parse(text=breaks) } # need this, for reasons
fruitplotNTO<- varfruitNTO + scale_x_discrete(label=label_parse) + theme(legend.position=c(0.8, 0.7), axis.text=element_text(size=13), legend.text=element_text(size=12), legend.title=element_text(size=13))
#4
levels(varfruit$data$names)

varfruit$data <- varfruit$data |> mutate(trees = factor(trees, c(10,20,50,100,150,200)))

levels(varfruit$data$names) <- c("Total*Obs", "PPT[Y0Q3]", "Min*VPD[Y0Q3]", "Delta[Y0-1]*Max*Temp", "Min*VPD[Y0Q4]", "Max*VPD[Y0Q4]", "Delta[Y0-1]*Max*VPD", "Max*Temp[Y0Q4]", "PPT[Y0Q4]", 
                                 "PPT[Y1]", "Min*VPD[Y1]", "Delta[Y0-1]*PPT", "Delta[Y1-2]*PPT", "Delta[Y0-1]*Min*VPD", "Delta[Y0-1]*Min*Temp", "Min*Temp[Y1]", "Min*Temp[Y0]", "Min*Temp[Y0Q3]", 
                                 "Min*VPD[Y0Q1]", "Min*Temp[Y0Q1]", "Max*Temp[Y0Q1]", "Max*VPD[Y0Q1]", "PPT[Y0]", "Min*VPD[Y0]", "Min*Temp[Y0Q4]", "PPT[Y0Q1]", "Max*VPD[Y0]", 
                                 "PPT[Y2]", "Max*Temp[Y0]", "Max*VPD[Y1]", "Max*VPD[Y0Q3]", "Max*Temp[Y0Q3]", "Max*Temp[Y1]")
                                 

varfruit$labels$group <- "Trees"
varfruit$labels$colour <- "Trees"

label_parse <- function(breaks){ parse(text=breaks) } # need this, for reasons
fruitplot<- varfruit + scale_x_discrete(label=label_parse) + theme(legend.position=c(0.8, 0.7), axis.text=element_text(size=13), legend.text=element_text(size=12), legend.title=element_text(size=13))



#combine into one plot
flowerplotnto <- flowerplotnto + 
  ggtitle("Flower Model Without") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))

flowerplot <- flowerplot + 
  ggtitle("Flower Model W Total Observations") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(axis.title.x = element_blank())

fruitplotNTO <- fruitplotNTO + 
  ggtitle("Fruit Model Without") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))

fruitplot <- fruitplot + 
  ggtitle("Fruit Model W Total Observations") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(axis.title.x = element_blank())



finalflowervar_plot <- plot_grid(flowerplot, flowerplotnto,
                        ncol = 1, 
                        nrow = 2,
                        labels = "AUTO") 

ggsave(
  filename = "C:/Users/13073/Desktop/Masters/Thesis/Jotr_phenology-main/Jotr_phenology-main/scripts/output/plots/finalflowervar_plot2.png", 
  plot = finalflowervar_plot,
  width = 8,    # Good width for a single-column plot
  height = 10,  # Tall enough for two stacked plots
  units = "in", # Inches (alternatively, use "cm" or "mm")
  dpi = 300     # High resolution for publications
)

finalfruitvar_plot <- plot_grid(fruitplot,fruitplotNTO, 
                           ncol = 1, 
                           nrow = 2,
                           labels = c("C","D")) 

ggsave(
  filename = "C:/Users/13073/Desktop/Masters/Thesis/Jotr_phenology-main/Jotr_phenology-main/scripts/output/plots/finalfruitvar_plot2.png", 
  plot = finalfruitvar_plot,
  width = 8,    # Same width for consistency
  height = 10,  # Same height for consistency
  units = "in",
  dpi = 300
)


#flowering predicted fruiting creator loop
#__________________________________________________________________________________

# Define your predictors and model
jotr.preds.fruit <- attr(jotr.fruit.mod$fit$data@x, "term.labels")

# LOOP over years
for(yr in 2008:2024){
  
  # Load the predictors for the year
  preds <- brick(paste("data/PRISM/derived_predictors/PRISM_derived_predictors_", yr, ".gri", sep=""))
  
  raster_crs <- projection(preds)  # Get the CRS of the predictor raster
  shapefile_crs <- st_crs(range)  # Get the CRS of the shapefile
  
  if (raster_crs != shapefile_crs) {
    range <- st_transform(range, crs = raster_crs)  # Align CRS of range to raster if they are different
  }
  
  # Make predictions using the model
  pred.ri0 <- predict(jotr.fruit.mod, preds[[jotr.preds.fruit]], splitby=20)
  
  # Ensure the prediction has the same extent and resolution as the range shapefile
  pred.ri0 <- crop(pred.ri0, extent(range))  # Crop the raster to the range extent
  pred.ri0 <- resample(pred.ri0, preds, method = "bilinear")  # Resample to ensure resolution matches
  
  # Mask the predictions to the range of the species
  pred.masked <- mask(pred.ri0, range)  # This will mask the predictions to the spatial extent of the range shapefile
  

  
  # Save the predictions as a raster file
  writeRaster(pred.masked, paste("output/BART/predictions/BART_predicted_fruiting_", yr, ".bil", sep=""), overwrite=TRUE)
  
}  # End of loop


#_______________________________________
#now flowering

# Define your predictors and model
jotr.preds.flower <- attr(jotr.flower.mod$fit$data@x, "term.labels")

# LOOP over years
for(yr in 2008:2024){
  # yr <- 2009
  # Load the predictors for the year
  preds <- brick(paste("data/PRISM/derived_predictors/PRISM_derived_predictors_", yr, ".gri", sep=""))
  
  raster_crs <- projection(preds)  # Get the CRS of the predictor raster
  shapefile_crs <- st_crs(range)  # Get the CRS of the shapefile
  
  if (raster_crs != shapefile_crs) {
    range <- st_transform(range, crs = raster_crs)  # Align CRS of range to raster if they are different
  }
  
  # Make predictions using the model
  pred.ri0 <- predict(jotr.flower.mod, preds[[jotr.preds.flower]], splitby=20)
  
  # Ensure the prediction has the same extent and resolution as the range shapefile
  pred.ri0 <- crop(pred.ri0, extent(range))  # Crop the raster to the range extent
  pred.ri0 <- resample(pred.ri0, preds, method = "bilinear")  # Resample to ensure resolution matches
  
  # Mask the predictions to the range of the species
  pred.masked <- mask(pred.ri0, range)  # This will mask the predictions to the spatial extent of the range shapefile
  
  # Optionally, plot or inspect the predictions
# This will show the prediction, you can customize it further
  
  # Save the predictions as a raster file
  writeRaster(pred.masked, paste("output/BART/predictions/BART_predicted_flowering_", yr, ".bil", sep=""), overwrite=TRUE)
  
}  # End of loop


#______________________________
#Binary Maping of models

# Define the cutoff value
cutoff <- 0.2007121    

# Extract predictor names
jotr.preds.flower <- attr(jotr.flower.mod$fit$data@x, "term.labels")

# Step 3: 
for (yr in 1900:2024) {
  # Load the predictors for the year
  preds <- brick(paste("data/PRISM/derived_predictors/PRISM_derived_predictors_", yr, ".gri", sep = ""))
  
  
  # Make predictions using the model
  pred.ri0 <- predict(jotr.flower.mod, preds[[jotr.preds.flower]], splitby = 20)
  
  # Ensure the prediction has the same extent and resolution as the range shapefile
  pred.ri0 <- crop(pred.ri0, extent(range))  # Crop the raster to the range extent
  pred.ri0 <- resample(pred.ri0, preds, method = "bilinear")  # Resample to ensure resolution matches
  
  # Mask the predictions to the range of the species
  pred.masked <- mask(pred.ri0, range)  # This will mask the predictions to the spatial extent of the range shapefile
  
  # Apply the cutoff to create a binary map
  binary_pred <- calc(pred.masked, fun = function(x) { ifelse(x >= cutoff, 1, 0) })
  
  # Save the binary predictions as a raster file
  writeRaster(binary_pred, paste("output/BART/predictions/BART_binary_flowering/BART_binary_flowering_", yr, ".bil", sep = ""), overwrite = TRUE)
}

#________________________________________________________________________________________________________
#FRUINGTING BINARY 

fruitcutoff <- 0.4620259


# Extract predictor names
jotr.preds.fruit <- attr(jotr.fruit.mod$fit$data@x, "term.labels")



for (yr in 1900:2025) {
  # Load the predictors for the year
  preds <- brick(paste("data/PRISM/derived_predictors/PRISM_derived_predictors_", yr, ".gri", sep = ""))
  
  # Make predictions using the model
  pred.ri0 <- predict(jotr.fruit.mod, preds[[jotr.preds.fruit]], splitby = 20)
  
  # Ensure the prediction has the same extent and resolution as the range shapefile
  pred.ri0 <- crop(pred.ri0, extent(range))  # Crop the raster to the range extent
  pred.ri0 <- resample(pred.ri0, preds, method = "bilinear")  # Resample to ensure resolution matches
  
  # Mask the predictions to the range of the species
  pred.masked <- mask(pred.ri0, range)  # This will mask the predictions to the spatial extent of the range shapefile
  
  # Apply the cutoff to create a binary map
  binary_pred <- calc(pred.masked, fun = function(x) { ifelse(x >= fruitcutoff, 1, 0) })
  
  # Save the binary predictions as a raster file
  writeRaster(binary_pred, paste("output/BART/predictions/BART_binary_fruiting/BART_binary_fruiting_", yr, ".bil", sep = ""), overwrite = TRUE)
}


#_____________________________________________________________
#Subtracting binary flowering and fruiting rasters 


for (yr in 1900:2025) {
  #yr=2023
  # Load the rasters for fruiting and flowering for the current year
  plotfruit <- raster(paste("output/BART/predictions/BART_binary_fruiting/BART_binary_fruiting_", yr, ".bil", sep=""))
  plotflower <- raster(paste("output/BART/predictions/BART_binary_flowering/BART_binary_flowering_", yr, ".bil", sep=""))
  
  nofruit <- overlay(plotflower, plotfruit, fun = function(plotflower, plotfruit) {
    ifelse(plotflower == 1 & plotfruit == 1, -2,  # Flowers & Fruits
           ifelse(plotflower == 1 & plotfruit == 0, 1,  # Flowers Only
                  ifelse(plotflower == 0 & plotfruit == 1, -1,  # Fruits Only
                         NA)))  # No Activity (transparent)
  })

  
  # Convert raster to points for ggplot
  raster_df <- as.data.frame(rasterToPoints(nofruit), xy = TRUE)
  colnames(raster_df) <- c("x", "y", "value")
  
  # Plot using ggplot
  p <- ggplot() +
    # Raster plot as a filled grid
    geom_tile(data = raster_df, aes(x = x, y = y, fill = factor(value))) +  
    scale_fill_manual(
      values = c("-2" = "orange",  # Flowers & Fruits
                 "-1" = "purple", # Fruits Only
                 "1" = "green"),  # Flowers Only
      na.value = "transparent",  # No Activity (transparent)
      labels = c("-2" = "Flowers & Moths",
                 "-1" = "Moths Only",
                 "1" = "Flowers Only")
    ) +
    labs(title = paste(yr, "Moth and Flowering activity"), 
         x = "Longitude", 
         y = "Latitude", 
         fill = "Legend") +  # Labels
    # Add the coastline shapefile
    geom_sf(data = coast, fill = "gray", color = "black", size = 0.3) +  
    # Add the states shapefile
    geom_sf(data = states, fill =alpha("gray",.2), color = "black", size = 0.5) +
    geom_sf(data = range, fill = alpha("darkgrey", 0.2), color = "transparent") +
    theme_minimal() +
    coord_sf(xlim = c(-119, -113.5), ylim = c(32.75, 38), expand = FALSE)
  
  
  
  ggsave(paste("output/plots/Flowering_No_Moths_", yr, ".png", sep=""), plot = p)
}




#Counting squars to check to see if predictions align with observations
#_____________________________________________________________________________
# Define all possible categories
categories <- data.frame(
  Value = c(-3, -2, -1, 1),
  Description = c("No activity", "Flowers & Fruits", "Fruits Only", "Flowers Only")
)

results_list <- list()

for (yr in 1900:2025) {
  # Load the rasters for fruiting and flowering for the current year
  plotfruit <- raster(paste("output/BART/predictions/BART_binary_fruiting/BART_binary_fruiting_", yr, ".bil", sep=""))
  plotflower <- raster(paste("output/BART/predictions/BART_binary_flowering/BART_binary_flowering_", yr, ".bil", sep=""))
  
  plotfruit_masked <- mask(plotfruit, range)
  plotflower_masked <- mask(plotflower, range)
  
  # Calculate nofruit overlay within the range
  nofruit <- overlay(plotflower_masked, plotfruit_masked, fun = function(plotflower, plotfruit) {
    ifelse(plotflower == 1 & plotfruit == 1, -2,  # Flowers & Fruits
           ifelse(plotflower == 1 & plotfruit == 0, 1,  # Flowers Only
                  ifelse(plotflower == 0 & plotfruit == 1, -1,  # Fruits Only
                         ifelse(plotflower == 0 & plotfruit == 0, -3, NA))))
  })
  
  pixel_counts <- freq(nofruit, useNA = "no")  # Exclude NA values
  
  # Convert to a data frame with descriptive names
  if (!is.null(pixel_counts)) {  # Ensure there are non-NA values
    counts_df <- as.data.frame(pixel_counts)
    colnames(counts_df) <- c("Value", "Count")
  } else {
    counts_df <- data.frame(Value = numeric(0), Count = numeric(0))
  }
  
  # Merge with all possible categories to ensure missing ones are included
  counts_df <- merge(categories, counts_df, by = "Value", all.x = TRUE)
  counts_df$Count[is.na(counts_df$Count)] <- 0  # Set missing counts to 0
  counts_df$Year <- yr  # Add the current year
  
  # Add to results list
  results_list[[as.character(yr)]] <- counts_df
}

# Combine all results into a single data frame (optional)
results_combined <- do.call(rbind, results_list)


# Reshape the data so years are columns
results_reshaped <- results_combined %>%
  pivot_wider(names_from = Year, values_from = Count, values_fill = 0)
# View reshaped results
print(results_reshaped)

#combinign all flowering to just see what happens 
results_flower <- results_combined %>%
  group_by(Year) %>%
  mutate(Count = ifelse(Description %in% c("Flowers & Fruits", "Flowers Only"), 
                        sum(Count[Description %in% c("Flowers & Fruits", "Flowers Only")]), Count))
#ploting the counts with all the categorys

ggplot() + 
  geom_point(data = results_combined, 
             aes(fill = Description, color = Description, x = Year, y = Count)) + 
  geom_line(data = results_combined , 
            aes(x = Year, y = Count, color = Description, group = Description))+
scale_x_continuous(breaks = unique(results_combined$Year)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))+
geom_smooth(data = results_combined, 
              aes(x = Year, y = Count, color = Description, fill=Description, group = Description), 
              method = "lm", se = TRUE, fullrange = TRUE)

# Create the stacked bar graph
ggplot(data = results_combined, aes(x = Year, y = Count, fill = Description)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bars
  scale_x_continuous(breaks = unique(results_combined$Year)) +  # Set x-axis breaks
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Count by Year and Description",  # Add a title
       x = "Year", y = "Count", fill = "Description") 
#okay so now regressions on those lines to see if there is change in flowering/fuiting over time

flnfru <- subset(results_combined %>%
                   filter(Description == "Flowers & Fruits"))
fruo <- subset(results_combined %>%
                   filter(Description == "Fruits Only"))
flro <- subset(results_combined %>%
                   filter(Description == "Flowers Only"))
na <- subset(results_combined %>%
                   filter(Description == "No activity"))


allFruiting <- results_combined %>%
  filter(Description %in% c("Fruits Only", "Flowers & Fruits")) %>%
  mutate(Description = "allfruiting") %>%  # Reassign category
  group_by(Year, Description) %>%
  summarize(Count = sum(Count, na.rm = TRUE))
allFlowering <- results_combined %>%
  filter(Description %in% c("Flowers Only", "Flowers & Fruits")) %>%
  mutate(Description = "allfruiting") %>%  # Reassign category
  group_by(Year, Description) %>%
  summarize(Count = sum(Count, na.rm = TRUE)) 



#linear regressions all flat and nothing really going on... intersting
modallfruit <- lm(Count ~ Year, data=allFruiting)
summary(modallfruit)

modallflower <- lm(Count ~ Year, data=allFlowering)
summary(modallflower)

modff <- lm(Count ~ Year, data=flnfru)
summary(modff)
predictedff <- fitted(modff)
residff <- residuals(modff)
plot(residff~predictedff)
abline(h=0)
qqp(residff, "norm")
outlierTest(modff)
influencePlot(modff)
#-p-value: 0.9624 	Adjusted R-squared:  -0.008112 
modfr <- lm(Count ~ Year, data=fruo)
summary(modfr)
#p-value: 0.8144 Adjusted R-squared:  -0.007677 
modflr <- lm(Count ~ Year, data=flro)
summary(modflr)
#p-value: 0.6164 Adjusted R-squared:  -0.006066 
modna <- lm(Count ~ Year, data=na)
summary(modna)
#p-value: 0.809 Adjusted R-squared:  -0.007649 

#jeremys paper used a spearmans corrolation lets do that I guess
#use cor.test actually
spearman_corrff <- cor.test(allFruiting$Count, allFruiting$Year, method = "spearman")
print(spearman_corrff)
#rho  -0.005279403  p=0.9532 

#or Should I look at the count for every square and then run a spearmans row on that?.... hmmm

mean(allFruiting$Count)
#1310.095
median(allFruiting$Count)
#1359.5


spearman_corrfo <- cor.test(allFlowering$Count, allFlowering$Year, method = "spearman")
print(spearman_corrfo)
#0.06170893 p-value = 0.4924

mean(allFlowering$Count)
#761.627
median(allFlowering$Count)
#508

#flower mistmatch 
spearman_corrfo <- cor.test(flro$Count, flro$Year, method = "spearman")
print(spearman_corrfo)
#0.06170893 p-value = 0.4591

mean(flro$Count)
#483.064
median(flro$Count)
#356

#fruit mismatch
spearman_corrff <- cor.test(fruo$Count, fruo$Year, method = "spearman")
print(spearman_corrff)
#rho  --0.08256247  p=0.36

mean(fruo$Count)
#1032.48
median(fruo$Count)
#842


#okay lets try to devide in half? 

results62 <- subset(results_combined %>%
                   filter(Year >1962))

ggplot() + 
  geom_point(data = results62, 
             aes(fill = Description, color = Description, x = Year, y = Count)) + 
  geom_line(data = results62 , 
            aes(x = Year, y = Count, color = Description, group = Description))+
  scale_x_continuous(breaks = unique(results62$Year)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))+
  geom_smooth(data = results62, 
              aes(x = Year, y = Count, color = Description, fill=Description, group = Description), 
              method = "lm", se = TRUE, fullrange = TRUE)

flnfru62 <- subset(results62 %>%
                   filter(Description == "Flowers & Fruits"))
fruo62 <- subset(results62 %>%
                 filter(Description == "Fruits Only"))
flro62 <- subset(results62 %>%
                 filter(Description == "Flowers Only"))
na62 <- subset(results62 %>%
               filter(Description == "No activity"))

spearman_corrff62 <- cor(flnfru62$Count, flnfru62$Year, method = "spearman")
print(spearman_corrff62)
#-.12
spearman_corrfo62 <- cor(fruo62$Count, fruo62$Year, method = "spearman")
print
#-.13
spearman_corrflr62 <- cor(flro62$Count, flro62$Year, method = "spearman")
print(spearman_corrflr62)
#0.04835947
spearman_corrna62 <- cor(na62$Count, na62$Year, method = "spearman")
print(spearman_corrna62)
#-0.1149391


#______________________________________________________________________________
#Okay so right now I have the flowering rasters for each year that show each grid square and if 
#it is predicted to flower in that year. For every year. So right now I just need to have R count
#for each square how many times between 1900-1929 it was predicted to flower then do the same for 
#1995-2024. then subtract late from early to get the differences in flowering between early and late 
#for each square. 

#okay lets start with flowering. So I'll make the early then the late and then subtract them? 
Floweringearly0029 <- NULL

# Loop through the years 1900 to 1929
for (yr in 1900:1929) {
  # Load the raster for the current year
 Floweringraster <- raster(paste("output/BART/predictions/BART_binary_flowering/BART_binary_flowering_", yr, ".bil", sep=""))
  
  # If cumulative_fruiting is NULL, initialize it with the first raster
  if (is.null(Floweringearly0029)) {
    Floweringearly0029 <- Floweringraster
  } else {
    # Add the current year's raster to the cumulative raster
    Floweringearly0029 <- Floweringearly0029 + Floweringraster
  }
}

#OKay Now Flowering Late

Floweringlate9524 <- NULL

# Loop through the years 1900 to 1929
for (yr in 1996:2025) {
  # Load the raster for the current year
  Floweringraster <- raster(paste("output/BART/predictions/BART_binary_flowering/BART_binary_flowering_", yr, ".bil", sep=""))
  
  # If cumulative_fruiting is NULL, initialize it with the first raster
  if (is.null(Floweringlate9524)) {
    Floweringlate9524 <- Floweringraster
  } else {
    # Add the current year's raster to the cumulative raster
    Floweringlate9524 <- Floweringlate9524 + Floweringraster
  }
}

#now subtract them to get the change over time

Changeinflowering <- Floweringlate9524 - Floweringearly0029  

hist(
  Changeinflowering,
  main = "",
  xlab = "Change in years with predicted Flowering",  # Change x-axis label here
  col = "skyblue",
  border = "black"
)


cellStats(Changeinflowering, stat = 'mean')
#2.67452
cellStats(Changeinflowering, stat = 'countNA')

raster_values <- values(Changeinflowering)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#2
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-2     8 

cellStats(Changeinflowering > 0, stat = 'sum', na.rm = TRUE)
#2415

total_cells <- ncell(Changeinflowering) - cellStats(is.na(Changeinflowering), 'sum')
cells_above_zero <- cellStats(Changeinflowering > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero

#81.28576 of range shows an increase

change_df <- as.data.frame(Changeinflowering, xy = TRUE)
colnames(change_df) <- c("x", "y", "value")  
change_df <- na.omit(change_df)

# Custom diverging palette (alternative to "RdBu")
diverging_palette <- brewer.pal(11, "BrBG")  # Pink-Green alternative
# Other options: "PRGn" (Purple-Green), "BrBG" (Brown-Blue-Green), "PuOr" (Purple-Orange)

# Define fixed scale limits (-12 to +12)
Changeinfloweringplot <- ggplot() +
  # Base layers (states and coast)
  geom_sf(data = coast, fill = "gray90", color = "white", size = 0.3) +  # Lighter fill
  geom_sf(data = states, fill = "#2D3E50", color = "white", size = 0.5) +
  
  # State labels
  annotate("text", x = -113.5, y = 37.7, label = "UT", size = 4, fontface = "bold", color = "white") +
  annotate("text", x = -114, y = 35.5, label = "AZ", size = 4, fontface = "bold", color = "white") +
  annotate("text", x = -118.5, y = 37, label = "CA", size = 4, fontface = "bold", color = "white") +
  annotate("text", x = -115.8, y = 37.7, label = "NV", size = 4, fontface = "bold", color = "white") +
  
  # Raster layer with fixed scale
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  
  # Color scale with white midpoint
  scale_fill_gradientn(
    colors = diverging_palette,
    limits = c(-12, 12),  # Force scale from -12 to +12
    breaks = c(-12, -6, 0, 6, 12),  # Optional: Custom breaks
    name = "Change (years)",  # Legend title
    oob = scales::squish  # Clamp values outside -12/12 to nearest color
  ) +
  
  # Legend adjustments
  guides(fill = guide_colorbar(
    barwidth = 1.5, 
    barheight = 20,
    title.position = "right"
  )) +
  
  # Titles and theme
  labs(
    title = "Change in flowering years",
    x = "", 
    y = "",
  ) +
  coord_sf(xlim = c(-119, -112.3), ylim = c(33.7, 38.2), expand = FALSE) +
  theme_minimal()+ theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

# Save plot
ggsave("output/plots/Changeinfloweringplot.png", plot = Changeinfloweringplot, width = 10, height = 8, dpi = 300)

############################################
#Okay so fruiting lets try it

# Initialize an empty raster to store the cumulative count
Earlyfruiting0029 <- NULL

# Loop through the years 1900 to 1929
for (yr in 1900:1929) {
  # Load the raster for the current year
  fruiting_raster <- raster(paste("output/BART/predictions/BART_binary_fruiting/BART_binary_fruiting_", yr, ".bil", sep=""))
  
  # If cumulative_fruiting is NULL, initialize it with the first raster
  if (is.null(Earlyfruiting0029)) {
    Earlyfruiting0029 <- fruiting_raster
  } else {
    # Add the current year's raster to the cumulative raster
    Earlyfruiting0029 <- Earlyfruiting0029 + fruiting_raster
  }
}

#plotting Early Fruting 
Early_df <- as.data.frame(Earlyfruiting0029, xy = TRUE)
Early_df <- na.omit(Early_df)

Earlyplot <- ggplot() +
  # Add the states and coast layers
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  # Add the raster layer
  geom_raster(data = Early_df, aes(x = x, y = y, fill = layer)) +
  # Add the color gradient from white to blue
  scale_fill_gradient(low = "white", high = "blue", 
                      limits = c(0, 22),  # Set the range of values
                      name = "Years") +   # Legend title
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 20)) +
  # Add titles and theme
  labs(
    title = "How many years saw moth activity 1900-1929",
    x = "Longitude", y = "Latitude"
  ) +
  coord_sf(xlim = c(-119, -112.3), ylim = c(33.7, 38.2), expand = FALSE) +
  theme_minimal()



#okay now late 
Latefruiting9524 <- NULL

# Loop through the years 1900 to 1929
for (yr in 1996:2025) {
  # Load the raster for the current year
  fruiting_raster <- raster(paste("output/BART/predictions/BART_binary_fruiting/BART_binary_fruiting_", yr, ".bil", sep=""))
  
  # If cumulative_fruiting is NULL, initialize it with the first raster
  if (is.null(Latefruiting9524)) {
    Latefruiting9524 <- fruiting_raster
  } else {
    # Add the current year's raster to the cumulative raster
    Latefruiting9524 <- Latefruiting9524 + fruiting_raster
  }
}

#late Fruiting 

Late_df <- as.data.frame(Latefruiting9524, xy = TRUE)
Late_df <- na.omit(Late_df)

Earlyplot <- ggplot() +
  # Add the states and coast layers
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  # Add the raster layer
  geom_raster(data = Late_df, aes(x = x, y = y, fill = layer)) +
  # Add the color gradient from white to blue
  scale_fill_gradient(low = "white", high = "blue", 
                      limits = c(0, 22),  # Set the range of values
                      name = "Years") +   # Legend title
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 20)) +
  # Add titles and theme
  labs(
    title = "How many years saw moth activity 1995-2",
    x = "Longitude", y = "Latitude"
  ) +
  coord_sf(xlim = c(-119, -112.3), ylim = c(33.7, 38.2), expand = FALSE) +
  theme_minimal()


Changeinfruiting <- Latefruiting9524- Earlyfruiting0029


hist(
  Changeinfruiting,
  main = "",
  xlab = "Change in years with predicted Fruiting",  # Change x-axis label here
  col = "skyblue",
  border = "black"
)


cellStats(Changeinfruiting, stat = 'mean')
#mean 0.502861
raster_values2 <- values(Changeinfruiting)
na.omit(raster_values2)
median(raster_values2, na.rm = TRUE)
#0
quantile(raster_values2, c(0.025, 0.975), na.rm = TRUE)
#-8    11 


total_cells <- ncell(Changeinfruiting) - cellStats(is.na(Changeinfruiting), 'sum')
cells_above_zero <- cellStats(Changeinfruiting > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#45.70852 cells show increase in moth emergance
#Plotting to see visually


change_df <- as.data.frame(Changeinfruiting, xy = TRUE)
change_df <- na.omit(change_df)
colnames(change_df) <- c("x", "y", "value")  
diverging_palette <- brewer.pal(11, "RdBu")  

Changeinfruitplot <- ggplot() +
  # Base layers (states and coast) - matching flowering plot style
  geom_sf(data = coast, fill = "gray90", color = "white", size = 0.3) +
  geom_sf(data = states, fill = "#2D3E50", color = "white", size = 0.5) +
  
  # State labels (white text)
  annotate("text", x = -113.5, y = 37.7, label = "UT", size = 4, fontface = "bold", color = "white") +
  annotate("text", x = -114, y = 35.5, label = "AZ", size = 4, fontface = "bold", color = "white") +
  annotate("text", x = -118.5, y = 37, label = "CA", size = 4, fontface = "bold", color = "white") +
  annotate("text", x = -115.8, y = 37.7, label = "NV", size = 4, fontface = "bold", color = "white") +
  
  # Raster layer
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  
  # Color scale (BrBG palette, -12 to +12 with 0=white)
  scale_fill_gradientn(
    colors = brewer.pal(11, "BrBG"),
    limits = c(-12, 12),  # Force scale from -12 to +12
    breaks = c(-12, -6, 0, 6, 12),  # Consistent breaks
    name = "Change (years)",
    oob = scales::squish  # Clamp values outside -12/12
  ) +
  
  # Legend adjustments (matching flowering plot)
  guides(fill = guide_colorbar(
    barwidth = 1.5,
    barheight = 20,
    title.position = "right"
  )) +
  
  # Titles and theme
  labs(
    title = "Change in fruiting years",
    x = "",
    y = ""
  ) +
  coord_sf(xlim = c(-119, -112.3), ylim = c(33.7, 38.2), expand = FALSE) +
  theme_minimal() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

# Save with consistent dimensions
ggsave("output/plots/Changeinfruitplot.png", plot = Changeinfruitplot, width = 10, height = 8, dpi = 300)




########
# Okay counting how often Mistmatches are happening. 

#so make a flowering raster for all the years

Floweringall <- NULL

# Loop through the years 1900 to 1929
for (yr in 1900:2025) {
  # Load the raster for the current year
  Floweringraster <- raster(paste("output/BART/predictions/BART_binary_flowering/BART_binary_flowering_", yr, ".bil", sep=""))
  fruiting_raster <- raster(paste("output/BART/predictions/BART_binary_fruiting/BART_binary_fruiting_", yr, ".bil", sep=""))
  
  # Create a raster where flowering = 1 and fruiting = 0
  condition_raster <- (Floweringraster == 1) & (fruiting_raster == 0)
  
  # Convert logical raster to numeric (1 for TRUE, 0 for FALSE)
  condition_raster <- condition_raster * 1
  
  # If Floweringall is NULL, initialize it with the condition_raster
  if (is.null(Floweringall)) {
    Floweringall <- condition_raster
  } else {
    # Add the condition_raster to Floweringall
    Floweringall <- Floweringall + condition_raster
  }
}


cellStats(Floweringall, stat = 'mean')
#17.84214
change_df <- as.data.frame(Floweringall, xy = TRUE)
change_df <- na.omit(change_df)
colnames(change_df) <- c("x", "y", "value") 
diverging_palette <- brewer.pal(9, "Reds")  

ggplot() +
  
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +  
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = diverging_palette, 
                       limits = c(0,44),  
                       name = "Flowering mismatch") +
  # Add titles and theme
  labs(title = "Flowers no fruit count",
       x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-119, -112), ylim = c(33, 38), expand = FALSE) +
  theme_minimal() 


################################ 
#now fruiting 

Mothsall <- NULL

# Loop through the years 1900 to 1929
for (yr in 1900:2025) {
  # Load the raster for the current year
  Floweringraster <- raster(paste("output/BART/predictions/BART_binary_flowering/BART_binary_flowering_", yr, ".bil", sep=""))
  fruiting_raster <- raster(paste("output/BART/predictions/BART_binary_fruiting/BART_binary_fruiting_", yr, ".bil", sep=""))
  
  # Create a raster where flowering = 0 and fruiting = 1
  condition_raster <- (fruiting_raster == 1) & (Floweringraster == 0)
  
  # Convert logical raster to numeric (1 for TRUE, 0 for FALSE)
  condition_raster <- condition_raster * 1
  
  # If Floweringall is NULL, initialize it with the condition_raster
  if (is.null(Mothsall)) {
    Mothsall <- condition_raster
  } else {
    # Add the condition_raster to Floweringall
    Mothsall <- Mothsall + condition_raster
  }
}

cellStats(Mothsall, stat = 'mean')
#48.29956


change_df2 <- as.data.frame(Mothsall, xy = TRUE)
change_df3 <- na.omit(change_df2)
colnames(change_df3) <- c("x", "y", "value") 

diverging_palette <- brewer.pal(9, "Reds")  

ggplot() +
  
  coord_sf(xlim = c(-119, -112), ylim = c(33, 38), expand = FALSE) +
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +  
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  geom_raster(data = change_df3, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = diverging_palette, 
                       limits = c(0,54),  
                       name = "Change in Fruiting") +
  # Add titles and theme
  labs(title = "Flowers no fruit count",
       x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-119, -112), ylim = c(33, 38), expand = FALSE) +
  theme_minimal() 


##__________________________________________________________________________________________

#Okay now change in flower no fruit mismatch

Floweringmmearly <- NULL

# Loop through the years 1900 to 1929
for (yr in 1900:1929) {
  # Load the raster for the current year
  Floweringraster <- raster(paste("output/BART/predictions/BART_binary_flowering/BART_binary_flowering_", yr, ".bil", sep=""))
  fruiting_raster <- raster(paste("output/BART/predictions/BART_binary_fruiting/BART_binary_fruiting_", yr, ".bil", sep=""))
  
  # Create a raster where flowering = 1 and fruiting = 0
  condition_raster <- (Floweringraster == 1) & (fruiting_raster == 0)
  
  # Convert logical raster to numeric (1 for TRUE, 0 for FALSE)
  condition_raster <- condition_raster * 1
  
  # If Floweringall is NULL, initialize it with the condition_raster
  if (is.null(Floweringmmearly)) {
    Floweringmmearly <- condition_raster
  } else {
    # Add the condition_raster to Floweringall
    Floweringmmearly <- Floweringmmearly + condition_raster
  }
}

Floweringmmlate <- NULL

# Loop through the years 1900 to 1929
for (yr in 1996:2025) {
  # Load the raster for the current year
  Floweringraster <- raster(paste("output/BART/predictions/BART_binary_flowering/BART_binary_flowering_", yr, ".bil", sep=""))
  fruiting_raster <- raster(paste("output/BART/predictions/BART_binary_fruiting/BART_binary_fruiting_", yr, ".bil", sep=""))
  
  # Create a raster where flowering = 1 and fruiting = 0
  condition_raster <- (Floweringraster == 1) & (fruiting_raster == 0)
  
  # Convert logical raster to numeric (1 for TRUE, 0 for FALSE)
  condition_raster <- condition_raster * 1
  
  # If Floweringall is NULL, initialize it with the condition_raster
  if (is.null(Floweringmmlate)) {
    Floweringmmlate <- condition_raster
  } else {
    # Add the condition_raster to Floweringall
    Floweringmmlate <- Floweringmmlate + condition_raster
  }
}


Changeinfloweringmismatch <- Floweringmmlate - Floweringmmearly

hist(
  Changeinfloweringmismatch,
  main = "",
  xlab = "Change in years with predicted Flowering No Fruiting ",  
  col = "skyblue",
  border = "black"
)

cellStats(Changeinfloweringmismatch, stat = 'mean')
#1.021878
raster_values <- values(Changeinfloweringmismatch)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#1
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-3     6 

total_cells <- ncell(Changeinfloweringmismatch) - cellStats(is.na(Changeinfloweringmismatch), 'sum')
cells_above_zero <- cellStats(Changeinfloweringmismatch > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#56.04174

change_df <- as.data.frame(Changeinfloweringmismatch, xy = TRUE)
change_df <- na.omit(change_df)
colnames(change_df) <- c("x", "y", "value") 

# Use BrBG palette (brown for negative, white at 0, blue-green for positive)
diverging_palette <- brewer.pal(11, "BrBG")  
diverging_palette2 <- brewer.pal(11, "PRGn")

Floweringmismatchplot <- ggplot() +
  # Base layers with matching style
  geom_sf(data = coast, fill = "gray90", color = "white", size = 0.3) +
  geom_sf(data = states, fill = "#2D3E50", color = "white", size = 0.5) +
  
  # State labels (white text)
  annotate("text", x = -113.5, y = 37.7, label = "UT", size = 4, fontface = "bold", color = "white") +
  annotate("text", x = -114, y = 35.5, label = "AZ", size = 4, fontface = "bold", color = "white") +
  annotate("text", x = -118.5, y = 37, label = "CA", size = 4, fontface = "bold", color = "white") +
  annotate("text", x = -115.8, y = 37.7, label = "NV", size = 4, fontface = "bold", color = "white") +
  
  # Raster layer with fixed -12 to +12 scale
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  
  # Color scale (BrBG palette, white at 0)
  scale_fill_gradientn(
    colors = rev(diverging_palette2),
    limits = c(-12, 12),  # Force symmetric scale
    breaks = c(-12, -6, 0, 6, 12),  # Consistent breaks
    name = "Change (years)",
    oob = scales::squish  # Clamp values outside -12/12
  ) +
  
  # Legend adjustments
  guides(fill = guide_colorbar(
    barwidth = 1.5,
    barheight = 20,
    title.position = "right"
  )) +
  
  # Titles and theme
  labs(
    title = "Change in flowering without fruiting",
    x = "",
    y = "") +
  coord_sf(xlim = c(-119, -112.3), ylim = c(33.7, 38.2), expand = FALSE) +
  theme_minimal() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("output/plots/Floweringmismatchplot.png", plot = Floweringmismatchplot, width = 10, height = 8, dpi = 300)


#____________________________________________________________________
#and now moths but no flowers

Mothmismatchearly <- NULL

# Loop through the years 1900 to 1929
for (yr in 1900:1929) {
  # Load the raster for the current year
  Floweringraster <- raster(paste("output/BART/predictions/BART_binary_flowering/BART_binary_flowering_", yr, ".bil", sep=""))
  fruiting_raster <- raster(paste("output/BART/predictions/BART_binary_fruiting/BART_binary_fruiting_", yr, ".bil", sep=""))
  
  # Create a raster where flowering = 0 and fruiting = 1
  condition_raster <- (fruiting_raster == 1) & (Floweringraster == 0)
  
  # Convert logical raster to numeric (1 for TRUE, 0 for FALSE)
  condition_raster <- condition_raster * 1
  
  # If Floweringall is NULL, initialize it with the condition_raster
  if (is.null(Mothmismatchearly)) {
    Mothmismatchearly <- condition_raster
  } else {
    # Add the condition_raster to Floweringall
    Mothmismatchearly <- Mothmismatchearly + condition_raster
  }
}


Mothmismatchlate <- NULL

# Loop through the years 1900 to 1929
for (yr in 1996:2025) {
  # Load the raster for the current year
  Floweringraster <- raster(paste("output/BART/predictions/BART_binary_flowering/BART_binary_flowering_", yr, ".bil", sep=""))
  fruiting_raster <- raster(paste("output/BART/predictions/BART_binary_fruiting/BART_binary_fruiting_", yr, ".bil", sep=""))
  
  # Create a raster where flowering = 0 and fruiting = 1
  condition_raster <- (fruiting_raster == 1) & (Floweringraster == 0)
  
  # Convert logical raster to numeric (1 for TRUE, 0 for FALSE)
  condition_raster <- condition_raster * 1
  
  # If Floweringall is NULL, initialize it with the condition_raster
  if (is.null(Mothmismatchlate)) {
    Mothmismatchlate <- condition_raster
  } else {
    # Add the condition_raster to Floweringall
    Mothmismatchlate <- Mothmismatchlate + condition_raster
  }
}

Changeinmothmismatch <-Mothmismatchlate-Mothmismatchearly


hist(
  Changeinmothmismatch,
  main = "",
  xlab = "Change in years with predicted Flowering No Fruiting ",  # Change x-axis label here
  col = "skyblue",
  border = "black"
)


change_df <- as.data.frame(Changeinmothmismatch, xy = TRUE)
change_df <- na.omit(change_df)
colnames(change_df) <- c("x", "y", "value") 

# Use BrBG palette (brown for negative, white at 0, blue-green for positive)
diverging_palette <- brewer.pal(11, "BrBG")  
diverging_palette2 <- brewer.pal(11, "PRGn")

Changeinmothmismatchplot <- ggplot() +
  # Base layers with matching style
  geom_sf(data = coast, fill = "gray90", color = "white", size = 0.3) +
  geom_sf(data = states, fill = "#2D3E50", color = "white", size = 0.5) +
  
  # State labels (white text)
  annotate("text", x = -113.5, y = 37.7, label = "UT", size = 4, fontface = "bold", color = "white") +
  annotate("text", x = -114, y = 35.5, label = "AZ", size = 4, fontface = "bold", color = "white") +
  annotate("text", x = -118.5, y = 37, label = "CA", size = 4, fontface = "bold", color = "white") +
  annotate("text", x = -115.8, y = 37.7, label = "NV", size = 4, fontface = "bold", color = "white") +
  
  # Raster layer with fixed -12 to +12 scale
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  
  # Color scale (BrBG palette, white at 0)
  scale_fill_gradientn(
    colors = rev(diverging_palette2),
    limits = c(-12, 12),  # Force symmetric scale
    breaks = c(-12, -6, 0, 6, 12),  # Consistent breaks
    name = "Change (years)",
    oob = scales::squish  # Clamp values outside -12/12
  ) +
  
  # Legend adjustments
  guides(fill = guide_colorbar(
    barwidth = 1.5,
    barheight = 20,
    title.position = "right"
  )) +
  
  # Titles and theme
  labs(
    title = "Change in fruiting without flowering",
    x = "",
    y = "",
  ) +
  coord_sf(xlim = c(-119, -112.3), ylim = c(33.7, 38.2), expand = FALSE) +
  theme_minimal() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

# Save with consistent dimensions
ggsave("output/plots/Changeinmothmismatch.png", plot= Changeinmothmismatchplot, width = 10, height = 8, dpi = 300)

cellStats(Changeinmothmismatch, stat = 'mean')
#-1.149781
raster_values <- values(Changeinmothmismatch)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#-1
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-10 10

total_cells <- ncell(Changeinmothmismatch) - cellStats(is.na(Changeinmothmismatch), 'sum')
cells_above_zero <- cellStats(Changeinmothmismatch > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#33.79334

###########
#okay now all of the 4 change plots together! Yay
library(patchwork)
library(cowplot) # for get_legend

# 1. Extract each row's legend BEFORE stripping legends, and BEFORE adding tags
legend_top <- get_legend(
  Changeinfloweringplot + 
    theme(legend.position = "right",
          legend.title = element_text(hjust = 0.5, angle = 90),
          legend.text = element_text(hjust = 0.5)) +
    guides(fill = guide_colorbar(
      title = "Years",           # <-- set title directly here
      title.position = "right",
      barwidth = 1.5, 
      barheight = 10
    ))
)

legend_bottom <- get_legend(
  Floweringmismatchplot + 
    theme(legend.position = "right",
          legend.title = element_text(hjust = 0.5, angle = 90),
          legend.text = element_text(hjust = 0.5)) +
    guides(fill = guide_colorbar(
      title = "Years",           # <-- set title directly here
      title.position = "right",
      barwidth = 1.5, 
      barheight = 10
    ))
)

# 2. Strip legends AND manually assign tags directly on each plot
flowering_change_plot <- Changeinfloweringplot + theme(legend.position = "none") + labs(tag = "A")
fruit_change_plot     <- Changeinfruitplot + theme(legend.position = "none") + labs(tag = "B")
flowering_mismatch_plot <- Floweringmismatchplot + theme(legend.position = "none") + labs(tag = "C")
moth_mismatch_plot      <- Changeinmothmismatchplot + theme(legend.position = "none") + labs(tag = "D")

# 3. Build each row as plots + its own legend (legends carry no tag)
row1 <- (flowering_change_plot | fruit_change_plot | legend_top) +
  plot_layout(widths = c(10, 10, 2))

row2 <- (flowering_mismatch_plot | moth_mismatch_plot | legend_bottom) +
  plot_layout(widths = c(10, 10, 2))

# 4. Stack the two rows — note: NO tag_levels here, just the title
final_plot <- (row1 / row2)

final_plot

# Save high-resolution output
ggsave("output/plots/combined_phenology_plots.pdf", 
       plot = final_plot,
       width = 8, 
       height = 7,
       dpi = 1200)



#______________________________________________________________________________________
##Latitude and Elevatoion 
# Download elevation data (adjust extent to your region if needed)
elev <- getData('SRTM', lon = mean(xmin(Changeinflowering), xmax(Changeinflowering)),
                lat = mean(ymin(Changeinflowering), ymax(Changeinflowering)))

# Resample elevation to match each raster
elev_flower <- resample(elev, Changeinflowering, method = "bilinear")
elev_fruit <- resample(elev, Changeinfruiting, method = "bilinear")
elev_flower_mm <- resample(elev, Changeinfloweringmismatch, method = "bilinear")
elev_fruit_mm <- resample(elev, Changeinmothmismatch, method = "bilinear")

# Define a reusable plotting function
make_corr_plot <- function(df, xvar, yvar, xlab, ylab, title) {
  df_clean <- na.omit(df[, c(xvar, yvar)])
  ct <- cor.test(df_clean[[xvar]], df_clean[[yvar]])
  p_val <- signif(ct$p.value, 3)
  r_val <- signif(ct$estimate, 3)
  stat_text <- paste0("r = ", r_val, ", p = ", p_val)
  
  ggplot(df_clean, aes_string(x = xvar, y = yvar)) +
    geom_point(alpha = 0.7, color = "black") +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(x = xlab, y = ylab, title = title) +
    annotate("text", x = Inf, y = Inf, label = stat_text,
             hjust = 1.1, vjust = 1, size = 4, fontface = "italic") +
    theme_minimal(base_size = 12)
}

# Create data frames with elevation and coordinates
df_flower <- as.data.frame(stack(Changeinflowering, elev_flower), xy = TRUE, na.rm = TRUE)
names(df_flower)[3:4] <- c("Change", "Elevation")

df_fruit <- as.data.frame(stack(Changeinfruiting, elev_fruit), xy = TRUE, na.rm = TRUE)
names(df_fruit)[3:4] <- c("Change", "Elevation")

df_flower_mm <- as.data.frame(stack(Changeinfloweringmismatch, elev_flower_mm), xy = TRUE, na.rm = TRUE)
names(df_flower_mm)[3:4] <- c("Change", "Elevation")

df_fruit_mm <- as.data.frame(stack(Changeinmothmismatch, elev_fruit_mm), xy = TRUE, na.rm = TRUE)
names(df_fruit_mm)[3:4] <- c("Change", "Elevation")

# Elevation plots
p1 <- make_corr_plot(df_flower, "Elevation", "Change", "Elevation (m)", "Years", "Flowering")
p2 <- make_corr_plot(df_fruit, "Elevation", "Change", "Elevation (m)", "Years", "Fruiting")
p3 <- make_corr_plot(df_flower_mm, "Elevation", "Change", "Elevation (m)", "Years", "Flowering without Fruiting")
p4 <- make_corr_plot(df_fruit_mm, "Elevation", "Change", "Elevation (m)", "Years", "Fruiting without Flowering")

# Latitude plots (use y column from df_* data frames)
p5 <- make_corr_plot(df_flower, "y", "Change", "Latitude", "Years", "Flowering")
p6 <- make_corr_plot(df_fruit, "y", "Change", "Latitude", "Years", "Fruiting")
p7 <- make_corr_plot(df_flower_mm, "y", "Change", "Latitude", "Years", "Flowering without Fruiting")
p8 <- make_corr_plot(df_fruit_mm, "y", "Change", "Latitude", "Years", "Fruiting without Flowering")

# Combine plots into panels
elevation_panel <- (p1 / p2 / p3 / p4) 


latitude_panel <- (p5 / p6 / p7 / p8) 
 

combinedpanel <- (elevation_panel | latitude_panel) +  plot_annotation(title = "Change 1900-1929 between 1996-2025

Vs Elevation:                                                    Vs Latitude:")



#Try it diffrent way

library(dplyr)
library(tidyr)

# 1. Tag each existing df with its response variable
df_flower$variable    <- "Flowering"
df_fruit$variable     <- "Fruiting"
df_flower_mm$variable <- "Flowering without Fruiting"
df_fruit_mm$variable  <- "Fruiting without Flowering"

all_df <- bind_rows(df_flower, df_fruit, df_flower_mm, df_fruit_mm)

# 2. Reshape so Elevation and Latitude (y) become one "predictor" column
long_df <- all_df %>%
  select(Change, Elevation, y, variable) %>%
  rename(Latitude = y) %>%
  pivot_longer(cols = c(Elevation, Latitude), 
               names_to = "predictor", values_to = "x_value")

# 3. Set factor order so panels appear in the order you want
long_df$variable <- factor(long_df$variable, 
                           levels = c("Flowering", "Fruiting", 
                                      "Flowering without Fruiting", 
                                      "Fruiting without Flowering"))
long_df$predictor <- factor(long_df$predictor, levels = c("Elevation", "Latitude"))

# 4. Compute significance per facet, so only significant panels get a trend line
sig_df <- long_df %>%
  group_by(variable, predictor) %>%
  summarise(p_value = cor.test(x_value, Change)$p.value, .groups = "drop") %>%
  mutate(significant = p_value < 0.05)

long_df <- left_join(long_df, sig_df, by = c("variable", "predictor"))

# 5. Custom labels for the predictor columns (so Elevation gets "(m)" but Latitude doesn't)
predictor_labels <- c(Elevation = "Elevation (m)", Latitude = "Latitude")

# 6. Build the faceted plot

# Add a formatted p-value label to your existing sig_df
sig_df <- sig_df %>%
  mutate(p_label = paste0("p = ", signif(p_value, 3)))

# Add geom_text to the plot, pulling from sig_df instead of long_df
combinedpanel <- ggplot(long_df, aes(x = x_value, y = Change)) +
  geom_point(alpha = 0.7, color = "black") +
  geom_smooth(data = subset(long_df, significant), 
              method = "lm", se = FALSE, color = "red") +
  geom_text(data = sig_df, aes(x = Inf, y = Inf, label = p_label),
            hjust = 1.1, vjust = 1.5, size = 4, fontface = "italic",
            inherit.aes = FALSE) +
  facet_grid(variable ~ predictor, scales = "free_x", 
             labeller = labeller(predictor = predictor_labels)) +
  labs(x = NULL, y = "Years",
       title = "") +
  theme_minimal(base_size = 12) +
  theme(
    strip.text.y = element_text(angle = 90, size = 14, face = "bold"),
    strip.text.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 8),          # <-- bigger tick label text
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # <-- box around each panel
    panel.spacing = unit(0.3, "lines")             # a little breathing room between panels
  )

combinedpanel


ggsave("output/plots/combindedpanel.pdf", combinedpanel, width = 6, height = 12, dpi = 1200)


#Okay doing the same thing for mismatches but with Todds Refugia areas
#__________________________________________________________________________________________
#2100 SSP5-8.5 senerio

library(terra)

rastermask2100_8.5 <- raster(paste("data/Refugia/ssp_5_8_5_2071_2100_accessible_bin/ssp_5_8.5_2071_2100_accessible_bin.tif"))
#change in flowering in refugia
Changeinflowering
change_terra <- rast(Changeinflowering)
mask_terra <- rast(rastermask2100_8.5)

## 3. Align CRS if needed
if (!same.crs(change_terra, mask_terra)) {
  mask_terra <- project(mask_terra, change_terra)
}

## 4. Resample mask to match mismatch raster exactly
mask_resampled <- resample(mask_terra, change_terra, method = "near")

## 5. Apply the mask (values != 1 will become NA)
filtered_change <- mask(change_terra, mask_resampled, maskvalues = c(NA, 0))

## 6. Convert back to raster format if needed
filtered_change_raster8.5ciflower <- raster(filtered_change)

## 7. Prepare for plotting
change_df <- as.data.frame(filtered_change_raster8.5ciflower, xy = TRUE)
colnames(change_df) <- c("x", "y", "value")  
change_df <- na.omit(change_df)
diverging_palette <- brewer.pal(11, "RdBu")  


Changeinfloweringplotrefugia <- ggplot() +
  # Add the states and coast layers
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  
  # Add the raster layer
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  # Customize the color scale
  scale_fill_gradientn(
    colors = c("red", "white", "blue"),  # Blue for negative, white at 0, red for positive
    values = scales::rescale(c(-5, 0, 8)),  # Rescale to ensure white is at 0
    limits = c(-5, 8)  # Set limits for the color scale
  )+ 
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 20)) +
  # Add titles and theme
  labs(
    title = "20th & 21st Century Flowering Trends in 2100 SSP5-8.5 Refugia",
    x = "Longitude", y = "Latitude"
  ) +
  coord_sf(xlim = c(-119, -112.3), ylim = c(33.7, 38.2), expand = FALSE) +
  theme_minimal()

ggsave(paste("output/plots/Changeinfloweringplotrefugia_8.5",".png"), plot = Changeinfloweringplotrefugia, width = 10, height = 6, bg="white", dpi = 300)

cellStats(filtered_change_raster8.5, stat = 'mean')
#2.361446
raster_values <- values(filtered_change_raster8.5)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#2
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-3 7

total_cells <- ncell(filtered_change_raster8.5) - cellStats(is.na(filtered_change_raster8.5), 'sum')
cells_above_zero <- cellStats(filtered_change_raster8.5 > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#75.90361

total_cells <- ncell(filtered_change_raster8.5) - cellStats(is.na(filtered_change_raster8.5), 'sum')
cells_below_zero <- cellStats(filtered_change_raster8.5 < 0, 'sum', na.rm = TRUE)
percentage_below_zero <- (cells_below_zero / total_cells) * 100
percentage_below_zero
#12.4498

#change in fruiting 
Changeinfruiting
change_terra <- rast(Changeinfruiting)
mask_terra <- rast(rastermask2100_8.5)

## 3. Align CRS if needed
if (!same.crs(change_terra, mask_terra)) {
  mask_terra <- project(mask_terra, change_terra)
}

## 4. Resample mask to match mismatch raster exactly
mask_resampled <- resample(mask_terra, change_terra, method = "near")

## 5. Apply the mask (values != 1 will become NA)
filtered_change <- mask(change_terra, mask_resampled, maskvalues = c(NA, 0))

## 6. Convert back to raster format if needed
filtered_change_raster8.5cifruit <- raster(filtered_change)

## 7. Prepare for plotting
change_df <- as.data.frame(filtered_change_raster8.5cifruit, xy = TRUE)
colnames(change_df) <- c("x", "y", "value")  
change_df <- na.omit(change_df)
diverging_palette <- brewer.pal(11, "RdBu")  


Changeinfruitingplotrefugia <- ggplot() +
  # Add the states and coast layers
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  
  # Add the raster layer
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  # Customize the color scale
    scale_fill_gradientn(
      colors = c("red", "white", "blue"),  # Blue for negative, white at 0, red for positive
      values = scales::rescale(c(-12, 0, 12)),  # Rescale to ensure white is at 0
      limits = c(-12, 12)  # Set limits for the color scale
    )+ 
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 20)) +
  # Add titles and theme
  labs(
    title = "20th & 21st Century Fruiting Trends in 2100 SSP5-8.5 Refugia",
    x = "Longitude", y = "Latitude"
  ) +
  coord_sf(xlim = c(-119, -112.3), ylim = c(33.7, 38.2), expand = FALSE) +
  theme_minimal()

ggsave(paste("output/plots/Changeinfruitingplotrefugia_8.5",".png"), plot = Changeinfruitingplotrefugia, width = 10, height = 6, bg="white", dpi = 300)


cellStats(filtered_change_raster, stat = 'mean')
#-2.477912
raster_values <- values(filtered_change_raster)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#-4
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-11 8

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_above_zero <- cellStats(filtered_change_raster > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#26.90763

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_below_zero <- cellStats(filtered_change_raster < 0, 'sum', na.rm = TRUE)
percentage_below_zero <- (cells_below_zero / total_cells) * 100
percentage_below_zero
#68.27309


#change in fruiting mismatch
Changeinmothmismatch

change_terra <- rast(Changeinmothmismatch)
mask_terra <- rast(rastermask2100_8.5)

## 3. Align CRS if needed
if (!same.crs(change_terra, mask_terra)) {
  mask_terra <- project(mask_terra, change_terra)
}

## 4. Resample mask to match mismatch raster exactly
mask_resampled <- resample(mask_terra, change_terra, method = "near")

## 5. Apply the mask (values != 1 will become NA)
filtered_change <- mask(change_terra, mask_resampled, maskvalues = c(NA, 0))

## 6. Convert back to raster format if needed
filtered_change_raster8.5cimothmismatch <- raster(filtered_change)

## 7. Prepare for plotting
change_df <- as.data.frame(filtered_change_raster8.5cimothmismatch, xy = TRUE)
change_df <- na.omit(change_df)
colnames(change_df) <- c("x", "y", "value")

## 8. Plotting (your existing code)
Changeinmothmismatchrefugia <- ggplot() +
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(
    colors = c("blue", "white", "red"),  # Blue for negative, white at 0, red for positive
    values = scales::rescale(c(-12, 0, 12)),  # Rescale to ensure white is at 0
    limits = c(-12, 12)  # Set limits for the color scale
  )+ 
  labs(title = "20th & 21st Century Moth Mismatch Trends in 2100 SSP5-8.5 Refugia") +
  coord_sf(xlim = c(-119, -112), ylim = c(33, 38)) +
  theme_minimal()

ggsave(paste("output/plots/Changeinmothmismatch_8.5",".png"), plot = Changeinmothmismatchrefugia, width = 10, height = 6, bg="white", dpi = 300)

cellStats(filtered_change_raster, stat = 'mean')
#-4.285141
raster_values <- values(filtered_change_raster)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#-5
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-12.8 7

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_above_zero <- cellStats(filtered_change_raster > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#18.07229

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_below_zero <- cellStats(filtered_change_raster < 0, 'sum', na.rm = TRUE)
percentage_below_zero <- (cells_below_zero / total_cells) * 100
percentage_below_zero

#77.91165


#Changeinfloweringmismatch
Changeinfloweringmismatch
change_terra <- rast(Changeinfloweringmismatch)
mask_terra <- rast(rastermask2100_8.5)

## 3. Align CRS if needed
if (!same.crs(change_terra, mask_terra)) {
  mask_terra <- project(mask_terra, change_terra)
}

## 4. Resample mask to match mismatch raster exactly
mask_resampled <- resample(mask_terra, change_terra, method = "near")

## 5. Apply the mask (values != 1 will become NA)
filtered_change <- mask(change_terra, mask_resampled, maskvalues = c(NA, 0))

## 6. Convert back to raster format if needed
filtered_change_raster8.5ciflowermismatch <- raster(filtered_change)

## 7. Prepare for plotting
change_df <- as.data.frame(filtered_change_raster8.5ciflowermismatch, xy = TRUE)
change_df <- na.omit(change_df)
colnames(change_df) <- c("x", "y", "value")

## 8. Plotting (your existing code)
Changeinflowerismatchrefugia <- ggplot() +
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
    scale_fill_gradientn(
      colors = c("blue", "white", "red"),  # Blue for negative, white at 0, red for positive
      values = scales::rescale(c(-12, 0, 12)),  # Rescale to ensure white is at 0
      limits = c(-12, 12)  # Set limits for the color scale
    )+ 
  labs(title = "20th & 21st Century Flowering Mismatch Trends in 2100 SSP5-8.5 Refugia") +
  coord_sf(xlim = c(-119, -112), ylim = c(33, 38)) +
  theme_minimal()

ggsave(paste("output/plots/Changeinflowermismatchrefugia_8.5",".png"), plot = Changeinflowerismatchrefugia, width = 10, height = 6, bg="white", dpi = 300)

cellStats(filtered_change_raster, stat = 'mean')
#0.5542169
raster_values <- values(filtered_change_raster)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#1
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-4 5

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_above_zero <- cellStats(filtered_change_raster > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#50.60241

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_below_zero <- cellStats(filtered_change_raster < 0, 'sum', na.rm = TRUE)
percentage_below_zero <- (cells_below_zero / total_cells) * 100
percentage_below_zero
#26.50602



#______________________________________________________________

#2100 SSP3-7 senerio
#ssp_3_7_0_2071_2100_accessible_bin

rastermask2100_3_7 <- raster(paste("data/Refugia/ssp_3_7_0_2071_2100_accessible_bin/ssp_3_7.0_2071_2100_accessible_bin.tif"))
#change in flowering in refugia
Changeinflowering
change_terra <- rast(Changeinflowering)
mask_terra <- rast(rastermask2100_3_7)

## 3. Align CRS if needed
if (!same.crs(change_terra, mask_terra)) {
  mask_terra <- project(mask_terra, change_terra)
}

## 4. Resample mask to match mismatch raster exactly
mask_resampled <- resample(mask_terra, change_terra, method = "near")

## 5. Apply the mask (values != 1 will become NA)
filtered_change <- mask(change_terra, mask_resampled, maskvalues = c(NA, 0))

## 6. Convert back to raster format if needed
filtered_change_raster3_7ciflower <- raster(filtered_change)

## 7. Prepare for plotting
change_df <- as.data.frame(filtered_change_raster3_7ciflower, xy = TRUE)
colnames(change_df) <- c("x", "y", "value")  
change_df <- na.omit(change_df)
diverging_palette <- brewer.pal(11, "RdBu")  


Changeinfloweringplotrefugia <- ggplot() +
  # Add the states and coast layers
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  
  # Add the raster layer
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  # Customize the color scale
  scale_fill_gradientn(
    colors = c("red", "white", "blue"),  # Blue for negative, white at 0, red for positive
    values = scales::rescale(c(-5, 0, 8)),  # Rescale to ensure white is at 0
    limits = c(-5, 8)  # Set limits for the color scale
  )+ 
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 20)) +
  # Add titles and theme
  labs(
    title = "20th & 21st Century Flowering Trends in 2100 SSP3-7 Refugia",
    x = "Longitude", y = "Latitude"
  ) +
  coord_sf(xlim = c(-119, -112.3), ylim = c(33.7, 38.2), expand = FALSE) +
  theme_minimal()

ggsave(paste("output/plots/Changeinfloweringplotrefugia3_7",".png"), plot = Changeinfloweringplotrefugia, width = 10, height = 6, bg="white", dpi = 300)

cellStats(filtered_change_raster3_7, stat = 'mean')
#2.45
raster_values <- values(filtered_change_raster3_7)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#2
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-3 7

total_cells <- ncell(filtered_change_raster3_7) - cellStats(is.na(filtered_change_raster3_7), 'sum')
cells_above_zero <- cellStats(filtered_change_raster3_7 > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#77.11

total_cells <- ncell(filtered_change_raster3_7) - cellStats(is.na(filtered_change_raster3_7), 'sum')
cells_below_zero <- cellStats(filtered_change_raster3_7 < 0, 'sum', na.rm = TRUE)
percentage_below_zero <- (cells_below_zero / total_cells) * 100
percentage_below_zero
#12.80654

#change in fruiting 
rastermask2100_3_7 <- raster(paste("data/Refugia/ssp_3_7_0_2071_2100_accessible_bin/ssp_3_7.0_2071_2100_accessible_bin.tif"))
Changeinfruiting
change_terra <- rast(Changeinfruiting)
mask_terra <- rast(rastermask2100_3_7)

## 3. Align CRS if needed
if (!same.crs(change_terra, mask_terra)) {
  mask_terra <- project(mask_terra, change_terra)
}

## 4. Resample mask to match mismatch raster exactly
mask_resampled <- resample(mask_terra, change_terra, method = "near")

## 5. Apply the mask (values != 1 will become NA)
filtered_change <- mask(change_terra, mask_resampled, maskvalues = c(NA, 0))

## 6. Convert back to raster format if needed
filtered_change_raster3_7cifruit <- raster(filtered_change)

## 7. Prepare for plotting
change_df <- as.data.frame(filtered_change_raster3_7cifruit, xy = TRUE)
colnames(change_df) <- c("x", "y", "value")  
change_df <- na.omit(change_df)
diverging_palette <- brewer.pal(11, "RdBu")  


Changeinfruitingplotrefugia <- ggplot() +
  # Add the states and coast layers
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  
  # Add the raster layer
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  # Customize the color scale
  scale_fill_gradientn(
    colors = c("red", "white", "blue"),  # Blue for negative, white at 0, red for positive
    values = scales::rescale(c(-12, 0, 12)),  # Rescale to ensure white is at 0
    limits = c(-12, 12)  # Set limits for the color scale
  )+ 
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 20)) +
  # Add titles and theme
  labs(
    title = "20th & 21st Century Fruiting Trends in 2100 SSP3-7 Refugia",
    x = "Longitude", y = "Latitude"
  ) +
  coord_sf(xlim = c(-119, -112.3), ylim = c(33.7, 38.2), expand = FALSE) +
  theme_minimal()

ggsave(paste("output/plots/Changeinfruitingplotrefugia3-7",".png"), plot = Changeinfruitingplotrefugia, width = 10, height = 6, bg="white", dpi = 300)

cellStats(filtered_change_raster, stat = 'mean')
#-1.70
raster_values <- values(filtered_change_raster)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#-2
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-10 8

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_above_zero <- cellStats(filtered_change_raster > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#31.33515

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_below_zero <- cellStats(filtered_change_raster < 0, 'sum', na.rm = TRUE)
percentage_below_zero <- (cells_below_zero / total_cells) * 100
percentage_below_zero
#61.58038



#change in fruiting mismatch
Changeinmothmismatch

change_terra <- rast(Changeinmothmismatch)
mask_terra <- rast(rastermask2100_3_7)

## 3. Align CRS if needed
if (!same.crs(change_terra, mask_terra)) {
  mask_terra <- project(mask_terra, change_terra)
}

## 4. Resample mask to match mismatch raster exactly
mask_resampled <- resample(mask_terra, change_terra, method = "near")

## 5. Apply the mask (values != 1 will become NA)
filtered_change <- mask(change_terra, mask_resampled, maskvalues = c(NA, 0))

## 6. Convert back to raster format if needed
filtered_change_raster3_7mothmismatch <- raster(filtered_change)

## 7. Prepare for plotting
change_df <- as.data.frame(filtered_change_raster3_7mothmismatch, xy = TRUE)
change_df <- na.omit(change_df)
colnames(change_df) <- c("x", "y", "value")

## 8. Plotting (your existing code)
Changeinmothmismatchrefugia <- ggplot() +
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(
    colors = c("blue", "white", "red"),  # Blue for negative, white at 0, red for positive
    values = scales::rescale(c(-12, 0, 12)),  # Rescale to ensure white is at 0
    limits = c(-12, 12)  # Set limits for the color scale
  )+ 
  labs(title = "20th & 21st Century Moth Mismatch Trends in 2100 SSP3-7 Refugia") +
  coord_sf(xlim = c(-119, -112), ylim = c(33, 38)) +
  theme_minimal()

ggsave(paste("output/plots/Changeinmothmismatch3-7",".png"), plot = Changeinmothmismatchrefugia, width = 10, height = 6, bg="white", dpi = 300)

cellStats(filtered_change_raster, stat = 'mean')
#-3.623978
raster_values <- values(filtered_change_raster)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#-4
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-12 7

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_above_zero <- cellStats(filtered_change_raster > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#22.88828

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_below_zero <- cellStats(filtered_change_raster < 0, 'sum', na.rm = TRUE)
percentage_below_zero <- (cells_below_zero / total_cells) * 100
percentage_below_zero
#71.38965

#Changeinfloweringmismatch
Changeinfloweringmismatch

change_terra <- rast(Changeinfloweringmismatch)
mask_terra <- rast(rastermask2100_3_7)

## 3. Align CRS if needed
if (!same.crs(change_terra, mask_terra)) {
  mask_terra <- project(mask_terra, change_terra)
}

## 4. Resample mask to match mismatch raster exactly
mask_resampled <- resample(mask_terra, change_terra, method = "near")

## 5. Apply the mask (values != 1 will become NA)
filtered_change <- mask(change_terra, mask_resampled, maskvalues = c(NA, 0))

## 6. Convert back to raster format if needed
filtered_change_raster3_7cifloweringmismatch <- raster(filtered_change)

## 7. Prepare for plotting
change_df <- as.data.frame(filtered_change_raster3_7cifloweringmismatch, xy = TRUE)
change_df <- na.omit(change_df)
colnames(change_df) <- c("x", "y", "value")

## 8. Plotting (your existing code)
Changeinflowerismatchrefugia <- ggplot() +
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(
    colors = c("blue", "white", "red"),  # Blue for negative, white at 0, red for positive
    values = scales::rescale(c(-12, 0, 12)),  # Rescale to ensure white is at 0
    limits = c(-12, 12)  # Set limits for the color scale
  )+ 
  labs(title = "20th & 21st Century Flowering Mismatch Trends in 2100 SSP3-7 Refugia") +
  coord_sf(xlim = c(-119, -112), ylim = c(33, 38)) +
  theme_minimal()

ggsave(paste("output/plots/Changeinflowermismatchrefugia3-7",".png"), plot = Changeinflowerismatchrefugia, width = 10, height = 6, bg="white", dpi = 300)

cellStats(filtered_change_raster, stat = 'mean')
#0.5258856
raster_values <- values(filtered_change_raster)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#0
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-5 5

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_above_zero <- cellStats(filtered_change_raster > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#49.59128

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_below_zero <- cellStats(filtered_change_raster < 0, 'sum', na.rm = TRUE)
percentage_below_zero <- (cells_below_zero / total_cells) * 100
percentage_below_zero
#31.06267


#2100 SSP3-7 senerio
#ssp_2_4_5_2071_2100_accessible_bin

rastermask21002_4.5 <- raster(paste("data/Refugia/ssp_2_4_5_2071_2100_accessible_bin/ssp_2_4.5_2071_2100_accessible_bin.tif"))
#change in flowering in refugia
Changeinflowering
change_terra <- rast(Changeinflowering)
mask_terra <- rast(rastermask21002_4.5)

## 3. Align CRS if needed
if (!same.crs(change_terra, mask_terra)) {
  mask_terra <- project(mask_terra, change_terra)
}

## 4. Resample mask to match mismatch raster exactly
mask_resampled <- resample(mask_terra, change_terra, method = "near")

## 5. Apply the mask (values != 1 will become NA)
filtered_change <- mask(change_terra, mask_resampled, maskvalues = c(NA, 0))

## 6. Convert back to raster format if needed
filtered_change_raster4.5ciflowering <- raster(filtered_change)

## 7. Prepare for plotting
change_df <- as.data.frame(filtered_change_raster4.5ciflowering, xy = TRUE)
colnames(change_df) <- c("x", "y", "value")  
change_df <- na.omit(change_df)
diverging_palette <- brewer.pal(11, "RdBu")  


Changeinfloweringplotrefugia <- ggplot() +
  # Add the states and coast layers
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  
  # Add the raster layer
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  # Customize the color scale
  scale_fill_gradientn(
    colors = c("red", "white", "blue"),  # Blue for negative, white at 0, red for positive
    values = scales::rescale(c(-5, 0, 8)),  # Rescale to ensure white is at 0
    limits = c(-5, 8)  # Set limits for the color scale
  )+ 
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 20)) +
  # Add titles and theme
  labs(
    title = "20th & 21st Century Flowering Trends in 2100 SSP2-4.5 Refugia",
    x = "Longitude", y = "Latitude"
  ) +
  coord_sf(xlim = c(-119, -112.3), ylim = c(33.7, 38.2), expand = FALSE) +
  theme_minimal()

ggsave(paste("output/plots/Changeinfloweringplotrefugia2_4.5",".png"), plot = Changeinfloweringplotrefugia, width = 10, height = 6, bg="white", dpi = 300)

cellStats(filtered_change_raster, stat = 'mean')
#2.588015
raster_values <- values(filtered_change_raster)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#2
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-2 7

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_above_zero <- cellStats(filtered_change_raster > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#80.02497

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_below_zero <- cellStats(filtered_change_raster < 0, 'sum', na.rm = TRUE)
percentage_below_zero <- (cells_below_zero / total_cells) * 100
percentage_below_zero
#8.2397


#change in fruiting 

Changeinfruiting
change_terra <- rast(Changeinfruiting)
mask_terra <- rast(rastermask21002_4.5)

## 3. Align CRS if needed
if (!same.crs(change_terra, mask_terra)) {
  mask_terra <- project(mask_terra, change_terra)
}

## 4. Resample mask to match mismatch raster exactly
mask_resampled <- resample(mask_terra, change_terra, method = "near")

## 5. Apply the mask (values != 1 will become NA)
filtered_change <- mask(change_terra, mask_resampled, maskvalues = c(NA, 0))

## 6. Convert back to raster format if needed
filtered_change_raster4.5cifruiting <- raster(filtered_change)

## 7. Prepare for plotting
change_df <- as.data.frame(filtered_change_raster4.5cifruiting, xy = TRUE)
colnames(change_df) <- c("x", "y", "value")  
change_df <- na.omit(change_df)
diverging_palette <- brewer.pal(11, "RdBu")  


Changeinfruitingplotrefugia <- ggplot() +
  # Add the states and coast layers
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  
  # Add the raster layer
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  # Customize the color scale
  scale_fill_gradientn(
    colors = c("red", "white", "blue"),  # Blue for negative, white at 0, red for positive
    values = scales::rescale(c(-12, 0, 12)),  # Rescale to ensure white is at 0
    limits = c(-12, 12)  # Set limits for the color scale
  )+ 
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 20)) +
  # Add titles and theme
  labs(
    title = "20th & 21st Century Fruiting Trends in 2100 SSP2-4.5 Refugia",
    x = "Longitude", y = "Latitude"
  ) +
  coord_sf(xlim = c(-119, -112.3), ylim = c(33.7, 38.2), expand = FALSE) +
  theme_minimal()

ggsave(paste("output/plots/Changeinfruitingplotrefugia2-4.5",".png"), plot = Changeinfruitingplotrefugia, width = 10, height = 6, bg="white", dpi = 300)

cellStats(filtered_change_raster, stat = 'mean')
#-1.400749
raster_values <- values(filtered_change_raster)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#-2
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-10 8

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_above_zero <- cellStats(filtered_change_raster > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#33.70787

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_below_zero <- cellStats(filtered_change_raster < 0, 'sum', na.rm = TRUE)
percentage_below_zero <- (cells_below_zero / total_cells) * 100
percentage_below_zero
#58.05243

#change in fruiting mismatch
Changeinmothmismatch

change_terra <- rast(Changeinmothmismatch)
mask_terra <- rast(rastermask21002_4.5)

## 3. Align CRS if needed
if (!same.crs(change_terra, mask_terra)) {
  mask_terra <- project(mask_terra, change_terra)
}

## 4. Resample mask to match mismatch raster exactly
mask_resampled <- resample(mask_terra, change_terra, method = "near")

## 5. Apply the mask (values != 1 will become NA)
filtered_change <- mask(change_terra, mask_resampled, maskvalues = c(NA, 0))

## 6. Convert back to raster format if needed
filtered_change_raster4.5cimothmismatch <- raster(filtered_change)

## 7. Prepare for plotting
change_df <- as.data.frame(filtered_change_raster4.5cimothmismatch, xy = TRUE)
change_df <- na.omit(change_df)
colnames(change_df) <- c("x", "y", "value")

## 8. Plotting (your existing code)
Changeinmothmismatchrefugia <- ggplot() +
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(
    colors = c("blue", "white", "red"),  # Blue for negative, white at 0, red for positive
    values = scales::rescale(c(-12, 0, 12)),  # Rescale to ensure white is at 0
    limits = c(-12, 12)  # Set limits for the color scale
  )+ 
  labs(title = "20th & 21st Century Moth Mismatch Trends in 2100 SSP2-4.5 Refugia") +
  coord_sf(xlim = c(-119, -112), ylim = c(33, 38)) +
  theme_minimal()

ggsave(paste("output/plots/Changeinmothmismatch2-4.5",".png"), plot = Changeinmothmismatchrefugia, width = 10, height = 6, bg="white", dpi = 300)

cellStats(filtered_change_raster, stat = 'mean')
#-3.059925
raster_values <- values(filtered_change_raster)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#-3
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-12 6

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_above_zero <- cellStats(filtered_change_raster > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#23.22097

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_below_zero <- cellStats(filtered_change_raster < 0, 'sum', na.rm = TRUE)
percentage_below_zero <- (cells_below_zero / total_cells) * 100
percentage_below_zero
#69.28839



#Changeinfloweringmismatch
Changeinfloweringmismatch

change_terra <- rast(Changeinfloweringmismatch)
mask_terra <- rast(rastermask21002_4.5)

## 3. Align CRS if needed
if (!same.crs(change_terra, mask_terra)) {
  mask_terra <- project(mask_terra, change_terra)
}

## 4. Resample mask to match mismatch raster exactly
mask_resampled <- resample(mask_terra, change_terra, method = "near")

## 5. Apply the mask (values != 1 will become NA)
filtered_change <- mask(change_terra, mask_resampled, maskvalues = c(NA, 0))

## 6. Convert back to raster format if needed
filtered_change_raster4.5cifloweringmismatch <- raster(filtered_change)

## 7. Prepare for plotting
change_df <- as.data.frame(filtered_change_raster4.5cifloweringmismatch, xy = TRUE)
change_df <- na.omit(change_df)
colnames(change_df) <- c("x", "y", "value")

## 8. Plotting (your existing code)
Changeinflowerismatchrefugia <- ggplot() +
  geom_sf(data = coast, fill = "red", color = "black", size = 0.3) +
  geom_sf(data = states, fill = "tan", color = "black", size = 0.5) +
  geom_raster(data = change_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(
    colors = c("blue", "white", "red"),  # Blue for negative, white at 0, red for positive
    values = scales::rescale(c(-12, 0, 12)),  # Rescale to ensure white is at 0
    limits = c(-12, 12)  # Set limits for the color scale
  )+ 
  labs(title = "20th & 21st Century Flowering Mismatch Trends in 2100 SSP2-4.5 Refugia") +
  coord_sf(xlim = c(-119, -112), ylim = c(33, 38)) +
  theme_minimal()

ggsave(paste("output/plots/Changeinflowermismatchrefugia2-4.5",".png"), plot = Changeinflowerismatchrefugia, width = 10, height = 6, bg="white", dpi = 300)

cellStats(filtered_change_raster, stat = 'mean')
#0.928839

raster_values <- values(filtered_change_raster)
na.omit(raster_values)
median(raster_values, na.rm = TRUE)
#1
quantile(raster_values, c(0.025, 0.975), na.rm = TRUE)
#-4 6

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_above_zero <- cellStats(filtered_change_raster > 0, 'sum', na.rm = TRUE)
percentage_above_zero <- (cells_above_zero / total_cells) * 100
percentage_above_zero
#53.55805

total_cells <- ncell(filtered_change_raster) - cellStats(is.na(filtered_change_raster), 'sum')
cells_below_zero <- cellStats(filtered_change_raster < 0, 'sum', na.rm = TRUE)
percentage_below_zero <- (cells_below_zero / total_cells) * 100
percentage_below_zero
#24.46941
###
#ploting Climate scenerios as box plots


# ── 1. Flowering ──────────────────────────────────────────────
raster1 <- Changeinflowering
raster2 <- filtered_change_raster8.5ciflower
raster3 <- filtered_change_raster3_7ciflower
raster4 <- filtered_change_raster4.5ciflowering

df <- data.frame(
  values = c(values(raster1), values(raster4), values(raster3), values(raster2)),
  raster = factor(rep(c("Whole Range", "Medium Emissions Refugia", "Low Emissions Refugia", "High Emissions Refugia"),
                      times = c(ncell(raster1), ncell(raster4), ncell(raster3), ncell(raster2))),
                  levels = c("Whole Range", "Low Emissions Refugia", "Medium Emissions Refugia", "High Emissions Refugia"))
)
df <- na.omit(df)

flowering_change_plot <- ggplot(df, aes(x = raster, y = values, fill = raster)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Whole Range" = "#8B4513",
                               "Medium Emissions Refugia" = "#009E73",
                               "Low Emissions Refugia" = "#0072B2",
                               "High Emissions Refugia" = "#D55E00")) +
  labs(title = "Change in flowering years", x = "", y = "Change in Years", fill = "IPCC Climate Scenario") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave(filename = "output/plots/Flowering_Change_Boxplot.png",
       plot = flowering_change_plot,
       width = 3, height = 6, dpi = 300, bg = "transparent")

# ── 2. Fruiting ───────────────────────────────────────────────
raster1 <- Changeinfruiting
raster2 <- filtered_change_raster8.5cifruit
raster3 <- filtered_change_raster3_7cifruit
raster4 <- filtered_change_raster4.5cifruiting

df <- data.frame(
  values = c(values(raster1), values(raster4), values(raster3), values(raster2)),
  raster = factor(rep(c("Whole Range", "Medium Emissions Refugia", "Low Emissions Refugia", "High Emissions Refugia"),
                      times = c(ncell(raster1), ncell(raster4), ncell(raster3), ncell(raster2))),
                  levels = c("Whole Range", "Low Emissions Refugia", "Medium Emissions Refugia", "High Emissions Refugia"))
)
df <- na.omit(df)

fruiting_change_plot <- ggplot(df, aes(x = raster, y = values, fill = raster)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Whole Range" = "#8B4513",
                               "Medium Emissions Refugia" = "#009E73",
                               "Low Emissions Refugia" = "#0072B2",
                               "High Emissions Refugia" = "#D55E00")) +
  labs(title = "Change in fruiting years", x = "", y = "Change in Years", fill = "IPCC Climate Scenario") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave(filename = "output/plots/Fruiting_Change_Boxplot.png",
       plot = fruiting_change_plot,
       width = 3, height = 6, dpi = 300, bg = "transparent")

# ── 3. Flowering mismatch ─────────────────────────────────────
raster1 <- Changeinfloweringmismatch
raster2 <- filtered_change_raster8.5ciflowermismatch
raster3 <- filtered_change_raster3_7cifloweringmismatch
raster4 <- filtered_change_raster4.5cifloweringmismatch

df <- data.frame(
  values = c(values(raster1), values(raster4), values(raster3), values(raster2)),
  raster = factor(rep(c("Whole Range", "Medium Emissions Refugia", "Low Emissions Refugia", "High Emissions Refugia"),
                      times = c(ncell(raster1), ncell(raster4), ncell(raster3), ncell(raster2))),
                  levels = c("Whole Range", "Low Emissions Refugia", "Medium Emissions Refugia", "High Emissions Refugia"))
)
df <- na.omit(df)

floweringmismatch_change_plot <- ggplot(df, aes(x = raster, y = values, fill = raster)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Whole Range" = "#8B4513",
                               "Medium Emissions Refugia" = "#009E73",
                               "Low Emissions Refugia" = "#0072B2",
                               "High Emissions Refugia" = "#D55E00")) +
  labs(title = "Change in Flowering without fruiting", x = "", y = "Change in Years", fill = "IPCC Climate Scenario") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave(filename = "output/plots/Floweringmismatch_Change_Boxplot.png",
       plot = floweringmismatch_change_plot,
       width = 3, height = 6, dpi = 300, bg = "transparent")

# ── 4. Moth mismatch (fruiting, no flowering) ─────────────────
raster1 <- Changeinmothmismatch
raster2 <- filtered_change_raster8.5cimothmismatch
raster3 <- filtered_change_raster3_7mothmismatch
raster4 <- filtered_change_raster4.5cimothmismatch

df <- data.frame(
  values = c(values(raster1), values(raster4), values(raster3), values(raster2)),
  raster = factor(rep(c("Whole Range", "Medium Emissions Refugia", "Low Emissions Refugia", "High Emissions Refugia"),
                      times = c(ncell(raster1), ncell(raster4), ncell(raster3), ncell(raster2))),
                  levels = c("Whole Range", "Low Emissions Refugia", "Medium Emissions Refugia", "High Emissions Refugia"))
)
df <- na.omit(df)

mothmismatch_change_plot <- ggplot(df, aes(x = raster, y = values, fill = raster)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Whole Range" = "#8B4513",
                               "Medium Emissions Refugia" = "#009E73",
                               "Low Emissions Refugia" = "#0072B2",
                               "High Emissions Refugia" = "#D55E00")) +
  labs(title = "Change in fruiting without flowering", x = "", y = "Change in Years", fill = "IPCC Climate Scenario") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave(filename = "output/plots/Mothmismatch_Change_Boxplot.png",
       plot = mothmismatch_change_plot,
       width = 3, height = 6, dpi = 300, bg = "transparent")

# ── 5. Combine into one figure ────────────────────

p1 <- flowering_change_plot
p2 <- fruiting_change_plot          + theme(axis.title.y = element_blank())
p3 <- floweringmismatch_change_plot + theme(axis.title.y = element_blank())
p4 <- mothmismatch_change_plot      + theme(axis.title.y = element_blank())

combined_plot <- (p1 | p2 | p3 | p4) +
  plot_layout(nrow = 1, guides = "collect") +      # nrow = 2 for a 2x2 grid instead
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10, face = "bold"))

ggsave(filename = "output/plots/Combined_Phenology_Change_Figure.pdf",
       plot = combined_plot,
       width = 12, height = 6, dpi = 300, bg = "white")



####Plotting Partials of both Moth and Trees _____________________________________________________________
# Load Partials


jotr.preds.fruit.nto <- c("vpdminY1Q3" ,"tmaxY0Y1"   ,"vpdminY1"  , "pptY1Q3"  ,  "vpdmaxY1Q4", "tmaxY1Q4")
pfrunto <- read_rds("output/BART/bart.fruitNTO.model.Jotr.partials.rds")

jotr.preds.flower.nto <- c("pptY1Y2"  ,  "tminY0Y1"  , "pptY0Y1" ,  "pptY1Q3"   , "vpdminY1" ,  "tminY1Q4")
pflnto <- read_rds("output/BART/bart.flowerNTO.model.Jotr.partials.rds")


# ---- 1. Math-formatted variable title helper ----
library(ggplot2)
library(cowplot)
library(dplyr)
library(stringr)
library(ggtext)

# ---- 1. Variable name conversion ----
convert_var_names_expr <- function(var_name) {
  switch(var_name,
         "pptY1Y2"    = bquote(bold(Delta[Y1-2] ~ PPT)),
         "pptY0Y1"    = bquote(bold(Delta[Y0-1] ~ PPT)),
         "tmaxY0Y1"   = bquote(bold(Delta[Y0-1] ~ MaxTemp)),
         "tminY0Y1"   = bquote(bold(Delta[Y0-1] ~ MinTemp)),
         "vpdminY0Y1" = bquote(bold(Delta[Y0-1] ~ MinVPD)),
         "vpdmaxY0Y1" = bquote(bold(Delta[Y0-1] ~ MaxVPD)),
         
         "pptY1Q3"    = bquote(bold(PPT[Y0Q3])),
         "pptY1Q4"    = bquote(bold(PPT[Y0Q4])),
         "pptY0Q1"    = bquote(bold(PPT[Y0Q1])),
         "pptY1"      = bquote(bold(PPT[Y1])),
         "pptY2"      = bquote(bold(PPT[Y2])),
         "tmaxY1Q4"   = bquote(bold(MaxTemp[Y0Q4])),
         "tminY1Q4"   = bquote(bold(MinTemp[Y0Q4])),
         "vpdminY1Q3" = bquote(bold(MinVPD[Y0Q3])),
         "vpdminY1Q4" = bquote(bold(MinVPD[Y0Q4])),
         "vpdmaxY1Q4" = bquote(bold(MaxVPD[Y0Q4])),
         "vpdminY1"   = bquote(bold(MinVPD[Y1])),
         "vpdminY0"   = bquote(bold(MinVPD[Y0])),
         "tminY1"     = bquote(bold(MinTemp[Y1])),
         var_name
  )
}

# ---- 2. X-axis label helper ----
get_x_axis_label <- function(var_name) {
  if (grepl("ppt", var_name, ignore.case = TRUE)) {
    "mm"
  } else if (grepl("vpd", var_name, ignore.case = TRUE)) {
    "hPa"
  } else if (grepl("tmin|tmax", var_name, ignore.case = TRUE)) {
    "°C"
  } else {
    "Value"
  }
}

# ---- 3. Variables ----
shared_vars <- intersect(jotr.preds.fruit.nto, jotr.preds.flower.nto)
fruit_unique <- setdiff(jotr.preds.fruit.nto, jotr.preds.flower.nto)
flower_unique <- setdiff(jotr.preds.flower.nto, jotr.preds.fruit.nto)

# Interleave unique vars: flowering (left column) then fruiting (right column),
# one pair per row, so plot_grid(ncol = 2) puts purple on the left, green on the right
unique_pairs <- c(rbind(flower_unique, fruit_unique))
unique_pairs <- unique_pairs[!is.na(unique_pairs)]  # guard against unequal lengths

all_vars <- c(shared_vars, unique_pairs)

fruit_indices <- setNames(seq_along(jotr.preds.fruit.nto), jotr.preds.fruit.nto)
flower_indices <- setNames(seq_along(jotr.preds.flower.nto), jotr.preds.flower.nto)

# ---- 4. Plotting function ----
# show_y_labels = FALSE strips the y-axis title/text (used for right-column plots)
create_flower_fruit_plot <- function(var_name, show_y_labels = TRUE) {
  in_fruit <- var_name %in% jotr.preds.fruit.nto
  in_flower <- var_name %in% jotr.preds.flower.nto
  x_label <- get_x_axis_label(var_name)
  title_label <- convert_var_names_expr(var_name)
  
  if (in_fruit && in_flower) {
    fruit_df <- pfrunto[[fruit_indices[var_name]]]$data
    flower_df <- pflnto[[flower_indices[var_name]]]$data
    
    combined_df <- data.frame(
      x = c(fruit_df$x, flower_df$x),
      y = c(fruit_df$med, flower_df$med),
      ymin = c(fruit_df$q05, flower_df$q05),
      ymax = c(fruit_df$q95, flower_df$q95),
      source = rep(c("Fruiting", "Flowering"), times = c(nrow(fruit_df), nrow(flower_df)))
    )
    
    p <- ggplot(combined_df, aes(x = x)) +
      geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = source), alpha = 0.4) +
      geom_line(aes(y = y, color = source), size = 1.2) +
      scale_color_manual(values = c("Fruiting" = "#1b9e77", "Flowering" = "#7570b3")) +
      scale_fill_manual(values = c("Fruiting" = "#1b9e77", "Flowering" = "#7570b3"))
    
  } else if (in_flower) {
    df <- pflnto[[flower_indices[var_name]]]$data
    
    p <- ggplot(df, aes(x = x)) +
      geom_ribbon(aes(ymin = q05, ymax = q95), fill = "#7570b3", alpha = 0.4) +
      geom_line(aes(y = med), color = "#7570b3", size = 1.2)
    
  } else if (in_fruit) {
    df <- pfrunto[[fruit_indices[var_name]]]$data
    
    p <- ggplot(df, aes(x = x)) +
      geom_ribbon(aes(ymin = q05, ymax = q95), fill = "#1b9e77", alpha = 0.4) +
      geom_line(aes(y = med), color = "#1b9e77", size = 1.2)
  }
  
  p <- p +
    labs(title = title_label, x = x_label, y = "Response") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 28, face = "bold"),
      axis.title.x = element_text(size = 24, face = "bold"),
      axis.title.y = element_text(size = 24, face = "bold"),
      axis.text.x = element_text(size = 24, face = "bold"),
      axis.text.y = element_text(size = 16, face = "bold"),
      legend.position = "none",
      plot.margin = margin(t = 15, r = 15, b = 25, l = 15),
      plot.background = element_rect(fill = "white", color = "grey50", linewidth = 0.8)
    )
  
  if (!show_y_labels) {
    p <- p + theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )
  }
  
  return(p)
}

# ---- 5. Generate all plots ----
n_cols <- 2
# Left column = odd positions in all_vars, right column = even positions
show_y_flags <- (seq_along(all_vars) %% n_cols) == 1

all_comparison_plots <- Map(
  create_flower_fruit_plot,
  var_name = all_vars,
  show_y_labels = show_y_flags
)
names(all_comparison_plots) <- all_vars

# ---- 6. Legend ----
comparison_legend_plot <- ggplot(data.frame(
  source = c("Flowering","Fruiting"),
  x = 2:1, y = 2:1
)) +
  geom_line(aes(x, y, color = source), linewidth = 4) +  # thicker legend lines
  scale_color_manual(values = c("Fruiting" = "#1b9e77", "Flowering" = "#7570b3")) +
  guides(color = guide_legend(override.aes = list(linewidth = 6))) +  # extra-bold key swatches
  theme_minimal(base_size = 30) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 28, face = "bold"),
    legend.key.size = unit(1.5, "cm"),           # bigger legend key area  # visible box around each key
    legend.spacing.x = unit(1, "cm")
  )

comparison_legend <- get_legend(comparison_legend_plot)

# ---- 7. Plot grid ----
comparison_plot_grid <- plot_grid(
  plotlist = all_comparison_plots,
  ncol = n_cols,
  labels = "AUTO",
  label_size = 24,  # Larger label size
  label_fontface = "bold"  # Bold labels
)

# ---- 8. Final layout ----
final_comparison_plot <- plot_grid(
  comparison_plot_grid,
  comparison_legend,
  ncol = 1,
  rel_heights = c(1, 0.05)
) +
  theme(plot.background = element_rect(fill = "white", color = NA))

# ---- 9. Save ----
ggsave(
  "C:/Users/13073/Desktop/Masters/Thesis/Jotr_phenology-main/Jotr_phenology-main/scripts/output/plots/Partials/all_fruit_flower_comparison.png",
  final_comparison_plot,
  width = 20,
  height = 5 * ceiling(length(all_vars) / n_cols),
  dpi = 300,
  bg = "white"
)

#move to validation_records_analysis for validation code of results