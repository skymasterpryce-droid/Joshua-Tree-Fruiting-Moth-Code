#Script 8 
#Comparing predicted historical flowering to independent validation records
#Assumes local environment
#PWM 8/24/2026

#setwd("C:/Users/13073/Desktop/Masters/Thesis/Jotr_phenology-main/Jotr_phenology-main/scripts")

# starting up ------------------------------------------------------------



library("tidyverse")
library("raster")
library("sp")
library("sf")
library("hexbin")
library("embarcadero")
library(lubridate)
library(readxl)
library(raster)
library(dplyr)
library(stringr)



#-------------------------------------------------------------------------
# load up and organize historical data

# Jotr predicted flowering

jotr.histStack <- raster::stack("output/BART/jotr_BART_predicted_flowering_1900-2023_nomask.grd")
jotr.ri.histStack <- raster::stack("output/BART/jotr_BART_RI_predicted_flowering_1900-2023_nomask.grd")

yubr.histStack <- raster::stack("output/BART/YUBR_BART_predicted_flowering_1900-2023_nomask.grd")
yuja.histStack <- raster::stack("output/BART/YUJA_BART_predicted_flowering_1900-2023_nomask.grd")


# expert validation observations
vobs <- read.csv("data/Validation_obs_by_spp.csv")

glimpse(vobs)

table(vobs$obs_by)


#-------------------------------------------------------------------------
# Incorporating NPN records

# US NPN data
npn <- read_csv("data/Validation data/NPN data.csv")

glimpse(npn)

table(npn$Partner_Group) # JTNP is the single external source
length(unique(npn$Site_ID)) # 16 unique sites
length(unique(paste(npn$Longitude, npn$Latitude))) # confirm 16 locations at resolution of this data

table(year(ymd(npn$Observation_Date)), npn$Site_ID) # records for 13 years, 2011-2023
sum(table(year(ymd(npn$Observation_Date)), npn$Site_ID)>0) # works out to 108 location-year combos

table(npn$Phenophase_Description) # ONLY flowers or fruits recorded

table(npn$Intensity_Category_ID)
# 23 = fruits present
# 24 = ripe fruits present
# 25 = recent fruit drop
# 31 = open flowers (peak)
# 35 = flowers or flower heads present
# 48 = flowers and flower buds 
# 50 = open flowers percentage (individual)
# 56 = fruits present
# 58 = ripe fruit percentage
# 59 = recent fruit or seed drop


filter(npn, year(ymd(Observation_Date))==2011, Site_ID==5389) %>% dplyr::select(Observation_Date, Phenophase_Description)


sdm.pres <- read_sf("data/Yucca/jotr_BART_sdm_pres/jotr_BART_sdm_pres.shp")
ggplot() + geom_sf(data=sdm.pres) + geom_point(data=npn, aes(x=Longitude, y=Latitude))

filter(npn, Longitude < -122) # HUH
filter(npn, Longitude > -113) %>% group_by(year(ymd(Observation_Date))) %>% summarize(n=length(Observation_Date))
filter(npn, Longitude > -113, year(ymd(Observation_Date))==2018) %>% group_by(Phenophase_Description) %>% summarize(n=length(Observation_Date))

# okay, let's try this ... natural range records only
npn.vobs <- npn %>% filter(Longitude > -120, Longitude < -114) %>% 
				   mutate(year = year(ymd(Observation_Date))) %>% 
				   filter(!duplicated(paste(Site_ID, year))) %>%
				   dplyr::select(Longitude, Latitude, Site_ID, year) %>%
				   rename(lon=Longitude, lat=Latitude, location=Site_ID) %>%
				   mutate(type="YUBR", obs_by="NPN", obs_flowers=TRUE, obs_fruit=TRUE, obs_moths=NA, obs_no_flowers=NA)

npn.vobs <- npn.vobs %>% dplyr::select(lon, lat, year, type, obs_flowers, obs_fruit)

glimpse(npn.vobs)

#fruit
flow <- read.csv("output/fruiting_obs_climate_subsp.csv") # flowering/fruiting, gridded and annualized
flow2 <- flow %>% filter(!(year==2018.5 & flr==TRUE), year>=2008) %>% mutate(year=floor(year)) # drop the late-flowering anomaly

#flower 
flowerflow <- read.csv("output/flowering_obs_climate_subsp.csv")
flowerflow2 <- flowerflow %>% filter(!(year==2018.5 & flr==TRUE), year>=2008) %>% mutate(year=floor(year)) # drop the late-flowering anomaly

#__________________________________________________________________________
#incorporating field observations but i need to psudo rasterze them
#lon, lat, year, type, obs_flowers, obs_fruit
field <- read.csv("data/JT_Transect_Data.csv")
glimpse(field)

field <- field[-c(40,41,42,50,51,52,174:198), ]

field$First.Trip<- as.Date(field$First.Trip, "%m/%d/%Y")
field$First.Trip <- as.numeric(format(field$First.Trip, "%Y"))

field <- field %>% dplyr::select(First.Trip, Species, latitude, longitude, Flowers, Fruits.1, Moths)

field <- field %>%
  mutate(Flowers = Flowers >= 1)
field <- field %>%
  mutate(Fruits.1 = Fruits.1 >= 1)

field <- field %>%
  mutate(Fruits.1 = if_else(Moths == 1, TRUE, Fruits.1, missing = Fruits.1))

field <- field %>%
  mutate(Flowers = if_else(Fruits.1, TRUE, Flowers))

field <- field %>%
  mutate(
    Species = case_when(
      Species == "Yucca jagerina" ~ "YUJA",
      Species == "Yucca brevifolia" ~ "YUBR",
      TRUE ~ Species
    )
  )

field <- field %>% dplyr::select(-Moths) %>%
  rename(year=First.Trip, type=Species, lat=latitude, lon=longitude, 
         obs_fruit=Fruits.1, obs_flowers=Flowers)

field.vobs <- field

#great I think that is all righ now i hope
#-------------------------------------------------------------------------
# Incorporating herbarium records

# Cal Consortium of Herbaria
herberium <- read.csv("data/cal herb data.csv")
glimpse(herberium)
table(herberium$eventDate)
table(herberium$reproductiveCondition)

herberium <- herberium %>%
  mutate(
    year = case_when(
      # dates like "1882-05-13" or "1853-00-00"
      str_detect(eventDate, "^[0-9]{4}-") ~ as.integer(str_sub(eventDate, 1, 4)),
      # dates like "4/25/1982" or "10/4/1935"
      str_detect(eventDate, "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}") ~
        as.integer(str_match(eventDate, "([0-9]{4})$")[,2]),
      TRUE ~ NA_integer_
    )
  )




herberium <- herberium %>% dplyr::select(scientificName,year,obs_flowers,obs_fruit,decimalLatitude,decimalLongitude,coordinateUncertaintyInMeters)

herberium$eventDate<- as.Date(herberium$eventDate, "%m/%d/%Y")

herberium <- herberium %>% filter(year>=1900, coordinateUncertaintyInMeters<=4000| is.na(coordinateUncertaintyInMeters)) %>%
  filter(!is.na(decimalLatitude)) 

herberium <- herberium %>%
  mutate(
    type        = scientificName,
    obs_flowers = as.logical(obs_flowers),
    obs_fruit   = as.logical(obs_fruit)
  ) %>%
  dplyr::select(
    year,
    type,
    lat  = decimalLatitude,
    lon  = decimalLongitude,
    obs_flowers,
    obs_fruit
  )

herberium <- herberium %>%
  filter(obs_flowers == TRUE)

herberium <- herberium %>%
  mutate(obs_fruit = ifelse(obs_fruit == FALSE, NA, obs_fruit))

table(herberium$obs_flowers)
table(herberium$obs_fruit)

#herberium
#___________________________________________________________

#and finallay the couple moth reccoreds we have I will just put obs_fruit true cause we just saw moths

MothRec <- read_csv("data/Moth records.csv")
MothRec <- MothRec %>% dplyr::select(Lat, lon, `Event date`, `Scientific name`)

MothRec <- MothRec %>%
  rename(
    lat = Lat,
    type = `Scientific name`
  ) %>%
  mutate(
    obs_flowers = NA,
    obs_fruit = TRUE,
    year = as.integer(str_extract(`Event date`, "^\\d{4}"))
  ) %>% 
  dplyr::select(
    year,
    type,
    lat,
    lon, 
    obs_flowers,
    obs_fruit
  )


#-------------------------------------------------------------------------
# Merge validation sources
old_vobs<- read_csv("output/Jyoder_Expert_validations.csv")
glimpse(old_vobs)
#view(old_vobs)
old_vobs_simplified <- old_vobs %>%
  filter(!obs_by %in% c()) %>%
  mutate(
    obs_fruit = na_if(obs_fruit, FALSE),
    obs_fruit = if_else(obs_moths == TRUE, TRUE, obs_fruit),
  ) %>%
  dplyr::select(lon, lat, year, type, obs_flowers, obs_fruit)

#view(old_vobs_simplified)
#add in all the other sources
vobs.all1 <- bind_rows(npn.vobs, field.vobs, herberium, MothRec)
# Step 2: Combine with vobs.all
vobs.all <- bind_rows(vobs.all1, old_vobs_simplified)

#vobs.all <- rbind(vobs, npn.vobs) %>% mutate(flr=(obs_flowers | obs_fruit)) %>% rbind(herb.vobs) # %>% rbind(mdep.vobs) #%>% rbind(inatEarly) # %>% rbind(inat)


glimpse(vobs.all)
table(vobs.all$obs_flowers, vobs.all$year)

write.csv(vobs.all, file= ("data/Validation data/allvalidationdata.csv"))


#okayy
table(vobs.all$obs_flowers, vobs.all$year)



#-------------------------------------------------------------------------
# List and stack BART binary flowering raster files
jotr.files <- list.files("output/BART/predictions/BART_binary_flowering", pattern = ".bil$", full.names = TRUE)
jotr.rasters <- stack(jotr.files)

# Assign year names based on filename
layer_years <- as.integer(str_extract(jotr.files, "\\d{4}"))
names(jotr.rasters) <- paste0("yr", layer_years)


valid <- NULL  # To collect matched data

for (yr in unique(vobs.all$year)) {
  vsub <- filter(vobs.all, year == yr)
  
  # Find matching raster layer
  layer_name <- paste0("yr", yr)
  
  if (layer_name %in% names(jotr.rasters)) {
    # Extract predicted flowering (0/1) for field obs locations
    vsub$predicted_flowering <- raster::extract(jotr.rasters[[layer_name]], vsub[, c("lon", "lat")])
    valid <- rbind(valid, vsub)
  } else {
    warning(paste("Year", yr, "not found in raster stack — skipping"))
  }
}

# Keep only valid comparisons (i.e., no NA predictions)
valid <- filter(valid, !is.na(predicted_flowering))
valid <- filter(valid, !is.na(obs_flowers))
# Convert logical obs to numeric for comparison (TRUE/FALSE → 1/0)
valid$obs_flowers_num <- as.numeric(valid$obs_flowers)

# Confusion matrix
table(Predicted = valid$predicted_flowering, Observed = valid$obs_flowers_num)

accuracy <- mean(valid$predicted_flowering == valid$obs_flowers_num)

# Optional: precision, recall, F1 score
tp <- sum(valid$predicted_flowering == 1 & valid$obs_flowers_num == 1)
tn <- sum(valid$predicted_flowering == 0 & valid$obs_flowers_num == 0)
fp <- sum(valid$predicted_flowering == 1 & valid$obs_flowers_num == 0)
fn <- sum(valid$predicted_flowering == 0 & valid$obs_flowers_num == 1)

precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f1 <- 2 * (precision * recall) / (precision + recall)

cat("Accuracy: ", round(accuracy, 3), "\n")
cat("Precision: ", round(precision, 3), "\n")
cat("Recall: ", round(recall, 3), "\n")
cat("F1 Score: ", round(f1, 3), "\n")

#now lets do fruiting accuracy
# List and stack BART binary fruiting raster files
jotr.files <- list.files("output/BART/predictions/BART_binary_fruiting", pattern = ".bil$", full.names = TRUE)
jotr.rasters <- stack(jotr.files)

# Extract years from filenames and name raster layers accordingly
layer_years <- as.integer(str_extract(jotr.files, "\\d{4}"))
names(jotr.rasters) <- paste0("yr", layer_years)

valid <- NULL  # Initialize dataframe to hold matched data

for (yr in unique(vobs.all$year)) {
  vsub <- filter(vobs.all, year == yr)
  
  layer_name <- paste0("yr", yr)
  
  if (layer_name %in% names(jotr.rasters)) {
    # Extract predicted fruiting (0/1) from raster at observation points
    vsub$predicted_fruiting <- raster::extract(jotr.rasters[[layer_name]], vsub[, c("lon", "lat")])
    valid <- rbind(valid, vsub)
  } else {
    warning(paste("Year", yr, "not found in raster stack — skipping"))
  }
}

# Convert observed fruiting logicals to numeric 0/1 for comparison
valid$obs_fruit_num <- as.numeric(valid$obs_fruit)

# Filter out missing predictions
valid <- filter(valid, !is.na(predicted_fruiting))
valid <- filter(valid, !is.na(obs_fruit_num))

# Confusion matrix
table(Predicted = valid$predicted_fruiting, Observed = valid$obs_fruit_num)

# Calculate accuracy and other metrics
accuracy <- mean(valid$predicted_fruiting == valid$obs_fruit_num)
tp <- sum(valid$predicted_fruiting == 1 & valid$obs_fruit_num == 1)
tn <- sum(valid$predicted_fruiting == 0 & valid$obs_fruit_num == 0)
fp <- sum(valid$predicted_fruiting == 1 & valid$obs_fruit_num == 0)
fn <- sum(valid$predicted_fruiting == 0 & valid$obs_fruit_num == 1)

precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f1 <- 2 * (precision * recall) / (precision + recall)

cat("Accuracy: ", round(accuracy, 3), "\n")
cat("Precision: ", round(precision, 3), "\n")
cat("Recall: ", round(recall, 3), "\n")
cat("F1 Score: ", round(f1, 3), "\n")



#_____ 
#RHO validation 
#Flowering prediction rho thing

jotr.fruit.mod <- read_rds(file="output/BART/bart.fruitNTO.model.Jotr.rds")
jotr.flower.mod <-  read_rds(file="output/BART/flowerNTO.bart.model.Jotr.rds")

# Extract predictor names
jotr.preds.flower <- attr(jotr.flower.mod$fit$data@x, "term.labels")


for (yr in 1900:2025) {
  # Load the predictors for the year
  preds <- brick(paste("data/PRISM/derived_predictors/PRISM_derived_predictors_", yr, ".gri", sep = ""))
  
  
  # Make predictions using the model
  pred.ri0 <- predict(jotr.flower.mod, preds[[jotr.preds.flower]], splitby = 20)
  
  # Ensure the prediction has the same extent and resolution as the range shapefile
  pred.ri0 <- crop(pred.ri0, extent(range))  # Crop the raster to the range extent
  pred.ri0 <- resample(pred.ri0, preds, method = "bilinear")  # Resample to ensure resolution matches
  
  # Mask the predictions to the range of the species
  pred.masked <- mask(pred.ri0, range)  # This will mask the predictions to the spatial extent of the range shapefile
  
  # Save the binary predictions as a raster file
  writeRaster(pred.masked, paste("output/BART/predictions/BART_flowering/BART_predicted_flowering_", yr, ".bil", sep = ""), overwrite = TRUE)
}

#same but for Fruiting

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
  
  # Save the binary predictions as a raster file
  writeRaster(pred.masked, paste("output/BART/predictions/BART_fruiting/BART_predicted_fruiting_", yr, ".bil", sep = ""), overwrite = TRUE)
}



library(raster)

years <- 1900:2025

## ---- FLOWERING TREND ----

# Build file list and stack them (order matters - make sure it matches `years`)
flower.files <- paste0("output/BART/predictions/BART_flowering/BART_predicted_flowering_",
                       years, ".bil")
flower.stack <- stack(flower.files)

# Function applied to each pixel (a vector of 126 values, one per year)
spearman.trend <- function(x) {
  if (all(is.na(x))) return(c(rho = NA, p.value = NA))
  ct <- cor.test(years, x, method = "spearman", exact = FALSE)
  c(rho = unname(ct$estimate), p.value = ct$p.value)
}

flower.trend <- calc(flower.stack, fun = spearman.trend)
names(flower.trend) <- c("rho", "p_value")

writeRaster(flower.trend[["rho"]],
            "output/BART/predictions/BART_flowering/trends/flower_trend_rho.tif", overwrite = TRUE)
writeRaster(flower.trend[["p_value"]],
            "output/BART/predictions/BART_flowering/trends/flower_trend_pvalue.tif", overwrite = TRUE)

## ---- FRUITING TREND (same pattern) ----

fruit.files <- paste0("output/BART/predictions/BART_fruiting/BART_predicted_fruiting_",
                      years, ".bil")
fruit.stack <- stack(fruit.files)

fruit.trend <- calc(fruit.stack, fun = spearman.trend)
names(fruit.trend) <- c("rho", "p_value")

writeRaster(fruit.trend[["rho"]],
            "output/BART/trends/fruit_trend_rho.tif", overwrite = TRUE)
writeRaster(fruit.trend[["p_value"]],
            "output/BART/trends/fruit_trend_pvalue.tif", overwrite = TRUE)


#OKay now paired t-test of the rhos

# Load the rho rasters you saved earlier
flower.rho <- flower.trend[["rho"]]
fruit.rho  <- fruit.trend[["rho"]]

getValues(flower.trend[["p_value"]])
fruit.trend[["p_value"]]

# Extract values (this keeps them aligned cell-by-cell since same grid/extent)
flower.vals <- getValues(flower.rho)
fruit.vals  <- getValues(fruit.rho)

# Keep only cells where BOTH have valid (non-NA) values
valid <- complete.cases(flower.vals, fruit.vals)
flower.vals <- flower.vals[valid]
fruit.vals  <- fruit.vals[valid]

## ---- Paired t-test 
paired.test <- t.test(flower.vals, fruit.vals, paired = TRUE)
print(paired.test)

mean(flower.vals)
#0.04809736
mean(fruit.vals)
#0.006646872

median(flower.vals)
#0.03295088
median(fruit.vals)
#0.009067867

t.test(flower.vals)
#95 percent confidence interval:
# 0.04508474 0.05110998
t.test(fruit.vals)
#95 percent confidence interval:
#0.00203733 0.01125642




#Okay final validation stuff AUC of validation data so it's easy to compare to traing data

library(pROC)

flower.files <- paste0("output/BART/predictions/BART_flowering/BART_predicted_flowering_",
                       years, ".bil")
flower.stack <- stack(flower.files)

names(flower.stack) <- paste0("yr", years)

valid <- NULL

for (yr in unique(vobs.all$year)) {
  vsub <- filter(vobs.all, year == yr)
  
  layer_name <- paste0("yr", yr)
  
  if (layer_name %in% names(flower.stack)) {
    vsub$predicted_flowering_prob <- raster::extract(flower.stack[[layer_name]], vsub[, c("lon", "lat")])
    valid <- rbind(valid, vsub)
  } else {
    warning(paste("Year", yr, "not found in flower.stack — skipping"))
  }
}

valid <- filter(valid, !is.na(predicted_flowering_prob))
valid <- filter(valid, !is.na(obs_flowers))
valid$obs_flowers_num <- as.numeric(valid$obs_flowers)

# AUC / ROC on continuous probabilities
roc_flower <- roc(response = valid$obs_flowers_num, predictor = valid$predicted_flowering_prob)
auc_flower <- auc(roc_flower)
cat("Flowering AUC: ", round(auc_flower, 3), "\n")
plot(roc_flower, main = "ROC Curve - Flowering")


#--------------------------- fruit

fruit.files <- paste0("output/BART/predictions/BART_fruiting/BART_predicted_fruiting_",
                      years, ".bil")
fruit.stack <- stack(fruit.files)

names(fruit.stack) <- paste0("yr", years)

valid <- NULL

for (yr in unique(vobs.all$year)) {
  vsub <- filter(vobs.all, year == yr)
  
  layer_name <- paste0("yr", yr)
  
  if (layer_name %in% names(fruit.stack)) {
    vsub$predicted_fruiting_prob <- raster::extract(fruit.stack[[layer_name]], vsub[, c("lon", "lat")])
    valid <- rbind(valid, vsub)
  } else {
    warning(paste("Year", yr, "not found in fruit.stack — skipping"))
  }
}

valid <- filter(valid, !is.na(predicted_fruiting_prob))
valid <- filter(valid, !is.na(obs_fruit))
valid$obs_fruit_num <- as.numeric(valid$obs_fruit)

roc_fruit <- roc(response = valid$obs_fruit_num, predictor = valid$predicted_fruiting_prob)
auc_fruit <- auc(roc_fruit)
cat("Fruiting AUC: ", round(auc_fruit, 3), "\n")
plot(roc_fruit, main = "ROC Curve - Fruiting")

ci.auc(roc_fruit)
ci.auc(roc_flower)





