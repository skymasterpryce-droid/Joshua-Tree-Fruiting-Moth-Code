#script number 3 for flowering obs
# working with phenology-annotated iNat observations orginizes 
#observations for flowering used later in BART process
# PWM 8/24/2026

# starting up ------------------------------------------------------------
#set working directory to your own
# setwd("~/Documents/Active_projects/Jotr_phenology")
# setwd("~/Documents/Academic/Active_projects/Jotr_phenology")

library("tidyverse")
library("lubridate")

library("raster")
library("sf")
library("dplyr")
update.packages()
# Mojave crop extent (deliberately generous)
MojExt <- extent(-119, -112, 33, 38)

#-------------------------------------------------------------------------
# read in iNat observations compiled using inat_phenology_obs.R

inat <- read.csv("data/Combined_Pheno_Data.csv", h=TRUE) %>% mutate(observed_on = ymd(observed_on))

view(inat)
glimpse(inat) # raw observations 14,286 Now 2024
table(inat$phenology)
table(inat$phenology, inat$year)
table(inat$phenology, inat$source)

# is fruit ever observed in Jan, Feb, or March? That's really the PREVIOUS flowering year
filter(inat, phenology=="Fruiting", source=="inat", month(observed_on)<4) # hmmm okay
inatfilter <- filter(inat, !(phenology=="Fruiting" & source=="inat" & month(observed_on)<4)) # clean those out
#changed to inatfilter to use all observations. cange to inat if going to filter out the no evidence trees

#for now filter out all observations that the trees aren't doing anything

#filter(inat, phenology=="No Evidence of Flowering")
#inatfilter <- filter(inat, !(phenology=="No Evidence of Flowering"))

#-------------------------------------------------------------------------
#* "Flowering","Flower Budding"

# organize iNat observations for extraction of summarized PRISM data

# dealing with the 2018 "anomaly" by creating a second pseudo-year
inatfilter$y2 <- inatfilter$year
inatfilter$y2[inatfilter$year==2018 & month(inatfilter$observed_on)>6] <- 2018.5

# Initialize the dataframe
flowering <- data.frame(matrix(0, 0, 6))  # Added 6 columns to include total_obs per year
names(flowering) <- c("lon", "lat", "year", "type", "flr", "total_obs")

# Load the raster
prism_temp_rast <- raster("data/PRISM/annual/ppt_Mojave_2010Q1.bil")

#loop that makes it a fruiting model yes= "Fruiting" no = "Flowering", "Flower Budding"
#or a flowering model yes = "Flowering", "Flower Budding" no = "No Evidence of Flowering"


for (yr in unique(inatfilter$y2)) {
  # Filter the data for the current year
  filtered_data <- dplyr::filter(inatfilter, y2 == yr)
  
  # Create a yearly raster for total observations
  total_observations_year <- rasterize(filtered_data[, c("longitude", "latitude")], prism_temp_rast, fun = "count", background = 0)
  
  # Check for "Fruiting" condition
  if (any(filtered_data$phenology %in% c("Flowering", "Flower Budding"))) {
    yes <- rasterize(filtered_data[filtered_data$phenology %in% c("Flowering", "Flower Budding"), c("longitude", "latitude")], prism_temp_rast, fun = sum, background = 0)
    
    yearyes <- rasterToPoints(yes, fun = function(x) { x >= 1 })
    
    outyes <- data.frame(lon = yearyes[, "x"], lat = yearyes[, "y"], year = yr, flr = TRUE)
  } else {
    outyes <- NULL
  }
  
  # Check for "Flowering" condition
  if (any(filtered_data$phenology %in% c("No Evidence of Flowering"))) {
    no <- rasterize(filtered_data[filtered_data$phenology %in% c("No Evidence of Flowering"), c("longitude", "latitude")], prism_temp_rast, fun = sum, background = 0)
    
    yearno <- rasterToPoints(no, fun = function(x) { x >= 1 })
    
    outno <- data.frame(lon = yearno[, "x"], lat = yearno[, "y"], year = yr, flr = FALSE)
  } else {
    outno <- NULL
  }
  
  # Combine and ensure positive observations overwrite negative, if duplicated
  if (!is.null(outyes) || !is.null(outno)) {
    outall <- rbind(outyes, outno) |> filter(!(duplicated(paste(lon, lat)) & flr == FALSE))
    
    # Get the total observation count for each grid square in the current year
    obs_counts_year <- raster::extract(total_observations_year, cbind(outall$lon, outall$lat))
    
    # Add the observation count to the data frame
    outall$total_obs <- obs_counts_year
    
    # Combine with the main dataframe
    flowering <- rbind(flowering, outall)
  }
}


head(flowering) # oh right we've lost type
glimpse(flowering)
table(flowering$year)
view(flowering)
# separate (sub)species again

# identify (sub)species
# inat_pheno_data <- read.csv("data/inat_phenology_data.csv", h=TRUE)
jtssps <- read_sf("data/Jotr_ssp_range.kml")

flo_in_ssp <- st_join(st_as_sf(flowering, coords=c("lon", "lat"), crs=crs(jtssps)), jtssps, join = st_within) %>% cbind(flowering[,c("lon", "lat")]) %>% as.data.frame(.) %>% dplyr::select(-geometry, -Description) %>% rename(type=Name) %>% dplyr::select(lon, lat, type, year, flr,total_obs)

glimpse(flo_in_ssp) # okay okay okay!
view(flo_in_ssp)
table(flo_in_ssp$type, useNA="ifany")
table(flo_in_ssp$year, flo_in_ssp$flr) # think that looks good ...

write.table(flo_in_ssp, "output/flowering_obs_rasterized_subsp.csv", sep=",", col.names=TRUE, row.names=FALSE)

flo_in_ssp <- read.csv("output/flowering_obs_rasterized_subsp.csv")
glimpse(flo_in_ssp)
view(flo_in_ssp)
#-------------------------------------------------------------------------
# attach PRISM data to flowering/not flowering observations

# NEW predictor variables, informed by more specific hypotheses
# DEFINED: y0 is the year flowers are observed; y1 the year before, y2 two years before ...
# for each of YEAR 0, 1, and 2 ...
# total precip (ppt)
# max and min temperature (tmax and tmin)
# max and min vapor pressure deficit (vpdmax and vpdmin)
# and then also CONTRASTS Y0-Y1, Y1-Y2 for each of these

flr.clim <- data.frame(matrix(0,0,ncol(flo_in_ssp)+13))
names(flr.clim) <- c(colnames(flo_in_ssp), "pptW0", "pptY0", "pptW0W1", "pptY0W1", "pptY0Y1", "tmaxW0", "tminW0", "tmaxW0vW1", "tminW0vW1", "vpdmaxW0", "vpdminW0", "vpdmaxW0vW1", "vpdminW0vW1")

# LOOP over years, because of the current year previous year thing ...
for(y in unique(flo_in_ssp$year)){
  
  # y <- 2021 # test condition
  {  # Exclude 2024
    # assemble weather data predictors for a given year
    yr <- floor(y) # not going to try to lag 2019.5 for this predictor set 
    
    preds <- brick(paste("data/PRISM/derived_predictors/PRISM_derived_predictors_", yr,".grd", sep=""))
    
    # pull subset of flowering observations for year
    flsub <- subset(flo_in_ssp, year==y)
    flsub <- cbind(flsub, raster::extract(preds, flsub[,c("lon","lat")], df=FALSE))
    
    flr.clim <- rbind(flr.clim,flsub) 
    
    write.table(flr.clim, "output/flowering_obs_climate_subsp.csv", sep=",", col.names=TRUE, row.names=FALSE)
    
  }
}# END LOOP over years

glimpse(flr.clim)
table(flr.clim$type, useNA="ifany")
table(flr.clim$year, useNA="ifany")

# and that's generated a data file for flowering we can feed into Embarcadero in flowering_phenology modeling script

