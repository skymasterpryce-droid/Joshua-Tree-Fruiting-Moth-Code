#Script number 2
# downloades and orginzes PRISM historical monthlys that will be used later down the line
# 

getwd()

library("tidyverse")
library("lubridate")
library("raster")
library("prism")

if(!dir.exists("data/PRISM")) dir.create("data/PRISM")

prism_set_dl_dir("data/PRISM")

# Mojave crop extent (deliberately generous)
MojExt <- extent(-119, -112, 33, 38)


#-------------------------------------------------------------------------
# process PRISM data layers into a timeline archive I can use later

# make places to stash actual values and anomaly values
if(!dir.exists("data/PRISM/annual")) dir.create("data/PRISM/annual")


# FOR LOOP each year in PRISM
for(yr in 2025){

# yr=1990

get_prism_monthlys(type="tmax", mon=1:12, year=yr, keepZip=FALSE)
get_prism_monthlys(type="tmin", mon=1:12, year=yr, keepZip=FALSE)
get_prism_monthlys(type="ppt", mon=1:12, year=yr, keepZip=FALSE)
get_prism_monthlys(type="vpdmax", mon=1:12, year=yr, keepZip=FALSE)
get_prism_monthlys(type="vpdmin", mon=1:12, year=yr, keepZip=FALSE)


# FOR LOOP over quarters to do the thing ...
for(yr in 2025){
for(q in 1:4){
	
	# q <- 1
	
	mos <- list(1:3,4:6,7:9,10:12)[[q]]
	
	# max temp in each quarter
	tmaxQ <- crop(max(raster(pd_to_file(prism_archive_subset("tmax", "monthly", year=yr, mon=mos[1]))), raster(pd_to_file(prism_archive_subset("tmax", "monthly", year=yr, mon=mos[2]))), raster(pd_to_file(prism_archive_subset("tmax", "monthly", year=yr, mon=mos[3])))), MojExt)
	
	writeRaster(tmaxQ, paste("data/PRISM/annual/tmax_Mojave_", yr, "Q", q, ".bil", sep=""), overwrite=TRUE)
	
	# min temp in each quarter
	tminQ <- crop(min(raster(pd_to_file(prism_archive_subset("tmin", "monthly", year=yr, mon=mos[1]))), raster(pd_to_file(prism_archive_subset("tmin", "monthly", year=yr, mon=mos[2]))), raster(pd_to_file(prism_archive_subset("tmin", "monthly", year=yr, mon=mos[3])))), MojExt)

	writeRaster(tminQ, paste("data/PRISM/annual/tmin_Mojave_", yr, "Q", q, ".bil", sep=""), overwrite=TRUE)
	
	# sum of precip in each quarter
	pptQ <- crop(sum(raster(pd_to_file(prism_archive_subset("ppt", "monthly", year=yr, mon=mos[1]))), raster(pd_to_file(prism_archive_subset("ppt", "monthly", year=yr, mon=mos[2]))), raster(pd_to_file(prism_archive_subset("ppt", "monthly", year=yr, mon=mos[3])))), MojExt)

	writeRaster(pptQ, paste("data/PRISM/annual/ppt_Mojave_", yr, "Q", q, ".bil", sep=""), overwrite=TRUE)

	# max vpd in each quarter
	vpdmaxQ <- crop(max(raster(pd_to_file(prism_archive_subset("vpdmax", "monthly", year=yr, mon=mos[1]))), raster(pd_to_file(prism_archive_subset("vpdmax", "monthly", year=yr, mon=mos[2]))), raster(pd_to_file(prism_archive_subset("vpdmax", "monthly", year=yr, mon=mos[3])))), MojExt)

	writeRaster(vpdmaxQ, paste("data/PRISM/annual/vpdmax_Mojave_", yr, "Q", q, ".bil", sep=""), overwrite=TRUE)

	# min vpd in each quarter
	vpdminQ <- crop(min(raster(pd_to_file(prism_archive_subset("vpdmin", "monthly", year=yr, mon=mos[1]))), raster(pd_to_file(prism_archive_subset("vpdmin", "monthly", year=yr, mon=mos[2]))), raster(pd_to_file(prism_archive_subset("vpdmin", "monthly", year=yr, mon=mos[3])))), MojExt)

	writeRaster(vpdminQ, paste("data/PRISM/annual/vpdmin_Mojave_", yr, "Q", q, ".bil", sep=""), overwrite=TRUE)

	# re-insert file clearance here!!
	
	} # END loop over quarters


}
}# END loop over years

#-------------------------------------------------------------------------
# normalize PRISM data to a 1981-2010 baseline

# make places to stash actual values and anomaly values
if(!dir.exists("data/PRISM/annual_normed_1981-2010")) dir.create("data/PRISM/annual_normed_1981-2010")
if(!dir.exists("data/PRISM/norms_1981-2010")) dir.create("data/PRISM/norms_1981-2010")

quarterlies <- list.files("data/PRISM/annual", pattern=".bil")

# first, our normals, 1981-2010
for(q in 1:4){

# q = 1

tmax_mn <- calc(brick(lapply(paste("data/PRISM/annual/tmax_Mojave_",1981:2010,"Q",q,".bil", sep=""), raster)), mean)
tmax_sd <- calc(brick(lapply(paste("data/PRISM/annual/tmax_Mojave_",1981:2010,"Q",q,".bil", sep=""), raster)), sd)

writeRaster(tmax_mn, paste("data/PRISM/norms_1981-2010/tmax_mean_Mojave_1981-2010_Q", q, ".bil", sep=""), overwrite=TRUE)
writeRaster(tmax_sd, paste("data/PRISM/norms_1981-2010/tmax_sd_Mojave_1981-2010_Q", q, ".bil", sep=""), overwrite=TRUE)


tmin_mn <- calc(brick(lapply(paste("data/PRISM/annual/tmin_Mojave_",1981:2010,"Q",q,".bil", sep=""), raster)), mean)
tmin_sd <- calc(brick(lapply(paste("data/PRISM/annual/tmin_Mojave_",1981:2010,"Q",q,".bil", sep=""), raster)), sd)

writeRaster(tmin_mn, paste("data/PRISM/norms_1981-2010/tmin_mean_Mojave_1981-2010_Q", q, ".bil", sep=""), overwrite=TRUE)
writeRaster(tmin_sd, paste("data/PRISM/norms_1981-2010/tmin_sd_Mojave_1981-2010_Q", q, ".bil", sep=""), overwrite=TRUE)


ppt_mn <- calc(brick(lapply(paste("data/PRISM/annual/ppt_Mojave_",1981:2010,"Q",q,".bil", sep=""), raster)), mean)
ppt_sd <- calc(brick(lapply(paste("data/PRISM/annual/ppt_Mojave_",1981:2010,"Q",q,".bil", sep=""), raster)), sd)

writeRaster(ppt_mn, paste("data/PRISM/norms_1981-2010/ppt_mean_Mojave_1981-2010_Q", q, ".bil", sep=""), overwrite=TRUE)
writeRaster(ppt_sd, paste("data/PRISM/norms_1981-2010/ppt_sd_Mojave_1981-2010_Q", q, ".bil", sep=""), overwrite=TRUE)


vpdmax_mn <- calc(brick(lapply(paste("data/PRISM/annual/vpdmax_Mojave_",1981:2010,"Q",q,".bil", sep=""), raster)), mean)
vpdmax_sd <- calc(brick(lapply(paste("data/PRISM/annual/vpdmax_Mojave_",1981:2010,"Q",q,".bil", sep=""), raster)), sd)

writeRaster(vpdmax_mn, paste("data/PRISM/norms_1981-2010/vpdmax_mean_Mojave_1981-2010_Q", q, ".bil", sep=""), overwrite=TRUE)
writeRaster(vpdmax_sd, paste("data/PRISM/norms_1981-2010/vpdmax_sd_Mojave_1981-2010_Q", q, ".bil", sep=""), overwrite=TRUE)


vpdmin_mn <- calc(brick(lapply(paste("data/PRISM/annual/vpdmin_Mojave_",1981:2010,"Q",q,".bil", sep=""), raster)), mean)
vpdmin_sd <- calc(brick(lapply(paste("data/PRISM/annual/vpdmin_Mojave_",1981:2010,"Q",q,".bil", sep=""), raster)), sd)

writeRaster(vpdmin_mn, paste("data/PRISM/norms_1981-2010/vpdmin_mean_Mojave_1981-2010_Q", q, ".bil", sep=""), overwrite=TRUE)
writeRaster(vpdmin_sd, paste("data/PRISM/norms_1981-2010/vpdmin_sd_Mojave_1981-2010_Q", q, ".bil", sep=""), overwrite=TRUE)

}


# FOR LOOP to norm the historical data ...
for(yr in 2025){

# yr=1895

# FOR LOOP over quarters to do the thing ...
	for(q in 1:4){
	
	# q <- 1
	
	# max temp in each quarter
	tmaxQnorm <- (raster(paste("data/PRISM/annual/tmax_Mojave_", yr, "Q", q, ".bil", sep="")) - raster(paste("data/PRISM/norms_1981-2010/tmax_mean_Mojave_1981-2010_Q", q, ".bil", sep="")))/ raster(paste("data/PRISM/norms_1981-2010/tmax_sd_Mojave_1981-2010_Q", q, ".bil", sep=""))
	
	writeRaster(tmaxQnorm, paste("data/PRISM/annual_normed_1981-2010/tmax_normed_Mojave_", yr, "Q", q, ".bil", sep=""), overwrite=TRUE)
	
	# min temp in each quarter
	tminQnorm <- (raster(paste("data/PRISM/annual/tmin_Mojave_", yr, "Q", q, ".bil", sep="")) - raster(paste("data/PRISM/norms_1981-2010/tmin_mean_Mojave_1981-2010_Q", q, ".bil", sep="")))/ raster(paste("data/PRISM/norms_1981-2010/tmin_sd_Mojave_1981-2010_Q", q, ".bil", sep=""))
	
	writeRaster(tminQnorm, paste("data/PRISM/annual_normed_1981-2010/tmin_normed_Mojave_", yr, "Q", q, ".bil", sep=""), overwrite=TRUE)
	
	# sum of precip in each quarter
	pptQnorm <- crop((raster(paste("data/PRISM/annual/ppt_Mojave_", yr, "Q", q, ".bil", sep="")) - raster(paste("data/PRISM/norms_1981-2010/ppt_mean_Mojave_1981-2010_Q", q, ".bil", sep="")))/ raster(paste("data/PRISM/norms_1981-2010/ppt_sd_Mojave_1981-2010_Q", q, ".bil", sep="")), MojExt)
	
	writeRaster(pptQnorm, paste("data/PRISM/annual_normed_1981-2010/ppt_normed_Mojave_", yr, "Q", q, ".bil", sep=""), overwrite=TRUE)
	
	# max VPD in each quarter
	vpdmaxQnorm <- (raster(paste("data/PRISM/annual/vpdmax_Mojave_", yr, "Q", q, ".bil", sep="")) - raster(paste("data/PRISM/norms_1981-2010/vpdmax_mean_Mojave_1981-2010_Q", q, ".bil", sep="")))/ raster(paste("data/PRISM/norms_1981-2010/vpdmax_sd_Mojave_1981-2010_Q", q, ".bil", sep=""))
	
	writeRaster(vpdmaxQnorm, paste("data/PRISM/annual_normed_1981-2010/vpdmax_normed_Mojave_", yr, "Q", q, ".bil", sep=""), overwrite=TRUE)	

	# min VPD in each quarter
	vpdminQnorm <- (raster(paste("data/PRISM/annual/vpdmin_Mojave_", yr, "Q", q, ".bil", sep="")) - raster(paste("data/PRISM/norms_1981-2010/vpdmin_mean_Mojave_1981-2010_Q", q, ".bil", sep="")))/ raster(paste("data/PRISM/norms_1981-2010/vpdmin_sd_Mojave_1981-2010_Q", q, ".bil", sep=""))
	
	writeRaster(vpdminQnorm, paste("data/PRISM/annual_normed_1981-2010/vpdmin_normed_Mojave_", yr, "Q", q, ".bil", sep=""), overwrite=TRUE)	

	
	} # END loop over quarters

}

# and now I have quarterly historical data for the Mojave to work with for all downstream analysis


#-------------------------------------------------------------------------
# calculate hypothesized predictors from normalized PRISM data

# make places to stash actual values and anomaly values
if(!dir.exists("data/PRISM/derived_predictors")) dir.create("data/PRISM/derived_predictors")

# NEW predictor variables, informed by more specific hypotheses
# DEFINED: y0 is Apr-Mar of the year flowers are observed; y1 the year before, y2 two years before ...
# for each of YEAR 0, 1, and 2 ...
# total precip (ppt)
# max and min temperature (tmax and tmin)
# max and min vapor pressure deficit (vpdmax and vpdmin)
# and then also CONTRASTS Y0-Y1, Y1-Y2 for each of these

# FOR LOOP over the historical data ...
# nb, because retrospective, can't do all the way back to 1895
inat <- read.csv("data/Combined_Pheno_Data.csv", h=TRUE) %>% mutate(observed_on = ymd(observed_on))

inat_clean <- inat %>%
  filter(!is.na(latitude) & !is.na(longitude))

inat <-inat_clean[c(3,2,8)]



rast_ref <- raster("data/PRISM/annual/ppt_Mojave_2015Q1.bil") # standard to resample against, because WTF

for(yr in 2024:2025){

# yr <- 1900

# ppt, all summarized
pptY0 <- calc(brick(lapply(c(paste("data/PRISM/annual/ppt_Mojave_",yr,"Q1.bil", sep=""), paste("data/PRISM/annual/ppt_Mojave_",yr-1,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),sum) # borked in 2021
pptY1 <- calc(brick(lapply(c(paste("data/PRISM/annual/ppt_Mojave_",yr-1,"Q1.bil", sep=""), paste("data/PRISM/annual/ppt_Mojave_",yr-2,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),sum)
pptY2 <- calc(brick(lapply(c(paste("data/PRISM/annual/ppt_Mojave_",yr-2,"Q1.bil", sep=""), paste("data/PRISM/annual/ppt_Mojave_",yr-3,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),sum)

pptY0Y1 <- pptY0 - pptY1
pptY1Y2 <- pptY1 - pptY2

# ppt Q 

pptY0Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/ppt_Mojave_",yr,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),sum)
pptY0Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/ppt_Mojave_",yr,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),sum)
pptY1Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/ppt_Mojave_",yr-1,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),sum)
pptY1Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/ppt_Mojave_",yr-1,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),sum)
pptY1Q3 <- calc(brick(lapply(c(paste("data/PRISM/annual/ppt_Mojave_",yr-1,"Q3.bil", sep="")), function(x) resample(raster(x), rast_ref))),sum)
pptY1Q4 <- calc(brick(lapply(c(paste("data/PRISM/annual/ppt_Mojave_",yr-1,"Q4.bil", sep="")), function(x) resample(raster(x), rast_ref))),sum)
pptY2Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/ppt_Mojave_",yr-2,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),sum)
pptY2Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/ppt_Mojave_",yr-2,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),sum)
pptY2Q3 <- calc(brick(lapply(c(paste("data/PRISM/annual/ppt_Mojave_",yr-2,"Q3.bil", sep="")), function(x) resample(raster(x), rast_ref))),sum)
pptY2Q4 <- calc(brick(lapply(c(paste("data/PRISM/annual/ppt_Mojave_",yr-2,"Q4.bil", sep="")), function(x) resample(raster(x), rast_ref))),sum)

# tmax, all summarized
tmaxY0 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmax_Mojave_",yr,"Q1.bil", sep=""), paste("data/PRISM/annual/tmax_Mojave_",yr-1,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),max) # borked in 2021
tmaxY1 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmax_Mojave_",yr-1,"Q1.bil", sep=""), paste("data/PRISM/annual/tmax_Mojave_",yr-2,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
tmaxY2 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmax_Mojave_",yr-2,"Q1.bil", sep=""), paste("data/PRISM/annual/tmax_Mojave_",yr-3,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),max)

tmaxY0Y1 <- tmaxY0 - tmaxY1
tmaxY1Y2 <- tmaxY1 - tmaxY2

# tmax Q

tmaxY0Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmax_Mojave_",yr,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
tmaxY0Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmax_Mojave_",yr,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
tmaxY1Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmax_Mojave_",yr-1,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
tmaxY1Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmax_Mojave_",yr-1,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
tmaxY1Q3 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmax_Mojave_",yr-1,"Q3.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
tmaxY1Q4 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmax_Mojave_",yr-1,"Q4.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
tmaxY2Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmax_Mojave_",yr-2,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
tmaxY2Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmax_Mojave_",yr-2,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
tmaxY2Q3 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmax_Mojave_",yr-2,"Q3.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
tmaxY2Q4 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmax_Mojave_",yr-2,"Q4.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)


# tmin, all summarized
tminY0 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmin_Mojave_",yr,"Q1.bil", sep=""), paste("data/PRISM/annual/tmin_Mojave_",yr-1,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),min) # borked in 2021
tminY1 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmin_Mojave_",yr-1,"Q1.bil", sep=""), paste("data/PRISM/annual/tmin_Mojave_",yr-2,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
tminY2 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmin_Mojave_",yr-2,"Q1.bil", sep=""), paste("data/PRISM/annual/tmin_Mojave_",yr-3,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),min)

tminY0Y1 <- tminY0 - tminY1
tminY1Y2 <- tminY1 - tminY2

# tmin Q

tminY0Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmin_Mojave_",yr,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
tminY0Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmin_Mojave_",yr,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
tminY1Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmin_Mojave_",yr-1,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
tminY1Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmin_Mojave_",yr-1,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
tminY1Q3 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmin_Mojave_",yr-1,"Q3.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
tminY1Q4 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmin_Mojave_",yr-1,"Q4.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
tminY2Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmin_Mojave_",yr-2,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
tminY2Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmin_Mojave_",yr-2,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
tminY2Q3 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmin_Mojave_",yr-2,"Q3.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
tminY2Q4 <- calc(brick(lapply(c(paste("data/PRISM/annual/tmin_Mojave_",yr-2,"Q4.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)

# vpdmax, all summarized
vpdmaxY0 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmax_Mojave_",yr,"Q1.bil", sep=""), paste("data/PRISM/annual/vpdmax_Mojave_",yr-1,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),max) # borked in 2021
vpdmaxY1 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmax_Mojave_",yr-1,"Q1.bil", sep=""), paste("data/PRISM/annual/vpdmax_Mojave_",yr-2,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
vpdmaxY2 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmax_Mojave_",yr-2,"Q1.bil", sep=""), paste("data/PRISM/annual/vpdmax_Mojave_",yr-3,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),max)

vpdmaxY0Y1 <- vpdmaxY0 - vpdmaxY1
vpdmaxY1Y2 <- vpdmaxY1 - vpdmaxY2

# vpdmax Q

vpdmaxY0Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmax_Mojave_",yr,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
vpdmaxY0Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmax_Mojave_",yr,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
vpdmaxY1Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmax_Mojave_",yr-1,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
vpdmaxY1Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmax_Mojave_",yr-1,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
vpdmaxY1Q3 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmax_Mojave_",yr-1,"Q3.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
vpdmaxY1Q4 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmax_Mojave_",yr-1,"Q4.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
vpdmaxY2Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmax_Mojave_",yr-2,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
vpdmaxY2Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmax_Mojave_",yr-2,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
vpdmaxY2Q3 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmax_Mojave_",yr-2,"Q3.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)
vpdmaxY2Q4 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmax_Mojave_",yr-2,"Q4.bil", sep="")), function(x) resample(raster(x), rast_ref))),max)

# vpdmin, all summarized
vpdminY0 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmin_Mojave_",yr,"Q1.bil", sep=""), paste("data/PRISM/annual/vpdmin_Mojave_",yr-1,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),min) # borked in 2021
vpdminY1 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmin_Mojave_",yr-1,"Q1.bil", sep=""), paste("data/PRISM/annual/vpdmin_Mojave_",yr-2,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
vpdminY2 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmin_Mojave_",yr-2,"Q1.bil", sep=""), paste("data/PRISM/annual/vpdmin_Mojave_",yr-3,"Q",4:2,".bil", sep="")), function(x) resample(raster(x), rast_ref))),min)

vpdminY0Y1 <- vpdminY0 - vpdminY1
vpdminY1Y2 <- vpdminY1 - vpdminY2

# vpdmin Q

vpdminY0Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmin_Mojave_",yr,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
vpdminY0Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmin_Mojave_",yr,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
vpdminY1Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmin_Mojave_",yr-1,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
vpdminY1Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmin_Mojave_",yr-1,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
vpdminY1Q3 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmin_Mojave_",yr-1,"Q3.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
vpdminY1Q4 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmin_Mojave_",yr-1,"Q4.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
vpdminY2Q1 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmin_Mojave_",yr-2,"Q1.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
vpdminY2Q2 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmin_Mojave_",yr-2,"Q2.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
vpdminY2Q3 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmin_Mojave_",yr-2,"Q3.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)
vpdminY2Q4 <- calc(brick(lapply(c(paste("data/PRISM/annual/vpdmin_Mojave_",yr-2,"Q4.bil", sep="")), function(x) resample(raster(x), rast_ref))),min)

#raster for Sampling density

inat_year <- inat[inat$year == yr, ]

if (nrow(inat_year) > 0) {
  # If there are observations for the year
  coordinates(inat_year) <- ~longitude + latitude  # Specify the longitude and latitude columns
  
  # Rasterize with 'count' function
  total_obs <- rasterize(inat_year, rast_ref, fun = 'count')
} else {
  # If there are no observations, create a raster of NAs with the same extent and resolution as rast_ref
  total_obs <- raster(rast_ref)
  values(total_obs) <- NA  # Fill raster with NA
}

total_obs <- total_obs[[1]]
print(total_obs)

#yr=2022
                  
preds <- brick(c(pptY0, pptY1, pptY2, pptY0Y1, pptY1Y2, tmaxY0, tmaxY1, tmaxY2, tmaxY0Y1, tmaxY1Y2, tminY0, tminY1, tminY2, tminY0Y1, tminY1Y2, vpdmaxY0,
                 vpdmaxY1, vpdmaxY2, vpdmaxY0Y1, vpdmaxY1Y2, vpdminY0, vpdminY1, vpdminY2, vpdminY0Y1, vpdminY1Y2, pptY0Q1,pptY0Q2, pptY1Q1, pptY1Q2, pptY1Q3, pptY1Q4, pptY2Q1, pptY2Q2, pptY2Q3, pptY2Q4, 
                 tmaxY0Q1, tmaxY0Q2, tmaxY1Q1, tmaxY1Q2, tmaxY1Q3, tmaxY1Q4, tmaxY2Q1, tmaxY2Q2, tmaxY2Q3, tmaxY2Q4, tminY0Q1, tminY0Q2, tminY1Q1, tminY1Q2, tminY1Q3, tminY1Q4, tminY2Q1, tminY2Q2, tminY2Q3, tminY2Q4, vpdmaxY0Q1, vpdmaxY0Q2, vpdmaxY1Q1, vpdmaxY1Q2, vpdmaxY1Q3, vpdmaxY1Q4, vpdmaxY2Q1, vpdmaxY2Q2, vpdmaxY2Q3, vpdmaxY2Q4, vpdminY0Q1, vpdminY0Q2,
                 vpdminY1Q1, vpdminY1Q2, vpdminY1Q3, vpdminY1Q4, vpdminY2Q1, vpdminY2Q2, vpdminY2Q3, vpdminY2Q4, total_obs))

names(preds) <- c("pptY0", "pptY1", "pptY2", "pptY0Y1", "pptY1Y2", 
                  "tmaxY0", "tmaxY1", "tmaxY2", "tmaxY0Y1", "tmaxY1Y2", 
                  "tminY0", "tminY1", "tminY2", "tminY0Y1", "tminY1Y2", 
                  "vpdmaxY0", "vpdmaxY1", "vpdmaxY2", "vpdmaxY0Y1", "vpdmaxY1Y2", 
                  "vpdminY0", "vpdminY1", "vpdminY2", "vpdminY0Y1", "vpdminY1Y2", 
                  "pptY0Q1", "pptY0Q2", "pptY1Q1", "pptY1Q2", "pptY1Q3", "pptY1Q4", 
                  "pptY2Q1", "pptY2Q2", "pptY2Q3", "pptY2Q4", 
                  "tmaxY0Q1", "tmaxY0Q2", "tmaxY1Q1", "tmaxY1Q2", "tmaxY1Q3", "tmaxY1Q4", 
                  "tmaxY2Q1", "tmaxY2Q2", "tmaxY2Q3", "tmaxY2Q4", 
                  "tminY0Q1", "tminY0Q2", "tminY1Q1", "tminY1Q2", "tminY1Q3", "tminY1Q4", 
                  "tminY2Q1", "tminY2Q2", "tminY2Q3", "tminY2Q4", 
                  "vpdmaxY0Q1", "vpdmaxY0Q2", "vpdmaxY1Q1", "vpdmaxY1Q2", "vpdmaxY1Q3", "vpdmaxY1Q4", 
                  "vpdmaxY2Q1", "vpdmaxY2Q2", "vpdmaxY2Q3", "vpdmaxY2Q4", 
                  "vpdminY0Q1", "vpdminY0Q2", "vpdminY1Q1", "vpdminY1Q2", "vpdminY1Q3", "vpdminY1Q4", 
                  "vpdminY2Q1", "vpdminY2Q2", "vpdminY2Q3", "vpdminY2Q4","total_obs")

writeRaster(preds, paste("data/PRISM/derived_predictors/PRISM_derived_predictors_", yr,".grd", sep=""), overwrite=TRUE) # confirmed write-out and read-in

} # and now each year has the predictor set stashed as a multilayer raster, cool
#_____________________________________________________________________________


