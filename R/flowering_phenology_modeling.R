# Script 5 for flowing
# Using BARTs to model Joshua tree flowering
# modified PWM 8/24/2026

rm(list=ls())  # Clears memory of all objects -- useful for debugging! But doesn't kill packages.

# setwd("~/Documents/Active_projects/Jotr_phenology")
# setwd("~/Jotr_phenology-main")
# setwd("~/Documents/Academic/Active_projects/Jotr_phenology")

library("tidyverse")
library("embarcadero")
library("ggdark")
library("raster")
library("sf")
library("cowplot")


source("../shared/Rscripts/base_graphics.R")

setwd("C:/Users/13073/Desktop/Masters/Thesis/Jotr_phenology-main/Jotr_phenology-main/scripts")

#full list "pptY0Y1", "pptY1Y2", "vpdmaxY0", "vpdminY0Y1", "tminY0", "tmaxY0Y1", "pptY0", "pptY1", "tminY0Q2", "vpdminY0Q3", "vpdminY0Q4", "tminY0Q4", "tmaxY0Q4","vpdminY0Q1", "tminY0Q1", "tmaxY0Q1"

#list with all Q back 2 years "pptY0Q1", "pptY1Q1", "pptY1Q2", "pptY1Q3", "pptY1Q4", 
#"pptY2Q1", "pptY2Q2", "pptY2Q3", "pptY2Q4", "tmaxY0Q1", "tmaxY1Q1", "tmaxY1Q2", "tmaxY1Q3", "tmaxY1Q4", "tmaxY2Q1", "tmaxY2Q2", "tmaxY2Q3", "tmaxY2Q4", 
#"tminY0Q1", "tminY1Q1", "tminY1Q2", "tminY1Q3", "tminY1Q4", "tminY2Q1", "tminY2Q2", "tminY2Q3", "tminY2Q4", "vpdmaxY0Q1", "vpdmaxY1Q1", "vpdmaxY1Q2", 
#"vpdmaxY1Q3", "vpdmaxY1Q4", "vpdmaxY2Q1", "vpdmaxY2Q2", "vpdmaxY2Q3", "vpdmaxY2Q4", "vpdminY0Q1", "vpdminY1Q1", "vpdminY1Q2", "vpdminY1Q3", 
#"vpdminY1Q4", "vpdminY2Q1", "vpdminY2Q2", "vpdminY2Q3", "vpdminY2Q4"

#-----------------------------------------------------------
# initial file loading


flow <- read.csv("output/flowering_obs_climate_subsp.csv") # flowering/not flowering, biologically-informed candidate predictors, subspecies id'd

dim(flow)
glimpse(flow)


# variant datasets -- dealing with the second flowering in 2018
flow2 <- flow %>% filter(!(year==2018.5 & flr==TRUE), year>=2008) %>% mutate(year=floor(year)) # drop the late-flowering anomaly
flow3 <- flow |> filter(year>=2008)
flow3$year[flow3$year==2018.5] <- 2018 # or merge 2018.5 into 2018?

glimpse(flow2)# 3,954 in our final working set


table(flow2$year)

flow4 <- flow2 |> filter(year>=2016) # years with at least 100 records
glimpse(flow4) # 2,319 from 2016 on

# ggplot(flow2, aes(x=lon, y=lat, color=flr)) + geom_point() + facet_wrap("year") + theme_bw()

# split by subspecies
# swap input datasets to change --- current most trustworthy is flow2, ignoring 2019.5
yuja <- filter(flow2, type=="YUJA") 
yubr <- filter(flow2, type=="YUBR")

glimpse(yuja) # 2502 obs 
glimpse(yubr) # 3223 obs

#-------------------------------------------------------------------------

# predictors
xnames <- c("pptY0", "pptY1", "pptY2", "pptY0Y1", "pptY1Y2", 
            "tmaxY0", "tmaxY1", "tmaxY0Y1", 
            "tminY0", "tminY1", "tminY0Y1",  
            "vpdmaxY0", "vpdmaxY1", "vpdmaxY0Y1", 
            "vpdminY0", "vpdminY1", "vpdminY0Y1", 
            "pptY0Q1", "pptY1Q3", "pptY1Q4", 
            "tmaxY0Q1", "tmaxY1Q3", "tmaxY1Q4", 
            "tminY0Q1", "tminY1Q3", "tminY1Q4", 
            "vpdmaxY0Q1", "vpdmaxY1Q3", "vpdmaxY1Q4", 
            "vpdminY0Q1",  "vpdminY1Q3", "vpdminY1Q4") # weather data, curated

# Full range ------------------------------------

# variable importance across the whole predictor set
jotr.varimp <- varimp.diag(y.data=as.numeric(flow2[,"flr"]), x.data=flow2[,xnames]) # now sans RI
# favors "vpdminY2Q4","pptY2Q3", "pptY2Q4", "vpdminY1Q1", "vpdmaxY0Q1", "tminY1Q3", "pptY2Q2", "vpdminY2Q3", "vpdminY1Q3", 
#"tminY0Q1", "pptY1Q3", "vpdminY1Q2", "vpdminY1Q4", "pptY1Q2", "vpdmaxY2Q3"

write_rds(jotr.varimp, file="output/BART/bart.flowerNTO.varimp.Jotr.rds") # switching save modes now
# jotr.varimp <- read_rds("output/BART/bart.flower.varimp.Jotr.rds")

# publication-ready figure assembly ...
glimpse(jotr.varimp$data)

jotr.varimp$data <- jotr.varimp$data |> mutate(trees = factor(trees, c(10,20,50,100,150,200)))

levels(jotr.varimp$data$names) 


#flower with Total obs an area recived here after "TO"
jotr.preds <- c("total_obs" , "pptY1Y2"  ,  "tminY0Y1"  , "pptY0Y1" ,   "vpdminY1Q3" ,"tminY1Q4" ,  "pptY1" )
jotr.mod <- bart(y.train=as.numeric(flow2[,"flr"]), x.train=flow2[,jotr.preds], keeptrees=TRUE)

summary(jotr.mod)

invisible(jotr.mod$fit$state)
write_rds(jotr.mod, file="output/BART/flower.bart.model.Jotr.rds")


p <- partial(jotr.mod, jotr.preds, trace=FALSE, smooth=5) # visualize partials
varimp(jotr.mod)

#par(mfrow = c(3, 4)) use this to plot them all at once? 

write_rds(p, file="output/BART/bart.flower.model.Jotr.partials.rds")

#flower without Total observations an area recived no totoal obs here after "NTO"

jotr.preds2 <- c("pptY1Y2"  ,  "tminY0Y1"  , "pptY0Y1" ,  "pptY1Q3"   , "vpdminY1" ,  "tminY1Q4")
                 jotr.mod2 <- bart(y.train=as.numeric(flow2[,"flr"]), x.train=flow2[,jotr.preds2], keeptrees=TRUE)

summary(jotr.mod2)

invisible(jotr.mod2$fit$state)
write_rds(jotr.mod2, file="output/BART/flowerNTO.bart.model.Jotr.rds")

# jotr.mod <- read_rds("output/BART/bart.model.Jotr.rds")


p2 <- partial(jotr.mod2, jotr.preds2, trace=FALSE, smooth=5) # visualize partials
varimp(jotr.mod)

#par(mfrow = c(3, 4)) use this to plot them all at once? 

write_rds(p2, file="output/BART/bart.flowerNTO.model.Jotr.partials.rds")
# p <- read_rds("output/BART/bart.flower.model.Jotr.partials.rds")

partplot <- rbind(
  data.frame(predictor=p[[1]]$data),
  data.frame(predictor=p[[2]]$data),
  data.frame(predictor=p[[3]]$data),
  data.frame(predictor=p[[4]]$data),
  data.frame(predictor=p[[5]]$data),
  data.frame(predictor=p[[6]]$data),
  data.frame(predictor=p[[7]]$data),
  data.frame(predictor=p[[8]]$data),
  data.frame(predictor=p[[9]]$data),
)


# now, spartials ...

for(yr in unique(flow2$year)){
  
  # yr <- 2022
  
  spYr <- spartial(jotr.mod, brick(paste("data/PRISM/derived_predictors/PRISM_derived_predictors_", yr, ".grd", sep="")), x.vars=jotr.preds)
  
  {cairo_pdf(paste("output/figures/BART_flower_spartials_jotr_", yr, ".pdf", sep=""), width=9, height=6.5)
    plot(spYr)
  }
  dev.off()
  
  writeRaster(spYr, paste("output/BART/Jotr_flower_BART_spartials_", yr,".grd", sep=""), overwrite=TRUE) # confirmed write-out and read-in
}

#-------------------------------------------------------------------------
# Partials and spartials in example years

# and then spartials ...
sdm.pres <- read_sf("data/Yucca/jotr_BART_sdm_pres/jotr_BART_sdm_pres.shp", "jotr_BART_sdm_pres")

states <- read_sf(dsn = "data/spatial/10m_cultural/ne_10m_admin_1_states_provinces", lay= "ne_10m_admin_1_states_provinces")
coast <- read_sf("data/spatial/10m_physical/ne_10m_coastline", "ne_10m_coastline")


# 2019, "good year" example
goodSpart <- brick("output/BART/Jotr_BART_spartials_2019.grd")
projection(goodSpart)<-CRS("+init=epsg:4269")

goodSpart.mask <- mask(goodSpart, st_transform(sdm.pres[,2], crs=4269))

goodSpart.df <- cbind(coordinates(goodSpart.mask), as.data.frame(goodSpart.mask)) %>% filter(!is.na(pptY1Y2)) |> rename(lon=x, lat=y) |> pivot_longer(all_of(jotr.preds), names_to="predictor", values_to="prFL") |> mutate(predictor=factor(predictor,jotr.preds))

levels(goodSpart.df$predictor) <- c("Delta[Y1-2]*PPT", "Delta[Y0-1]*PPT", "Max*VPD[Y0]", "Delta[Y0-1]*Min*VPD", "Min*Temp[Y0]", "Delta[Y0-1]*Max*Temp", "PPT[Y0]", "PPT[Y1]", "Min*VPD[Y0]", "Delta[Y0-1]*Max*VPD", "Max*Temp[Y0]", "PPT[Y2]", "Delta[Y0-1]*Min*Temp")

goodex <- ggplot(goodSpart.df, aes(x=lon, y=lat, fill=prFL)) + geom_tile() + facet_wrap("predictor", nrow=3, labeller="label_parsed") + scale_fill_gradient(low="#ffffcc", high="#253494", name="Marginal Pr(Fruit)", breaks=c(0.5,0.55,0.6,0.65), limits=c(0.5,0.675), labels=c("", 0.55, 0.6, 0.65)) + labs(title="Spatial partial effects for 2019") + theme_minimal() + theme(axis.title=element_blank(), axis.text=element_blank(), legend.position="bottom", legend.text=element_text(size=9), panel.background=element_rect(fill="white"), panel.grid=element_blank())

goodex

# map of observations
goodobs <- ggplot() + 
  geom_sf(data=coast, color="slategray2", linewidth=2.5) + 
  geom_sf(data=states, fill="antiquewhite2", color="antiquewhite4") + 
  geom_sf(data=sdm.pres, fill="red", alpha=0.5, color=NA) +
  geom_point(data=filter(flow2, year==2019), aes(x=lon, y=lat, shape=flr, color=flr, size=flr), alpha=0.75) +
  scale_shape_manual(values=c(21,20), labels=c("Flowering no Fruit", "Fruiting"), name="Flowering") +
  scale_size_manual(values=c(1.1,1.3), labels=c("Flowering no Fruit", "Fruiting"), name="Flowering") +
  scale_color_manual(values=c("#ffffcc", "#253494"), labels=c("Flowering no Fruit", "Fruiting"), name="Flowering") + # nb these are false, true
  coord_sf(xlim = c(-119, -112.75), ylim = c(32.75, 38), expand = FALSE) +
  labs(title="2019 observations") +
  theme_bw(base_size=11) + theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "slategray3"), axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), legend.position=c(0.5, 0.1), legend.box.margin=unit(c(0.005, 0.005, 0.005, 0.005), "in"), legend.title=element_blank(), legend.text=element_text(size=9), plot.margin=unit(c(0.05,0.05,0.05,0.05), "in"), legend.key.size=unit(0.075, "in"), legend.key=element_rect(fill="#A8A378"), legend.spacing=unit(c(0.005, 0.005, 0.005, 0.005), "in"), legend.direction="horizontal", legend.background=element_rect(color="black", linewidth=0.1))


# 2020, "bad year" example
badSpart <- brick("output/BART/Jotr_BART_spartials_2020.grd")
projection(badSpart)<-CRS("+init=epsg:4269")

badSpart.mask <- mask(badSpart, st_transform(sdm.pres[,2], crs=4269))

badSpart.df <- cbind(coordinates(badSpart.mask), as.data.frame(badSpart.mask)) %>% filter(!is.na(pptY1Y2)) |> rename(lon=x, lat=y) |> pivot_longer(all_of(jotr.preds), names_to="predictor", values_to="prFL") |> mutate(predictor=factor(predictor,jotr.preds))

levels(badSpart.df$predictor) <- c("Delta[Y1-2]*PPT", "Delta[Y0-1]*PPT", "Max*VPD[Y0]", "Delta[Y0-1]*Min*VPD", "Min*Temp[Y0]", "Delta[Y0-1]*Max*Temp", "PPT[Y0]", "PPT[Y1]", "Min*VPD[Y0]", "Delta[Y0-1]*Max*VPD", "Max*Temp[Y0]", "PPT[Y2]", "Delta[Y0-1]*Min*Temp")

badex <- ggplot(badSpart.df, aes(x=lon, y=lat, fill=prFL)) + geom_tile() + facet_wrap("predictor", nrow=3, labeller="label_parsed") + scale_fill_gradient(low="#ffffcc", high="#253494", name="Marginal Pr(Flowers)", breaks=c(0.5,0.55,0.6,0.65), limits=c(0.5,0.675), labels=c("", 0.55, 0.6, 0.65)) + labs(title="Spatial partial effects for 2020") + theme_minimal() + theme(axis.title=element_blank(), axis.text=element_blank(), legend.position="bottom", legend.text=element_text(size=9), panel.background=element_rect(fill="white"), panel.grid=element_blank())


# map of observations
badobs <- ggplot() + 
  geom_sf(data=coast, color="slategray2", linewidth=2.5) + 
  geom_sf(data=states, fill="antiquewhite2", color="antiquewhite4") + 
  geom_sf(data=sdm.pres, fill="red", alpha=0.5, color=NA) +
  geom_point(data=filter(flow2, year==2020), aes(x=lon, y=lat, shape=flr, color=flr, size=flr), alpha=0.75) +
  scale_shape_manual(values=c(21,20), labels=c("Flowering no Fruit", "Fruiting"), name="Flowering") +
  scale_size_manual(values=c(1.1,1.3), labels=c("Flowering no Fruit", "Fruiting"), name="Flowering") +
  scale_color_manual(values=c("#ffffcc", "#253494"), labels=c("Flowering no Fruit", "Fruiting"), name="Flowering") + # nb these are false, true
  coord_sf(xlim = c(-119, -112.75), ylim = c(32.75, 38), expand = FALSE) +
  labs(title="2020 observations") +
  theme_bw(base_size=11) + theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "slategray3"), axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), legend.position=c(0.5, 0.1), legend.box.margin=unit(c(0.005, 0.005, 0.005, 0.005), "in"), legend.title=element_blank(), legend.text=element_text(size=9), plot.margin=unit(c(0.05,0.05,0.05,0.05), "in"), legend.key.size=unit(0.075, "in"), legend.key=element_rect(fill="#A8A378"), legend.spacing=unit(c(0.005, 0.005, 0.005, 0.005), "in"), legend.direction="horizontal", legend.background=element_rect(color="black", linewidth=0.1))

badobs

# lower panel assembly
spartials <- goodex + badex + plot_layout(guides="collect") & theme(legend.position="bottom", legend.text=element_text(size=9), legend.key.height=unit(0.15, "in"), legend.box.margin=unit(c(0.01,0.05,0.01,0.05), "in"), plot.margin=unit(c(0.05,0.1,0.1,0.1),"in"))


{cairo_pdf("output/figures/Fig03_spartials.pdf", width=6.5, height=9)
  
  ggdraw() + draw_plot(goodobs, 0, 0.65, 0.5, 0.34)  + draw_plot(badobs, 0.5, 0.65, 0.5, 0.34) + draw_plot(spartials, 0.01, 0, 0.995, 0.65) + draw_plot_label(label=c("A", "B", "C", "D"), x=c(0, 0, 0.5, 0.48), y=c(0.995, 0.65, 0.995, 0.65))
  
}
dev.off()


#-------------------------------------------------------------------------
# Partials and spartials in each year with observations, for SI

sdm.pres <- read_sf("data/Yucca/jotr_BART_sdm_pres.shp", "jotr_BART_sdm_pres")

states <- read_sf(dsn = "data/spatial/10m_cultural/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")
coast <- read_sf("data/spatial/10m_physical/ne_10m_coastline/ne_10m_coastline.shp")



for(yr in 2008:2022){
  
  # yr <- 2008
  
  obs <- ggplot() + 
    geom_sf(data=coast, color="slategray2", linewidth=2.5) + 
    geom_sf(data=states, fill="antiquewhite2", color="antiquewhite4") + 
    geom_sf(data=sdm.pres, fill="red", alpha=0.5, color=NA) +
    geom_point(data=filter(flow2, year==yr), aes(x=lon, y=lat, shape=flr, color=flr, size=flr), alpha=0.75) +
    scale_shape_manual(values=c(21,20), labels=c("Flowering no Fruit", "Fruiting"), name="Flowering") +
    scale_size_manual(values=c(1.1,1.3), labels=c("Flowering no Fruit", "Fruiting"), name="Flowering") +
    scale_color_manual(values=c("#ffffcc", "#253494"), labels=c("Flowering no Fruit", "Fruiting"), name="Flowering") + # nb these are false, true
    coord_sf(xlim = c(-119, -112.75), ylim = c(32.75, 38), expand = FALSE) +
    labs(title=paste(yr, "observations")) +
    theme_bw(base_size=11) + theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "slategray3"), axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), legend.position=c(0.5, 0.1), legend.box.margin=unit(c(0.005, 0.005, 0.005, 0.005), "in"), legend.title=element_blank(), legend.text=element_text(size=9), plot.margin=unit(c(0.05,0.05,0.05,0.05), "in"), legend.key.size=unit(0.075, "in"), legend.key=element_rect(fill="#A8A378"), legend.spacing=unit(c(0.005, 0.005, 0.005, 0.005), "in"), legend.direction="horizontal", legend.background=element_rect(color="black", linewidth=0.1))
  
  spart <- brick(paste("output/BART/Jotr_BART_spartials_", yr, ".grd", sep=""))
  projection(spart)<-CRS("+init=epsg:4269")
  
  spart.mask <- mask(spart, st_transform(sdm.pres[,2], crs=4269))
  
  spart.df <- cbind(coordinates(spart.mask), as.data.frame(spart.mask)) %>% filter(!is.na(pptY1Y2)) |> rename(lon=x, lat=y) |> pivot_longer(all_of(jotr.preds), names_to="predictor", values_to="prFL") |> mutate(predictor=factor(predictor,jotr.preds))
  
  levels(spart.df$predictor) <- c("Delta[Y1-2]*PPT", "Delta[Y0-1]*PPT", "Max*VPD[Y0]", "Delta[Y0-1]*Min*VPD", "Min*Temp[Y0]", "Delta[Y0-1]*Max*Temp", "PPT[Y0]", "PPT[Y1]", "Min*VPD[Y0]", "Delta[Y0-1]*Max*VPD", "Max*Temp[Y0]", "PPT[Y2]", "Delta[Y0-1]*Min*Temp")
  
  spartplot <- ggplot(spart.df, aes(x=lon, y=lat, fill=prFL)) + geom_tile() + facet_wrap("predictor", nrow=3, labeller="label_parsed") + scale_fill_gradient(low="#ffffcc", high="#253494", name="Marginal Pr(Flowers)", breaks=c(0.5,0.55,0.6,0.65), limits=c(0.5,0.675), labels=c("", 0.55, 0.6, 0.65)) + labs(title=paste("Spatial partial effects for", yr)) + theme_minimal() + theme(axis.title=element_blank(), axis.text=element_blank(), legend.position="bottom", legend.text=element_text(size=9), panel.background=element_rect(fill="white"), panel.grid=element_blank())
  
  
  
  {cairo_pdf(paste("output/figures/SIFigX_spartials_", yr, ".pdf", sep=""), width=4, height=9)
    
    plot <- ggdraw() + draw_plot(obs, 0, 0.65, 1, 0.34) + draw_plot(spartplot, 0.05, 0, 0.85, 0.65) + draw_plot_label(label=c("A", "B"), x=0, y=c(0.995, 0.65))
    
    print(plot)
    
  }
  dev.off()
  
  
} # END loop over years 




#-------------------------------------------------------------------------
# LOO by year, for more confirmation

table(flow2$year) # do we have enough for all years? not enough for 9,10, or 11 
#so skip them I guess

LOOvalid <- data.frame(matrix(0,0,4))
colnames(LOOvalid) <- c("year", "N_flr", "N_noflr", "AUC")

# LOOP over years
for(yr in unique(flow2$year)){
  if (yr %in% c(2009, 2010, 2011)) {
    next  # Skip these years
  }
  # yr <- 2010
  
  inbag <- flow2 |> filter(year!=yr)
  oobag <- flow2 |> filter(year==yr)
  
  testmod <- bart(y.train=as.numeric(inbag[,"flr"]), x.train=inbag[,jotr.preds], keeptrees=TRUE)
  
  # data for OOB year
  OOBpreds <- brick(paste("data/PRISM/derived_predictors/PRISM_derived_predictors_",yr,".gri", sep=""))
  
  # prediction at OOB sites with the RI predictor (year) removed
  testpred.ri0 <- predict(testmod, OOBpreds[[attr(testmod$fit$data@x, "term.labels")]], splitby=20, ri.data=yr, ri.name='year', ri.pred=FALSE)
  
  OOBpreds <- raster::extract(testpred.ri0, oobag[,c("lon", "lat")]) # predicted OOB sites with model
  
  # hacked out of the summary function for rbarts
  auc <- performance(prediction(OOBpreds, oobag$flr),"auc")@y.values[[1]]
  
  LOOvalid <- rbind(LOOvalid, data.frame(year=yr, N_flr=length(which(oobag$flr)), N_noflr=length(which(!oobag$flr)), AUC=auc))
  
} # END loop over years

LOOvalid <- LOOvalid |> arrange(year) # eeeeeh

write.table(LOOvalid, "output/BART/year-year-LOO.csv", sep=",", col.names=TRUE, row.names=FALSE)

# LOOvalid <- read.csv("output/BART/year-year-LOO.csv")

mean(LOOvalid$AUC) # 0.49
sd(LOOvalid$AUC) # 0.108
sd(LOOvalid$AUC)/sqrt(nrow(LOOvalid)) # SE = 0.022

ggplot(LOOvalid, aes(x=N_flr, y=AUC)) + geom_point()
ggplot(LOOvalid, aes(x=N_noflr, y=AUC)) + geom_point()
ggplot(LOOvalid, aes(x=N_flr+N_noflr, y=AUC)) + geom_point()
ggplot(LOOvalid, aes(x=N_flr/N_noflr, y=AUC)) + geom_point()  




#-------------------------------------------------------------------------
# "recent" model based only on years with >20 observations? see if it give me anything better

table(flow2$year) # at least 100/yr from 2016 onward

flow4 <- flow2 |> filter(year >= 2015)

jotr.recent.mod <- bart(y.train=as.numeric(flow4[,"flr"]), x.train=flow4[,jotr.preds], keeptrees=TRUE)

summary(jotr.recent.mod)

invisible(jotr.recent.mod$fit$state)
write_rds(jotr.recent.mod, file="output/BART/bart.recent.model.Jotr.rds")
# jotr.mod <- read_rds("output/BART/bart.recent.model.Jotr.rds")

# cross-validate with earlier data ---------------
earlyValid <- data.frame(matrix(0,0,5))
colnames(earlyValid) <- c("year", "lat", "lon", "flr", "PrFlr")

# LOOP over years
for(yr in 2008:2014){
  
  # yr <- 2010
  oobag <- flow2 |> filter(year==yr)
  
  # data for OOB year
  OOBpreds <- brick(paste("data/PRISM/derived_predictors/PRISM_derived_predictors_",yr,".gri", sep=""))
  
  # prediction at OOB sites with the RI predictor (year) removed
  testpred <- predict(jotr.recent.mod, OOBpreds[[attr(jotr.recent.mod$fit$data@x, "term.labels")]])
  
  earlyPreds <- raster::extract(testpred, oobag[,c("lon", "lat")]) # predicted OOB sites with model
  
  earlyValid <- rbind(earlyValid, data.frame(year=yr, lon=oobag$lon, lat=oobag$lat, flr=oobag$flr, PrFlr=earlyPreds[,"layer"]))
  
}

glimpse(earlyValid)

# hacked out of the summary function for rbarts
auc <- performance(prediction(earlyValid$PrFlr, earlyValid$flr), "auc")@y.values[[1]]

auc # 0.648, okay



#go to #combined_flowering_fruiting_modeling along with completed Fruiting phenology script 