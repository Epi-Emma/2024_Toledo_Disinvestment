################################################
## Add Covariate Data Necessary for Matching  ##
################################################

# Written By: Emma Gause
# Date: 05/01/23

#Load in Libraries
library("tidyverse")
library("dplyr")
library("sf")
library("lubridate")
library("mapview")
library("nngeo")
library("stringr")
library("data.table")

#Create path to directory
datadir <- "/[path to data directory]/"
dataexp <- "/[path to export directory]/"

#read in case/control street points
streets <- read_sf(paste0(dataexp, "case_control_points/case_control_points.shp"))

#read in zoning data
zone <-  read_sf(paste0(datadir, "Toledo_Zoning/Zoning_Districts.shp"))
  
#read in poverty data (data and shape)
pov <- fread(paste0(datadir, "Poverty/nhgis0010_ds254_20215_blck_grp.csv"))
pov_bg <- read_sf(paste0(datadir, "Poverty/nhgis0010_shapefile_tl2021_390_blck_grp_2021/OH_blck_grp_2021.shp"))

#read in toledo city boundary to clip poverty data
bound <- read_sf(paste0(datadir, "City_Bountary/Toledo_City_Boundary.shp"))


##----------------------------------------------------------------------------##

#first create a unique ID for each street point for reference
streets$uid <- seq(1:nrow(streets))

##----------------------------------------------------------------------------##
#merge case/control points with their Toledo zoning info
str(zone)
mapview(test, zcol = "Descriptio")

table(zone$Descriptio, useNA = "ifany")

#condense categories -- residential vs. commercial vs. open space vs. industrial
zcats <- unique(zone$Descriptio) #35 caetgories
res <- zcats[(grepl("Residential", zcats, ignore.case = TRUE) | 
               grepl("Housing", zcats, ignore.case = TRUE)) &
               !grepl("Commercial", zcats,  ignore.case = TRUE)] #16 cats
mixed <- zcats[grepl("Residential", zcats, ignore.case = TRUE) & 
               grepl("Commercial", zcats,  ignore.case = TRUE)] #7 cats
com <- zcats[!grepl("Residential", zcats, ignore.case = TRUE) & 
               grepl("Commercial", zcats,  ignore.case = TRUE)] #7 cats
ind <- zcats[grepl("Industrial", zcats,  ignore.case = TRUE)] #3 cats
park <- zcats[grepl("Park", zcats,  ignore.case = TRUE)] #1 cat
ins <- zcats[grepl("Institutional", zcats,  ignore.case = TRUE)] #1 cat

zone <- zone %>% mutate(zonetype = case_when(
  Descriptio %in% res ~ "residential",
  Descriptio %in% mixed ~ "mixed",
  Descriptio %in% com ~ "commercial",
  Descriptio %in% ind ~ "industrial",
  Descriptio %in% park ~ "open_space",
  Descriptio %in% ins ~ "institutional"))

mapview(zone, zcol = "zonetype")

#dissolve boundaries by type
zone_dis <- zone %>% 
  group_by(zonetype) %>% 
  summarise(zonetype = first(zonetype)) %>% 
  st_cast() %>% ungroup()
str(zone_dis)
mapview(zone_dis, zcol = "zonetype")

#merge case/control points to zones - there are some boundary, hole, and sliver issues so need to do a nn join
pts <- st_join(streets, zone_dis, join = nngeo::st_nn, k=1) #this takes FOREVER
str(pts)
table(pts$zonetype, pts$case, useNA = "ifany")


##----------------------------------------------------------------------------##
#prepare the poverty data 
str(pov)
str(pov_bg)
mapview(pov_bg)

#AOQGE001:    Total
#AOQGE002:    Income in the past 12 months below poverty level
#AOQGE003:    Income in the past 12 months at or above poverty level

#create % households in poverty for each BG
pov <- pov %>% mutate(pov_pct = AOQGE002/AOQGE001) %>% select(GISJOIN, pov_pct)

#keep only necessary columns from shapefile for join to data
str(pov_bg)
pov_bg <- pov_bg %>% select(GISJOIN, GEOID, geometry)

#merge data to shape
pov_shp <- left_join(pov_bg, pov, by = "GISJOIN")
mapview(pov_shp, zcol = "pov_pct")

#clip to Toledo city boundary (buffered a little to keep all relevant info)
st_crs(bound)
st_crs(pov_shp)
bound <- st_transform(bound, st_crs(pts))
pov_shp <- st_transform(pov_shp, st_crs(pts))
bound_buff <- st_buffer(bound, dist = 5000)
pov_tol <- st_intersection(pov_shp, bound_buff)

#look at distribution of values
hist(pov_tol$pov_pct)
mapview(pov_tol, zcol = "pov_pct")

#PERFORM AREA-WEIGHTED AVERAGE OF POVERTY % FOR EACH POINT
  #Distance weighted to get around some MAUP problems

#first create 1/8 mile buffers around all points
st_crs(pts)$units # units = ft
#660 ft in 1/8 mi
pts_buff <- st_buffer(pts, dist = 660)

#now perform spatial interpolation with area weighting for each buffer based on BGs
pts_povint <- st_interpolate_aw(pov_tol["pov_pct"], pts_buff, extensive = FALSE, keep_NA = TRUE)

#cbind
pts_povint <- st_drop_geometry(pts_povint)
ptsx <- cbind(pts, pts_povint)

summary(ptsx$pov_pct)
#mapview(ptsx, zcol = "pov_pct")

##----------------------------------------------------------------------------##

#save them 
write_sf(ptsx, dsn = paste0(dataexp, "case_control_points/case_control_covariates.shp"),
         delete_dsn = TRUE)
