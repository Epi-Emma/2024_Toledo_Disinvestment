#########################################
## Prepare Toledo PD Data for Analysis ##
#########################################

# Written By: Emma Gause
# Date: 04/05/23

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

#read in Toledo PD data
pd <- fread(paste0(datadir, "Toledo_PD/ShootingsV5_6179116494251382402.csv"))
  #Shootings Data Source: Toledo Police Shooting Crime Stats (ArcGIS)
  # https://police-transparency-toledopolice.hub.arcgis.com/pages/crime-stats

#read in street nodes (intersections and midpoints) to match to cases
streets <- read_sf(paste0(dataexp, "Street_Nodes/Street_Nodes/Street_Nodes_MajorMinor_Subset.shp"))

##----------------------------------------------------------------------------##

#subset Toledo PD firearm incidents to dates after last 2021 survey completed
  #include incidents post 10/06/2021
str(pd)
pd <- pd %>% mutate(date_time = str_replace(Date_Time, "T", " "),
                    date_time = as_datetime(date_time, format = c("%Y-%m-%d %H:%M:%S")),
                    date = date(date_time)) %>% select(-Date_Time)
summary(pd$date)
inc <- pd %>% filter(date > "2021-10-06") #3315 incidents included
summary(inc$date)

#create "any person shot" var:
inc <- inc %>% mutate(shotY = if_else(Shot>0, 1, 0))
table(inc$shotY, useNA = "ifany")

#create spatial points object for incidents
incshp = st_as_sf(inc, coords = c("X", "Y"), crs = 4326, remove = FALSE)
mapview(incshp)

#save subset data
#write_sf(incshp, paste0(dataexp, "Toledo_PD/Toledo_Inc_postOct62021.shp"))

##----------------------------------------------------------------------------##

#assign cases and exclusions
#CASES: injurious interpersonal incidents [type=="assault"|type=="homicide" & shot>0]
#EXCLUSIONS: non-injurious interpersonal incidents [type=="assault"|type=="homicide" & shot==0]

#for homicide where shot ==0, change to 1 -- this is an obvious typo

incshp$Shot[incshp$Type=="Homicide"&incshp$Shot==0] <- 1
incshp$shotY[incshp$Type=="Homicide"&incshp$Shot==0] <- 1
table(incshp$Type, incshp$shotY, useNA = "ifany")


#assign cases & exclusions
incshp <- incshp %>% mutate(case = if_else((Type=="Homicide"|Type=="Aggravated Assault")&
                                             shotY==1, 1, 0),
                            exclude = if_else((Type=="Homicide"|Type=="Aggravated Assault")&
                                                shotY!=1, 1, 0))

table(incshp$case, incshp$exclude, useNA = "ifany", deparse.level = 2)

#keep cases and exclusions only to merge to street points
cases <- incshp %>% filter(case==1|exclude==1)


#make the crs between inc and street nodes the same
st_crs(cases)
st_crs(streets)
cases <- st_transform(cases, st_crs(streets))

#cases
#join to nearest street node - within a max distance of 2000 ft
casepts <- st_join(cases, streets, join = nngeo::st_nn, maxdist=2000 , k=1, left = FALSE)
table(casepts$type, casepts$case, useNA = "ifany") #122 intersections; 159 midpoints
nrow(cases) - nrow(casepts) #1 points not joined -- outside city limits

#mapview(caseonly, color="red") + mapview(casepts, color = "yellow")

#merge back to street points to assign cases and non-cases
str(casepts)
#remove duplicate street columns, leaving "streetid" for merge, and remove geometry
casesxx <- casepts %>% select(FID, streetid, Type, DOW, HOD, Year, X, Y, Call911,
                              date_time, date, Shot, shotY, case, exclude) %>% 
          st_drop_geometry()
str(casesxx)

casestreets <- left_join(streets, casesxx, by = "streetid")
table(casestreets$case, useNA = "ifany") #counts match
table(casestreets$exclude, useNA = "ifany")
table(casestreets$type, useNA = "ifany")

str(casestreets)
#fix merged columns with the same name -- make more informative
casestreets <- casestreets %>% rename("inc_type" = "Type",
                                      "road_type" = "type")
str(casestreets)

#save street snapped data
saveRDS(casestreets, paste0(dataexp, "case_control_points.rds"))
write_sf(casestreets, dsn = paste0(dataexp, "case_control_points/case_control_points.shp"),
         delete_dsn = TRUE)



