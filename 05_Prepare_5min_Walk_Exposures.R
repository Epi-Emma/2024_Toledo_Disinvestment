###############################
## Create Exposure Variables ##
###############################

# Written By: Emma Gause
# Date: 06/07/23
# Last Updated: 11/09/23

#Load in Libraries
library("tidyverse")
library("dplyr")
library("sf")
library("mapview")

#Create path to directory
datadir <- "/[path to data directory]/"

#read in case/control street points
dat <- read_sf(paste0(datadir, "Data/Matched/Matched_Analsis_Set.shp"))

#read in exposure parcels
exp <- read_sf(paste0(datadir, "Raw_Data/Parcels2021/parcels2021.shp"))

#read in 5min walking buffers [these were created in ArcPro for every case and control point]
walks <- read_sf(paste0(datadir, "Data/Service_Areas/precise.shp"))

##----------------------------------------------------------------------------##

st_crs(dat) # NAD83 / Ohio North (ftUS)
st_crs(exp) # NAD83 / Ohio North (ftUS)
st_crs(walks) # World Geodetic System 1984

#reproject walks to align with crs of other data
walks <- st_transform(walks, st_crs(exp))

#now projection is accurate, calculate area within buffers
walks$area <- st_area(walks) #Take care of units
str(walks$area) #this is in ft^2

#mapview(walks) + mapview(dat)

#get id of case/control point to merge with walk buffers
str(dat)
ccid <- dat %>% select(randmID) %>% st_drop_geometry()

walks <- cbind(walks, ccid)
#write_sf(walks, paste0(datadir, "Data/Scratch/test_walk_buffers_ids.shp")) # testing to see if cbind aligned correctly -- they did!

##----------------------------------------------------------------------------##

expx <- exp %>% select(parcelnumb, status, parcelnum2, address:sidewalk, lat, lon, created_at)
str(expx)
summary(expx)

#Exposure defined as:
#disrepair: 0 or 1 - anything but perfect
#Land Bank disinvestment Index: responses range from 10 (perfect) with points subtracted for disrepair conditions
#abandoned: abandoned or the worst conditions 0/1
#vacancy: abandoned vacancy 0/1

##----------------------------------------------------------------------------##
#create two files - one for structure present and one for no structure present
struc <- expx %>% filter(struc_pres == "Yes")
open <- expx %>% filter(struc_pres == "No")

#mapview(struc)
#mapview(open)

colnames(struc)
table(struc$roof, useNA = "ifany")
table(struc$fire, useNA = "ifany")
table(struc$walls, useNA = "ifany")
table(struc$doors, useNA = "ifany")
table(struc$windows, useNA = "ifany")
table(struc$struc_intg, useNA = "ifany")

#create the 0-2 levels for each structure question
struc <- struc %>% mutate(roof_n = 
                            case_when(roof == "Functional" ~ 0,
                                roof == "Ready to be replaced" ~ 1,
                                roof == "Open to the elements" ~ 2),
                          fire_n = 
                            case_when(fire == "No damage" ~ 0,
                                      fire == "Minor damage - still habitable" ~ 1,
                                      fire == "Significant damage - uninhabitable" ~ 2),
                          walls_n = 
                            case_when(walls == "Maintained" ~ 0,
                                      walls == "Minor peeling paint/missing siding" ~ 1,
                                      walls == "Significant peeling paint/missing siding" ~ 2),
                          doors_n = 
                            case_when(doors == "Secure" ~ 0,
                                      doors == "Boarded or missing" ~ 2),
                          windows_n = 
                            case_when(windows == "Secure" ~ 0,
                                      windows == "Multiple boarded/damaged/missing windows" ~ 2),
                          struc_intg_n = 
                            case_when(struc_intg == "Functional" ~ 0,
                                      struc_intg == "Shifted" ~ 1,
                                      struc_intg == "Collapsed" ~ 2))
table(struc$roof, struc$roof_n)
table(struc$fire, struc$fire_n)
table(struc$walls, struc$walls_n)
table(struc$doors, struc$doors_n)
table(struc$windows, struc$windows_n)
table(struc$struc_intg, struc$struc_intg_n)


struc <- struc %>% mutate(disinvest = roof_n+fire_n+walls_n+doors_n+windows_n+windows_n, # we decided not to use this metric -- use Land Bank Index instead
                          disrepr01 = if_else(disinvest>0, 1, 0))
table(struc$disrepr01)

#create abandoned var
struc <- struc %>% mutate(aband = if_else((roof_n==2|
                                                 fire_n==2|
                                                 doors_n==2|
                                                 windows_n==2|
                                                 struc_intg_n==2), 1, 0))
table(struc$occupancy, struc$aband)
                          

#create the Land Bank grading scores
struc <- struc %>% mutate(roof_lb = 
                            case_when(roof == "Functional" ~ 0,
                                      roof == "Ready to be replaced" ~ 4,
                                      roof == "Open to the elements" ~ 7),
                          fire_lb = 
                            case_when(fire == "No damage" ~ 0,
                                      fire == "Minor damage - still habitable" ~ 3,
                                      fire == "Significant damage - uninhabitable" ~ 7),
                          walls_lb = 
                            case_when(walls == "Maintained" ~ 0,
                                      walls == "Minor peeling paint/missing siding" ~ 1,
                                      walls == "Significant peeling paint/missing siding" ~ 3),
                          doors_lb = 
                            case_when(doors == "Secure" ~ 0,
                                      doors == "Boarded or missing" ~ 3),
                          windows_lb = 
                            case_when(windows == "Secure" ~ 0,
                                      windows == "Multiple boarded/damaged/missing windows" ~ 2),
                          struc_intg_lb = 
                            case_when(struc_intg == "Functional" ~ 0,
                                      struc_intg == "Shifted" ~ 2,
                                      struc_intg == "Collapsed" ~ 7))
table(struc$roof, struc$roof_lb)
table(struc$fire, struc$fire_lb)
table(struc$walls, struc$walls_lb)
table(struc$doors, struc$doors_lb)
table(struc$windows, struc$windows_lb)
table(struc$struc_intg, struc$struc_intg_lb)


struc <- struc %>% mutate(lb_minus = roof_lb+fire_lb+walls_lb+doors_lb+windows_lb+windows_lb,
                          lb_score = 10-lb_minus)
summary(struc$lb_score)



#excellent! Get the vars we want to aggregate and st_join to buffers
str(struc)
expx2 <- struc %>% select(disinvest, disrepr01, aband, occupancy, lb_score)
walksx2 <- st_join(walks, expx2, join = st_intersects, left = TRUE)

#merge back to parcel shapefile and export for visualization
expx2_forvis <- struc %>% select(parcelnumb, disinvest, disrepr01, aband, occupancy, lb_score) %>% st_drop_geometry()
exp_for_vis <- left_join(expx, expx2_forvis, by = "parcelnumb")


#aggregate
walksxxx <- walksx2 %>% group_by(randmID) %>%
  summarise(disinvest = mean(disinvest, na.rm = TRUE),
            disrep_av = mean(disrepr01, na.rm = TRUE),
            disrep_n = sum(disrepr01, na.rm = TRUE),
            aband_av = mean(aband, na.rm = TRUE),
            aband_n = sum(aband, na.rm = TRUE),
            lb_score_5min = mean(lb_score, na.rm = TRUE),
            n_ps_strc = n()) %>% 
  ungroup() %>% st_drop_geometry()

#percent of structures abandoned and percent of structures with any visual disrepair
walksxxx <- walksxxx %>% mutate(pct_aband = aband_n/n_ps_strc,
                                pct_disrepr = disrep_n/n_ps_strc)

walks <- left_join(walks, walksxxx, by = "randmID")

##----------------------------------------------------------------------------##

#vacant areas
table(open$u_kind, useNA = "ifany")
table(open$u_abandon, useNA = "ifany")

#we want an any/none flag for vacant lots, similar to what we did for abandoned structures 
table(expx$struc_pres, expx$u_abandon, useNA = "ifany", deparse.level = 2)

#create a vacant abandoned vs. other field
expx <- expx %>% mutate(vacnt = if_else(u_abandon=="Yes", 1, 0))
table(expx$vacnt, useNA = "ifany")
expx$vacnt[is.na(expx$vacnt)] <- 0 #the missing are those with structures (i.e. NOT vacant)

#excellent! Get the vacancy var we want to aggregate and st_join to buffers
expx3 <- expx %>% select(vacnt)
walksx3 <- st_join(walks, expx3, join = st_intersects, left = TRUE)

#merge back to parcel shapefile and export for visualization
expx3_forvis <- expx %>% select(parcelnumb, vacnt) %>% st_drop_geometry()
exp_for_visx <- left_join(exp_for_vis, expx3_forvis, by = "parcelnumb")

#create new vacnt_x: set to missing is structure present (i.e. not vacant)
exp_for_visx <- exp_for_visx %>% mutate(vacnt_x = if_else(struc_pres=="No", vacnt, NA))

#save it
write_sf(exp_for_visx, paste0(datadir, "Data/Exposure_Parcels/Exposure_Parcels_110923.shp"))


#calculate number of vacant abandoned lots within 5-min walking buffers
walksxxxx <- walksx3 %>% group_by(randmID) %>% 
  summarize(vacnt_n = sum(vacnt, na.rm = TRUE),
            n_lots = n()) %>% #denominator
  ungroup() %>% st_drop_geometry() 
  
walksxxxx <- walksxxxx %>% mutate(any_vacnt = if_else(vacnt_n>0, 1, 0),
                                  vacnt_pct = vacnt_n/n_lots,
                                  vacnt_pct100 = vacnt_pct*100) %>% 
  select(randmID, vacnt_n, any_vacnt, vacnt_pct, vacnt_pct100)

walks <- left_join(walks, walksxxxx, by = "randmID")

table(walks$any_vacnt, useNA = "ifany", deparse.level = 2)

##----------------------------------------------------------------------------##

#prepare to merge to case/control points and then export
str(walks)

walks_final <- walks %>% select(-Name:-Shape_Area) %>% st_drop_geometry()
colnames(walks_final)
str(dat)

analysis_pts <- left_join(dat, walks_final, by = "randmID")

write_sf(analysis_pts, paste0(datadir, "Data/Matched/Matched_wExposure_110923.shp"))
  
  
  
  
  
  
  


