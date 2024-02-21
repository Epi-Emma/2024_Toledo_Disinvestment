######################
## Create Table One ##
######################

# Written By: Emma Gause
# Date: 05/18/23
# Last updated: 08/08/23

#Load in Libraries
library("tidyverse")
library("dplyr")
library("sf")
library("tableone")

#Create path to directory
datadir <- "/[path to data directory]/"

#read in case/control street points
dat <- read_sf(paste0(datadir, "Matched/Matched_wExposure.shp"))

##----------------------------------------------------------------------------##


#Create Table 1 
str(dat)
data <- st_drop_geometry(dat)

#create an any vs. none field for abandoned structure
data <- data %>% mutate(any_aband = if_else(pct_bnd>0, 1, 0))

table(data$any_aband, data$cctype, useNA = "ifany", deparse.level = 2)

tab1 <- CreateCatTable(vars = c("inc_typ", "rd_typ_", "mjr_rd_", "zntyp__", "Year",
                                "any_aband", "any_vcn"),
                        data = data, strata = "cctype", test = FALSE, includeNA = TRUE,
                        addOverall = FALSE)
tab1


mean(data$pov_pct[data$cctype=="case"])*100
mean(data$pov_pct[data$cctype=="control"])*100


case <- data %>% filter(cctype=="case")
contrl <- data %>% filter(cctype=="control")
summary(case$pov_pct)
summary(contrl$pov_pct)

summary(case$disnvst)
summary(contrl$disnvst)

summary(case$pct_dsr)
summary(contrl$pct_dsr)

summary(case$pct_trs)
summary(contrl$pct_trs)

summary(case$pct_gsd)
summary(contrl$pct_gsd)

