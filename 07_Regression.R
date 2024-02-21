##################################
## Logistic Regression Analysis ##
##################################

# Written By: Emma Gause
# Date: 05/18/23
# Last Updated: 12/05/23

#Load in Libraries
library("tidyverse")
library("dplyr")
library("sf")
library("survival")
library("mgcv")
library("splines")
library("mapview")

#Create path to directory
datadir <- "/[path to data directory]/"

#read in case/control street points
dat <- read_sf(paste0(datadir, "Matched/Matched_wExposure.shp"))

##----------------------------------------------------------------------------##

xy <- st_coordinates(dat)
colnames(xy) <- c("xcoord", "ycoord")
datx <- cbind(dat, xy)

#drop geometry from df for analysis
data <- st_drop_geometry(datx)
str(data)

#OUTCOME: ccty_01
#EXPOSURE(S):
  # disinvestment: disnvst
  # % any disrepair: pct_dsr
  # Abandonned structures: pct_bnd
  # Abandonned vacancy: vcnt_pc
  # Tree canopy: pct_trs

#MATCHING VARS: zntyp__, mjr_rd_, rd_typ_, pov_pct

##----------------------------------------------------------------------------##

# disinvestment: Land Bank disinvestment Index

summary(data$lb_sc_5)

#make score inverse so higher score == higher risk (to match directionality of other exposures)
data <- data %>% mutate(lb_score_inv = abs(lb_sc_5-10))
summary(data$lb_score_inv)

#look at the data -- last verification step before modeling
hist(data$lb_score_inv[data$ccty_01==1])
hist(data$lb_score_inv[data$ccty_01==0])


lbg <- glm(ccty_01 ~ lb_score_inv + 
             zntyp__ + mjr_rd_ + rd_typ_+ pov_pct,
           data = data,
           family = binomial(link="logit"))

summary(lbg)
cbind(exp(coef(lbg)), exp(confint(lbg)))


#W/ THIN PLATE SPLINE ADJUSTMENT
lbgspat <- gam(ccty_01 ~ lb_score_inv + 
                 zntyp__ + mjr_rd_ + rd_typ_+ pov_pct +  
                 s(xcoord, ycoord, bs = "tp"),
               data = data,
               select = FALSE,
               method="ML",   # using "ML" to align w/ non-spatial model (default is GCV.Cp)
               family = binomial(link="logit"))


summary(lbgspat)
(lbgOR <-lbgspat$coefficients[2])

exp(lbgOR)
exp(lbgOR + (1.96*0.107154)) #gather se from summary above
exp(lbgOR - (1.96*0.107154))

#FIT DISINVESTMENT AS A SPLINE
nllbg <- gam(ccty_01 ~ ns(lb_score_inv, k=2) + 
               zntyp__ + mjr_rd_ + rd_typ_+ pov_pct,
             data = data,
             select = FALSE,
             method="ML",   # using "ML" to align w/ non-spatial model (default is GCV.Cp)
             family = binomial(link="logit"))
summary(nllbg)
AIC(lbgspat, nllbg) #spline model not a better fit
BIC(lbgspat, nllbg) #spline model IS a better fit...


# PLOT SPLINE RESULTS 
pred.lbg <- predict(nllbg, data, type='terms', se.fit=TRUE)
newdat.lbg <- data.frame(cbind(data$lb_score_inv, pred.lbg$fit[,"ns(lb_score_inv, k = 2)"], pred.lbg$se.fit[,"ns(lb_score_inv, k = 2)"]))
names(newdat.lbg) <- c("lb_score_inv", "lb_sc_5fit", "lb_sc_5se")
#create point estimates, lower confidence bounds, and upper bounds for predicted outcomes (log and exponentiated scales)
newdat.lbg$est_lbg_log <- newdat.lbg$lb_sc_5fit
newdat.lbg$lc_lbg_log <- newdat.lbg$lb_sc_5fit - 1.96*newdat.lbg$lb_sc_5se
newdat.lbg$uc_lbg_log <- newdat.lbg$lb_sc_5fit + 1.96*newdat.lbg$lb_sc_5se

newdat.lbg$est_lbg <- exp(newdat.lbg$lb_sc_5fit)
newdat.lbg$lc_lbg <- exp(newdat.lbg$lb_sc_5fit - 1.96*newdat.lbg$lb_sc_5se)
newdat.lbg$uc_lbg <- exp(newdat.lbg$lb_sc_5fit + 1.96*newdat.lbg$lb_sc_5se)
newdat.lbg<-newdat.lbg[order(newdat.lbg[,"lb_score_inv"]), ]


jpeg(filename = "/[path to directory]/Figures/Land_Bank_INVERSE_Score_Splines.jpg",
     width = 5, height = 5, units = "in", res = 300)

plot(c(0,6), c(0, 10), pch='',
     xlab = "Land Bank Grade Score [INVERSE]", ylab = "Odds of a Firearm Incident", las = 1,
     cex.lab=0.9, bty="l", cex.axis=0.9)
legend(0, 10, c("Spline", "95% CI"), lty=1:2, box.lty=1, cex=0.65)

#add a rug to show density of values
rug(newdat.lbg$lb_score_inv)

#plot point estimates as solid line
lines(newdat.lbg$lb_score_inv, newdat.lbg$est_lbg, lwd=1.5)
#plot lower confidence interval as dotted line
lines(newdat.lbg$lb_score_inv, newdat.lbg$lc_lbg, lty=4)
#plot upper confidence interval as dotted line
lines(newdat.lbg$lb_score_inv, newdat.lbg$uc_lbg, lty=4)

dev.off()

##----------------------------------------------------------------------------##

# % any disrepair: pct_dsr

#make it a 10% change for more interpretable results <-- this is just a linear transformation of the data
data <- data %>% mutate(disrep10 = pct_dsr*10)

#look at the data -- last verifiication step before modeling
hist(data$disrep10[data$ccty_01==1])
hist(data$disrep10[data$ccty_01==0])

repairp <- glm(ccty_01 ~ disrep10 + 
               zntyp__ + mjr_rd_ + rd_typ_+ pov_pct,
             data = data,
             family = binomial(link="logit"))

cbind(exp(coef(repairp)), exp(confint(repairp)))
summary(repairp)


#W/ THIN PLATE SPLINE ADJUSTMENT
repairpspat <- gam(ccty_01 ~ disrep10 + 
                    zntyp__ + mjr_rd_ + rd_typ_+ pov_pct +  
                    s(xcoord, ycoord, bs = "tp"),
                  data = data,
                  select = FALSE,
                  method="ML",   # using "ML" to align w/ non-spatial model (default is GCV.Cp)
                  family = binomial(link="logit"))


summary(repairpspat)
(drepOR <-repairpspat$coefficients[2])

exp(drepOR)
exp(drepOR + (1.96*0.060589)) #gather se from summary above
exp(drepOR - (1.96*0.060589))


#FIT DISREPAIR AS A SPLINE
data <- data %>% mutate(pct_dsr100 = pct_dsr*100)
nlrepair <- gam(ccty_01 ~ ns(pct_dsr100, k=2) + 
                     zntyp__ + mjr_rd_ + rd_typ_+ pov_pct +  
                     s(xcoord, ycoord, bs = "tp"),
                   data = data,
                   select = FALSE,
                   method="ML",   # using "ML" to align w/ non-spatial model (default is GCV.Cp)
                   family = binomial(link="logit"))
summary(nlrepair)
AIC(repairpspat, nlrepair) #spline model not a better fit
BIC(repairpspat, nlrepair) #spline model IS a better fit


# PLOT SPLINE RESULTS 
pred.rep <- predict(nlrepair, data, type='terms', se.fit=TRUE)
newdat.rep <- data.frame(cbind(data$pct_dsr100, pred.rep$fit[,"ns(pct_dsr100, k = 2)"], pred.rep$se.fit[,"ns(pct_dsr100, k = 2)"]))
names(newdat.rep) <- c("disrep", "disrepfit", "disrepse")
#create point estimates, lower confidence bounds, and upper bounds for predicted outcomes (log and exponentiated scales)
newdat.rep$est_rep_log <- newdat.rep$disrepfit
newdat.rep$lc_rep_log <- newdat.rep$disrepfit - 1.96*newdat.rep$disrepse
newdat.rep$uc_rep_log <- newdat.rep$disrepfit + 1.96*newdat.rep$disrepse

newdat.rep$est_rep <- exp(newdat.rep$disrepfit)
newdat.rep$lc_rep <- exp(newdat.rep$disrepfit - 1.96*newdat.rep$disrepse)
newdat.rep$uc_rep <- exp(newdat.rep$disrepfit + 1.96*newdat.rep$disrepse)
newdat.rep<-newdat.rep[order(newdat.rep[,"disrep"]), ]


jpeg(filename = "/[path to directory]/Figures/Disrepair_Splines.jpg",
     width = 5, height = 5, units = "in", res = 300)

plot(c(0,80), c(0, 20), pch='',
     xlab = "Disrepair %", ylab = "Odds of a Firearm Incident", las = 1,
     cex.lab=0.9, bty="l", cex.axis=0.9)
legend(0, 20, c("Spline", "95% CI"), lty=1:2, box.lty=1, cex=0.65)

#add a rug to show density of values
rug(newdat.rep$disrep)

#plot point estimates as solid line
lines(newdat.rep$disrep, newdat.rep$est_rep, lwd=1.5)
#plot lower confidence interval as dotted line
lines(newdat.rep$disrep, newdat.rep$lc_rep, lty=4)
#plot upper confidence interval as dotted line
lines(newdat.rep$disrep, newdat.rep$uc_rep, lty=4)

dev.off()


##----------------------------------------------------------------------------##

# Abandonned structures: pct_bnd

#look at the data -- last verifiication step before modeling
summary(data$pct_bnd[data$ccty_01==1])
summary(data$pct_bnd[data$ccty_01==0])

#create an any vs. none field
data <- data %>% mutate(any_aband = if_else(pct_bnd>0, 1, 0))

aband01 <- glm(ccty_01 ~ any_aband + 
                 zntyp__ + mjr_rd_ + rd_typ_+ pov_pct,
               data = data,
               family = binomial(link="logit"))

cbind(exp(coef(aband01)), exp(confint(aband01)))


#W/ THIN PLATE SPLINE ADJUSTMENT
aband01spat <- gam(ccty_01 ~ any_aband + 
                     zntyp__ + mjr_rd_ + rd_typ_+ pov_pct +  
                     s(xcoord, ycoord, bs = "tp"),
                   data = data,
                   select = FALSE,
                   method="ML",   # using "ML" to align w/ non-spatial model (default is GCV.Cp)
                   family = binomial(link="logit"))


summary(aband01spat)
(abdnOR <-aband01spat$coefficients[2])

exp(abdnOR)
exp(abdnOR + (1.96*0.21100)) #gather se from model summary above
exp(abdnOR - (1.96*0.21100))

#where are these - after adj for spatial autocorrelation, OR changes 
mapview(dat, zcol = "pct_bnd") ##seem to cluster?



##----------------------------------------------------------------------------##

# Abandonned vacancy: vcnt_pc

#look at the data -- last verifiication step before modeling
hist(data$any_vcn[data$ccty_01==1])
hist(data$any_vcn[data$ccty_01==0])

vacant01 <- glm(ccty_01 ~ any_vcn + 
                 zntyp__ + mjr_rd_ + rd_typ_+ pov_pct,
               data = data,
               family = binomial(link="logit"))

cbind(exp(coef(vacant01)), exp(confint(vacant01)))


#W/ THIN PLATE SPLINE ADJUSTMENT
vacant01spat <- gam(ccty_01 ~ any_vcn + 
                     zntyp__ + mjr_rd_ + rd_typ_+ pov_pct +  
                     s(xcoord, ycoord, bs = "tp"),
                   data = data,
                   select = FALSE,
                   method="ML",   # using "ML" to align w/ non-spatial model (default is GCV.Cp)
                   family = binomial(link="logit"))


summary(vacant01spat)
(vacOR <-vacant01spat$coefficients[2])

exp(vacOR)
exp(vacOR + (1.96*0.20264)) #gather se from model summary above
exp(vacOR - (1.96*0.20264))


##----------------------------------------------------------------------------##

# Tree canopy: pct_trs

#look at the data -- last verifiication step before modeling
hist(data$pct_trs[data$ccty_01==1])
hist(data$pct_trs[data$ccty_01==0])

#make it a 10% change for more interpretable results <-- this is just a linear transformation of the data
data <- data %>% mutate(pcttree10 = pct_trs*10)

treep <- glm(ccty_01 ~ pcttree10 + 
                 zntyp__ + mjr_rd_ + rd_typ_+ pov_pct,
               data = data,
               family = binomial(link="logit"))

cbind(exp(coef(treep)), exp(confint(treep)))
summary(treep)


#W/ THIN PLATE SPLINE ADJUSTMENT
treepspat <- gam(ccty_01 ~ pcttree10 + 
                      zntyp__ + mjr_rd_ + rd_typ_+ pov_pct +  
                      s(xcoord, ycoord, bs = "tp"),
                    data = data,
                    select = FALSE,
                    method="ML",   # using "ML" to align w/ non-spatial model (default is GCV.Cp)
                    family = binomial(link="logit"))


summary(treepspat)
(treeOR <-treepspat$coefficients[2])

exp(treeOR)
exp(treeOR + (1.96*0.046110)) #gather se from model summary above
exp(treeOR - (1.96*0.046110))



#FIT TREE PRESENT % AS A SPLINE

#first get % trees as a 0-100 <-- just easier to plot than 0 to 1
data$pct_trees <- data$pct_trs*100
summary(data$pct_trees)

nltreep <- gam(ccty_01 ~ ns(pct_trees, k=2) + 
                  zntyp__ + mjr_rd_ + rd_typ_+ pov_pct,
                data = data,
                select = FALSE,
                method="ML",   # using "ML" to align w/ non-spatial model (default is GCV.Cp)
                family = binomial(link="logit"))
summary(nltreep)
AIC(treepspat, nltreep) #spline model not a better fit
BIC(treepspat, nltreep) #spline model IS a better fit...


# PLOT SPLINE RESULTS 
pred.tre <- predict(nltreep, data, type='terms', se.fit=TRUE)
newdat.tre <- data.frame(cbind(data$pct_trees, pred.tre$fit[,"ns(pct_trees, k = 2)"], pred.tre$se.fit[,"ns(pct_trees, k = 2)"]))
names(newdat.tre) <- c("pct_trees", "pct_treesfit", "pct_treesse")
#create point estimates, lower confidence bounds, and upper bounds for predicted outcomes (log and exponentiated scales)
newdat.tre$est_tre_log <- newdat.tre$pct_treesfit
newdat.tre$lc_tre_log <- newdat.tre$pct_treesfit - 1.96*newdat.tre$pct_treesse
newdat.tre$uc_tre_log <- newdat.tre$pct_treesfit + 1.96*newdat.tre$pct_treesse

newdat.tre$est_tre <- exp(newdat.tre$pct_treesfit)
newdat.tre$lc_tre <- exp(newdat.tre$pct_treesfit - 1.96*newdat.tre$pct_treesse)
newdat.tre$uc_tre <- exp(newdat.tre$pct_treesfit + 1.96*newdat.tre$pct_treesse)
newdat.tre<-newdat.tre[order(newdat.tre[,"pct_trees"]), ]


jpeg(filename = "/[path to directory]/Figures/Trees_Splines.jpg",
     width = 5, height = 5, units = "in", res = 300)

plot(c(0,90), c(0, 6), pch='',
     xlab = "Tree Present %", ylab = "Odds of a Firearm Incident", las = 1,
     cex.lab=0.9, bty="l", cex.axis=0.9)
legend(0, 6, c("Spline", "95% CI"), lty=1:2, box.lty=1, cex=0.65)

#add a rug to show density of values
rug(newdat.tre$pct_trees)

#plot point estimates as solid line
lines(newdat.tre$pct_trees, newdat.tre$est_tre, lwd=1.5)
#plot lower confidence interval as dotted line
lines(newdat.tre$pct_trees, newdat.tre$lc_tre, lty=4)
#plot upper confidence interval as dotted line
lines(newdat.tre$pct_trees, newdat.tre$uc_tre, lty=4)

dev.off()

