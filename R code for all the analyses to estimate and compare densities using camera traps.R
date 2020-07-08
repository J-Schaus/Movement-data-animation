#######################################################################################################################
#Supplementary Material -R code for all analyses
#
#Application of the Random Encounter Model in citizen science projects to monitor animal densities 
#Journal: Remote Sensing in Ecology and Conservation
#
#Jessica Schaus, Antonio Uzal, Louise K. Gentle, Philip J. Baker,Lucy Bearman-Brown, Simone Bullion, Abigail Gazzard,
#Hannah Lockwood,Alexandra North, Tom Reader, Dawn M. Scott, Christopher S. Sutherland, Richard W. Yarnell
#
# Script Author: Jessica Schaus
# SPDX-License-Identifier: GPL-3.0-or-later
# GNU GPLv3+ © 2020 Jessica Schaus

######################################################################################################################
# Data to run all the analysis in the code can be found in https://figshare.com/s/18252f7b85939e6b9b72

#activity.xlsx--> includes time-of-day data required to obtain activity levels from camera trap data

#camDat.xlsx ---> contains the number of videos and camera detection parameters for each camera location


######################################################################################################################
#1. Calculate densities using the Random Encounter Model (REM)
#Instal package remBoot

install.packages("devtools")
devtools::install_github("arcaravaggi/remBoot")


#Load package
library(remBoot)
library(readxl)
#For more detail on calculating densities using the Random Encounter Model,
#please refer to the viggetes from the remBoot package (https://github.com/arcaravaggi/remBoot)
#This is an example on how to calculate REM densities at two sites (Southwell, Brackenhurs2017)
#and the confidence intervals (CI) for one of those sites (Southwell). 
#Load the data(camDat)
#Each observation (row) in the data set corresponds to a camera trap and it needs to have 4 columns:
#"Site" = Site ID      
#"count"     = Number of videos recorded 
#"dist"      = averaged detention distance (km) of the camera
#"theta"   = angle of detection  (detection arc *2) of the camera

camDat <- read_excel("camDat.xlsx")

##Prepare the data

camDat<-as.data.frame(camDat)
camDat$Site[camDat$Site=="Southwell"] <- 1
camDat$Site[camDat$Site=="Brackenhurst2017"] <- 2
camDat$Site<-as.integer(camDat$Site)
camDat$count<-as.integer(camDat$count)

#Split the data by survey
grpDat <- split_dat(camDat)

#Obtain the density estimates (animals/km2) using the specific trapping effort (tm;hours) 
#and daily movement range (v;km/h) for each survey
rem(grpDat[[1]], tm = 5222, v = 0.64) #Southwell
rem(grpDat[[2]], tm = 6507, v = 0.53) #Brackenhurst2017


#CI for Southwell
#Set the number of bootstrap iterations 
nboots <- 1000
tm <- 5222
v <- 0.64
#Calculate SD
remBoot(grpDat[[1]], tm, v, nboots, error_stat = c("sd"))

#Calculate 95% confidence intervals using the quantiles
remBoot(grpDat[[1]], tm, v, nboots, error_stat = c("quant"))

#################################################################################################
#####################################################################################################
###################################################################################################
#2. Calculate activity 
##Calculate activity levels for Southwell

library(statsr)
library(activity)

#Load the data activity.xlsx 
activity <- read_excel("activity.xlsx")

#Convert time of day data to numeric
htime <- gettime(activity$Time, "%Y-%m-%d %H:%M:%S", "hour")#decimal hours [0-24]
ptime <- gettime(activity$Time, "%Y-%m-%d %H:%M:%S", "proportion")#proportion [0-1]

#Extract radian time-of-day
tm <- 2*pi*ptime
#Obtain (in radians) when the first and last video was recorded, and calculate the activity within
#this window time
#Angular conversion from hours and minutes to radians
library("astroFns")
first_video<-hms2rad(h = '21h 22 m')
last_video<-hms2rad(h = '4h 20m')

#Fit activity models to time-of-day data
activity<-fitact(tm,bounds=c(first_video,last_video))#use "bounds" to define radian bounds at which 
#to truncate
activity
#     act 
#0.812732
plot(activity,centre="night")
########################################################################################################
####################################################################################################
####################################################################################################
#3. Power analysis of the averREM

#An example of how to calculate the power analysis of the aveREM in Southwell
#load package
library(pwr)
#REM density in Southwell
#number of cameras = 110
#Density= 25.9
#Standard Deviation= 3.7
#10% difference in the densities, will indicate an increase of 2.59 animals/km2
#25% difference in the densities, will indicate an increase of 6.475 animals/km2
#50% difference in the densities, will indicate an increase of 12.95 animals/km2

#To calculate power, we need the effect size (Cohen's d), which is the difference
#between the means divided by the SD
#effect size= Difference / SD
#10% effect size= 2.59/3.7 =  0.777
#25% effect size= 6.475/3.7=  1.7500
#50% effect size= 12.95/3.7=  3.5000


#Power calculations for t-tests of means
pwr.t.test(n=110, d=0.7000,sig.level = 0.05,type = "two.sample", alternative = "two.sided")
pwr.t.test(n=110, d=1.7500,sig.level = 0.05,type = "two.sample", alternative = "two.sided")
pwr.t.test(n=110, d=3.5000,sig.level = 0.05,type = "two.sample", alternative = "two.sided")


##############################################################################################
#Calculate sample size (camera traps) needed to  detect 10%, 25% and 50% population change with 
#0.80, 0.90 and 0.95 statistical power in future surveys

#10%
pwr.t.test(power = 0.80, d=0.7000,sig.level = 0.05,type = "two.sample", alternative = "two.sided")
pwr.t.test(power = 0.90, d=0.7000,sig.level = 0.05,type = "two.sample", alternative = "two.sided")
pwr.t.test(power = 0.95, d=0.7000,sig.level = 0.05,type = "two.sample", alternative = "two.sided")

#detect 25% change
pwr.t.test(power = 0.80, d=1.7500,sig.level = 0.05,type = "two.sample", alternative = "two.sided")
pwr.t.test(power = 0.90, d=1.7500,sig.level = 0.05,type = "two.sample", alternative = "two.sided")
pwr.t.test(power = 0.95, d=1.7500,sig.level = 0.05,type = "two.sample", alternative = "two.sided")
#detect 50% change
pwr.t.test(power = 0.80, d=3.5000,sig.level = 0.05,type = "two.sample", alternative = "two.sided")
pwr.t.test(power = 0.90, d=3.5000,sig.level = 0.05,type = "two.sample", alternative = "two.sided")
pwr.t.test(power = 0.95, d=3.5000,sig.level = 0.05,type = "two.sample", alternative = "two.sided")


########################################################################################################
####################################################################################################
####################################################################################################
#3. Calculate densities using Spatial Capture-Recapture models
#For further details, refer to users detailed vignette of the oSCR package (https://rdrr.io/github/jaroyle/oSCR/)
#Data can be found in https://figshare.com/s/18252f7b85939e6b9b72
#SCRdata.xlsx ---> includes the detectors information and capture history files for all surveys
#install package
install.packages("remotes")
remotes::install_github("jaroyle/oSCR")

#load package
library(oSCR)
library(ggplot2)
#load data
edf <- data.frame(readxl::read_xlsx("SCRdata.xlsx", sheet = 1))
edf$numSession <- as.numeric(factor(edf$session))

tdf <- data.frame(readxl::read_xlsx("SCRdata.xlsx", sheet = 2))
tdf$session <- do.call(rbind,strsplit(tdf$detector,"_"))[,1]
tdf$UR <- factor(tdf$UR,levels=c("rural","urban"))
tdf$Xkm <- tdf$X/1000
tdf$Ykm <- tdf$Y/1000

tdf1_brak17 <- subset(tdf, session %in% "Brk1")[,c(1,7,8,4,5)]
tdf2_brak18 <- subset(tdf, session %in% "Brk2")[,c(1,7,8,4,5)]
tdf3_bright <- subset(tdf, session %in% "B")[,c(1,7,8,4,5)]
tdf4_hart   <- subset(tdf, session %in% "H")[,c(1,7,8,4,5)]
tdf5_ips17  <- subset(tdf, session %in% "Ip1")[,c(1,7,8,4,5)]
tdf6_ips18  <- subset(tdf, session %in% "Ip2")[,c(1,7,8,4,5)]
tdf7_read   <- subset(tdf, session %in% "R")[,c(1,7,8,4,5)]
tdf8_sb     <- subset(tdf, session %in% "SB")[,c(1,7,8,4,5)]
tdf9_south  <- subset(tdf, session %in% "S")[,c(1,7,8,4,5)]
tdf.ls <- list(tdf1_brak17,tdf2_brak18,tdf3_bright,tdf4_hart,
               tdf5_ips17,tdf6_ips18,tdf7_read,tdf8_sb,tdf9_south)

sf <- data2oscr(edf,
                tdf = tdf.ls,
                sess.col = 6, 
                id.col = 2, 
                occ.col = 3, 
                trap.col = 4, 
                sex.col = 5,
                K = c(13,17,10,10,6,15,8,17,11),
                ntraps = sapply(tdf.ls,nrow),
                sex.nacode = "NA",
                tdf.sep = "\\",
                trapcov.names = "UR",
                remove.extracaps = TRUE
)$scrFrame

par(mfrow=c(3,3),mar=c(1,1,1,1),oma=c(0,0,0,0))
plot(sf,ax = F)

par(mfrow=c(3,3),mar=c(1,1,1,1),oma=c(0,0,0,0))
ss <- make.ssDF(sf,buffer = 0.4, res = 0.05)
plot(ss, sf)

run <- FALSE #TRUE for run models, FALSE for not

if(run == TRUE){
  mls <- list(oSCR.fit(model=list(D~1,       p0~1,      sig~1),      sf, ss, encmod = "B", trimS=0.4),
              oSCR.fit(model=list(D~session, p0~1,      sig~1),      sf, ss, encmod = "B", trimS=0.4),
              oSCR.fit(model=list(D~1,       p0~1,      sig~sex),    sf, ss, encmod = "B", trimS=0.4),
              oSCR.fit(model=list(D~session, p0~1,      sig~sex),    sf, ss, encmod = "B", trimS=0.4),
              oSCR.fit(model=list(D~1,       p0~sex,    sig~1),      sf, ss, encmod = "B", trimS=0.4),
              oSCR.fit(model=list(D~session, p0~sex,    sig~1),      sf, ss, encmod = "B", trimS=0.4),
              oSCR.fit(model=list(D~1,       p0~sex,    sig~sex),    sf, ss, encmod = "B", trimS=0.4),
              oSCR.fit(model=list(D~session, p0~sex,    sig~sex),    sf, ss, encmod = "B", trimS=0.4))
  save(mls,file="models.RData")
}

load("models.RData")
(ms <- modSel.oSCR(fitList.oSCR(mls,rename = T)))


############################################
#Prediction plots:

#Best: D(~session) p(~1) sig(~sex)
pdf.dens.best <- data.frame(session=factor(1:9))
pred.d.best <- get.real(mls[[4]], type="dens", newdata = pdf.dens.best, d.factor = 400)
est.tab <- rbind(pred.d.best[1:9,c(1,3,4)],
                 pred.d.best[10:18,c(1,3,4)],
                 pred.d.best[1:9,c(1,3,4)]+pred.d.best[10:18,c(1,3,4)])
est.tab$Group <- factor(rep(c("Female","Male","Total"),each=9))
est.tab$Site <- rep(levels(factor(edf$session)),3)
est.tab$Site <- factor(est.tab$Site,levels=c("SB","Brack2017","Brack2018","Hartpury",
                                             "Reading","Southwell","Brighton2018",
                                             "Ipswich2017","Ipswich2018"))
est.tab$Type <- factor(rep(c("Rural","Rural","Urban","Rural","Urban","Urban","Urban","Rural","Urban"),3))


gd <- ggplot(est.tab,aes(x=Site,y=estimate, fill=Type)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr,width=0)) +
  geom_point(size=4,shape=21) +
  facet_wrap(.~Group,nrow=1) +
  theme_bw() + ylab("Density (per km2)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
gd

#END
