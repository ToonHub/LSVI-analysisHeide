########################
#### Bestandsnamen invoergegevens
########################

#---------------------
# Gegevens heide en 6510
#---------------------

dbHeideEn6510_2014_2015 <- "../Data/MeetgegevensHeide6510/FieldMapData_Heide6510_2014_2015.mdb"

dbHeideEn6510_2016 <- "../Data/MeetgegevensHeide6510/FieldMapData_Heide6510_2016.accdb"

#---------------------
# GegevensVBI2
#---------------------

dbVBI2 <- "../Data/MeetgegevensVBI2/Versie3/FieldMapDataVal.accdb"

#---------------------
# Meetproces
#---------------------

dbMeetproces<-"../Data/MeetgegevensVBI2/Versie3/VBI_Meetproces_v2016-08-31.accdb"

#------------------------
# Analyse../Databank
#-------------------------

dbAnalyse <- "../Data/MeetgegevensVBI2/Versie3/VBI_AnalyseDatabank_v2016-08-31.accdb"

#---------------------
#Externe ../Data
#---------------------

dbExterneData<-"../Data/ExterneData/VBIExterneData.accdb"

#---------------------
#../Databank strata
#---------------------

dbStrata<-"../Data/MeetgegevensVBI2/Versie3/VBI_Strata_v2016-08-31.accdb"

#------------------------
# LSVI indicatoren
#-------------------------

dbLSVI <- "../Data/LSVI_indicatoren/LSVI.accdb"

#----------------------
# Sample
#----------------------

dirSample <- "../Data/Steekproef/."
sampleHeideFile <- "meetnet_heide_versie201400611"
sample6510File <- "steekproef_6510_versie20140506"
sampleSizeCalcFile <- "../Data/Steekproef/steekproefgrootte_versie20140324.txt"

########################
#### Libraries
########################

library("RODBC")
library("plyr")
library("lme4")
library("reshape2")
library("ggplot2")
library("maptools")
library("INBOtheme")
library("nlme")
library("glmmADMB")
library("geepack")
library("gridExtra")
library("rgeos")
library("rgdal")
library("multcomp")

cat("Omgevingsvariabelen ingelezen\n--------------------------------\n\n")


