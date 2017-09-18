#library(ggplot2)
library(reshape)
#library(maptools)
library(rgdal)
#library(rgeos)
library(dplyr)

###############################
#VOORBEREIDING STEEKPROEFKADER#
###############################
getwd()
# inlezen BWK - 31xx - 2900

BWK_shape <- readOGR(dsn = "C:/Users/toon_westra/toon.westra@inbo.be/Meetnet_habitatkwaliteit/Basisdata/BWK/.", layer =  "BWK2_N2000Habitat")

BWK_df <- BWK_shape@data

BWK_bos_shape <- BWK_shape[(substr(BWK_shape$HAB1,1,1) == "9" & !is.na(substr(BWK_shape$HAB1,1,1))) |
                             (substr(BWK_shape$HAB2,1,1) == "9" & !is.na(substr(BWK_shape$HAB2,1,1))) |
                             (substr(BWK_shape$HAB3,1,1) == "9" & !is.na(substr(BWK_shape$HAB3,1,1))) |
                             (substr(BWK_shape$HAB4,1,1) == "9" & !is.na(substr(BWK_shape$HAB4,1,1))) |
                             (substr(BWK_shape$HAB5,1,1) == "9" & !is.na(substr(BWK_shape$HAB5,1,1))) ,]

BWK_bos_shape$Pol_ID <- 1:nrow(BWK_bos_shape@data)

BWK_bos_shape$Pol_beschrijving <- paste(BWK_bos_shape$PHAB1,"% ",BWK_bos_shape$HAB1,
                                      ifelse(is.na(BWK_bos_shape$HAB2),"",paste("; ",BWK_bos_shape$PHAB2,"% ",BWK_bos_shape$HAB2,sep="")),
                                      ifelse(is.na(BWK_bos_shape$HAB3),"",paste("; ",BWK_bos_shape$PHAB3,"% ",BWK_bos_shape$HAB3,sep="")),
                                      ifelse(is.na(BWK_bos_shape$HAB4),"",paste("; ",BWK_bos_shape$PHAB4,"% ",BWK_bos_shape$HAB4,sep="")),
                                      ifelse(is.na(BWK_bos_shape$HAB5),"",paste("; ",BWK_bos_shape$PHAB5,"% ",BWK_bos_shape$HAB5,sep="")),sep="")


versie <- "20160419"

names(BWK_shape)
# [1] "eval"       "eenh1"      "eenh2"      "eenh3"      "eenh4"      "eenh5"      "eenh6"      "v1"
# [9] "v2"         "v3"         "herk_bwk"   "HAB1"       "pHAB1"      "HAB2"       "pHAB2"      "HAB3"
# [17] "pHAB3"      "HAB4"       "pHAB4"      "HAB5"       "pHAB5"      "HERK_HAB"   "HERK_pHAB"  "referentie"
# [25] "info"       "aanpassing" "plot_ID"    "GLC1"       "GLC2"       "GLC5"       "stavaza"    "SHAPE_Leng"
# [33] "SHAPE_Area"

BWK_hab <- BWK_bos_shape@data
BWK_hab <- dplyr::rename(BWK_hab, Shape_Area = "SHAPE_Area")

#data ordenen: verschillende habitatfracties binnen polygoon --> aparte records
temp1 <- melt(BWK_hab, id.vars =c("Pol_ID","Shape_Area", "Pol_beschrijving"),
            measure.vars=c("HAB1", "HAB2","HAB3","HAB4","HAB5"),
            variable_name="hab")

temp2 <- melt(BWK_hab, id.vars =c("Pol_ID","Shape_Area","Pol_beschrijving"),
             measure.vars=c("PHAB1", "PHAB2","PHAB3","PHAB4","PHAB5"),
             variable_name="phab")

temp1$test <- temp2$phab
temp1$phab <- temp2$value
BWK_hab <- temp1

BWK_hab <- rename(BWK_hab, c(value="code"))
BWK_hab$test <- NULL
BWK_hab <- BWK_hab[!is.na(BWK_hab$code),]

#enkel habitatvlekken selecteren met habitat


BWK_hab <- BWK_hab[substr(BWK_hab$code,1,1) == "9",]
unique(BWK_hab$code)

BWK_hab$code <- as.character(BWK_hab$code)

#achtervoegsel ,gh verwijderen
BWK_hab$code <- ifelse(substr(BWK_hab$code, nchar(BWK_hab$code) - 2, nchar(BWK_hab$code)) == ",gh", substr(BWK_hab$code, 1, nchar(BWK_hab$code) - 3), BWK_hab$code)

BWK_hab$habt <- substr(BWK_hab$code,1,4)
BWK_hab$habsubt <- BWK_hab$code
BWK_hab$habsubt <- ifelse(BWK_hab$habsubt == "91E0", "91E0_vn", BWK_hab$habsubt)
BWK_hab$habsubt <- ifelse(BWK_hab$habsubt == "9130", "9130_end", BWK_hab$habsubt)

unique(BWK_hab$habsubt)

# SBZH

SBZH <- readOGR("../Basisdata/SBZH/.", "SBZH")
proj4string(SBZH) <- proj4string(BWK_shape)

BWK_bos_shape_SBZH <- BWK_bos_shape[SBZH,]

BWK_hab$SBZH <- ifelse(BWK_hab$Pol_ID %in% BWK_bos_shape_SBZH$Pol_ID, 1 , 0)



#dataset herorganiseren: 1 observatie per habitatsubtype/polygoon combinatie (sommige polygonen bevatten twee vlekken van een zelfde habitatsubtype maar verschillend code)
BWK_habsubt<- aggregate(BWK_hab[,"phab",drop=FALSE],by=BWK_hab[,c("Pol_ID","habsubt")],FUN=sum)

BWK_habsubt <- merge(unique(BWK_hab[,c("Pol_ID","habt","habsubt", "Shape_Area","SBZH","Pol_beschrijving")]), BWK_habsubt)

summary(BWK_habsubt)

BWK_habsubt$Patch_Area<-BWK_habsubt$Shape_Area*BWK_habsubt$phab/100
BWK_habsubt$Subtype<-ifelse(nchar(as.character(BWK_habsubt$habsubt))>4,1,0)



VBI_raster <- readOGR("../Basisdata/VBI/.", "invb2_ecod_bodem_BHR")

VBI_raster$Pol_ID <- over(VBI_raster,BWK_bos_shape)$Pol_ID

VBI_raster_df <- VBI_raster@data

VBI_habitat <- VBI_raster_df[ !is.na(VBI_raster_df$Pol_ID ),]

VBI_habitat <- merge(VBI_habitat[,c("Pol_ID","PLOTNR")], BWK_habsubt[,c("Pol_ID","habsubt","Pol_beschrijving","phab")], by= "Pol_ID", all.x= TRUE)

VBI_habitat <- plyr::rename(VBI_habitat, c(PLOTNR= "IDPlots",habsubt = "HabCode", phab = "Phab"))

VBI_habitat$HabCode <- revalue (VBI_habitat$HabCode, c("9130_end" = "9130", "9120_qb" = "9120"))

VBI_habitat <- ddply(VBI_habitat,.(Pol_ID,IDPlots,HabCode), summarise,
                     Phab = sum(Phab,na.rm= TRUE),
                     Pol_beschrijving = unique(Pol_beschrijving))

VBI_habitat <- VBI_habitat[,c("IDPlots","HabCode","Phab","Pol_beschrijving")]

dbName <-       "Q:/WOD/Projecten2014/projectenINBO/ToonW/09769_VerwerkingBosinventarisatie/Bosinventaris_finaal/Output/VBI_Strata_v2015-07-31.accdb"

connection <- odbcConnectAccess2007(dbName)
sqlSave(connection, VBI_habitat, paste("tblN2000Habitat_versie",versie, sep =""))
odbcClose(connection)
