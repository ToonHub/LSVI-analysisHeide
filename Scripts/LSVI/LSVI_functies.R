########################################################################################################
### FUNCTIES VOOR OPHALEN  GEGEVENS VEGETATIEOPNAME VOOR BEREKENING INDICATOREN VEGETATIE EN VERSTORING
#########################################################################################################

### geobserveerd habitattype uit MHK databank halen

#db <- dbHeideEn6510_2016

getObservedHabMHK <- function (db = dbHeideEn6510_2014_2015){

  query_HabTypeObserved <- "
  SELECT
  Standdescription.IDPlots,
  Standdescription.ID,
  Standdescription.Area_m2,
  Standdescription.HABITAT,
  qHABITAT.Value1
  FROM Standdescription LEFT JOIN qHABITAT ON Standdescription.HABITAT = qHABITAT.ID;
  "

  if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  habObservedOrig <- sqlQuery(connectieDB, query_HabTypeObserved, stringsAsFactors = TRUE)

  odbcClose(connectieDB)

  habObserved <- plyr::rename(habObservedOrig, c(ID = "IDSegments", HABITAT = "HabObservedCode", Value1 = "HabObserved"))

  habObserved$HabObserved <- revalue(habObserved$HabObserved,c('6510 hu'="6510_hu","2330 bu" = "2330_bu", "6510 hua" = "6510_hua", "6510 hus" = "6510_hus", "6510 huk" = "6510_huk"))

  habObserved$Weight <- ifelse (is.na(habObserved$Area_m2),1,habObserved$Area_m2/(pi*18^2))

  result <- habObserved

  return (result)
}

####################################################


# plotID's en habitattypes van opgemeten plots uit MHK-databank halen

getMeasuredPlotsMHK <- function(db = dbHeideEn6510_2014_2015){

  speciesMeasured <- getCoverSpeciesMHK(db)
  coverPlots <- getCoverVeglayersMHK(db)

  habMeasured <- getObservedHabMHK(db)

  measuredPlots <- habMeasured %>%
                  rename(HabCode = HabObserved) %>%
                  left_join(select(coverPlots, IDPlots, HabObservedPQ), by = "IDPlots") %>%
                  group_by(IDPlots) %>%
                  mutate(MixedPlot = n() > 1) %>%
                  ungroup()

  measuredPlots$HabCode <- as.character(measuredPlots$HabCode)
  measuredPlots$HabObservedPQ <- as.character(measuredPlots$HabObservedPQ)

  #enkel segmenten met habitat
  measuredPlotsHab <- measuredPlots %>%
                      filter(!MixedPlot | HabCode == HabObservedPQ) %>%
                      group_by(IDPlots) %>%
                      summarise(HabCode = unique(HabObservedPQ),
                                Weight = round(sum(Weight),3))

  return (measuredPlotsHab)
}

###################################################
getMetaDataMHK <- function(db= dbHeideEn6510_2014_2015){

  query_metadataMHK <- "
   SELECT
  Standdescription.IDPlots,
  Observer_date.Add_date,
  Grid_points.X_m,
  Grid_points.Y_m,
  GPS_REF.X_m,
  GPS_REF.Y_m,
  Grid_points.SBZH
  FROM GPS_REF
  RIGHT JOIN ((Standdescription INNER JOIN Observer_date ON Standdescription.IDPlots = Observer_date.IDPlots)   LEFT JOIN Grid_points ON Standdescription.IDPlots = Grid_points.ID) ON GPS_REF.SingleID = Standdescription.IDPlots;
    "

  connectieDB <-   odbcConnectAccess(db)

  metadataMHK <- sqlQuery(connectieDB, query_metadataMHK, stringsAsFactors = TRUE)

  odbcClose(connectieDB)

  metadataMHK <- rename(metadataMHK, X_sampleframe=  X_m, Y_sampleframe = Y_m, X_measured = X_m.1, Y_measured = Y_m.1, Date = Add_date  )

  metadataMHK$Date <- as.Date(metadataMHK$Date)
  metadataMHK$Year <- as.numeric(format(metadataMHK$Date,'%Y'))

  metadata <- unique(metadataMHK)

  return(metadata)

}



####################################################
getStatusFieldWork <- function(db = dbHeideEn6510_2014_2015){

  query_HabTypeTarget <- "
SELECT
Grid_points.ID,
Grid_points.SingleID,
Grid_points.Status_Fieldwork,
Grid_points.Info_Status_Fieldwork,
qinfo_Status_Fieldwork.Value,
Grid_points.HABSUBT1,
Grid_points.HABT2,
Grid_points.SBZH
FROM Grid_points LEFT JOIN qinfo_Status_Fieldwork ON Grid_points.Info_Status_Fieldwork = qinfo_Status_Fieldwork.ID;
"
  connectieDB <-   odbcConnectAccess(db)
  statusFieldworkOrig <- sqlQuery(connectieDB, query_HabTypeTarget, stringsAsFactors = TRUE)
  odbcClose(connectieDB)

  statusFieldwork <- plyr::rename(statusFieldworkOrig, c(ID = "IDPlots", SingleID = "ID", Info_Status_Fieldwork = "Info_Status_Fieldwork_Code",Value = "Info_Status_Fieldwork",HABSUBT1 = "HabTarget1", HABT2 = "HabTarget2"))

  return (statusFieldwork)

}




###################################################
getMetaDataMHK_2016 <- function(db= dbHeideEn6510_2016){

  query_metadataMHK <- "
  SELECT
  Observer_date.IDPlots,
  Observer_date.Add_date
  FROM Observer_date;
    "

  if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  metadataMHK <- sqlQuery(connectieDB, query_metadataMHK, stringsAsFactors = TRUE)

  odbcClose(connectieDB)

  metadataMHK <- rename(metadataMHK,  Date = Add_date  )

  metadataMHK$Date <- as.Date(metadataMHK$Date)
  metadataMHK$Year <- as.numeric(format(metadataMHK$Date,'%Y'))

  # SBZH

  steekproefHeide <- getSample(fileSample =  sampleHeideFile)
  steekproef6510 <- getSample(fileSample = sample6510File)
  steekproefHeide6510 <- rbind(select(steekproefHeide, -Orthocontr), steekproef6510)

  steekproefHeide6510$IDPlots<- as.numeric(as.character(steekproefHeide6510$IDPlots))
  metadataMHK <- left_join(metadataMHK,
                        select(steekproefHeide6510, IDPlots, x, y, SBZH),
                        by = "IDPlots" )


  metadataMHK <- unique(metadataMHK)

  return(metadataMHK)

}

getHabtarget <- function(){

  steekproefHeide <- getSample(fileSample =  sampleHeideFile)
  steekproef6510 <- getSample(fileSample = sample6510File)
  steekproefHeide6510 <- rbind(select(steekproefHeide, -Orthocontr), steekproef6510)

  steekproefHeide6510$IDPlots<- as.numeric(as.character(steekproefHeide6510$IDPlots))
  steekproefHeide6510

  steekproefHabTarget <- summarise(group_by(steekproefHeide6510, IDPlots),
                                  HabTarget1 = HabTarget1[1],
                                  HabTarget2 = HabTarget2[1])

  steekproefHabTarget <- ungroup(steekproefHabTarget)
  return(steekproefHabTarget)

}



####################################################



### Selectie van VBI-plots met bijhorend N2000-habitattype (volgens habitatkaart versie 20140324), waarvoor we LSVI willen berekenen


getMeasuredPlotsVBI2 <- function(db = dbVBI2){

  connectionStrata <- odbcConnectAccess2007(dbStrata)

  VBI_plotsOrig <- sqlFetch(connectionStrata,"tblN2000Habitat_versie20140324" )

  odbcClose(connectionStrata)

  ### Selectie van VBI-plots die al opgemeten werden in 2de cyclus

  connectionMeetproces <- odbcConnectAccess2007(dbMeetproces)

  records <- sqlFetch(connectionMeetproces, "tblRecordsVBI2+")

  odbcClose(connectionMeetproces)

  ### Selectie van plots met Natura 2000 habitat EN waarvoor gegevens werden ingezameld in 2de cyclys

  VBI_Hab <- VBI_plotsOrig[VBI_plotsOrig$IDPlots %in% records$IDPlots, c("IDPlots","HabCode")]

  return(VBI_Hab)

}






##############################################################


### Haal bedekking soorten per vegetatielaag in 16x16m proefvlak uit VBI2 databank; duid aan welke soorten bomen zijn op basis van lijst in externe databank
#?# Is het wel nodig om aan te duiden welke soorten bomen zijn voor berekening LSVI? #?#

getCoverSpeciesVBI2 <- function (db =  dbVBI2, plotIDs = NULL){

  query_herblayer<-"
  SELECT Herblayer.IDPlots,
  Herblayer.Species,
  [qVEG_HerbSpecies].Value1,
  [qVEG_HerbSpeciesScientific].Value1,
  Herblayer.Coverage_date1,
  Herblayer.Coverage_date2
  FROM (Herblayer
  LEFT JOIN [qVEG_HerbSpecies] ON Herblayer.Species = [qVEG_HerbSpecies].ID)
  LEFT JOIN [qVEG_HerbSpeciesScientific] ON Herblayer.Species = [qVEG_HerbSpeciesScientific].ID;
  "
  query_shrublayer<-"
  SELECT Shrublayer.IDPlots,
  Shrublayer.Species,
  [qVEG_TreeSpecies].Value1,
  [qVEG_TreeSpeciesScientific].Value1,
  Shrublayer.Coverage
  FROM (Shrublayer
  LEFT JOIN [qVEG_TreeSpecies] ON Shrublayer.Species = [qVEG_TreeSpecies].ID)
  LEFT JOIN [qVEG_TreeSpeciesScientific] ON Shrublayer.Species = [qVEG_TreeSpeciesScientific].ID;
  "

  query_treelayer<-"
  SELECT Treelayer.IDPlots,
  Treelayer.Species,
  [qVEG_TreeSpecies].Value1,
  [qVEG_TreeSpeciesScientific].Value1,
  Treelayer.Coverage
  FROM (Treelayer
  LEFT JOIN [qVEG_TreeSpecies] ON Treelayer.Species = [qVEG_TreeSpecies].ID)
  LEFT JOIN [qVEG_TreeSpeciesScientific] ON Treelayer.Species = [qVEG_TreeSpeciesScientific].ID;
  "

  query_scaleBB <-"
  SELECT qCoverHerbs.ID,
  qCoverHerbs.Value1
  FROM qCoverHerbs"

  connectieVBI2 <- odbcConnectAccess2007(db)

  herblayerOrig <- sqlQuery(connectieVBI2, query_herblayer, stringsAsFactors = TRUE)
  shrublayerOrig <- sqlQuery(connectieVBI2, query_shrublayer, stringsAsFactors = TRUE)
  treelayerOrig <- sqlQuery(connectieVBI2, query_treelayer, stringsAsFactors = TRUE)
  scaleBBOrig <- sqlQuery(connectieVBI2,query_scaleBB, stringsAsFactors = TRUE)

  odbcClose(connectieVBI2)

  herblayer <- plyr::rename(herblayerOrig[!is.na(herblayerOrig$Species),],c(Species = "IDSpVBI2",Value1="NameNl",Value1.1="NameSc"))
  shrublayer <- plyr::rename(shrublayerOrig[!is.na(shrublayerOrig$Species),],c(Species = "IDSpVBI2",Value1="NameNl",Value1.1="NameSc"))
  treelayer <- plyr::rename(treelayerOrig[!is.na(treelayerOrig$Species),],c(Species = "IDSpVBI2",Value1="NameNl",Value1.1="NameSc"))
  scaleBB <- plyr::rename(scaleBBOrig, c(ID = "BBID", Value1 = "ClassName"))


  herblayer$Coverage<-pmax(herblayer$Coverage_date1,herblayer$Coverage_date2,na.rm=TRUE)
  herblayer<-herblayer[,!names(herblayer) %in% c("Coverage_date1","Coverage_date2")]

  herblayer$Vegetatielaag <- "kruidlaag"
  shrublayer$Vegetatielaag <- "struiklaag"
  treelayer$Vegetatielaag <- "boomlaag"

  veglayers <- rbind(herblayer,shrublayer,treelayer)

  # externe data

  connectieExterneData <- odbcConnectAccess2007(dbExterneData) #dit is een accdb file

  treeList<-sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics",stringsAsFactors = TRUE)

  treeListExtra <- sqlFetch(connectieExterneData,"tblSpeciesTreelayerCharacteristics",stringsAsFactors = TRUE)

  #speciesListComb<-sqlFetch(connectieExterneData, "tblspeciesListComb", stringsAsFactors = TRUE)


  odbcClose(connectieExterneData)

  scaleBB$Cover <- c(0.25,1,2.25,4,8.75,18.75,37.5,62.5,87.5)

  veglayers$Scale <- "Braun-Blanquet"

  veglayers<-merge(veglayers,scaleBB,by.x="Coverage",by.y="BBID",all.x=TRUE)

  veglayers<-veglayers[order(veglayers$IDPlots),]

  veglayers$Tree <- (veglayers$NameNl %in% treeList$NameNl) | (veglayers$NameNl %in% treeListExtra$NameNl[treeListExtra$Tree==1])

  veglayers[veglayers$NameNl == "_ANDERE SOORT",]$Tree <- FALSE

  veglayers <- veglayers[,!names(veglayers) %in% c("Coverage","IDSpVBI2")]


  if (is.null(plotIDs)){

    result <- veglayers

  } else {

    result <- veglayers[veglayers$IDPlots %in% plotIDs,]

  }

  return(result)

}

#################################################################################################
### Haal bedekking soorten per vegetatielaag in 16x16m proefvlak uit databank meetnet habitatkwaliteit voor heide en 6510; duid aan welke soorten bomen zijn op basis van lijst in externe databank
#?# Is het wel nodig om aan te duiden welke soorten bomen zijn voor berekening LSVI? #?#

#db <- dbHeideEn6510_2014_2015

getCoverSpeciesMHK <- function(db = dbHeideEn6510_2014_2015, plotIDs =NULL){

  query_herblayer<-"
  SELECT Herblayer.IDPlots,
  Herblayer.Species,
  qVEG_HerbSpecies.Value1,
  qVEG_HerbSpeciesScientific.Value1,
  Herblayer.Coverage_date1,
  qLondo.Value1
  FROM ((Herblayer
  LEFT JOIN qVEG_HerbSpecies ON Herblayer.Species = qVEG_HerbSpecies.ID)
  LEFT JOIN qVEG_HerbSpeciesScientific ON Herblayer.Species_scientific = qVEG_HerbSpeciesScientific.ID)
  LEFT JOIN qLondo ON Herblayer.Coverage_date1 = qLondo.ID;
  "

  query_shrublayer <- "
  SELECT Shrublayer.IDPlots,
  Shrublayer.Species,
  qVEG_TreeSpecies.Value1,
  qVEG_TreeSpeciesScientific.Value1,
  Shrublayer.Coverage,
  qLondo.Value1
  FROM ((Shrublayer
  LEFT JOIN qVEG_TreeSpecies ON Shrublayer.Species = qVEG_TreeSpecies.ID)
  LEFT JOIN qVEG_TreeSpeciesScientific ON Shrublayer.Species_Scientific = qVEG_TreeSpeciesScientific.ID)
  LEFT JOIN qLondo ON Shrublayer.Coverage = qLondo.ID;

  "

  query_treelayer <- "
  SELECT Treelayer.IDPlots,
  Treelayer.Species,
  qVEG_TreeSpecies.Value1,
  qVEG_TreeSpeciesScientific.Value1,
  Treelayer.Coverage,
  qLondo.Value1
  FROM ((Treelayer
  LEFT JOIN qVEG_TreeSpecies ON Treelayer.Species = qVEG_TreeSpecies.ID)
  LEFT JOIN qVEG_TreeSpeciesScientific ON Treelayer.Species_Scientific = qVEG_TreeSpeciesScientific.ID)
  LEFT JOIN qLondo ON Treelayer.Coverage = qLondo.ID;
  "

  query_mosslayer <- "
  SELECT Mosslayer.IDPlots,
  Mosslayer.Species,
  qVEG_MossSpecies.Value1,
  qVEG_MossSpeciesScientific.Value1,
  Mosslayer.Coverage,
  qLondo.Value1
  FROM ((Mosslayer
  LEFT JOIN qVEG_MossSpecies ON Mosslayer.Species = qVEG_MossSpecies.ID)
  LEFT JOIN qVEG_MossSpeciesScientific ON Mosslayer.Species_Scientific = qVEG_MossSpeciesScientific.ID)
  LEFT JOIN qLondo ON Mosslayer.Coverage = qLondo.ID;
  "

  if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  herblayerOrig <- sqlQuery(connectieDB, query_herblayer, stringsAsFactors = TRUE)
  shrublayerOrig <- sqlQuery(connectieDB, query_shrublayer, stringsAsFactors = TRUE)
  treelayerOrig <- sqlQuery(connectieDB, query_treelayer, stringsAsFactors = TRUE)
  mosslayerOrig <- sqlQuery(connectieDB, query_mosslayer, stringsAsFactors = TRUE)

  odbcClose(connectieDB)

  herblayer <- plyr::rename(herblayerOrig[!is.na(herblayerOrig$Species),],c(Species = "IDHerbSpMHK",Value1="NameNl",Value1.1="NameSc", Coverage_date1 = "ClassCode", Value1.2 = "ClassName"))
  shrublayer <- plyr::rename(shrublayerOrig[!is.na(shrublayerOrig$Species),],c(Species = "IDTreeSpMHK",Value1="NameNl",Value1.1="NameSc", Coverage = "ClassCode", Value1.2 = "ClassName"))
  treelayer <- plyr::rename(treelayerOrig[!is.na(treelayerOrig$Species),],c(Species = "IDTreeSpMHK",Value1="NameNl",Value1.1="NameSc", Coverage = "ClassCode", Value1.2 = "ClassName"))
  mosslayer <- plyr::rename(mosslayerOrig[!is.na(mosslayerOrig$Species),],c(Species = "IDMossSpMHK",Value1="NameNl",Value1.1="NameSc", Coverage = "ClassCode", Value1.2 = "ClassName"))

  herblayer$Vegetatielaag <- "kruidlaag"
  shrublayer$Vegetatielaag <- "struiklaag"
  treelayer$Vegetatielaag <- "boomlaag"
  mosslayer$Vegetatielaag <- "moslaag"
  veglayers <- rbind.fill(herblayer,shrublayer,treelayer,mosslayer)
  veglayers$Scale <- "Londo"


  # externe data
#
#   connectieExterneData <- odbcConnectAccess2007(dbExterneData) #dit is een accdb file
#
#   treeList<-sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics",stringsAsFactors = TRUE)
#
#   treeListExtra <- sqlFetch(connectieExterneData,"tblSpeciesTreelayerCharacteristics",stringsAsFactors = TRUE)

  #speciesListComb<-sqlFetch(connectieExterneData, "tblspeciesListComb", stringsAsFactors = TRUE)


  # odbcClose(connectieExterneData)

  londoScale <- data.frame(ClassCode=c(8, 10, 11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,33,34,35),Cover=c(7.5,20,30,40,50,60,70,80,90,97.5,10,0.5,2,4,0.5,2,4,0.5,2,4,0.5,2,4,12.5,47.5,52.5))



  veglayers<-merge(veglayers,londoScale,by = "ClassCode",all.x=TRUE)

  veglayers<-veglayers[order(veglayers$NameNl),]
  veglayers<-veglayers[order(veglayers$Vegetatielaag),]
  veglayers<-veglayers[order(veglayers$IDPlots),]

  # veglayers$Tree <- (veglayers$NameNl %in% treeList$NameNl) | (veglayers$NameNl %in% treeListExtra$NameNl[treeListExtra$Tree==1])
  #
  # veglayers$Tree <- ifelse(veglayers$NameNl == "_ANDERE SOORT",FALSE, veglayers$Tree)

  veglayers <- veglayers[,c("IDPlots","NameNl","NameSc","Vegetatielaag","Scale","ClassName","Cover")]

  if (is.null(plotIDs)){

    result <- veglayers

  } else {

    result <- veglayers[veglayers$IDPlots %in% plotIDs,]

  }

  return(result)


}

################################################################################################

### Haal bedekking van de afzonderlijke vegetatielagen in 16x16m proefvlak uit VBI2 databank

getCoverVeglayersVBI2 <- function (db =  dbVBI2, plotIDs = NULL) {

  query_veglayers <- "SELECT
  Vegetation.IDPlots,
  Vegetation.Total_herb_cover,
  Vegetation.Total_moss_cover,
  Vegetation.Total_shrub_cover,
  Vegetation.Total_tree_cover,
  Vegetation.Total_cover
  FROM Vegetation;"


  connectieVBI2 <- odbcConnectAccess2007(db) #dit is een mdb file

  veglayerOrig <- sqlQuery(connectieVBI2, query_veglayers, stringsAsFactors = TRUE)

  odbcClose(connectieVBI2)

  veglayer <- veglayerOrig

  veglayer <- ddply(veglayer,.(IDPlots),summarise,
                    Total_herb_cover= sum(Total_herb_cover,na.rm=TRUE),
                    Total_moss_cover= sum(Total_moss_cover,na.rm=TRUE),
                    Total_shrub_cover= sum(Total_shrub_cover,na.rm=TRUE),
                    Total_tree_cover= sum(Total_tree_cover,na.rm=TRUE))


  coverScale <- data.frame(CoverID = 0:6, Coverage = c(0,2.5, 8.75, 18.75, 37.50, 62.50, 87.50))

  veglayer <- merge(veglayer,coverScale,by.x="Total_herb_cover",by.y="CoverID",all.x=TRUE)
  veglayer <- plyr::rename(veglayer, c(Coverage = "CoverHerblayer"))

  veglayer <- merge(veglayer,coverScale,by.x="Total_moss_cover",by.y="CoverID",all.x=TRUE)
  veglayer <- plyr::rename(veglayer, c(Coverage = "CoverMosslayer"))

  veglayer <- merge(veglayer,coverScale,by.x="Total_shrub_cover",by.y="CoverID",all.x=TRUE)
  veglayer <- plyr::rename(veglayer, c(Coverage = "CoverShrublayer"))

  veglayer <- merge(veglayer,coverScale,by.x="Total_tree_cover",by.y="CoverID",all.x=TRUE)
  veglayer <- plyr::rename(veglayer, c(Coverage = "CoverTreelayer"))

  veglayer <- veglayer[,c("IDPlots","CoverHerblayer","CoverMosslayer","CoverShrublayer","CoverTreelayer")]

  veglayer$CoverTreeAndShrublayer <- 1 - (1 - veglayer$CoverTreelayer) * (1 - veglayer$CoverShrublayer)

  if (is.null(plotIDs)){

    result <- veglayer

  } else {

    result <- veglayer[veglayer$IDPlots %in% plotIDs,]

  }

  return(result)


}

###############################################################################################################################
# db <- dbHeideEn6510_2016

getCoverVeglayersMHK <- function(db = dbHeideEn6510_2014_2015, plotIDs = NULL){

  query_CoverPlot <- "
  SELECT
  VegPQ.IDPlots,
  VegPQ.HAB1,
  qHABITAT.Value1,
  VegPQ.Licheneslayer,
  VegPQ.Sphagnumlayer,
  VegPQ.OtherMosslayer,
  VegPQ.Herblayer,
  VegPQ.Shrublayer,
  VegPQ.Treelayer,
  VegPQ.Shrub_and_Treelayer
  FROM VegPQ LEFT JOIN qHABITAT ON VegPQ.HAB1 = qHABITAT.ID;"


   if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  coverPlots <- sqlQuery(connectieDB, query_CoverPlot, stringsAsFactors = TRUE)

  odbcClose(connectieDB)

  coverPlots$CoverMosslayer <- coverPlots$Sphagnumlayer + coverPlots$OtherMosslayer

  coverPlots <- plyr::rename(coverPlots, c(Value1 = "HabObservedPQ",Herblayer = "CoverHerblayer", Shrublayer = "CoverShrublayer", Treelayer = "CoverTreelayer", Shrub_and_Treelayer = "CoverTreeAndShrublayer" ))

  # een vegetatieopname per plot

  coverPlots <- mutate(group_by(coverPlots, IDPlots),
                       removeRecord = n() > 1 & sum(!is.na(CoverHerblayer)) > 0 & (is.na(CoverHerblayer)))

  coverPlots <- filter(coverPlots, removeRecord == FALSE)


  coverPlots <- coverPlots[,c("IDPlots","HabObservedPQ","CoverHerblayer","CoverMosslayer", "CoverShrublayer", "CoverTreelayer", "CoverTreeAndShrublayer")]

   coverPlots$HabObservedPQ <- revalue(coverPlots$HabObservedPQ,c('6510 hu'="6510_hu","2330 bu" = "2330_bu", "6510 hua" = "6510_hua", "6510 hus" = "6510_hus", "6510 huk" = "6510_huk"))

  if (is.null(plotIDs)){

    result <- coverPlots

  } else {

    result <-coverPlots[coverPlots$IDPlots %in% plotIDs,]

  }

  return(result)

}

##########################################################################♣
### FUNCTIES VOOR RAADPLEGEN STEEKPROEF
##########################################################################♣
#
# dirSampleHab <- dirSample
#  fileSample <- sampleHeide

getSample <- function(dirSampleHab = dirSample, fileSample){

sample <-  readOGR(dsn = dirSampleHab, layer = fileSample)
sample <- sample@data

if("Orthocontr" %in% colnames(sample)){

sample <- select(sample, ID,  IDPlots = Ranking, nb  ,HabTarget1 = habsubt, HabTarget2 = Doelhab2, Year = year, SBZH, Orthocontr, x, y )

} else {

   sample <- select(sample, ID,  IDPlots = Ranking, nb, HabTarget1 = habsubt, HabTarget2 = Doelhab2, Year = year, SBZH, x, y)

}

return(sample)

}

##########################################################################♣

getSampleSize <- function(sampleSizeCalc = sampleSizeCalcFile, habtypes = "All") {

  sampleSize <- read.table(sampleSizeCalc, header = TRUE, dec = ",")

if(habtypes == "All"){

  sampleSizeSelect <- sampleSize

} else {

  sampleSizeSelect <- sampleSize[sampleSize$habsubt %in% habtypes,]
}


sampleSizeSelect$nBezoekenSBZH <- sampleSizeSelect$n_habt_SBZH_bruto + sampleSizeSelect$extra_habsubt_SBZH_bruto
sampleSizeSelect$nGewenstSBZH <- sampleSizeSelect$n_habt_SBZH_netto + sampleSizeSelect$extra_habsubt_SBZH_netto

sampleSizeSelect$nBezoekenBuiten <- sampleSizeSelect$n_habt_buiten_bruto + sampleSizeSelect$extra_habsubt_buiten_bruto
sampleSizeSelect$nGewenstBuiten <- sampleSizeSelect$n_habt_buiten_netto + sampleSizeSelect$extra_habsubt_buiten_netto

#long formaat
sampleSizeSelect_long1 <- melt(sampleSizeSelect,id.vars = "habsubt", measure.vars = c("nBezoekenSBZH","nBezoekenBuiten"), variable.name = "SBZH", value.name = "nBezoeken")
sampleSizeSelect_long1$SBZH <- revalue(sampleSizeSelect_long1$SBZH,c(nBezoekenSBZH = "Binnen", nBezoekenBuiten= "Buiten"))

sampleSizeSelect_long2 <- melt(sampleSizeSelect,id.vars = "habsubt", measure.vars = c("nGewenstSBZH","nGewenstBuiten"), variable.name = "SBZH", value.name = "nGewenst")
sampleSizeSelect_long2$SBZH <- revalue(sampleSizeSelect_long2$SBZH,c(nGewenstSBZH = "Binnen", nGewenstBuiten= "Buiten"))

sampleSizeSelect_long3 <- melt(sampleSizeSelect,id.vars = "habsubt", measure.vars = c("prop_hab_SBZH","prop_hab_buiten"), variable.name = "SBZH", value.name = "Trefkans_verwacht")
sampleSizeSelect_long3$SBZH <- revalue(sampleSizeSelect_long2$SBZH,c(prop_hab_SBZH = "Binnen", prop_hab_buiten = "Buiten"))

sampleSizeSelect_long1$nGewenst <- sampleSizeSelect_long2$nGewenst
sampleSizeSelect_long1$Trefkans_verwacht <- sampleSizeSelect_long3$Trefkans_verwacht

sampleSizeSelect <- sampleSizeSelect_long1

return (sampleSizeSelect)

}



######################################################################################
### FUNCTIES VOOR OPHALEN DENDROMETRISCHE GEGEVENS VOOR BEREKENING INDICATOREN HABITATSTRUCTUUR BOSSEN
#######################################################################################

#'Haal gegevens over de bomen uit de A3A4 - plots van de tweede Vlaamse bosinventarisatie
#'
#'Deze functie haalt meetresultaten en soortgegevens over bomen uit de A3- en de A4-plots van #'de tweede Vlaamse bosinventarisatie.Ontbrekende hoogtes worden vervangen door de mediaan van #'de boomhoogtes binnen een plot.Bomen met ontbrekende waarden voor 'status' beschouwen we als #'levende bomen; Bomen met ontbrekende waarden voor 'individueel/hakhout' beschouwen we als een #'individuele stam.
#'Op basis van gegevens uit de databank 'VBImeetproces' wordt elke boom aan een segment binnen #'een plot toegekend en wordt aan elk segment de oppervlakte toegevoegd van het deel van de A4 #'en de A3 plot dat binnen dit segment valt. Op basis van deze oppervlaktes worden de
#'expansiecoefficienten bepaald.
#'
#'@param db Databank met meetgegevens (defaultwaarde = dbVBI2; te definiëren in 'Omgeveingsvariabelen.R')
#'@param dbMeetproc Databank met gegevens over meetproces (defaultwaarde = dbMeetproces; te definiëren in 'Omgeveingsvariabelen.R')
#'@param plotIDs ID's van plots waarvoor gegevens worden opgehaald (default: alle gegevens uit gespecifieerde databank)
#'
#'
#'@return dataframe met velden DataSet, IDPlots, IDSegments, ID (ID voor boom),
#' AreaA4_m2 en AreaA3_m2 (per segment),DBH_mm, Perimeter_cm, Height_m,
#' IDTree (ID voor boomsoort), Alive(1=levend, 0=dood), NameNl,
#' IntactTreeCode (10 = intacte boom, 20 = niet-intacte boom),
#' Coppice_IndividualCode (10=individuele stam, 20= hakhout), NameNl,
#' IsAutochtoon (1=ja, 0=nee), Genus en Species.
#'
#'@export
#'

getTreesA3A4VBI2 <- function (db = dbVBI2, dbMeetproc = dbMeetproces, plotIDs = NULL){


  connectieMetadata <- odbcConnectAccess2007(dbMeetproc)

  #tabel met plotgewichten en segmentgewichten en oppervlaktes van A2, A3 en A4 plots
  plotWeights<-sqlFetch(connectieMetadata,"tblPlotWeights")

  #tabel met er boom de ID van het segment waarbinnen de boom valt
  treesSegmentID<-sqlFetch(connectieMetadata,"tblTreesSegmentID")

  odbcClose(connectieMetadata)

  query_trees<-"
  SELECT Trees_2eBosinv.IDPlots,
  Trees_2eBosinv.ID,
  Trees_2eBosinv.Perimeter_cm,
  Trees_2eBosinv.DBH_mm,
  Trees_2eBosinv.Height_m,
  Trees_2eBosinv.Species,
  qTreeSpecies.Value1,
  Trees_2eBosinv.Status_tree,
  qStatusTree.Value1,
  Trees_2eBosinv.CodeCoppice_Individual,
  qCoppice_Individual.Value1,
  Trees_2eBosinv.IntactTree,
  qIntactTree.Value1
  FROM (((Trees_2eBosinv LEFT JOIN qTreeSpecies ON Trees_2eBosinv.Species = qTreeSpecies.ID)
  LEFT JOIN qStatusTree ON Trees_2eBosinv.Status_tree = qStatusTree.ID)
  LEFT JOIN qCoppice_Individual ON Trees_2eBosinv.CodeCoppice_Individual = qCoppice_Individual.ID)
  LEFT JOIN qIntactTree ON Trees_2eBosinv.IntactTree = qIntactTree.ID;
  "

  connectieVBI2 <- odbcConnectAccess2007(db)
  treesA3A4Orig <- sqlQuery(connectieVBI2, query_trees, stringsAsFactors = TRUE)
  odbcClose(connectieVBI2)

  treesA3A4 <- plyr::rename(treesA3A4Orig,c(Species="IDTreeSp",
                                    Value1="Species",
                                    Status_tree="StatusTreeCode",
                                    Value1.1="StatusTree",
                                    CodeCoppice_Individual="Coppice_IndividualCode",
                                    Value1.2="Coppice_Individual",
                                    IntactTree="IntactTreeCode",
                                    Value1.3="IntactTree"))

  #Bomen met ID=0 verwijderen
  treesA3A4 <- treesA3A4[treesA3A4$ID>0,]


  treesA3A4 <- merge(treesA3A4,treesSegmentID,by=c("IDPlots","ID"),all.x=TRUE)
  treesA3A4[is.na(treesA3A4$IDSegments),]$IDSegments<-1

  # ontbrekende waarde voor status --> we veronderstemmen levende boom; ontbrekende waarde voor hakhout-individueel --> we veronderstellen individuele boom; ontbrekende waarde voor intact/niet-intacte boom --> we veronderstellen een intacte boom

  treesA3A4[is.na(treesA3A4$StatusTreeCode),]$StatusTreeCode <- 1
  treesA3A4[is.na(treesA3A4$StatusTree),]$StatusTree<- "levend"

  treesA3A4[is.na(treesA3A4$Coppice_IndividualCode),]$Coppice_IndividualCode<-10
  treesA3A4[is.na(treesA3A4$Coppice_Individual),]$Coppice_Individual<- "Individuele boom"

  treesA3A4[is.na(treesA3A4$IntactTreeCode),]$IntactTreeCode<-10
  treesA3A4[is.na(treesA3A4$IntactTree),]$IntactTree<- "Intacte boom"

  #### Bijschatten ontbrekende hoogtes: mediaan van boomhoogte per plot

  plots_medianHeight<-ddply(treesA3A4,.(IDPlots,Periode),summarise,
                            medianHeight=median(Height_m,na.rm=TRUE))


  #Zet de bomen waarvoor geen Height_m gekend gelijka aan de mediaanhoogte van de bomen in het plot
  treesA3A4<-merge(treesA3A4,plots_medianHeight,by=c("IDPlots","Periode"), all.x=TRUE) #gewijzigd naar all.x
  treesA3A4$Height_m<-ifelse(is.na(treesA3A4$Height_m),treesA3A4$medianHeight,treesA3A4$Height_m)

  treesA3A4$Height_m <- ifelse(is.na(treesA3A4$Height_m), mean(treesA3A4$Height_m,na.rm=TRUE),treesA3A4$Height_m)

  #enkel bomen selecteren die in segmenten aangeduid als bos vallen
  #'plotWeightsVBI2' bevat, per segment met bos, de oppervlaktes van het deel van de A3 en A4 plot dat in het segment valt. 'treesA3A4' bevat ook enkele bomen die foutief gelocaliseerd zijn in segmenten zonder bos. Via een inner join, verwijderen we deze foutief gelocaliseerde bomen.

  treesA3A4 <- merge(treesA3A4,plotWeights,by=c("IDPlots","IDSegments"))

  treesA3A4$DataSet <- "VBI2"

  treesA3A4$Alive <- treesA3A4$StatusTreeCode==1

  treesA3A4 <- treesA3A4[!is.na(treesA3A4$IDPlots),c("DataSet","IDPlots","IDSegments","ID","AreaA4_m2","AreaA3_m2","Perimeter_cm","DBH_mm","Height_m","IDTreeSp","Alive","IntactTreeCode","Coppice_IndividualCode")]


  #extra informatie over boomsoorten toevoegen

  connectieExterneData <- odbcConnectAccess2007(dbExterneData)

    treeList <- sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics")

  odbcClose(connectieExterneData)

  treesA3A4 <- merge(treesA3A4,treeList,by="IDTreeSp",all.x=TRUE)


  if (is.null(plotIDs)){

    result <- treesA3A4

  } else {

    result <- treesA3A4[treesA3A4$IDPlots %in% plotIDs,]

  }

  return(result)

}

#############################################################################################################

#'Haal gegevens over de bomen uit de A3A4 - plots van de eerste Vlaamse bosinventarisatie
#'
#'Deze functie haalt meetresultaten en soortgegevens over bomen uit de A3- en de A4-plots van #'de tweede Vlaamse bosinventarisatie.Ontbrekende hoogtes worden vervangen door de mediaan van #'de boomhoogtes binnen een plot.Bomen met ontbrekende waarden voor 'status' beschouwen we als #'levende bomen; Bomen met ontbrekende waarden voor 'individueel/hakhout' beschouwen we als een #'individuele stam.
#'Op basis van gegevens uit de databank 'VBImeetproces' wordt elke boom aan een segment binnen #'een plot toegekend en wordt aan elk segment de oppervlakte toegevoegd van het deel van de A4 #'en de A3 plot dat binnen dit segment valt. Op basis van deze oppervlaktes worden de
#'expansiecoefficienten bepaald.
#'
#'@param db Databank met meetgegevens (defaultwaarde = dbVBI2; te definiëren in 'Omgeveingsvariabelen.R')
#'@param dbMeetproc Databank met gegevens over meetproces (defaultwaarde = dbMeetproces; te definiëren in 'Omgeveingsvariabelen.R')
#'@param plotIDs ID's van plots waarvoor gegevens worden opgehaald (default: alle gegevens uit gespecifieerde databank)
#'
#'
#'@return dataframe met velden DataSet, IDPlots, IDSegments, ID (ID voor boom),
#' AreaA4_m2 en AreaA3_m2 (per segment),DBH_mm, Perimeter_cm, Height_m,
#' IDTree (ID voor boomsoort), Alive(1=levend, 0=dood), NameNl,
#' IntactTreeCode (10 = intacte boom, 20 = niet-intacte boom),
#' Coppice_IndividualCode (10=individuele stam, 20= hakhout), NameNl,
#' IsAutochtoon (1=ja, 0=nee), Genus en Species.
#'
#'@export
#'

getTreesA3A4VBI1 <- function (db = dbVBI2, dbMeetproc = dbMeetproces, plotIDs = NULL){


  connectieMetadata <- odbcConnectAccess2007(dbMeetproc)

  #tabel met plotgewichten en segmentgewichten en oppervlaktes van A2, A3 en A4 plots
  plotWeights<-sqlFetch(connectieMetadata,"tblPlotWeights")

  #tabel met er boom de ID van het segment waarbinnen de boom valt
  treesSegmentID<-sqlFetch(connectieMetadata,"tblTreesSegmentID")

  odbcClose(connectieMetadata)

  query_trees<-"
  SELECT Trees_2eBosinv.IDPlots,
  Trees_2eBosinv.ID,
  Trees_2eBosinv.Perimeter_cm,
  Trees_2eBosinv.DBH_mm,
  Trees_2eBosinv.Height_m,
  Trees_2eBosinv.Species,
  qTreeSpecies.Value1,
  Trees_2eBosinv.Status_tree,
  qStatusTree.Value1,
  Trees_2eBosinv.CodeCoppice_Individual,
  qCoppice_Individual.Value1,
  Trees_2eBosinv.IntactTree,
  qIntactTree.Value1
  FROM (((Trees_2eBosinv LEFT JOIN qTreeSpecies ON Trees_2eBosinv.Species = qTreeSpecies.ID)
  LEFT JOIN qStatusTree ON Trees_2eBosinv.Status_tree = qStatusTree.ID)
  LEFT JOIN qCoppice_Individual ON Trees_2eBosinv.CodeCoppice_Individual = qCoppice_Individual.ID)
  LEFT JOIN qIntactTree ON Trees_2eBosinv.IntactTree = qIntactTree.ID;
  "

  connectieVBI2 <- odbcConnectAccess2007(db)
  treesA3A4Orig <- sqlQuery(connectieVBI2, query_trees, stringsAsFactors = TRUE)
  odbcClose(connectieVBI2)

  treesA3A4 <- plyr::rename(treesA3A4Orig,c(Species="IDTreeSp",
                                    Value1="Species",
                                    Status_tree="StatusTreeCode",
                                    Value1.1="StatusTree",
                                    CodeCoppice_Individual="Coppice_IndividualCode",
                                    Value1.2="Coppice_Individual",
                                    IntactTree="IntactTreeCode",
                                    Value1.3="IntactTree"))

  #Bomen met ID=0 verwijderen
  treesA3A4 <- treesA3A4[treesA3A4$ID>0,]


  treesA3A4 <- merge(treesA3A4,treesSegmentID,by=c("IDPlots","ID"),all.x=TRUE)
  treesA3A4[is.na(treesA3A4$IDSegments),]$IDSegments<-1

  # ontbrekende waarde voor status --> we veronderstemmen levende boom; ontbrekende waarde voor hakhout-individueel --> we veronderstellen individuele boom; ontbrekende waarde voor intact/niet-intacte boom --> we veronderstellen een intacte boom

  treesA3A4[is.na(treesA3A4$StatusTreeCode),]$StatusTreeCode <- 1
  treesA3A4[is.na(treesA3A4$StatusTree),]$StatusTree<- "levend"

  treesA3A4[is.na(treesA3A4$Coppice_IndividualCode),]$Coppice_IndividualCode<-10
  treesA3A4[is.na(treesA3A4$Coppice_Individual),]$Coppice_Individual<- "Individuele boom"

  treesA3A4[is.na(treesA3A4$IntactTreeCode),]$IntactTreeCode<-10
  treesA3A4[is.na(treesA3A4$IntactTree),]$IntactTree<- "Intacte boom"

  #### Bijschatten ontbrekende hoogtes: mediaan van boomhoogte per plot

  plots_medianHeight<-ddply(treesA3A4,.(IDPlots,Periode),summarise,
                            medianHeight=median(Height_m,na.rm=TRUE))


  #Zet de bomen waarvoor geen Height_m gekend gelijka aan de mediaanhoogte van de bomen in het plot
  treesA3A4<-merge(treesA3A4,plots_medianHeight,by=c("IDPlots","Periode"), all.x=TRUE) #gewijzigd naar all.x
  treesA3A4$Height_m<-ifelse(is.na(treesA3A4$Height_m),treesA3A4$medianHeight,treesA3A4$Height_m)

  treesA3A4$Height_m <- ifelse(is.na(treesA3A4$Height_m), mean(treesA3A4$Height_m,na.rm=TRUE),treesA3A4$Height_m)

  #enkel bomen selecteren die in segmenten aangeduid als bos vallen
  #'plotWeightsVBI2' bevat, per segment met bos, de oppervlaktes van het deel van de A3 en A4 plot dat in het segment valt. 'treesA3A4' bevat ook enkele bomen die foutief gelocaliseerd zijn in segmenten zonder bos. Via een inner join, verwijderen we deze foutief gelocaliseerde bomen.

  treesA3A4 <- merge(treesA3A4,plotWeights,by=c("IDPlots","IDSegments"))

  treesA3A4$DataSet <- "VBI2"

  treesA3A4$Alive <- treesA3A4$StatusTreeCode==1

  treesA3A4 <- treesA3A4[!is.na(treesA3A4$IDPlots),c("DataSet","IDPlots","IDSegments","ID","AreaA4_m2","AreaA3_m2","Perimeter_cm","DBH_mm","Height_m","IDTreeSp","Alive","IntactTreeCode","Coppice_IndividualCode")]


  #extra informatie over boomsoorten toevoegen

  connectieExterneData <- odbcConnectAccess2007(dbExterneData)

    treeList <- sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics")

  odbcClose(connectieExterneData)

  treesA3A4 <- merge(treesA3A4,treeList,by="IDTreeSp",all.x=TRUE)


  if (is.null(plotIDs)){

    result <- treesA3A4

  } else {

    result <- treesA3A4[treesA3A4$IDPlots %in% plotIDs,]

  }

  return(result)

}

#############################################################################################################



### Haal gegevens op van A2-bomen uit VBI2-databank

getTreesA2VBI2 <- function (db =  dbVBI2, plotIDs = NULL){

  query_treesA2<-"
SELECT
  Doorgroeiende_verjonging.IDPlots
  , Doorgroeiende_verjonging.Species
  , qTreeSpecies.Value1
  , Doorgroeiende_verjonging.Number
  FROM Doorgroeiende_verjonging
  LEFT JOIN qTreeSpecies
  ON Doorgroeiende_verjonging.Species = qTreeSpecies.ID
  ;"

  connectieVBI2 <- odbcConnectAccess2007(db) #dit is een mdb file

    treesA2Orig<-sqlQuery(connectieVBI2, query_treesA2, stringsAsFactors = TRUE)

  odbcClose(connectieVBI2)

  treesA2 <- plyr::rename(treesA2Orig,c(Species="IDTreeSp",Value1="Species"))

  #extra informatie over boomsoorten toevoegen

  connectieExterneData <- odbcConnectAccess2007(dbExterneData)

  treeList <- sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics")

  odbcClose(connectieExterneData)

  treesA2 <- treesA2[!is.na(treesA2$IDTreeSp),]

  treesA2 <- merge(treesA2,treeList,by="IDTreeSp",all.x=TRUE)


  if (is.null(plotIDs)){

    result <- treesA2

  } else {

    result <- treesA2[treesA2$IDPlots %in% plotIDs,]

  }

  return(result)


  }

##############################################################################################

### Haal gegevens op van hakhoutspillen uit VBI2 databank


getShootsVBI2 <- function(db =  dbVBI2, plotIDs = NULL){

query_shoots<-"
SELECT
  Shoots_2eBosinv.IDPlots
  , Shoots_2eBosinv.IDTrees_2eBosinv
  , Shoots_2eBosinv.ID
  , Shoots_2eBosinv.Perimeter_cm
  , Shoots_2eBosinv.Height_m
  , Shoots_2eBosinv.DBH_MM
  FROM Shoots_2eBosinv
  ;"

  connectieVBI2 <- odbcConnectAccess2007(db)

  shootsOrig <- sqlQuery(connectieVBI2, query_shoots, stringsAsFactors = TRUE);

  odbcClose(connectieVBI2)

  shoots <- plyr::rename(shootsOrig,c(ID="ShootID",IDTrees_2eBosinv="ID",Height_m="Height_shoot_m"))

  if (is.null(plotIDs)){

    result <- shoots

  } else {

    result <- shoots[shoots$IDPlots %in% plotIDs,]

  }

  return(result)


}


### Haal gegevens over liggend dood hout uit de VBI2-databank


getLogsVBI2 <- function (db = dbVBI2, plotIDs = NULL ) {

  query_LIM<-"SELECT LIM_data.IDPlots, LIM_data.IDLine_intersect_method, LIM_data.Diameter_cm, LIM_data.Angle_degrees
FROM LIM_data;
  "
  connectieVBI2 <- odbcConnectAccess2007(db)


  LIMOrig<- sqlQuery(connectieVBI2,query_LIM, stringsAsFactors = TRUE)

  odbcClose(connectieVBI2)


  logs <- LIMOrig

  Li<-45

  logs$Volume_ha <- pi^2/8/Li*(logs$Diameter_cm^2)/cos(logs$Angle_degrees*pi/180)

  if (is.null(plotIDs)){

    result <- logs

  } else {

    result <- logs[logs$IDPlots %in% plotIDs,]

  }

  return(result)

}

######################################################################################
### FUNCTIES VOOR BEREKENING GRONDVLAK EN VOLUME --> NODIG VOOR INDICATOREN HABITATSTRUCTUUR BOSSEN
#######################################################################################

#### Bereking grondvlak en volume waarbij tarieven en aantal ingangen worden gespecifieerd. De berekeningen gebeuren op basis van een data.frame met de omtrekgegevens (default variabele naam ='Perimeter_cm') en hoogte-gegevens (in geval van 2 ingangen; default variabele naam = 'Height_m'). Aan deze data.frame worden de berekende volumes toegevoegd (default variabele naam = 'Volume') en de berekende grondvlakken (default variabele naam = 'BasalArea_m2')


calcVolumeAndBasalAreaTree <-function(treeMeasurements, tarieven, nIngang, varNamePerimeter = "Perimeter_cm", varNameHeight = "Height_m", varNameVolume = "Volume", varNameBasalArea = "BasalArea_m2", varNameDiameter="D"){

  #code soortnaam identiek voor verschillende periodes, maar soortnaam kan verschillen
  tarieven <- tarieven[,names(tarieven) != "Species"]

  trees <- merge(treeMeasurements,tarieven,by="IDTreeSp",all.x=TRUE)

  #grondvlak

  #Hulpvariabelen
  Radius_m <- 1/100 * trees[,varNamePerimeter] / (2*pi)

  #Hulpvariabelen bewaard in dataset
  trees$BasalArea_m2 <- pi * Radius_m^2
  trees$D <- trees[,varNamePerimeter]/pi

  if (nIngang==2){

    trees$Volume <-
      ifelse( trees$Formule_type == 1,
              yes =
                trees$a + trees$b * trees[,varNamePerimeter] +
                trees$c *(trees[,varNamePerimeter]^2)+ trees$d *(trees[,varNamePerimeter]^3) +
                trees$e*trees[,varNameHeight] + trees$f*trees[,varNameHeight]* trees[,varNamePerimeter] +
                trees$g*trees[,varNameHeight]*(trees[,varNamePerimeter]^2),
              no =
                1/1000 *
                #spil
                (exp(1.10597 * log(trees[,varNameHeight]) + 1.78865 * log(trees$D) - 3.07192) -
                   #Verlies
                   exp(-4.608923 * log(trees$D) + 3.005989 * log(trees[,varNameHeight]) -
                         1.3209 * log(trees[,varNameHeight])*log(trees[,varNameHeight])+ 1.605266 * log(trees$D) * log(trees[,varNameHeight]) + 5.410272))
      )


  } else if (nIngang==1){
    trees$Volume<- trees$a + trees$b * trees[,varNamePerimeter] + trees$c *(trees[,varNamePerimeter]^2)+ trees$d *(trees[,varNamePerimeter]^3)
  } else {
    trees$Volume = NaN
  }

  trees<-trees[,!names(trees) %in% c("a","b","c","d","e","f","g","Formule_type","Tarief","groepNaam")]

  trees$Volume<-pmax(0,trees$Volume)

  trees <- plyr::rename(trees,c(Volume=varNameVolume,BasalArea_m2=varNameBasalArea,D=varNameDiameter))

  return(trees)
}


###################################################################################################

### Berkening volume en grondvlak op basis van hoogte- en omtrekgegevens VBI2

calculateVolumeAndBasalArea <- function(treesA3A4, shoots){

  # tarieven

  query_tarieven2ing<-"
SELECT
  tblTariefgroepBoomsoort.ID
  , tblTariefgroepBoomsoort.Value
  , tblTarieven_2ing.Tarief
  , tblTarieven_2ing.groepNaam
  , tblTarieven_2ing.a
  , tblTarieven_2ing.b
  , tblTarieven_2ing.c
  , tblTarieven_2ing.d
  , tblTarieven_2ing.e
  , tblTarieven_2ing.f
  , tblTarieven_2ing.g
  , tblTarieven_2ing.Formule_type
  FROM tblTariefgroepBoomsoort
  INNER JOIN tblTarieven_2ing ON tblTariefgroepBoomsoort.TariefID = tblTarieven_2ing.groepID
  ;"

  connectieExterneData <- odbcConnectAccess2007(dbExterneData)

  tarieven2ingOrig <- sqlQuery(connectieExterneData, query_tarieven2ing, stringsAsFactors = TRUE)

  odbcClose(connectieExterneData)

  tarieven2ing <- plyr::rename(tarieven2ingOrig,c(ID="IDTreeSp",Value="Species"))

  # volume van boom berekenen op basis van tarieven met 2 ingangen

  treesA3A4<- calcVolumeAndBasalAreaTree(treesA3A4,tarieven2ing,2)


  #aanpassen volume voor niet intacte bomen: volume cilinder

  treesA3A4$Volume<-ifelse(treesA3A4$IntactTreeCode==20,
                           yes =
                             treesA3A4$BasalArea_m2*treesA3A4$Height_m,
                           no =
                             ifelse(treesA3A4$IntactTreeCode==10 ,
                                    yes = treesA3A4$Volume,
                                    no = NA))

  #expansiefactoren --> volume per ha

  treesA3A4$Volume_ha <-
    ifelse(treesA3A4$Perimeter_cm < 122,
           yes =
             10000 * treesA3A4$Volume / treesA3A4$AreaA3_m2,
           no =
             10000 * treesA3A4$Volume / treesA3A4$AreaA4_m2
    )

  # grondvlak per hectare

  treesA3A4$BasalArea_ha<-ifelse(treesA3A4$Perimeter_cm < 122, treesA3A4$BasalArea_m2*10000/treesA3A4$AreaA3_m2,
                                 ifelse(treesA3A4$Perimeter_cm >= 122,treesA3A4$BasalArea_m2*10000/treesA3A4$AreaA4_m2,
                                        NA))

  treesA3A4<-treesA3A4[order(treesA3A4$IDPlots),]

  #volume & grondvlak hakhout


  #om volume te berekenen per shoot hebben we gegevens nodig uit 'treesA3A4': boomsoort, hoogte hakhoutstoof, gewichten etc

  shoots<-merge (shoots,treesA3A4[,!names(treesA3A4)%in% c("Perimeter_cm")],by=c("IDPlots","ID"),all.x=TRUE)

  shoots<- calcVolumeAndBasalAreaTree(shoots,tarieven2ing,2)

  #expansiefactoren

  shoots$Volume_ha<-ifelse(shoots$Perimeter_cm<122,shoots$Volume*10000/shoots$AreaA3_m2,
                               ifelse(shoots$Perimeter_cm>=122,shoots$Volume*10000/shoots$AreaA4_m2,NA))

  shoots$BasalArea_ha<-ifelse(shoots$Perimeter_cm<122,shoots$BasalArea_m2*10000/shoots$AreaA3_m2,
                                  ifelse(shoots$Perimeter_cm>=122,shoots$BasalArea_m2*10000/shoots$AreaA4_m2,NA))

  hakhout<-ddply(shoots,.(IDPlots,ID),summarise,
                     Volume_ha_hakhout=sum(Volume_ha,na.rm=TRUE),
                     BasalArea_ha_hakhout=sum(BasalArea_ha,na.rm=TRUE),
                     MaxPerimeter_cm=max(Perimeter_cm))

  treesA3A4<-merge(treesA3A4,hakhout,by=c("IDPlots","ID"),all.x=TRUE)

  treesA3A4$Volume_ha<-ifelse(!is.na(treesA3A4$Volume_ha_hakhout),treesA3A4$Volume_ha_hakhout,treesA3A4$Volume_ha)

  treesA3A4$BasalArea_ha<-ifelse(!is.na(treesA3A4$BasalArea_ha_hakhout),treesA3A4$BasalArea_ha_hakhout,treesA3A4$BasalArea_ha)

  #negatieve volumes = 0

  treesA3A4$Volume_ha<-pmax(0,treesA3A4$Volume_ha)

  treesA3A4 <- treesA3A4[,!colnames(treesA3A4) %in% c("BasalArea_m2","Volume","D","Volume_ha_hakhout","BasalArea_ha_hakhout","MaxPerimeter_cm")]


  return (treesA3A4)

}


############################################################################################


getStructurePlotHeide <- function(db = dbHeideEn6510_2014_2015){

  query_StructurePlotHeide <- "SELECT
  SiteDescription_HEIDE.IDPlots,
  SiteDescription_HEIDE.Shrub_and_Treelayer_18m,
  SiteDescription_HEIDE.Sphagnumlayer,
  SiteDescription_HEIDE.Campylopus_introflexus,
  SiteDescription_HEIDE.LowShrublayer,
  SiteDescription_HEIDE.Brushwood,
  SiteDescription_HEIDE.Herbs,
  SiteDescription_HEIDE.Calluna_phase_pioneer,
  SiteDescription_HEIDE.Calluna_phase_devel,
  SiteDescription_HEIDE.Calluna_phase_climax,
  SiteDescription_HEIDE.Calluna_phase_degen,
  SiteDescription_HEIDE.Pioneer_phase_open_soil,
  SiteDescription_HEIDE.Pioneer_Coryn_Aira,
  SiteDescription_HEIDE.Pioneer_Mos,
  SiteDescription_HEIDE.Pioneer_Lichenen
  FROM SiteDescription_HEIDE;"

  if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  structurePlots <- sqlQuery(connectieDB, query_StructurePlotHeide, stringsAsFactors = TRUE)

  #tansleyScale <- sqlFetch(connectieDB,"qCoverageTansley")

  odbcClose(connectieDB)

  tansleyScale <- data.frame(TansleyCode = c(1,5,11,16,22,23),Scale ="Tansley", Class = c("zeldzaam","occasioneel","frequent","abundant","codominant","dominant" ), Cover = c(0.5, 2.5, 15, 27.5,45,75 ))

  shrubTreeCover_long <- melt(structurePlots[,c("IDPlots","Shrub_and_Treelayer_18m")],id.vars = "IDPlots", value.name = "Cover", variable.name ="StructureVar"  )

  otherStructureVars_long <- melt(structurePlots[,!names(structurePlots) %in% c("Shrub_and_Treelayer_18m")],id.vars = "IDPlots", value.name = "TansleyCode", variable.name ="StructureVar"  )
  otherStructureVars_long2 <- merge(otherStructureVars_long,tansleyScale, by = "TansleyCode", all.x = TRUE)

  structurePlots_result <- rbind.fill(shrubTreeCover_long,otherStructureVars_long2)

  structurePlots_wide <- dcast (structurePlots_result[,c("IDPlots","StructureVar","Cover")], IDPlots ~ StructureVar)

  return(structurePlots_wide)

}

############################################################################################

getStructurePlot6510 <- function(db = dbHeideEn6510_2014_2015){

  query_StructurePlot6510 <- "SELECT
  SiteDescription_6510.IDPlots,
  SiteDescription_6510.Shrub_and_Treelayer_18m
  FROM SiteDescription_6510"

     if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  structurePlots <- sqlQuery(connectieDB, query_StructurePlot6510, stringsAsFactors = TRUE)

  #tansleyScale <- sqlFetch(connectieDB,"qCoverageTansley")

  odbcClose(connectieDB)

  return(structurePlots)
}



######################################################################################
### FUNCTIES VOOR BEREKENING LSVI-INDICATOREN (OP NIVEAU VAN ANALYSEVARIABELE)
#######################################################################################

### Berekening indicatoren habitatstructuur van boshabitats + berekening grondvlakaandeel sleutelsoorten (vegetatie-indicator)

calculateLSVI_dendroVBI2 <- function(db = dbVBI2, plotHabtypes, niveau = "segment", versieLSVI = "versie3"){

  ### Soortenlijst opvragen voor gewenste versie van LSVI
  connDB <-   odbcConnectAccess2007(dbLSVI)

  if (versieLSVI == "versie3"){

    soortenlijstLSVI <- sqlQuery(connDB, 'select * from tblSoortenlijst_LSVIv3')
    soortenlijstLSVI <- soortenlijstLSVI[soortenlijstLSVI$LSVI_v3 == 1,]
    indicatorenLSVI <- sqlQuery(connDB, 'select * from tblIndicatoren_LSVIv3_bossen')

  }

  odbcClose(connDB)

  # data voor berekening indicatoren

  treesA3A4 <- getTreesA3A4VBI2(db = db,plotIDs = plotHabtypes$IDPlots)

  # als we analyse op plotniveau wensen dan zetten we IDSegments overal op 1
  if (niveau == "plot"){
    treesA3A4$IDSegments <- 1
  }

  shoots <- getShootsVBI2(db,plotIDs = plotHabtypes$IDPlots)

  treesA3A4_VolBA <- calculateVolumeAndBasalArea(treesA3A4, shoots)

  ssBoomlaag <- soortenlijstLSVI[!is.na(soortenlijstLSVI$Omschrijving) & soortenlijstLSVI$Omschrijving == "sleutelsoorten_boomlaag",,drop=F]

  #groeiklasse

  treesA3A4_VolBA$Groeiklasse <- ifelse (treesA3A4_VolBA$DBH_mm >=70 & treesA3A4_VolBA$DBH_mm < 140 & treesA3A4_VolBA$Alive,"Groeiklasse4",
                                         ifelse(treesA3A4_VolBA$DBH_mm < 500 & treesA3A4_VolBA$Alive, "Groeiklasse5",
                                                ifelse(treesA3A4_VolBA$DBH_mm < 800 & treesA3A4_VolBA$Alive, "Groeiklasse6",
                                                       ifelse(treesA3A4_VolBA$DBH_mm > 800 & treesA3A4_VolBA$Alive, "Groeiklasse7",NA))))

  treesA2 <- getTreesA2VBI2(db,plotHabtypes$IDPlots)

  speciesVeglayers <- getCoverSpeciesVBI2(db,plotHabtypes$IDPlots)

  coverVeglayers <- getCoverVeglayersVBI2(db,plotHabtypes$IDPlots)

  logs <- getLogsVBI2(db,plotHabtypes$IDPlots)

  treesA3A4_VolBA <- merge(treesA3A4_VolBA,plotHabtypes,by="IDPlots",all.x=TRUE)

  calcIndic <- function(treedata,treelist){
    data.frame(
      VolumeStaandDood = sum(treedata$Volume_ha*(!treedata$Alive),na.rm = TRUE),
      VolumeStaandLevend = sum(treedata$Volume_ha*(treedata$Alive),na.rm = TRUE),
      GrondvlakDood = sum(treedata$BasalArea_ha*(!treedata$Alive),na.rm = TRUE),
      GrondvlakLevend = sum(treedata$BasalArea_ha*(treedata$Alive),na.rm = TRUE),
      GrondvlakLevendSs = sum(treedata$BasalArea_ha*(treedata$Alive)*(treedata$NameSc %in% treelist[treelist$HabCode == as.character(unique(treedata$HabCode)),]$WetNaam),na.rm = TRUE),
      DikDoodHoutStaand_ha = sum((!treedata$Alive)*(treedata$DBH_mm > 400)*ifelse(treedata$Perimeter_cm < 122, 10000.0/treedata$AreaA3_m2, 10000.0/treedata$AreaA4_m2),na.rm = TRUE),
      AantalGroeiklassenA3A4 = length (unique(na.omit(treedata$Groeiklasse))),
      Groeiklasse7 = "Groeiklasse7" %in% treedata$Groeiklasse,
      Groeiklasse5_6_7 = "Groeiklasse5" %in% treedata$Groeiklasse | "Groeiklasse6" %in% treedata$Groeiklasse | "Groeiklasse7" %in% treedata$Groeiklasse)
  }

  plots <- ddply(treesA3A4_VolBA, .(IDPlots,IDSegments,HabCode), calcIndic, treelist= ssBoomlaag)

  # een A2 boom komt oveeen met groeiklasse 3
  treesA2_plots <- data.frame(IDPlots = unique(treesA2$IDPlots), Groeiklasse3 = TRUE)
  treesA2_plots <- merge(treesA2_plots,plotHabtypes,by="IDPlots",all.x=TRUE)

  plots <- merge(plots,treesA2_plots,by=c("IDPlots","HabCode"), all= TRUE)

  # natuurlijke verjonging komt overeen met groeiklasse 2
  speciesVeglayers$Groeiklasse2_species <- speciesVeglayers$Tree & (speciesVeglayers$Vegetatielaag == "kruidlaag")

  speciesVeglayers_plot <- ddply(speciesVeglayers,.(IDPlots),summarise,
                                 Groeiklasse2 = sum(Groeiklasse2_species) > 0)

  speciesVeglayers_plot <- merge(speciesVeglayers_plot,plotHabtypes,by="IDPlots",all.x=TRUE)

  plots <- merge(plots, speciesVeglayers_plot[,c("IDPlots", "HabCode","Groeiklasse2")], by =c("IDPlots","HabCode"),all=TRUE)

  plots$IDSegments <- ifelse(is.na(plots$IDSegments),1,plots$IDSegments)

  plots$Groeiklasse2 <- ifelse(is.na(plots$Groeiklasse2),FALSE,plots$Groeiklasse2)
  plots$Groeiklasse3 <- ifelse(is.na(plots$Groeiklasse3),FALSE,plots$Groeiklasse3)
  plots$Groeiklasse7 <- ifelse(is.na(plots$Groeiklasse7),FALSE,plots$Groeiklasse7)
  plots$AantalGroeiklassenA3A4 <- ifelse(is.na(plots$AantalGroeiklassenA3A4),FALSE,plots$AantalGroeiklassenA3A4)

  plots$AantalGroeiklassen <- plots$AantalGroeiklassenA3A4 + plots$Groeiklasse3 + plots$Groeiklasse2

  # groeiklasse 1 komt overeen met 'open ruimte in bos', maar kunnen we niet afleiden uit VBI2-data

  # volume liggend dood hout per plot
  logs_plot <- ddply(logs,.(IDPlots), summarise,
                     VolumeLiggendDood = sum(Volume_ha,na.rm=TRUE))

  plots <- merge(plots,logs_plot, by = "IDPlots", all.x=TRUE)

  plots[is.na(plots$VolumeLiggendDood),]$VolumeLiggendDood <- 0

  # aandeel dood hout
  plots$VolumeAandeelDoodhoutStaand <- plots$VolumeStaandDood/(plots$VolumeStaandDood + plots$VolumeStaandLevend) * 100

  plots$GrondvlakAandeelDoodhoutStaand <- plots$GrondvlakDood/(plots$GrondvlakDood + plots$GrondvlakLevend) * 100

  plots$VolumeAandeelDoodhoutTotaal <- (plots$VolumeStaandDood + plots$VolumeLiggendDood) / (plots$VolumeStaandDood + plots$VolumeLiggendDood + plots$VolumeStaandLevend) * 100

  # grondvlakaandeel sleutelsoorten
  plots$GrondvlakAandeelSs <- ifelse(plots$GrondvlakLevend > 0,plots$GrondvlakLevendSs/plots$GrondvlakLevend * 100,0)

  calcIndic2 <- function(treedata,treelist){
    data.frame(
      GrondvlakLevendSs_soort = sum(treedata$BasalArea_ha*(treedata$Alive)*(treedata$NameSc %in% treelist[treelist$HabCode == as.character(unique(treedata$HabCode)),]$WetNaam),na.rm=TRUE)
    )
  }

  treeSpecies <- ddply(treesA3A4_VolBA,.(IDPlots,IDSegments,NameSc,HabCode), calcIndic2,treelist = ssBoomlaag)

  treeSpecies <- merge(treeSpecies,plots[,c("IDPlots","IDSegments","GrondvlakLevend")], by = c("IDPlots","IDSegments"), all.x =TRUE)

  treeSpecies$GrondvlakAandeelSs_soort <- treeSpecies$GrondvlakLevendSs_soort/treeSpecies$GrondvlakLevend * 100

  plots2 <- ddply(treeSpecies,.(IDPlots, IDSegments),summarise,
                  AantalSsBedekkingMinstens10 = sum(GrondvlakAandeelSs_soort >= 10))

  plots <- merge(plots, plots2, by=c("IDPlots","IDSegments"), all.x= TRUE)

  # aantal abundante vegetatielagen

  coverVeglayers$HerbLayerAbundant <- (coverVeglayers$CoverHerblayer >=25) | ((coverVeglayers$CoverHerblayer + coverVeglayers$CoverMosslayer) >= 25)

  coverVeglayers$ShrubLayerAbundant <- coverVeglayers$CoverShrublayer >= 25

  coverVeglayers$TreeLayerAbundant <- coverVeglayers$CoverTreelayer >= 25

  coverVeglayers$AantalAbundanteVegetatielagen <- coverVeglayers$HerbLayerAbundant + coverVeglayers$ShrubLayerAbundant + coverVeglayers$TreeLayerAbundant

  # aantal aanwezige vegetatielagen

  coverVeglayers$HerbLayerPresent <- (coverVeglayers$CoverHerblayer >0) | ((coverVeglayers$CoverHerblayer + coverVeglayers$CoverMosslayer) > 0)

  coverVeglayers$ShrubLayerPresent <- coverVeglayers$CoverShrublayer > 0

  coverVeglayers$TreeLayerPresent <- coverVeglayers$CoverTreelayer > 0

  coverVeglayers$AantalAanwezigeVegetatielagen <- coverVeglayers$HerbLayerPresent + coverVeglayers$ShrubLayerPresent + coverVeglayers$TreeLayerPresent

  # aantal frequente vegetatielagen

  coverVeglayers$HerbLayerFrequent <- (coverVeglayers$CoverHerblayer > 5) | ((coverVeglayers$CoverHerblayer + coverVeglayers$CoverMosslayer) > 5)

  coverVeglayers$ShrubLayerFrequent <- coverVeglayers$CoverShrublayer > 5

  coverVeglayers$TreeLayerFrequent <- coverVeglayers$CoverTreelayer > 5

  coverVeglayers$AantalFrequenteVegetatielagen <- coverVeglayers$HerbLayerFrequent + coverVeglayers$ShrubLayerFrequent + coverVeglayers$TreeLayerFrequent


  coverVeglayers <- merge(coverVeglayers,plotHabtypes,by="IDPlots",all.x=TRUE)

  plots <- merge(plots, coverVeglayers[,c("IDPlots","HabCode","AantalAbundanteVegetatielagen", "AantalAanwezigeVegetatielagen", "AantalFrequenteVegetatielagen")],by=c("IDPlots","HabCode"),all=TRUE)

  plots$VolumeAandeelDoodhoutStaand <- ifelse(is.na(plots$VolumeAandeelDoodhoutStaand),0,plots$VolumeAandeelDoodhoutStaand)

  plots$GrondvlakAandeelDoodhoutStaand <- ifelse(is.na(plots$GrondvlakAandeelDoodhoutStaand),0,plots$GrondvlakAandeelDoodhoutStaand)

  plots$VolumeAandeelDoodhoutTotaal <- ifelse(is.na(plots$VolumeAandeelDoodhoutTotaal),0,plots$VolumeAandeelDoodhoutTotaal)

  plots$AantalSsBedekkingMinstens10 <- ifelse(is.na(plots$AantalSsBedekkingMinstens10),0,plots$AantalSsBedekkingMinstens10)

  plots$GrondvlakAandeelSs <- ifelse(is.na(plots$GrondvlakAandeelSs),0,plots$GrondvlakAandeelSs)

  ### Selectie indicatoren voor LSVI_v3

  structuurIndicatoren_LSVI3 <- plots[,c("IDPlots","IDSegments","HabCode","AantalFrequenteVegetatielagen","AantalGroeiklassen", "Groeiklasse7", "Groeiklasse5_6_7", "VolumeAandeelDoodhoutTotaal","GrondvlakAandeelSs","DikDoodHoutStaand_ha")]

  #naamgeving conform databank indicatoren

  structuurIndicatoren_LSVI3 <- plyr::rename(structuurIndicatoren_LSVI3, c(AantalFrequenteVegetatielagen ="frequenteVegetatielagen", AantalGroeiklassen = "groeiklassen", Groeiklasse7 = "groeiklasse7",Groeiklasse5_6_7 ="groeiklasse5_6_7" , VolumeAandeelDoodhoutTotaal = "volumeAandeelDoodHout", GrondvlakAandeelSs = "sleutelsoorten_boomlaag_grondvlakAandeel", DikDoodHoutStaand_ha = "dikDoodHoutStaand_ha"))

structuurIndicatoren_LSVI3_long <- melt (structuurIndicatoren_LSVI3, id.vars = c("IDPlots","IDSegments","HabCode"), measure.vars = c("sleutelsoorten_boomlaag_grondvlakAandeel", "frequenteVegetatielagen","groeiklassen","groeiklasse7" ,"groeiklasse5_6_7", "volumeAandeelDoodHout","dikDoodHoutStaand_ha"), variable.name = "AnalyseVariabele", value.name = "Waarde")

structuurIndicatoren_LSVI3_selectie <- merge(structuurIndicatoren_LSVI3_long, indicatorenLSVI[indicatorenLSVI$Meting == "structuurplot",], by = c("HabCode", "AnalyseVariabele"))

indicatoren <- structuurIndicatoren_LSVI3_selectie[,c("HabCode","IDPlots", "IDSegments", "Criterium", "Indicator", "AnalyseVariabele", "Soortengroep", "Vegetatielaag","Eenheid", "Drempelwaarde","Indicatortype","Meting", "Combinatie", "Waarde")]

indicatoren$Beoordeling <- ifelse (indicatoren$Indicatortype == "negatief", ifelse(indicatoren$Waarde <= indicatoren$Drempelwaarde, 1,0),
                                   ifelse (indicatoren$Indicatortype == "positief", ifelse(indicatoren$Waarde >= indicatoren$Drempelwaarde, 1,0),NA))

if (niveau == "plot"){
  indicatoren <- indicatoren[,!colnames(indicatoren) %in% "IDSegments"]
}

  return(indicatoren)

}

###########################################################################################################


calculateLSVI_structuurplotHeide <- function(structurePlot, plotHabtypes, versieLSVI = "beide"){

  structurePlotHeide <- structurePlot

  ### Indicatoren opvragen voor beide versies van LSVI
  connDB <-   odbcConnectAccess2007(dbLSVI)

  indicatorenLSVI <- sqlQuery(connDB, 'select * from tblIndicatoren_LSVI_HeideEnBoshabitats')

  odbcClose(connDB)

  # dwergstruiken
  structurePlotHeide$dwergstruiken <-ifelse(is.na(structurePlotHeide$LowShrublayer),0, structurePlotHeide$LowShrublayer)

  # ouderdomstructuur struikhei
  structurePlotHeide$BedekkingPionierstadium <- ifelse(is.na(structurePlotHeide$Calluna_phase_pioneer),0,structurePlotHeide$Calluna_phase_pioneer)
  structurePlotHeide$BedekkingOntwikkelingsstadium <- ifelse(is.na(structurePlotHeide$Calluna_phase_devel),0,structurePlotHeide$Calluna_phase_devel)
  structurePlotHeide$BedekkingClimaxstadium <- ifelse(is.na(structurePlotHeide$Calluna_phase_climax),0,structurePlotHeide$Calluna_phase_climax)
  structurePlotHeide$BedekkingDegeneratiestadium <- ifelse(is.na(structurePlotHeide$Calluna_phase_degen),0,structurePlotHeide$Calluna_phase_degen)

  structurePlotHeide$ouderdomstadia <- (structurePlotHeide$BedekkingPionierstadium > 0) + (structurePlotHeide$BedekkingOntwikkelingsstadium > 0) + (structurePlotHeide$BedekkingClimaxstadium > 0) + (structurePlotHeide$BedekkingDegeneratiestadium > 0)

  structurePlotHeide$struikhei <- structurePlotHeide$ouderdomstadia > 0

  structurePlotHeide$climaxOfDegradatieStadium <- (structurePlotHeide$BedekkingClimaxstadium > 0) | (structurePlotHeide$BedekkingDegeneratiestadium > 0)

  # bedekking naakte bodem
  structurePlotHeide$naakte_bodem <- ifelse(is.na(structurePlotHeide$Pioneer_phase_open_soil),0,structurePlotHeide$Pioneer_phase_open_soil)

  # pionierstadia/ bedekking open vegetatie
  structurePlotHeide$moslaag <- ifelse(is.na(structurePlotHeide$Pioneer_Mos),0,structurePlotHeide$Pioneer_Mos)
  structurePlotHeide$BedekkingBuntgras <- ifelse(is.na(structurePlotHeide$Pioneer_Coryn_Aira),0,structurePlotHeide$Pioneer_Coryn_Aira)

  structurePlotHeide$korstmosvegetaties <- ifelse(is.na(structurePlotHeide$Pioneer_Lichenen),0,structurePlotHeide$Pioneer_Lichenen)
  #
  structurePlotHeide$pionierStadia <- (structurePlotHeide$naakte_bodem > 0) + (structurePlotHeide$BedekkingBuntgras > 0) + (structurePlotHeide$moslaag > 0) + (structurePlotHeide$korstmosvegetaties > 0)

  structurePlotHeide$openVegetatieOfKaalZand <- pmin(structurePlotHeide$moslaag + structurePlotHeide$korstmosvegetaties + structurePlotHeide$naakte_bodem + structurePlotHeide$BedekkingBuntgras,100)

  structurePlotHeide$openVegetatie <- pmin(structurePlotHeide$moslaag + structurePlotHeide$korstmosvegetaties +  structurePlotHeide$BedekkingBuntgras, 100)

  # bedekking moslaag (mos + korstmos)
  structurePlotHeide$BedekkingMosEnKorstmos <- structurePlotHeide$moslaag + structurePlotHeide$korstmosvegetaties

  # bedekking veenmos
  structurePlotHeide$veenmoslaag <- ifelse(is.na(structurePlotHeide$Sphagnumlayer),0,structurePlotHeide$Sphagnumlayer)

  # verbossing
  structurePlotHeide$verbossing <- structurePlotHeide$Shrub_and_Treelayer_18m

  # vergrassing
  structurePlotHeide$vergrassing <- ifelse(is.na(structurePlotHeide$Herbs),0,structurePlotHeide$Herbs)

  # verruiging
  structurePlotHeide$verruiging <- ifelse(is.na(structurePlotHeide$Brushwood),0,structurePlotHeide$Brushwood)


  # vergrassing+verruiging
  structurePlotHeide$vergrassingEnVerruiging <- structurePlotHeide$vergrassing + structurePlotHeide$verruiging

  #invasieve exoten
  structurePlotHeide$invasieve_exoten <- ifelse(is.na(structurePlotHeide$Campylopus_introflexus),0,structurePlotHeide$Campylopus_introflexus)

  #result <- structurePlotHeide[,c("IDPlots","BedekkingDwergstruiken","AantalOuderdomstadiaAanwezig", "AantalOuderdomstadiaFrequent","VoorkomenClimaxOfDegeneratieStadium","BedekkingNaakteBodem", "BedekkingMosEnKorstmos","AantalPionierstadiaAanwezig","BedekkingOpenVegetatieZand", "BedekkingOpenVegetatie","BedekkingVeenmos","Vergrassing","Verruiging","VergrassingEnVerruiging","Verbossing","BedekkingInvasieveExoten")]

  # habitattype per plot

  structurePlotHeide <- merge (structurePlotHeide, plotHabtypes, by = "IDPlots")

  structurePlotHeide_long <- melt(structurePlotHeide, id.vars = c("IDPlots","HabCode"),variable.name = "AnalyseVariabele", value.name = "Waarde")

  structurePlotHeide_indicatoren <- merge(structurePlotHeide_long, indicatorenLSVI[indicatorenLSVI$Meting == "structuurplot",],  by = c("HabCode","AnalyseVariabele") )

  indicatoren <- structurePlotHeide_indicatoren[,c("HabCode","IDPlots", "Criterium", "Indicator", "AnalyseVariabele", "Soortengroep", "Vegetatielaag","Eenheid", "Drempelwaarde","Indicatortype","Meting", "Combinatie", "Waarde","VersieLSVI")]

  indicatoren$Beoordeling <- ifelse (indicatoren$Indicatortype == "negatief",
                                     ifelse(indicatoren$Waarde <= indicatoren$Drempelwaarde, 1,0),
                                     ifelse (indicatoren$Indicatortype == "positief",
                                             ifelse(indicatoren$Waarde >= indicatoren$Drempelwaarde,
                                                    1,
                                                    0),
                                             NA))

  return (indicatoren)

}


#############################################################################################################

### Berekening van LSVI-indicatoren voor vegetatie en verstoring op basis van vegetatie-opname

#?# Is er nog een correctie nodig van totale bedekking van een soortengroep in functie van totale bedekking van vegetatielaag.
#?# Misschien best soortenlijst met bedekkingen als input, dan wordt het generieker #?#

calculateLSVI_vegetatieopname <- function (plotHabtypes, bedekkingSoorten, bedekkingVeglagen  ,versieLSVI = "beide"){




  ### Soortenlijst opvragen voor gewenste versie van LSVI

  connDB <-   odbcConnectAccess2007(dbLSVI)

  soortenlijstLSVI <- sqlQuery(connDB, 'select * from tblSoortenlijst_LSVI_HeideEnBoshabitats')
  #soortenlijstLSVI <- soortenlijstLSVI[soortenlijstLSVI$LSVI_v3==1,]
  indicatorenLSVI <- sqlQuery(connDB, 'select * from tblIndicatoren_LSVI_HeideEnBoshabitats')

  odbcClose(connDB)

  # Long formaat
  soortenlijstLSVI <- soortenlijstLSVI %>%
    gather( Versie2, Versie3, key = VersieLSVI, value = Selectie) %>%
    filter(Selectie == 1 & !is.na(Selectie))

  soortenlijstLSVI$VersieLSVI <- tolower(soortenlijstLSVI$VersieLSVI)

  ### Selectie van indicatoren die op basis van vegetatieopname worden berekend
  indicatorenLSVI_selectie <- indicatorenLSVI[indicatorenLSVI$Meting == "vegetatieplot",]


  ### Totale bedekking van soorten voor verschillende (combinaties van) vegetatielagen
  somBedekkingSoorten <- ddply(bedekkingSoorten,.(IDPlots),summarise,
                               SomBedekkingKruidlaag = (1 - prod((100 - Cover * (Vegetatielaag == "kruidlaag"))/100, na.rm=TRUE)) * 100,
                               SomBedekkingStruiklaag = (1 - prod((100 - Cover * (Vegetatielaag == "struiklaag"))/100, na.rm=TRUE)) * 100,
                               SomBedekkingBoomlaag = (1 - prod((100 - Cover * (Vegetatielaag == "boomlaag"))/100, na.rm=TRUE)) * 100,
                               SomBedekkingBoomEnStruiklaag = (1 - prod((100 - Cover * (Vegetatielaag %in% c("boomlaag", "struiklaag")))/100, na.rm=TRUE)) * 100,
                               SomBedekkingTotaal = (1 - prod((100 - Cover) /100, na.rm=TRUE)) * 100)


  ### Berekeing van totale bedekking van combinaties van vegetatielagen op basis van ingeschatte bedekkingen per laag
  bedekkingVeglagen$CoverHerbShrubTreeLayer <- (1 - (100- bedekkingVeglagen$CoverHerblayer)/100 * (100- bedekkingVeglagen$CoverShrublayer)/100 * (100- bedekkingVeglagen$CoverTreelayer)/100) *100

  ### Aanmaak van data.frame met berekende waarden voor analysevariabelen per segment/plot

  # Eerst kennen we aan elke plot de juiste indicatoren toe op basis van het habitattype
  indicatoren <- merge(plotHabtypes,indicatorenLSVI_selectie, by = "HabCode")

  # Vervolgens lopen we de indicatoren een voor een af en berkenen we de waarde op basis van de vegetatie-opnamegegevens

  indicatoren$Waarde <- NA

  for (i in 1: nrow(indicatoren)){

    # lijst met soorten binnen gespecifieerde soortgroep
    soortenlijst <- soortenlijstLSVI[(as.character(soortenlijstLSVI$HabCode) == as.character(indicatoren$HabCode[i])) & (as.character(soortenlijstLSVI$Omschrijving) == as.character(indicatoren$Soortengroep[i])) & (as.character(soortenlijstLSVI$VersieLSVI) == as.character(indicatoren$VersieLSVI[i])) ,]$WetNaam

    # selectie van (opgemeten) soorten voor gespecifieerde plot en binnen gespecifieerde vegetatielaag
    if (is.na(indicatoren$Vegetatielaag[i])| indicatoren$Vegetatielaag[i] == "" ){

      soortenOpname <-  bedekkingSoorten[bedekkingSoorten$IDPlots == indicatoren$IDPlots[i],]

    } else if (indicatoren$Vegetatielaag[i] == "kruidlaag"){

      soortenOpname <-  bedekkingSoorten[bedekkingSoorten$IDPlots == indicatoren$IDPlots[i] & bedekkingSoorten$Vegetatielaag == "kruidlaag",]

    } else if (indicatoren$Vegetatielaag[i] == "boomEnStruiklaag"){

      soortenOpname <-  bedekkingSoorten[bedekkingSoorten$IDPlots == indicatoren$IDPlots[i] & bedekkingSoorten$Vegetatielaag %in% c("boomlaag","struiklaag"),]
    }

    # selectie van soorten binnen soortengroep en berekening gezamelijke bedekking of aantal soorten
    if (nrow(soortenOpname) > 0 ){

      selectieSoortenOpname <- soortenOpname[soortenOpname$NameSc %in% soortenlijst,]

      if (nrow(selectieSoortenOpname) == 0){

        indicatoren$Waarde[i] <- 0

      } else {

        if (indicatoren$Eenheid[i] == "bedekking"){

          bedekking <- selectieSoortenOpname$Cover
          bedekkingInv <- (100 - bedekking)/100
          bedekkingSom <- (1 - prod(bedekkingInv, na.rm=T))*100

          # indien sleutelsoorten kruidlaag boshabitat: relatieve bedekking t.o.v. bedekking kruidlaag
          if (indicatoren$Indicator[i] == "sleutelsoorten_kruidlaag"  ){

            bedekkingKruidlaag <- bedekkingVeglagen[bedekkingVeglagen$IDPlots == indicatoren$IDPlots[i], ]$CoverHerblayer

            # indien de bedekking van de kruidlaag ontbreekt, nemen we de som van de bedekkingen van alle soorten in de kruidlaag

            if (is.na(bedekkingKruidlaag)){

              bedekkingKruidlaag <- somBedekkingSoorten[somBedekkingSoorten$IDplots == indicatoren$IDPlots[i],]$SomBedekkingKruidlaag

            }

            bedekkingSom <- min(100,bedekkingSom/bedekkingKruidlaag * 100)

          }

          indicatoren$Waarde[i] <- bedekkingSom

        } else if (indicatoren$Eenheid[i] == "aantal"){

          indicatoren$Waarde[i] <- length(unique(selectieSoortenOpname$NameSc))

        } else if (indicatoren$Eenheid[i] == "aantalFrequent"){

          indicatoren$Waarde[i] <-  sum(selectieSoortenOpname$Cover >= 5)

        }

      }

    } else if (nrow(soortenOpname) == 0 ){

      indicatoren$Waarde[i] <- NA

    }

  }

  # beoordeling van analysevariabele op basis van drempelwaarde

  indicatoren$Beoordeling <- ifelse (indicatoren$Indicatortype == "negatief", ifelse(indicatoren$Waarde <= indicatoren$Drempelwaarde, 1,0),
                             ifelse (indicatoren$Indicatortype == "positief", ifelse(indicatoren$Waarde >= indicatoren$Drempelwaarde, 1,0),NA))

  indicatoren <- indicatoren [,!colnames(indicatoren) %in% "Id"]

  return(indicatoren)

}


#############################################################################################################

# calculateLSVI_Indicatoren_Bos <- function(db = dbVBI2, plotHabtypes,versieLSVI = "versie3" , niveau ="segment"){
#
#
#   habitatStructuur <- calculateLSVI_HabitatstructuurBosHabitats(db = db, plotHabtypes = plotHabtypes, niveau = niveau, versieLSVI = versieLSVI)
#
#   verstoringVegetatie <- calculateLSVI_VegetatieVerstoring (db = db, plotHabtypes = plotHabtypes, versieLSVI = versieLSVI)
#
#   if (niveau == "segment"){
#
#     plotSegments <- unique(habitatStructuur[,c("IDPlots","IDSegments")])
#
#     verstoringVegetatie <- merge (plotSegments, verstoringVegetatie, by = "IDPlots", all =TRUE)
#
#     verstoringVegetatie$IDSegments <- ifelse(is.na(verstoringVegetatie$IDSegments),1,verstoringVegetatie$IDSegments)
#
#
#   }
#
#   indicatoren <- rbind(habitatStructuur,verstoringVegetatie)
#   indicatoren <- indicatoren[order(indicatoren$Criterium),]
#   indicatoren <- indicatoren[order(indicatoren$HabCode),]
#   indicatoren <- indicatoren[order(indicatoren$IDPlots),]
#
#   return(indicatoren)
#
# }
#




####################################################################################
### NOG WAT FUNCTIES VOOR HABITATSLEUTEL TE KUNNEN TOEPASSEN
####################################################################################


getStandAge <- function(db = dbAnalyse, plotIDs = NULL, Periode = 2){

query_StandAge <- "SELECT tbl2BestandskaraktKwal.IDPlots,
tbl2BestandskaraktKwal.IDSegments,
tbl2BestandskaraktKwal.Periode,
tbl2BestandskaraktKwal.v5_StandAge
FROM tbl2BestandskaraktKwal;
"
connectieAnalyse <- odbcConnectAccess2007(dbAnalyse)

standAgeOrig <- sqlQuery(connectieAnalyse, query_StandAge, stringsAsFactors = TRUE)

odbcClose(connectieAnalyse)

standAge <- standAgeOrig[standAgeOrig$Periode == 2,]
standAge <- rename(standAge,c(v5_StandAge = "Bestandsleeftijd"))

standAge <- standAge[,c("IDPlots","IDSegments","Bestandsleeftijd")]


if (is.null(plotIDs)){

  result <- standAge

} else {

  result <- standAge[standAge$IDPlots %in% plotIDs,]

}

return(result)


}


######################################################

getLargeTreesVBI2 <- function (db = dbVBI2, soort = c("Inlandse eik", "Beuk"), enkelLevend = FALSE, plotIDs = NULL){

  query_Trees <- "SELECT
Trees_2eBosinv.IDPlots,
Trees_2eBosinv.DBH_mm,
Trees_2eBosinv.Species,
qTreeSpecies.Value,
Trees_2eBosinv.Status_tree
FROM Trees_2eBosinv INNER JOIN qTreeSpecies ON Trees_2eBosinv.Species = qTreeSpecies.ID;
"

  connectieVBI2 <- odbcConnectAccess(dbVBI2)

  treesOrig <- sqlQuery(connectieVBI2, query_Trees, stringsAsFactors = TRUE)

  odbcClose(connectieVBI2)

  trees <- rename (treesOrig, c(Species = "IDSpVBI2", Value = "NameNl"))

  if (enkelLevend){

    trees <- trees[trees$Status_tree == 1,]

  }

  trees$SelectedSpecies <- trees$NameNl %in% soort

  maxTreeSize_Plot <- ddply(trees,.(IDPlots), summarise,
                            MaxDiameterAllspecies_cm = max(DBH_mm, na.rm = TRUE)/10,
                            MaxDiameterSelectedSpecies_cm =max(DBH_mm * SelectedSpecies, na.rm=TRUE)/10)

  if (soort == ""){

    result <- maxTreeSize_Plot[,c("IDPlots", "MaxDiameterAllspecies_cm")]
    result <- rename (result, c(MaxDiameterAllspecies_cm = "MaxDiameter_cm"))

  } else {

    result <- maxTreeSize_Plot[,c("IDPlots", "MaxDiameterSelectedSpecies_cm")]
    result <- rename (result, c(MaxDiameterSelectedSpecies_cm = "MaxDiameter_cm"))
  }

  if (is.null(plotIDs)){

    result <- result

  } else {

    result <- result[result$IDPlots %in% plotIDs,]

  }

  return(result)




}



###############################################
### NOG EEN OUDE FUNCTIE VAN PIETER DIE IK NIET MEER GEBRUIK
###########################################################

#Functie die de mogelijkheid geeft om een som van bedekking of aantal soorten binnen een bepaalde laag of over alle lagen heen te berekenen.
critVoorkomenSoorten <- function(NR, habitat, soorten="<ALL>", fun="Aantal", laag="", vegdata, soortenlijst)
  #NR: een (character)vector die de plotnummers weergeeft
  #habitat: een (character)vector met dezelfde lengte als NR die het habitat per plotnummer weergeeft
  #soorten: een (character)vector met 1 of meerdere elementen die de variabelengroepen weergeeft die bij de gewenste indicator horen. Indien deze leeg is worden alle soorten genomen.
  #fun: kan "Aantal", "Bedekking", "Maximum" zijn en controleert welke functie we wensen te gebruiken
  #laag: de soorten uit welke laag nemen we?, Indien "" wordt niet gekeken naar de lagen en alle corresponderende soorten gegeven
  #vegdata: dataset met de vegetatieopnames
  #soortenlijst: dataset met soortenlijst voor de  variabelegroepen per habitat
  #structuurdata: dataset die de structuurkenmerken weergeeft, nodig als er delingen moeten gebeuren door totale bedekking van soorten.
{
  #als laag leeg, betekent dit alle lagen
  if("<ALL>" %in% soorten) soorten <- unique(soortenlijst$Omschrijving)
  tmpdata <- data.frame(NR=NR, HRL_code=habitat, stringsAsFactors=F)

  #Volgende lange functie berekent het aantal soorten, de bedekking of de maximale bedekking
  #De functie gaat rij per rij de plotnummers af, kiest de bijhorende bedekkingen en berekent het resultaat
  rv <- apply(tmpdata, 1, vegdata=vegdata, omschrijving=soorten, lijst=soortenlijst,
              function(x, vegdata, omschrijving, lijst){
                #Welke soorten horen bij de indicator die we onderzoeken
                hablijst <- lijst[lijst$HRL_code==x["HRL_code"] &  (lijst$Omschrijving %in% omschrijving), , drop=F]

                #indien er geen corresponderende soorten zijn voor deze indicator, zet het resultaat op NA
                if(dim(hablijst)[1]==0){
                  rv <- NA
                } else
                  #anders haal de opname van dit plot op uit de vegetatieopnames
                {
                  opname <- vegdata[vegdata$IDPlots==x["NR"], , drop=F] #as.character nodig omdat x een character vector wordt
                  #als er geen opnames zijn, zet het resultaat op 0
                  if(dim(opname)[1]==0){
                    rv <- 0; warning(paste("Plot",x["NR"], "heeft geen vegetatieopnames, criterium op 0 gezet"))
                  } else
                    #anders kies de gewenste functie en bereken het resultaat
                  {
                    #het aantal is de hoeveelheid soorten uit de gekozen indicator waargenoemen zijn
                    if(fun == "Aantal"){
                      rv <- length(unique(opname$NameSc[opname$NameSc %in% hablijst$VolNaam ]))
                    } else
                      #de bedekking sommeert de bedekkingen van de waargenomen soorten van de indicator
                      if (fun=="Bedekking")  #Keuze om de absolute bedekkingsoppervlakte te nemen
                      {
                        #indien geen laag gedefinieerd is, neem de som van alle bedekkingen
                        if(laag == "") {
                          bedekking <- opname$Cover[opname$NameSc %in% hablijst$VolNaam]
                          bedekkingInv <- (100 - bedekking)/100
                          rv <- (1 - prod(bedekkingInv, na.rm=T))*100

                        } else
                          #anders neem enkel de soorten die uit de gedefinieerde laag komen
                        {
                          bedekking <- opname$Cover[(opname$NameSc %in% hablijst$VolNaam) & opname$Vegetatielaag %in% laag]
                          bedekkingInv <- (100 - bedekking)/100
                          rv <- (1 - prod(bedekkingInv, na.rm=T))*100
                        }
                      } else
                        #Berekent gewoon de maximale bedekking
                        if (fun == "Maximum")
                        {
                          tvar <- opname$Cover[opname$NameSc %in% hablijst$VolNaam ]
                          if(length(tvar)==0) tvar <- 0
                          rv <- max(as.numeric(tvar), na.rm=T)
                        }
                    #als geen van voorgaande functies doorgegeven was, zet het resultaat op NA
                    else rv <- NA
                  }
                }
                rv
              })
  rv
}



