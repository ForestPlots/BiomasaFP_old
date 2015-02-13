
#' @title Function for reading and merging data downloaded from ForestPlots.net
#' @description Function for reading and merging the 3 datasets needed for estimating biomass: Census Data (a), 
#' Metadata (b), wood density from each individual tree (c). The tree datasets can be donwloaded from ForestPlots.net.
#' This function can use data frames and as well as file paths as input objects.
#' @param a an individual's data file from "Advanced Search"
#' @param b a metadata file dowloaded from the "Query Library"
#' @param c a wood density file, with the wood density for each individual tree by PlotViewID downloaded from the "Query Library"
#' @author Gabriela Lopez-Gonzalez, Martin Sullivan
#' @references Lopez-Gonzalez G, Lewis SL, Burkitt M. and Phillips OL. 2011. ForestPlots.net: a web application and research tool to manage and analyse tropical forest plot data. Journal of Vegetation Science 22: 610–613. doi: 10.1111/j.1654-1103.2011.01312.x
#' 
#' Chave J, Coomes DA, Jansen S, Lewis SL, Swenson NG, Zanne AE. 2009. Towards a worldwide wood economics spectrum. Ecology Letters 12(4): 351-366. http://dx.doi.org/10.1111/j.1461-0248.2009.01285.x
#' 
#' Zanne AE, Lopez-Gonzalez G, Coomes DA et al. 2009. Data from: Towards a worldwide wood economics spectrum. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.234
#' 
#'  
#' @export
#' 
#' added line to test, Tim needs to add more text

mergefp <- function (a,b,d) {
 
        #1 read datasets
        
        if(is.data.frame(a)){
                CD<-a
        }else{
                CD <- read.csv(a, header = TRUE, stringsAsFactors = FALSE)
        }
        if(is.data.frame(b)){
                MT<-b
        }else{
                MT <- read.csv(b, header = TRUE)
        }
        if(is.data.frame(d)){
                W<-d
        }else{
                W <- read.csv(d, header = TRUE)
        }
    
        
        #2 clean census data file
        
        #find lines that are in wrong format: blank or with text
        CD$PlotID<- as.numeric(CD$PlotID)
        RNAS<-CD[!is.na(CD$PlotID),]
       
        #nrow(RemoveNA)
        
        #Set as numeric values that need to be numeric (Tree Census information)
        RNAS$Census.Mean.Date <- as.numeric(RNAS$Census.Mean.Date)
        RNAS$Census.No <- as.numeric(RNAS$Census.No)
        RNAS$PlotViewID <- as.numeric(RNAS$PlotViewID)
        RNAS$TreeID <- as.numeric(RNAS$TreeID)
        RNAS$FamilyAPGID <- as.numeric(RNAS$FamilyAPGID)
        RNAS$GenusID <- as.numeric(RNAS$GenusID)
        RNAS$SpeciesID <- as.numeric(RNAS$SpeciesID)
        RNAS$DBH1<- as.numeric(gsub("=","",RNAS$DBH1))
        RNAS$DBH2<- as.numeric(gsub("=","",RNAS$DBH2))
        RNAS$DBH3<- as.numeric(gsub("=","",RNAS$DBH3))
        RNAS$DBH4<- as.numeric(gsub("=","",RNAS$DBH4))
        RNAS$POM<- as.numeric(gsub("=","",RNAS$POM))
        RNAS$F1<- (gsub("=","",RNAS$F1))
        RNAS$F2<- (gsub("=","",RNAS$F2))
        RNAS$F3<- (gsub("=","",RNAS$F3))
        RNAS$F4<- (gsub("=","",RNAS$F4))
        
        #Remove Duplicated treeids due to vouchers; Select columns that will be used in the package
        RNASu <- RNAS[,c('PlotID','Plot.Code','Country','Census.Mean.Date',
                         'Census.No','PlotViewID','TreeID','FamilyAPGID',
                         'Family','GenusID','Genus','SpeciesID','Species',
                         'DBH1','DBH2','DBH3','DBH4','POM','F1','F2',
                         'F3','F4')]
        #extract unique elements, Unique dataset for census data
        Udfile <-unique(RNASu)
        #Set as numeric values that need to be numeric (WD values)
        W$PlotID <- as.numeric(W$PlotID)
        W$PlotViewID <- as.numeric (W$PlotViewID)
        W$TreeID<- as.numeric (W$TreeID)
        W$WD <- as.numeric(gsub("=","", W$WD))
        #merge information
        dataset <- merge(Udfile, MT, by = c('PlotViewID'), x.all=TRUE)
        datasetb <- merge(dataset, W, by = c('PlotViewID','TreeID','PlotID'), x.all=TRUE)
        #datasetb
        datasetb        
        }
#Function to tidy census information - This function is not visible to users. After reading data downloaded from the Advanced Search in ForestPlots.net, merged with WD and PlotMetadata
#
        CleaningCensusInfo <- function (dfmerged) {
        
        dfm <- dfmerged        
        
        #DeadorSnapped: First appearance of a tree that is dead or snapped
        # First gets all trees records that indicate a tree has died or is snapped
        #The following code can be removed once Alive and Snapped are added to the download 
        dfm$Alive <- as.numeric (ifelse(dfm$F2==1, 1 ,0))
        dfm$Snapped <- as.numeric(ifelse( grepl("k",dfm$F1)== TRUE, 1, 0))
        
        #Change Census.No to CensusNo once new format is implemented
        DeadorSnapped <-data.frame (dfm[(dfm$Alive == 0 | dfm$Snapped ==1) , c('PlotViewID', 'Census.No', 'TreeID',  'DBH1', 'Alive', 'Snapped')])
        # First time a dead or census tree  appears
        
        DeadFirstCensus <- data.frame( aggregate (Census.No ~ PlotViewID + TreeID, data = DeadorSnapped,  min ))
        colnames(DeadFirstCensus) <- c('PlotViewID', 'TreeID', 'CensusNoDead')
        #write.csv (DeadFirstCensus, file = 'dead.csv')
        
        # Merged with PlotData for selecting censuses before tree died or snapped 
        
        PlotDataDeadA <- merge(dfm, DeadFirstCensus, by= c('TreeID','PlotViewID'), all = TRUE)
        #Merged with Plot Data and select previous Census of when trees died/snapped
        #Change Census.No to CensusNo once new format is implemented
        PlotDataDeadB <- PlotDataDeadA[PlotDataDeadA$Census.No == PlotDataDeadA$CensusNoDead-1 & !is.na(PlotDataDeadA$CensusNoDead), c('PlotViewID','CensusNoDead', 'TreeID','DBH1', 'DBH2','DBH3','DBH4')]
        colnames(PlotDataDeadB)<- c('PlotViewID', 'CensusNoDead', 'TreeID', 'DBH1_D','DBH2_D','DBH3_D','DBH4_D')
        
        #Clean Dataset
        #Change Census.No to CensusNo once new format is implemented
        
        CleanA <- merge (dfm, PlotDataDeadB, by= c('TreeID', 'PlotViewID'), all= TRUE)
        #      CleanA$Dead <- ifelse(CleanA$CensusNoDead==CleanA$Census.No,1, NA) Censuswhen tree/stem contributes to dead biomass (either as dead or as snapped)
        CleanA$CensusStemDied <- ifelse(CleanA$CensusNoDead==CleanA$Census.No,CleanA$Census.No, NA)
        CleanA$Dead <- ifelse(CleanA$F1==0,1, 0) 
        CleanA$DBH1_D <- ifelse(CleanA$CensusNoDead==CleanA$Census.No,CleanA$DBH1_D, NA)
        CleanA$DBH2_D <- ifelse(CleanA$CensusNoDead==CleanA$Census.No,CleanA$DBH2_D, NA)
        CleanA$DBH3_D <- ifelse(CleanA$CensusNoDead==CleanA$Census.No,CleanA$DBH3_D, NA)
        CleanA$DBH4_D <- ifelse(CleanA$CensusNoDead==CleanA$Census.No,CleanA$DBH4_D, NA)
        Clean <- CleanA[ , c('TreeID','PlotViewID','ContinentName','ContinentName' ,'CountryName' ,'AllometricRegionID' ,'PlotID','PlotCode','Census.No','Census.Mean.Date', 'FamilyAPGID','Family'
                             ,'GenusID','Genus','SpeciesID','Species','DBH1','DBH2','DBH3','DBH4','POM','F1','F2','F3','F4',
                             'Alive','Snapped' ,'WD','CensusStemDied','Dead','DBH1_D','DBH2_D','DBH3_D','DBH4_D','Altitude','LatitudeDecimal', 'LongitudeDecimal','PlotArea')]
        # version to implement once Alive, POMChange,New Recruit, WD is available from Advanced Search
        #Clean <- CleanA[ , c('TreeID','PlotViewID','ContinentName','CountryID','CountryName','AllometricRegionID','PlotID','PlotCode','PlotViewPlotCensusID','CensusNo','MeanDecimalDate', 'Subplot_Standard','x_standard','y_standard','SubPlotT1','SubPlotT2','x','y','FamilyAPGID','FamilyAPGName','GenusID','GenusName','SpeciesID','FullSpeciesName',        'TagNumber','DBH1','DBH2','DBH3','DBH4','POM','Flag1','Flag2','Flag3','Flag4','CI','LI','Alive','NewRecruit','POMChange','AliveNormal',        'MultipleStem',	'Snapped','WD','CensusStemDied','Dead','DBH1_D','DBH2_D','DBH3_D','DBH4_D','Altitude','LatitudeDecimal', 'LongitudeDecimal','PlotArea')]
        
        Clean
}






        
        



