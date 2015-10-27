#' @title Function to summarise AGB by Plot and Census Dates. 
#' @description This function produces a summary of AGB by PlotViewID and Census No. AGB is estimated using a specified equation.
#' The input dataset should include tree diameter, wood density and the region used for defining the coefficients for estimating tree height.
#' @param xdataset a dataset  with diameter (use mergefp)
#' @param AGBEquation the AGB equation used to produce the summary
#' @param dbh a diameter (in mm) to estimate biomass
#' @author Gabriela Lopez-Gonzalez
#' @references Chave J, Andalo C, Brown S, et al. 2005. Tree allometry and improved estimation of carbon stocks and balance in tropical forests. Oecologia 145 (1):87-99. doi:10.1007/s00442-005-0100-x.
#' 
#' Chave J, Rejou-Mechain M, Burquez A et al. 2014. Improved allometric models to estimate the aboveground biomass of tropical trees. Global Change Biology 20: 3177-3190. doi: 10.1111/gcb.12629
#' 
#' Feldpausch TR, Banin L, Phillips OL, Baker TR, Lewis SL et al. 2011. Height-diameter allometry of tropical forest trees. Biogeosciences 8 (5):1081-1106. doi:10.5194/bg-8-1081-2011.
#' 
#' Chave J, Coomes DA, Jansen S, Lewis SL, Swenson NG, Zanne AE. 2009. Towards a worldwide wood economics spectrum. Ecology Letters 12(4): 351-366. http://dx.doi.org/10.1111/j.1461-0248.2009.01285.x
#' 
#' Zanne AE, Lopez-Gonzalez G, Coomes DA, Ilic J, Jansen S, Lewis SL, Miller RB, Swenson NG, Wiemann MC, Chave J. 2009. Data from: Towards a worldwide wood economics spectrum. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.234

#' @export

SummaryAGB <- function (xdataset, AGBEquation, dbh ="D4"){
        AGBData <- AGBEquation (xdataset, dbh) 
        IndAL <- aggregate (Alive/PlotArea ~ PlotViewID + Census.No,  data = AGBData, FUN=sum )
        AGBAlive <-aggregate (AGBind/PlotArea ~ Country + PlotViewID + PlotCode +PlotArea+ LatitudeDecimal + LongitudeDecimal+Census.No + Census.Mean.Date, data = AGBData, FUN=sum )
        # Find Recruits
        Recruits<- AGBData[AGBData$Recruit==1,]
        AGBRec <- aggregate (AGBind/PlotArea ~ PlotViewID + Census.No,  data = Recruits, FUN=sum )
        colnames(AGBRec) <- c('PlotViewID','Census.No','AGBRecs')
        IndRec <- aggregate (Recruit/PlotArea ~ PlotViewID + Census.No,  data = Recruits, FUN=sum )
     
        #merge recruit information
        Recs<- merge(AGBRec, IndRec, by =  c('PlotViewID','Census.No'), all.x=TRUE)
        
        # Dead stems  get only stems that are dead and have a agb, to count only dead trees ones
        DeadTrees <-AGBData[AGBData$Dead==1 & !is.na(AGBData$D1_D),]
        IndDead <- aggregate (Dead/PlotArea ~ PlotViewID + Census.No,  data = DeadTrees, FUN=sum )
        AGBDe <-aggregate (AGBDead/PlotArea ~  PlotViewID + Census.No, data = AGBData, FUN=sum )
        
        #merge Dead trees
        Deads <- merge(AGBDe, IndDead, by = c('PlotViewID','Census.No'),all.x=TRUE)
     
        # merge all summaries
        mergeAGBAlive <- merge (AGBAlive, IndAL, by = c('PlotViewID','Census.No'),all.x=TRUE)
        mergeB <-  merge(mergeAGBAlive, Recs, by = c('PlotViewID','Census.No'),all.x=TRUE)
        mergeC <- merge (mergeB, Deads, by=  c('PlotViewID','Census.No'),all.x=TRUE) 
        
        SummaryB <- mergeC

        #colnames(SummaryB) <- c(  'PlotViewID', 'Census.No', 'Country', 'PlotCode','PlotArea','LatitudeDecimal','LongitudeDecimal','Census.Mean.Date', 'AGB_ha', 'No.AliveInd_ha')
        
        SummaryB <- SummaryB[order(SummaryB$PlotViewID, SummaryB$Census.No, decreasing=FALSE), ]
        SummaryB
        
        
}
        
        
