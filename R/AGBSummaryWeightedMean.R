#' @title Function to estimate mean AGB per plot, weighted by census interval length
#' @description This function produces a summary of mean weighted AGB by PlotViewID. AGB is estimated by providing an AGB equation from the package. Weighting is by Census Dates.
#' @param xdataset a dataset  with diameter, wood density and census dates for each individual tree (use mergefp)
#' @param AGBEquation the AGB equation used to produce the summary
#' @param dbh a diameter (in mm) to estimate biomass
#' @author Gabriela Lopez-Gonzalez
#' @references 
#' Chave J, Coomes DA, Jansen S, Lewis SL, Swenson NG, Zanne AE. 2009. Towards a worldwide wood economics spectrum. Ecology Letters 12(4): 351-366. http://dx.doi.org/10.1111/j.1461-0248.2009.01285.x
#' 
#' Chave C, Andalo S, Brown, et al. 2005. Tree allometry and improved estimation of carbon stocks and balance in tropical forests. Oecologia 145 (1):87-99. doi:10.1007/s00442-005-0100-x.
#' 
#' Chave J, Rejou-Mechain M, Burquez A et al. 2014. Improved allometric models to estimate the aboveground biomass of tropical trees. Global Change Biology 20: 3177-3190. doi: 10.1111/gcb.12629
#' 
#' Feldpausch TR, Banin L, Phillips OL, Baker TR, Lewis SL et al. 2011. Height-diameter allometry of tropical forest trees. Biogeosciences 8 (5):1081-1106. doi:10.5194/bg-8-1081-2011.
#' 
#' Zanne AE, Lopez-Gonzalez G, Coomes DA, Ilic J, Jansen S, Lewis SL, Miller RB, Swenson NG, Wiemann MC, Chave J. 2009. Data from: Towards a worldwide wood economics spectrum. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.234
#' 
#' 
#' @export

MeanWtAGB <- function (xdataset, AGBEquation, dbh ="DBH4"){
        AGBData <- AGBEquation (xdataset, dbh) 
        AGBAlive <-aggregate (cbind(AGBAl,  Alive, AGBAl/PlotArea) ~ PlotCode + PlotViewID + PlotArea+ Census.No + Census.Mean.Date +LatitudeDecimal + LongitudeDecimal, data = AGBData, FUN=sum )
        SummaryB<-AGBAlive
        SummaryB <- SummaryB[order(SummaryB$PlotViewID, SummaryB$Census.No, decreasing=FALSE), ]
        #SummaryB
        # Code for weighted mean
        results <- lapply (split (SummaryB, SummaryB$PlotViewID), function (X) weighted.mean (X$V3, X$Census.Mean.Date))

        
        resultsvector2 <-  stack(results)
        colnames (resultsvector2) <- c('AGBWeightedMean', 'PlotViewID')
        #SUmmary PlotViewID first and last census
       
        fc<-SummaryB[SummaryB$Census.No==1,]
        maxCensusNo <-aggregate(Census.No ~ PlotViewID, data = SummaryB, max)
        lc<- merge(SummaryB, maxCensusNo, by = c('PlotViewID', 'Census.No'))
        lcs <- lc[,c('Census.No','Census.Mean.Date','PlotViewID')]
        colnames(lcs) <- c('LastCensus.No','Last.Census.Mean.Date', 'PlotViewID')
        
        fandl<-merge(fc, lcs, by= 'PlotViewID')
        wtm <-merge(fandl, resultsvector2, by ='PlotViewID')
        wtma <-wtm[,c('PlotViewID','PlotCode', 'PlotArea', 'LatitudeDecimal', 'LongitudeDecimal','Census.Mean.Date', 'Last.Census.Mean.Date', 'LastCensus.No', 'AGBWeightedMean')]
        wtma
       
        
        
        
        
}