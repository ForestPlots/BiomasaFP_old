#' @title Function to estimate AGB in the final census for each plot.
#' @description This function produces a summary of last AGB by PlotViewID. AGB is estimated by providing an AGB equation from the package. 
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

LastAGB <- function (xdataset, AGBEquation, dbh ="DBH4"){
        # function format change equation for equation, and populate information AGBData <- AGBEquation (mergedCensus, dbh4) 
        AGBData <- AGBEquation (xdataset, dbh)
        
        AGBAlive <-aggregate (cbind(AGBind,  Alive, AGBind/PlotArea) ~ PlotCode + PlotViewID + PlotArea+ Census.No + Census.Mean.Date +LatitudeDecimal + LongitudeDecimal, data = AGBData, FUN=sum )
        #toimplement once differnce between survivors and recruits is implemented
        #AGBAlive <-aggregate (cbind(AGBAl,  Alive, AGBAl/PlotArea) ~ PlotCode + PlotViewID + PlotArea+ Census.No + Census.Mean.Date +LatitudeDecimal + LongitudeDecimal, data = AGBData, FUN=sum )
        SummaryB<-AGBAlive
        SummaryB <- SummaryB[order(SummaryB$PlotViewID, SummaryB$Census.No, decreasing=FALSE), ]
        
        #SUmmary PlotViewID first and last census
        
        fc<-SummaryB[SummaryB$Census.No==1,]
        maxCensusNo <-aggregate(Census.No ~ PlotViewID, data = SummaryB, max)
        lc<- merge(SummaryB, maxCensusNo, by = c('PlotViewID', 'Census.No'))
        
        #head (lc)
        
        lcs <- lc[,c('Census.No','Census.Mean.Date','PlotViewID','V3')]
        colnames(lcs) <- c('LastCensus.No','Last.Census.Mean.Date', 'PlotViewID','AGB')
        #head(lcs)
        
        fandl<-merge(fc, lcs, by= 'PlotViewID')
        #head (fandl)
        #head(wtm)
        AGBLastCensus <-fandl[,c('PlotViewID','PlotCode', 'PlotArea', 'LatitudeDecimal', 'LongitudeDecimal', 'Last.Census.Mean.Date', 'LastCensus.No', 'AGB')]
        AGBLastCensus
        
        
        
        
        
}