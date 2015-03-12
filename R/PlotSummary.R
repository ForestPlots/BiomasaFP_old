#' @title Function for summarizing Plot View metadata
#' 
#' @description Function for summarizing the information of the Plot Views used (PlotViewID). Output headers are: Country, PlotCode, PlotViewID, LatitudeDecimal, LongitudeDecimal,
#' First.Census.Date (Decimal date when the Plot was established), No.Censuses (Number of censuses or field campaigns).
#' 
#' @param xdataset a dataset  that has been merged using the function mergefp.
#' @author Gabriela Lopez-Gonzalez
#'
#' @export

SummaryPlotviews <- function (xdataset){
        
        
        #1. Create Unique Dataset of PlotViewID        
        xdu <- xdataset[,c('PlotID','PlotCode','CountryName','PlotViewID','Census.No','Census.Mean.Date','LatitudeDecimal', 'LongitudeDecimal')]
        udfile <- unique(xdu)
        udfile
        # minimum
        minx <- aggregate(Census.Mean.Date ~ PlotViewID, data = udfile, FUN=min)
        colnames(minx) <- c('PlotViewID', 'First.Census.Date')
       
        # maximum
        maxx <- aggregate(cbind(Census.Mean.Date, Census.No) ~ PlotViewID +CountryName + PlotCode +
                                   LatitudeDecimal + LongitudeDecimal, data = xdu, FUN=max)
        colnames(maxx) <- c('PlotViewID', 'CountryName','PlotCode','LatitudeDecimal','LongitudeDecimal','Last.Census.Date','No.Censuses')
        #merge
        PlotsSumm <-merge(minx, maxx, by = 'PlotViewID', x.all = TRUE)
        PlotSumma<- PlotsSumm[,c('CountryName','PlotCode', 'PlotViewID', 'LatitudeDecimal','LongitudeDecimal','First.Census.Date','Last.Census.Date','No.Censuses')]
        PlotSummb <-PlotSumma[order(PlotSumma$CountryName, PlotSumma$PlotCode, PlotSumma$PlotViewID, decreasing=FALSE), ]
        PlotSummb
        
        
}