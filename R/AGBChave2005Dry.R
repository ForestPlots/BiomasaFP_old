#' @title Estimates biomass of individual tree records.
#' @description Estimates  individual biomass using Chave equation (2005) without height for Dry forests. The function adds columns with the biomass information for all alive trees.
#' This function needs a dataset with the following information: PlotViewID, PlotID, TreeID, CensusNo, Diameter (DBH1-DBH4), Wood density (WD).The function assumes that the diameter used is DBH4, unless other DBH is selected.
#' See ForestPlots.net documentation for more information.
#' @param xdataset a dataset for estimating biomass
#' @param dbh a diameter (in mm). 
#' @author Gabriela Lopez-Gonzalez
#' @references Chave C, Andalo S, Brown, et al. 2005. Tree allometry and improved estimation of carbon stocks and balance in tropical forests. Oecologia 145 (1):87-99. doi:10.1007/s00442-005-0100-x.
#' 
#' Chave J, Coomes DA, Jansen S, Lewis SL, Swenson NG, Zanne AE. 2009. Towards a worldwide wood economics spectrum. Ecology Letters 12(4): 351-366. http://dx.doi.org/10.1111/j.1461-0248.2009.01285.x
#' 
#' Zanne AE, Lopez-Gonzalez G, Coomes DA et al. 2009. Data from: Towards a worldwide wood economics spectrum. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.234
#' 
#' @export

AGBChv05D <- function (xdataset, dbh = "DBH4"){
        cdf <- xdataset
        ## Clean file 
        cdf <- CleaningCensusInfo(xdataset) 
        #dbh_d <- paste(dbh,"_D", sep="") 
        
        cdf$AGBind <- ifelse(cdf$DBH1>0,
                             cdf$WD * exp (-0.667 + (1.784*log(cdf[,dbh]/10))+ (0.207*(log(cdf[,dbh]/10))^2)- (0.0281*(log(cdf[,dbh]/10))^3))/1000, NA)
        cdf$AGBAl <-  ifelse(cdf$Alive == 1, cdf$AGBind, NA)
        #The code below was removed as it is difficult to find recruits with the current download format
        #cdf$AGBRec <- ifelse(cdf$NewRecruit == 1, cdf$AGBind, NA)
        #The code below was removed and would be implemented later
        #cdf$AGBDead<-ifelse(cdf$CensusStemDied==cdf$CensusNo, 
        # cdf$WD * exp (-1.499 + (2.148*log(cdf[,dbh_d]/10))+ (0.207*(log(cdf[,dbh_d]/10))^2)- (0.0281*(log(cdf[,dbh_d]/10))^3))/1000
        # , NA)
        
        cdf     
}
