#' @title Function to estimate tree biomass for dry forests (Chave et al. 2005).Uses diameter, wood density  and estimated height (based on Feldpausch et al. 2011) to estimate biomass.
#' @description Function to estimate individual tree aboveground biomass using Chave et al. (2005) equation for dry forests. Tree height is estimated from diameter using a Weibull equation with region-specific parameters from Feldpaush et al. (2011). The function adds columns to the dataset that contain the biomass estimates for all living trees. This function needs a dataset with the following columns: PlotViewID, PlotID, TreeID, CensusNo, Diameter (DBH1-DBH4), Wood density (WD) and Allometric RegionID.The function assumes that the diameter that should be used is DBH4, unless another DBH is selected. See ForestPlots.net documentation for more information.
#' 
#' @references Chave J, Andalo C, Brown S, et al. 2005. Tree allometry and improved estimation of carbon stocks and balance in tropical forests. Oecologia 145 (1):87-99. doi:10.1007/s00442-005-0100-x.
#' 
#' Feldpausch TR, Banin L, Phillips OL, Baker TR, Lewis SL et al. 2011. Height-diameter allometry of tropical forest trees. Biogeosciences 8 (5):1081-1106. doi:10.5194/bg-8-1081-2011.
#' 
#' Chave J, Coomes DA, Jansen S, Lewis SL, Swenson NG, Zanne AE. 2009. Towards a worldwide wood economics spectrum. Ecology Letters 12(4): 351-366. http://dx.doi.org/10.1111/j.1461-0248.2009.01285.x
#' 
#' Zanne AE, Lopez-Gonzalez G, Coomes DA et al. 2009. Data from: Towards a worldwide wood economics spectrum. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.234

#' @param xdataset a dataset for estimating individual tree biomass
#' @param dbh a diameter (in mm). 
#' 
#' @export
#' @author Gabriela Lopez-Gonzalez

AGBChv05DH <- function (xdataset, dbh = "D4"){
        cdf <- xdataset
        ## Clean file
        cdf <- CleaningCensusInfo(xdataset) 
        # Get Weibull Parameters
        data(WeibullHeightParameters)
        WHP <- WeibullHeightParameters
        cdf <-merge (cdf, WHP, by = "AllometricRegionID", all.x = TRUE )
        #Estimate height
        cdf$HtF <- ifelse(cdf$D1 > 0 | cdf$Alive == 1, cdf$a_par*(1-exp(-cdf$b_par*(cdf[,dbh]/10)^cdf$c_par)), NA)
        #Add dead and recruits when codes are improved
        dbh_d <- paste(dbh,"_D", sep="") 
        cdf$Htd <- ifelse(cdf$CensusStemDied==cdf$Census.No, cdf$a_par*(1-exp(-cdf$b_par*(cdf[,dbh_d]/10)^cdf$c_par)), NA)
        
        # Calculate AGB by stem Alive type
        cdf$AGBind <- ifelse(cdf$D1>0, 
                             0.112 *(cdf$WD * (cdf[,dbh]/10)^2* cdf$HtF)^0.916/1000, 
        NA)
        #cdf$AGBAl <-  ifelse(cdf$Alive == 1, cdf$AGBind, NA)
        #cdf$AGBRec <- ifelse(cdf$NewRecruit == 1, cdf$AGBind, NA)
        cdf$AGBDead <-ifelse(cdf$CensusStemDied==cdf$Census.No,
                             0.112 *(cdf$WD * (cdf[,dbh_d]/10)^2* cdf$HtFd)^0.916/1000
                             , NA)
        
        cdf  
        
}