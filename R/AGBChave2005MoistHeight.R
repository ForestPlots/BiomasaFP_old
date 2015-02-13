#' @title Function to estimate individual biomass for moist forests (Chave, 2005).
#' @description Function to estimate individual biomass using Chave equation (2005) with height for Moist forests. Height is estimated using Feldpaush (2011) regional parameters with Weibull equation. The function adds columns with the biomass information for all alive trees. This function needs a dataset with the following information: PlotViewID, PlotID, TreeID, CensusNo, Diameter (DBH1-DBH4), Wood density (WD), Allometric RegionID.The function assumes that the diameter used is DBH4, unless other DBH is selected. See ForestPlots.net documentation for more information.
#' @param xdataset a dataset for estimating biomass
#' @param dbh a diameter (in mm).
#' @references Chave C, Andalo S, Brown, et al. 2005. Tree allometry and improved estimation of carbon stocks and balance in tropical forests. Oecologia 145 (1):87-99. doi:10.1007/s00442-005-0100-x.
#' 
#' Feldpausch TR, Banin L, Phillips OL, Baker TR, Lewis SL et al. 2011. Height-diameter allometry of tropical forest trees. Biogeosciences 8 (5):1081-1106. doi:10.5194/bg-8-1081-2011.
#' 
#' Chave J, Coomes DA, Jansen S, Lewis SL, Swenson NG, Zanne AE. 2009. Towards a worldwide wood economics spectrum. Ecology Letters 12(4): 351-366. http://dx.doi.org/10.1111/j.1461-0248.2009.01285.x
#' 
#' Zanne AE, Lopez-Gonzalez G, Coomes DA et al. 2009. Data from: Towards a worldwide wood economics spectrum. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.234


#' @export

AGBChv05MH <- function (xdataset, dbh = "DBH4"){
        cdf <- xdataset
        ## Clean file 
        cdf <- CleaningCensusInfo(xdataset) 
        # Get Weibull Parameters
        data(WeibullHeightParameters)
        WHP <- WeibullHeightParameters
        cdf <-merge (cdf, WHP, by = "AllometricRegionID", all.x = TRUE )
        #Estimate height
        cdf$Ht <- ifelse(cdf$DBH1 > 0 | cdf$Alive == 1, cdf$a_par*(1-exp(-cdf$b_par*(cdf[,dbh]/10)^cdf$c_par)), NA)
        #Add dead and recruits when codes are improved
        #dbh_d <- paste(dbh,"_D", sep="") 
        #cdf$Htd <- ifelse(cdf$CensusStemDied==cdf$CensusNo, cdf$a_par*(1-exp(-cdf$b_par*(cdf[,dbh_d]/10)^cdf$c_par)), NA)
       
        # Calculate AGB by stem Alive type
        cdf$AGBind <- ifelse(cdf$DBH1>0, (0.0509*cdf$WD * ((cdf[,dbh]/10)^2)* cdf$Ht)/1000, NA)
        cdf$AGBAl <-  ifelse(cdf$Alive == 1, cdf$AGBind, NA)
        #cdf$AGBRec <- ifelse(cdf$NewRecruit == 1, cdf$AGBind, NA)
        #cdf$AGBDead <-ifelse(cdf$CensusStemDied==cdf$CensusNo,(0.0509*cdf$WD * ((cdf[,dbh_d]/10)^2)* cdf$Htd)/1000, NA)
                            
        cdf  
   
}

