#' @title Function to estimate tree biomass for moist forests (Chave et al. 2005).Uses diameter, wood density and estimated height (based on Feldpausch et al. 2011) to estimate biomass
#' @description Function to estimate individual biomass using the Chave equation (2005) with height for Moist forests. Tree height is estimated using Feldpausch et al. (2011) regional parameters with Weibull equation. The function adds columns with the biomass information for all alive trees. This function needs a dataset with the following information: PlotViewID, PlotID, TreeID, CensusNo, Diameter (DBH1-DBH4), Wood density (WD), Allometric RegionID.The function assumes that the diameter used is DBH4, unless other DBH is selected. See ForestPlots.net documentation for more information.
#' @param xdataset a dataset for estimating biomass
#' @param dbh a diameter (in mm).
#' @param height.data Object returned by param.merge. If NULL (default), then regional height-diameter equations are used.
#' @param param.type local height diameter to use. One of Best (defualt), BioRF,Cluster ... NEED TO DECIDE WHICH OF THESE TO RETURN
#' @references Chave J, Andalo C, Brown S, et al. 2005. Tree allometry and improved estimation of carbon stocks and balance in tropical forests. Oecologia 145 (1):87-99. doi:10.1007/s00442-005-0100-x.
#' 
#' Feldpausch TR, Banin L, Phillips OL, Baker TR, Lewis SL et al. 2011. Height-diameter allometry of tropical forest trees. Biogeosciences 8 (5):1081-1106. doi:10.5194/bg-8-1081-2011.
#' 
#' Chave J, Coomes DA, Jansen S, Lewis SL, Swenson NG, Zanne AE. 2009. Towards a worldwide wood economics spectrum. Ecology Letters 12(4): 351-366. http://dx.doi.org/10.1111/j.1461-0248.2009.01285.x
#' 
#' Zanne AE, Lopez-Gonzalez G, Coomes DA et al. 2009. Data from: Towards a worldwide wood economics spectrum. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.234


#' @export

AGBChv05MH <- function (xdataset, dbh = "D4",height.data=NULL,param.type="Best"){
        cdf <- xdataset
        ## Clean file 
        cdf <- CleaningCensusInfo(xdataset) 
        # Get Weibull Parameters
	if(is.null(height.data)){	
    		data(WeibullHeightParameters)
    		WHP <- WeibullHeightParameters
   		cdf <- merge(cdf, WHP, by = "AllometricRegionID", all.x = TRUE)
   	 }else{
		p1<-paste("a",param.type,sep="_")
		p2<-paste("b",param.type,sep="_")
		p3<-paste("c",param.type,sep="_")
		height.data<-height.data[,c("PlotViewID",p1,p2,p3)]
		height.data<-unique(height.data)
		names(height.data)<-c("PlotViewID","a_par","b_par","c_par")
		cdf<-merge(cdf,height.data,by="PlotViewID",all.x=TRUE)
  	  }	
        #Estimate height
        cdf$HtF <- ifelse(cdf$D1 > 0 | cdf$Alive == 1, cdf$a_par*(1-exp(-cdf$b_par*(cdf[,dbh]/10)^cdf$c_par)), NA)
        #Add dead and recruits when codes are improved
         dbh_d <- paste(dbh,"_D", sep="") 
         cdf$Htd <- ifelse(cdf$CensusStemDied==cdf$Census.No, cdf$a_par*(1-exp(-cdf$b_par*(cdf[,dbh_d]/10)^cdf$c_par)), NA)
       
        # Calculate AGB by stem Alive type
        cdf$AGBind <- ifelse(cdf$D1>0 & cdf$Alive == 1 & (cdf$CensusStemDied>cdf$Census.No | is.na(cdf$IsSnapped)), (0.0509*cdf$WD * ((cdf[,dbh]/10)^2)* cdf$HtF)/1000, NA)
        #cdf$AGBAl <-  ifelse(cdf$Alive == 1, cdf$AGBind, NA)
        #cdf$AGBRec <- ifelse(cdf$NewRecruit == 1, cdf$AGBind, NA)# maybe no need to estimate this by 
        cdf$AGBDead <-ifelse(cdf$CensusStemDied==cdf$Census.No,(0.0509*cdf$WD * ((cdf[,dbh_d]/10)^2)* cdf$Htd)/1000, NA)
                            
        cdf  
   
}

