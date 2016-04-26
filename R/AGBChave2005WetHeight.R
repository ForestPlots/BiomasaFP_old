#' @title Function to estimate tree biomass for wet forests (Chave et al. 2005). Uses diameter, wood density and estimated height (based on Feldpausch et al. 2011) to estimate biomass.
#' @description Function to estimate tree biomass using Chave et al. (2005) equation for wet forests. 
#' Height is estimated using Feldpausch (2011) regional parameters with Weibull equation.The function adds columns  to the dataset with the biomass information for all alive trees.
#' This function needs a dataset with the following information: PlotViewID, PlotID, TreeID, CensusNo, Diameter (DBH1-DBH4), Wood density (WD) and Allometric RegionID.The function assumes that the diameter used is DBH4, unless other DBH is selected.
#' See ForestPlots.net documentaion for more information.
#' @references Chave J, Andalo C, Brown, et al. 2005. Tree allometry and improved estimation of carbon stocks and balance in tropical forests. Oecologia 145 (1):87-99. doi:10.1007/s00442-005-0100-x.
#' 
#' Feldpausch TR, Banin L, Phillips OL, Baker TR, Lewis SL et al. 2011. Height-diameter allometry of tropical forest trees. Biogeosciences 8 (5):1081-1106. doi:10.5194/bg-8-1081-2011.
#' @param xdataset a dataset for estimating biomass
#' @param dbh a diameter (in mm). 
#' @param height.data Object returned by param.merge. If NULL (default), then regional height-diameter equations are used.
#' @param param.type Local height diameter to use. One of "Best" (defualt), "BioRF","ClusterF" ... NEED TO DECIDE WHICH OF THESE TO RETURN
#' 
#' @export
#' @author Gabriela Lopez-Gonzalez

AGBChv05WH <- function (xdataset, dbh = "D4",height.data=NULL,param.type="Best"){
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
        cdf$HtF <- ifelse(cdf$D1 > 0 | cdf$Alive == 1, height.mod(cdf[,dbh],cdf$a_par,cdf$b_par,cdf$c_par), NA)
        #Add dead and recruits when codes are improved
        dbh_d <- paste(dbh,"_D", sep="") 
        cdf$Htd <- ifelse(cdf$CensusStemDied==cdf$Census.No, height.mod(cdf[,dbh_d],cdf$a_par,cdf$b_par,cdf$c_par), NA)
        
        # Calculate AGB by stem Alive type
        cdf$AGBind <- ifelse(cdf$D1>0 & cdf$Alive == 1 & cdf$CensusStemDied>cdf$Census.No, 
                             0.0776 *(cdf$WD * (cdf[,dbh]/10)^2* cdf$HtF)^0.940/1000, 
                             NA)
        cdf$AGBAl <-  ifelse(cdf$Alive == 1, cdf$AGBind, NA)
        #cdf$AGBRec <- ifelse(cdf$NewRecruit == 1, cdf$AGBind, NA)
        cdf$AGBDead <-ifelse(cdf$CensusStemDied==cdf$Census.No,
                             0.0776 *(cdf$WD * (cdf[,dbh_d]/10)^2* cdf$HtFd)^0.940/1000
                             , NA)
        
        cdf  
        
}