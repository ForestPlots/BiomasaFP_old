#' @description Modified allometric equation function allowing inclusion of local height-diameter relationships. NEED TO UPDATE OTHER ALLOMETRIC EQUATIONS
#' @xdataset Object returned by mergefp
#' @dbh Diameter to use when estimating biomass
#' @height.data Object returned by param.merge. If NULL (default), then regional height-diameter equations are used.
#' @param.type Local height diameter to use. One of "Best" (defualt), "BioRF","ClusterF" ... NEED TO DECIDE WHICH OF THESE TO RETURN

AGBChv05MH2<-function (xdataset, dbh = "D4",height.data=NULL,param.type="Best") 
{
    cdf <- xdataset
    cdf <- CleaningCensusInfo(xdataset)
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
    cdf$HtF <- ifelse(cdf$D1 > 0 | cdf$Alive == 1, cdf$a_par * 
        (1 - exp(-cdf$b_par * (cdf[, dbh]/10)^cdf$c_par)), NA)
    dbh_d <- paste(dbh, "_D", sep = "")
    cdf$Htd <- ifelse(cdf$CensusStemDied == cdf$Census.No, cdf$a_par * 
        (1 - exp(-cdf$b_par * (cdf[, dbh_d]/10)^cdf$c_par)), 
        NA)
    cdf$AGBind <- ifelse(cdf$D1 > 0, (0.0509 * cdf$WD * ((cdf[, 
        dbh]/10)^2) * cdf$HtF)/1000, NA)
    cdf$AGBDead <- ifelse(cdf$CensusStemDied == cdf$Census.No, 
        (0.0509 * cdf$WD * ((cdf[, dbh_d]/10)^2) * cdf$Htd)/1000, 
        NA)
    cdf
}