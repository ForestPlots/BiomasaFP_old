#' @title local.heights
#' @description Wrapper function for estimating heights using local allometric equations.
#' @param xdataset Object returned by \code{mergefp}.
#' @param dbh Character giving the name of the column contining the diameter measurements. Defaults to "D4".
#' @param weib.only Logical. Should the search for the best model be constrained to Weibull models. If TRUE, log-log models are only selected when neither Weibull models converge. Default is FALSE.
#' @return Dataframe with best parameters for height-diameter models for each row in xdataset.
#' @author Martin Sullivan

#' @export

local.heights<-function(xdataset,dbh="D4"){
data<-xdataset
data$FT<-paste(data$ForestMoistureID,data$ForestEdaphicHeightID,data$ForestElevationHeightID,sep="")
data$ID.clust<-paste(data$ClusterID,data$FT,sep="_")
data$ID.clust[is.na(data$ClusterID)]<-NA
data$ID.bio<-paste(data$BiogeographicalRegionID,data$FT,sep="_")
data$ID.continent<-paste(data$Continent,data$FT,sep="_")
#Filter to select height data
data$Alive <- as.numeric (ifelse(data$F2==1, 1 ,0))
TreesHt <- data[data$Height>0 & data$Alive==1 & data$D1>90 & data$D1<5000    & !is.na(data$Height) & data$Height<90 
                   & data$Monocot==0 &  !is.na(data$F5), ]
TreesHt$Method<- ifelse(TreesHt$F5==1,1,
                        ifelse(TreesHt$F5==6,6,
                               ifelse( TreesHt$F5==2 | TreesHt$F5==3, 3,4
                               )
                        )
	)

# Exclude F3-3
TreesHt <- TreesHt[ grepl('3',TreesHt$F3)==FALSE,  ]
# Exclude F4 not like '60' or '0' NEED TO CHANGE
TreesHt <- TreesHt[ grepl('0',TreesHt$F4)==TRUE|grepl('06',TreesHt$F4)==TRUE ,  ]
# Exclude f1 flag1= b, c, d, j, k
htdata <- TreesHt[ grepl('b',TreesHt$F1)==FALSE & grepl('c',TreesHt$F1)==FALSE &
                            grepl('d',TreesHt$F1)==FALSE & grepl('j',TreesHt$F1)==FALSE&
                            grepl('k',TreesHt$F1)==FALSE
                    ,  ]

#Exclude treeswith method 1
htdata <- htdata[!(htdata$Method==1),]

#Make column with selected diameter
htdata$Diameter<-htdata[,dbh]

#Fit h-d models
plot.fit<-hd.fit(htdata,"Plot")
clust.fit<-hd.fit(htdata,"Cluster")
bio.fit<-hd.fit(htdata,"BioR",weib.only=TRUE)
cont.fit<-hd.fit(htdata,"Continent",weib.only=TRUE)

#Match in best parameters
data$a_Plot<-plot.fit[match(data$PlotID,plot.fit$ID),"Best_a"]
data$b_Plot<-plot.fit[match(data$PlotID,plot.fit$ID),"Best_b"]
data$c_Plot<-plot.fit[match(data$PlotID,plot.fit$ID),"Best_c"]
data$Plot.BestMod<-plot.fit[match(data$PlotID,plot.fit$ID),"BestMod"]

data$a_ClustF<-clust.fit[match(data$ID.clust,clust.fit$ID),"Best_a"]
data$b_ClustF<-clust.fit[match(data$ID.clust,clust.fit$ID),"Best_b"]
data$c_ClustF<-clust.fit[match(data$ID.clust,clust.fit$ID),"Best_c"]
data$ClustF.BestMod<-clust.fit[match(data$ID.clust,clust.fit$ID),"BestMod"]


data$a_BioRF<-bio.fit[match(data$ID.bio,bio.fit$ID),"Best_a"]
data$b_BioRF<-bio.fit[match(data$ID.bio,bio.fit$ID),"Best_b"]
data$c_BioRF<-bio.fit[match(data$ID.bio,bio.fit$ID),"Best_c"]
data$BioRF.BestMod<-bio.fit[match(data$ID.bio,bio.fit$ID),"BestMod"]

data$a_ContF<-cont.fit[match(data$ID.cont,cont.fit$ID),"Best_a"]
data$b_ContF<-cont.fit[match(data$ID.cont,cont.fit$ID),"Best_b"]
data$c_ContF<-cont.fit[match(data$ID.cont,cont.fit$ID),"Best_c"]
data$ContF.BestMod<-cont.fit[match(data$ID.cont,cont.fit$ID),"BestMod"]

#Find finest geographic scale matched. Note using a parameter to determine parameter presence, as c parameters are NA for log-log models
data$a_Best<-ifelse(!is.na(data$a_Plot),data$a_Plot,
	ifelse(!is.na(data$a_ClustF),data$a_ClustF,
		ifelse(!is.na(data$a_BioRF),data$a_BioRF,data$a_ContF)))
data$b_Best<-ifelse(!is.na(data$a_Plot),data$b_Plot,
	ifelse(!is.na(data$a_ClustF),data$b_ClustF,
		ifelse(!is.na(data$a_BioRF),data$b_BioRF,data$b_ContF)))
data$c_Best<-ifelse(!is.na(data$a_Plot),data$c_Plot,
	ifelse(!is.na(data$a_ClustF),data$c_ClustF,
		ifelse(!is.na(data$a_BioRF),data$c_BioRF,data$c_ContF)))
return(data)
}

