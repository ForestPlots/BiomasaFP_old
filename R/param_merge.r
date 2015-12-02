#' @title Merge fitted height-diameter parameters with tree by tree data
#' @description Function to merge fitted parameters of Weibull height-diameter equations to tree data returned by \code{mergefp}
#' @param data Object returned by mergefp
#' @param wparm Object returned by \code{fit.weib}
#' @author Martin Sullivan, Gabriela Lopez-Gonzalez

#' @export

param.merge<-function(data,wparm){

#CHANGE COLUMN NAMES FOR BACKCOMPATABILITY
data$EdaphicHeightCode<-data$ForestEdaphicHeightID
data$ElevationHeightCode<-data$ForestElevationHeightID

data$MatchLevel<-ifelse(data$PlotID%in%wparm$PlotID,"Plot",
	ifelse(paste(data$ClusterID,data$ForestMoistureID,data$EdaphicHeightCode,data$ElevationHeightCode)%in%paste(wparm$ClusterID,wparm$ForestMoistureID,wparm$EdaphicHeightCode,wparm$ElevationHeightCode),"Cluster",
		ifelse(paste(data$BiogeographicalRegionID,data$ForestMoistureID,data$EdaphicHeightCode,data$ElevationHeightCode)%in%paste(wparm$BiogeographicalRegionID,wparm$ForestMoistureID,wparm$EdaphicHeightCode,wparm$ElevationHeightCode),"Biogeog",
			"Cont")))


d1<-data[data$MatchLevel=="Plot",]
dw1<-merge(data,wparm,by=c('PlotID','ForestMoistureID','EdaphicHeightCode', 'ElevationHeightCode'))

d2<-data[data$MatchLevel=="Cluster",]
wparm2<-wparm[!duplicated(paste(wparm$ClusterID,wparm$ForestMoistureID,wparm$EdaphicHeightCode, wparm$ElevationHeightCode)),]
dw2<-merge(d2,wparm2,by=c('ClusterID','ForestMoistureID','EdaphicHeightCode', 'ElevationHeightCode'))

d3<-data[data$MatchLevel=="Biogeog",]
wparm3<-wparm[!duplicated(paste(wparm$BiogeographicalRegionID,wparm$ForestMoistureID,wparm$EdaphicHeightCode, wparm$ElevationHeightCode)),]
dw3<-merge(d3,wparm3,by=c('BiogeographicalRegionID','ForestMoistureID','EdaphicHeightCode', 'ElevationHeightCode'))

d4<-data[data$MatchLevel=="Cont",]
wparm4<-wparm[!duplicated(paste(wparm$Continent,wparm$ForestMoistureID,wparm$EdaphicHeightCode, wparm$ElevationHeightCode)),]
dw4<-merge(d4,wparm4,by=c('Continent','ForestMoistureID','EdaphicHeightCode', 'ElevationHeightCode'))

dw1<-dw1[,-c(grep(".x",names(dw1)),grep(".y",names(dw1)))]
dw2<-dw2[,-c(grep(".x",names(dw2)),grep(".y",names(dw2)))]
dw3<-dw3[,-c(grep(".x",names(dw3)),grep(".y",names(dw3)))]
dw4<-dw4[,-c(grep(".x",names(dw4)),grep(".y",names(dw4)))]

dw1$PlotID<-data[match(dw1$PlotViewID,data$PlotViewID),"PlotID"]
dw2$PlotID<-data[match(dw2$PlotViewID,data$PlotViewID),"PlotID"]
dw3$PlotID<-data[match(dw3$PlotViewID,data$PlotViewID),"PlotID"]
dw4$PlotID<-data[match(dw4$PlotViewID,data$PlotViewID),"PlotID"]

dw1$ClusterID<-data[match(dw1$PlotViewID,data$PlotViewID),"ClusterID"]
dw2$ClusterID<-data[match(dw2$PlotViewID,data$PlotViewID),"ClusterID"]
dw3$ClusterID<-data[match(dw3$PlotViewID,data$PlotViewID),"ClusterID"]
dw4$ClusterID<-data[match(dw4$PlotViewID,data$PlotViewID),"ClusterID"]

dw1$BiogeographicalRegionID<-data[match(dw1$PlotViewID,data$PlotViewID),"BiogeographicalRegionID"]
dw2$BiogeographicalRegionID<-data[match(dw2$PlotViewID,data$PlotViewID),"BiogeographicalRegionID"]
dw3$BiogeographicalRegionID<-data[match(dw3$PlotViewID,data$PlotViewID),"BiogeographicalRegionID"]
dw4$BiogeographicalRegionID<-data[match(dw4$PlotViewID,data$PlotViewID),"BiogeographicalRegionID"]

dw1$Continent<-data[match(dw1$PlotViewID,data$PlotViewID),"Continent"]
dw2$Continent<-data[match(dw2$PlotViewID,data$PlotViewID),"Continent"]
dw3$Continent<-data[match(dw3$PlotViewID,data$PlotViewID),"Continent"]
dw4$Continent<-data[match(dw4$PlotViewID,data$PlotViewID),"Continent"]



dw5<-rbind(dw1,dw2,dw3,dw4)

#Need to add in plots that do not match on forest type at any level
d6<-data[!data$PlotCode%in%dw5$PlotCode,]
d7<-cbind(d6,matrix(nrow=nrow(d6),ncol=(ncol(dw5)-ncol(d6))))
names(d7)[(ncol(d6)+1):ncol(d7)]<-names(dw5)[(ncol(d6)+1):ncol(d7)]
dw.all<-rbind(dw5,d7)

###
dw.all$a_Best<-with(dw.all,
	ifelse(!is.na(a_Plot),a_Plot,
		ifelse(!is.na(a_ClusterF),a_ClusterF,
			ifelse(!is.na(a_BioRF),a_BioRF,
	a_Continent_T))))

dw.all$b_Best<-with(dw.all,
	ifelse(!is.na(b_Plot),b_Plot,
		ifelse(!is.na(b_ClusterF),b_ClusterF,
			ifelse(!is.na(b_BioRF),b_BioRF,
	b_Continent_T))))

dw.all$c_Best<-with(dw.all,
	ifelse(!is.na(c_Plot),c_Plot,
		ifelse(!is.na(c_ClusterF),c_ClusterF,
			ifelse(!is.na(c_BioRF),c_BioRF,
	c_Continent_T))))

dw.all$Weib.parm.source<-with(dw.all,
	ifelse(!is.na(c_Plot),"Plot",
		ifelse(!is.na(c_ClusterF),"Cluster",
			ifelse(!is.na(c_BioRF),"BiogR",
	"Cont"))))
dw.all$HtEst<-dw.all$a_Best*(1-exp(-dw.all$b_Best*(dw.all$D4/10)^dw.all$c_Best))
dw.all$HtEst[is.na(dw.all$D4)]<-NA
dw.all$HtEst[dw.all$HtEst==0]<-NA

return(dw.all)
}