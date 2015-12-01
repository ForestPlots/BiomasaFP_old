#' @description Function for fitting height-diameter Weibull models by non-linear least squares
#' @data Object returned by mergefp
#' @return.mods Logical. If TRUE, full model fits are returned. Otherwise (default) only the fitted coefficents are returned.  
#' @author Martin Sullivan, Gabriela Lopez-Gonzalez

fit.weib<-function(data,return.mods=FALSE){

#CHANGE COLUMN NAMES FOR BACKCOMPATABILITY
data$EdaphicHeightCode<-data$ForestEdaphicHeightID
data$ElevationHeightCode<-data$ForestElevationHeightID

# Select data with diameters and height 
# Remove trees without heights or with height=0.  Remove Monocots. To generate D:H relationships main plot views should be used.
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
# Exclude F4 not like '60' or '0'
TreesHt <- TreesHt[ grepl('0',TreesHt$F4)==TRUE|grepl('06',TreesHt$F4)==TRUE ,  ]
# Exclude f1 flag1= b, c, d, f,g,j,k,m,o,p
TreesHt <- TreesHt[ grepl('b',TreesHt$F1)==FALSE & grepl('c',TreesHt$F1)==FALSE &
                            grepl('d',TreesHt$F1)==FALSE & grepl('f',TreesHt$F1)==FALSE&
                            grepl('g',TreesHt$F1)==FALSE & grepl('j',TreesHt$F1)==FALSE &
                            grepl('k',TreesHt$F1)==FALSE & grepl('m',TreesHt$F1)==FALSE &
                            grepl('o',TreesHt$F1)==FALSE & grepl('p',TreesHt$F1)==FALSE
                    ,  ]

#Exclude treeswith method 1
TreesHt <- TreesHt[!(TreesHt$Method==1),]

###1,ContinentData## 
HtCont<- TreesHt[,c('Continent','PlotID','Height','D1')]

library (nlme)
weib1 <- nlsList (Height ~ a*(1-exp(-b*(D1/10)^c))|Continent,
                  data=HtCont,
                  na.action=na.omit,
                  start = c(a=25, b= 0.05, c= 0.7),
                  pool=FALSE)
#summary(weib1)
ContinentCoef<-data.frame(coef(weib1))
colnames(ContinentCoef) <- c('a_Continent','b_Continent', 'c_Continent')
ContinentCoef$Continent = rownames(ContinentCoef)
#ContinentCoef
#plot(weib1)

rm(HtCont)

#2. ContinentData and Forest Type
HtCont_Type<- (TreesHt[,c('Continent','ForestMoistureID', 'EdaphicHeightCode', 'ElevationHeightCode','Height','D1')])
good<-complete.cases(HtCont_Type)
HtCont_Typea<-HtCont_Type[good,]
#head(HtCont_Typea)

weib2 <- nlsList (Height ~ a*(1-exp(-b*(D1/10)^c))|Continent/ForestMoistureID/EdaphicHeightCode/ElevationHeightCode,
                  data=HtCont_Typea,
                  na.action=na.omit,
                  start = c(a=25, b= 0.05, c= 0.7),
                  pool=FALSE)
#summary(weib2)
ContinentCoef_Type<-data.frame(coef(weib2))
colnames(ContinentCoef_Type) <- c('a_Continent_T','b_Continent_T', 'c_Continent_T')

ContinentCoef_Type$ContType <-rownames(ContinentCoef_Type)
#head(ContinentCoef_Type)

ContinentCoef_Type$Continent<-unlist(lapply(strsplit(ContinentCoef_Type$ContType, "/"),"[", 1))
ContinentCoef_Type$ForestMoistureID<-unlist(lapply(strsplit(ContinentCoef_Type$ContType, "/"),"[", 2))
ContinentCoef_Type$EdaphicHeightCode<-unlist(lapply(strsplit(ContinentCoef_Type$ContType, "/"),"[", 3))
ContinentCoef_Type$ElevationHeightCode<-unlist(lapply(strsplit(ContinentCoef_Type$ContType, "/"),"[", 4))
#ContinentCoef_Type
#plot (weib2)

rm(HtCont_Type)

#3. Biogeographic Region

HtBiogeo<- TreesHt[,c('Continent','Country','BiogeographicalRegionID', 'PlotID','Height','D1')]
weib3 <- nlsList (Height ~ a*(1-exp(-b*(D1/10)^c))|'BiogeographicalRegionID',
                  data=HtBiogeo,
                  na.action=na.omit,
                  start = c(a=25, b= 0.05, c= 0.7),
                  pool=FALSE)
#summary(weib3)
BioRCoef<-data.frame(coef(weib3))
colnames(BioRCoef) <- c('a_BioR','b_BioR', 'c_BioR')
BioRCoef$BiogeographicalRegionID = rownames(BioRCoef)

rm(HtBiogeo)

#4. Biogeographic Region/ForestType
HtBiogeoFt<- TreesHt[,c('Continent','Country','BiogeographicalRegionID', 'PlotID','ForestMoistureID', 'EdaphicHeightCode', 'ElevationHeightCode','Height','D1')]
weib4 <- nlsList (Height ~ a*(1-exp(-b*(D1/10)^c))|BiogeographicalRegionID/ForestMoistureID/EdaphicHeightCode/ElevationHeightCode,
                  data=HtBiogeoFt,
                  na.action=na.omit,
                  start = c(a=25, b= 0.05, c= 0.7),
                  pool=FALSE)
#summary(weib4)
BioRCoefFt<-data.frame(coef(weib4))
colnames(BioRCoefFt) <- c('a_BioRF','b_BioRF', 'c_BioRF')
BioRCoefFt$BioRCoefFtype = rownames(BioRCoefFt)
head(BioRCoefFt)
BioRCoefFt$BiogeographicalRegionID<-unlist(lapply(strsplit(BioRCoefFt$BioRCoefFtype, "/"),"[", 1))
BioRCoefFt$ForestMoistureID<-unlist(lapply(strsplit(BioRCoefFt$BioRCoefFtype, "/"),"[", 2))
BioRCoefFt$EdaphicHeightCode<-unlist(lapply(strsplit(BioRCoefFt$BioRCoefFtype, "/"),"[", 3))
BioRCoefFt$ElevationHeightCode<-unlist(lapply(strsplit(BioRCoefFt$BioRCoefFtype, "/"),"[", 4))

rm(HtBiogeoFt)

##5 Analyze data by country
HtCountry<- TreesHt[,c('Continent','Country', 'PlotID','Height','D1')]
#library (nlme)
weib5 <- nlsList (Height ~ a*(1-exp(-b*(D1/10)^c))|Country,
                  data=HtCountry,
                  na.action=na.omit,
                  start = c(a=25, b= 0.05, c= 0.7),
                  pool=FALSE)
#summary(weib5)
CountryCoef<-data.frame(coef(weib5))
colnames(CountryCoef) <- c('a_Country','b_Country', 'c_Country')
CountryCoef$Country = rownames(CountryCoef)

rm(HtCountry)

##6 Analyze data by countryand ForestType
HtCountryF<- TreesHt[,c('Continent','Country', 'PlotID','ForestMoistureID', 'EdaphicHeightCode', 'ElevationHeightCode','Height','D1')]
#library (nlme)
weib6 <- nlsList (Height ~ a*(1-exp(-b*(D1/10)^c))|Country/ForestMoistureID/EdaphicHeightCode/ElevationHeightCode,
                  data=HtCountryF,
                  na.action=na.omit,
                  start = c(a=25, b= 0.05, c= 0.7),
                  pool=FALSE)
#summary(weib6)

HtCountryFt<-data.frame(coef(weib6))
colnames(HtCountryFt) <- c('a_CountryF','b_CountryF', 'c_CountryF')
HtCountryFt$HtCountryFtypw = rownames(HtCountryFt)

HtCountryFt$Country<-unlist(lapply(strsplit(HtCountryFt$HtCountryFtypw, "/"),"[", 1))
HtCountryFt$ForestMoistureID<-unlist(lapply(strsplit(HtCountryFt$HtCountryFtypw, "/"),"[", 2))
HtCountryFt$EdaphicHeightCode<-unlist(lapply(strsplit(HtCountryFt$HtCountryFtypw, "/"),"[", 3))
HtCountryFt$ElevationHeightCode<-unlist(lapply(strsplit(HtCountryFt$HtCountryFtypw, "/"),"[", 4))

rm(HtCountryF)

#7.Analyze data by clusterid
HtCluster<- (TreesHt[,c('ClusterID','Height','D1')])
goodCl<-complete.cases(HtCluster)
HtClusterA<-HtCluster[goodCl,]

weib7 <- nlsList (Height ~ a*(1-exp(-b*(D1/10)^c))|ClusterID,
                  data=HtClusterA,
                  na.action=na.omit,
                  start = c(a=25, b= 0.05, c= 0.7),
                  pool=FALSE)


ClusterCoef<-data.frame(coef(weib7))
colnames(ClusterCoef) <- c('a_Cluster','b_Cluster', 'c_Cluster')
ClusterCoef$ClusterID = rownames(ClusterCoef)

rm(HtCluster)

#8. Cluster id and ForestType
HtClusterFt <- TreesHt[,c('ClusterID','ForestMoistureID', 'EdaphicHeightCode', 'ElevationHeightCode','Height','D1')]
goodClFt<-complete.cases(HtClusterFt)
HtClusterFtA<-HtClusterFt[goodClFt,]

weib8 <- nlsList (Height ~ a*(1-exp(-b*(D1/10)^c))| ClusterID/ForestMoistureID/EdaphicHeightCode/ElevationHeightCode,
                  data=HtClusterFtA,
                  na.action=na.omit,
                  start = c(a=25, b= 0.05, c= 0.7),
                  pool=FALSE)
ClusterFCoef<-data.frame(coef(weib8))
colnames(ClusterFCoef) <- c('a_ClusterF','b_ClusterF', 'c_ClusterF')
ClusterFCoef$ClusterF = rownames(ClusterFCoef)

ClusterFCoef$ClusterID<-unlist(lapply(strsplit(ClusterFCoef$ClusterF, "/"),"[", 1))
ClusterFCoef$ForestMoistureID<-unlist(lapply(strsplit(ClusterFCoef$ClusterF, "/"),"[", 2))
ClusterFCoef$EdaphicHeightCode<-unlist(lapply(strsplit(ClusterFCoef$ClusterF, "/"),"[", 3))
ClusterFCoef$ElevationHeightCode<-unlist(lapply(strsplit(ClusterFCoef$ClusterF, "/"),"[", 4))

rm(HtClusterFt,HtClusterFtA)

#Analyze data by  plot
HtPlot<- TreesHt[,c('PlotID','Height','D1')]
weib9 <- nlsList (Height ~ a*(1-exp(-b*(D1/10)^c))| PlotID,
                  data=HtPlot,
                  na.action=na.omit,
                  start = c(a =25, b= 0.05, c= 0.7),
                  pool=FALSE)
PlotCoef<-data.frame(coef(weib9))
colnames(PlotCoef) <- c('a_Plot','b_Plot', 'c_Plot')
PlotCoef$PlotID = rownames(PlotCoef)

rm(HtPlot)

#Get unique list of plots
Heights3<-unique(data[,c('PlotID','ClusterID','ForestMoistureID','EdaphicHeightCode','ElevationHeightCode','Country','BiogeographicalRegionID','Continent')])

Ht1 <- merge(Heights3,PlotCoef, by='PlotID', all.x=TRUE)

#Cluster and Forest type
Ht2 <- merge(Ht1,ClusterFCoef, by=c('ClusterID','ForestMoistureID','EdaphicHeightCode','ElevationHeightCode'), all.x=TRUE)


# CLUSTER
Ht3<- merge(Ht2, ClusterCoef, by='ClusterID', all.x=TRUE)

#Country/ForestType
Ht4 <- merge(Ht3,HtCountryFt, by=c('Country','ForestMoistureID','EdaphicHeightCode', 'ElevationHeightCode'), all.x=TRUE)

#Country
Ht5 <- merge(Ht4,CountryCoef, by='Country', all.x=TRUE)

#Biogeographic region and Forest type
Ht6<- merge (Ht5,BioRCoefFt, by=c('BiogeographicalRegionID','ForestMoistureID','EdaphicHeightCode', 'ElevationHeightCode'), all.x=TRUE)

#Biogeographic region 
Ht7<- merge (Ht6,BioRCoef, by='BiogeographicalRegionID', all.x=TRUE)

#Continent and ForestType

Ht8<- merge (Ht7,ContinentCoef_Type, by=c('Continent','ForestMoistureID','EdaphicHeightCode', 'ElevationHeightCode'), all.x=TRUE)

#Continent
Ht9 <- merge(Ht8, ContinentCoef, by='Continent', all.x=TRUE)

if(return.mods==TRUE){
return(list(Ht9,weib9))
}else{
return(Ht9)
}
}
