#' @title SummaryAGWP
#' @description Function to estimate AGWP by plotview and census interval, including estimating unobserved recruitment and growth of trees that died between census periods
#' @param xdataset Object returned by \code{mergefp}.
#' @param AGBEquation Allometric equation function to use when estimating AGB.Note only \code{AGBChv14} and \code{AGBChv05MH} are fully implemented, other Chave 2005 equations can by used for observed components by unobserved components will use \code{AGBChv05MH}. Plan to implement other equations fully in future.
#' @param dbh Name of column containing diameter data. Default is "D4".
#' @param rec.meth Method used to estimate AGWP of recruits. If 0 (default), estimates growth from starting diameter of 0mm. If another value is provided, then growth is estimated from a starting diameter of 100mm.
#' @param height.data Object returned by \code{param.merge}. Used to supply parameters of local allometric equations. If NULL (default), regional height diameter equations are used.
#' @param param.type Local height diameter to use. One of Best (defualt), BioRF,ClusterF ... NEED TO DECIDE WHICH OF THESE TO RETURN. Ignored if \code{height.data=NULL}.
#' @return A data frame with PlotViewID, CensusNo, and observed and unobserved elements of AGWP, stem dynamics and AGB mortality.
#' @author Martin Sullivan, Gabriela Lopez-Gonzalez

#' @export	




#Need to add palm equation element (palm.eq=TRUE)
SummaryAGWP <- function (xdataset, AGBEquation, dbh ="D4",rec.meth=0,height.data=NULL,param.type="Best"){
	#Use AGBEquation to set Chv14 argument
	if(all.equal(AGBEquation,AGBChv14)==TRUE){
		Chv14=TRUE
	}else{
		Chv14=FALSE
	}

        AGBData <- AGBEquation (xdataset, dbh,height.data=height.data,param.type=param.type) 
	  IndAL <- aggregate (Alive/PlotArea ~ PlotViewID + Census.No,  data = AGBData, FUN=sum )
        AGBAlive <-aggregate (AGBind/PlotArea ~ PlotViewID + Census.No, data = AGBData, FUN=sum ) 
	  AGBData$Census.prev<-AGBData[match(paste(AGBData$TreeID,AGBData$Census.No-1),paste(AGBData$TreeID,AGBData$Census.No)),"Census.Mean.Date"]
	  AGBData$Delta.time<-AGBData$Census.Mean.Date-AGBData$Census.prev	  


	  #Estimate AGB of recruits at 100mm
	  Height100<-AGBData$a_par*(1-exp(-AGBData$b_par*(100/10)^AGBData$c_par))
	  if(Chv14==F){
			AGBData$AGB.100<-(0.0509*AGBData$WD * ((100/10)^2)* Height100)/1000
		}else{
			AGBData$AGB.100<-(0.0673*(AGBData$WD * ((100/10)^2)* Height100)^0.976)/1000
	  }		
	  AGBData$AGWPRec<-AGBData$AGBind-AGBData$AGB.100

	  #Calculate growth of surviving trees
	  AGBData$AGB.Prev<-AGBData[match(paste(AGBData$TreeID,AGBData$Census.No-1),paste(AGBData$TreeID,AGBData$Census.No)),"AGBind"]
	  AGBData$Delta.AGB<-AGBData$AGBind-AGBData$AGB.Prev
	  AGBData$Survive2<-ifelse(AGBData$Census.No>1 & AGBData$Recruit==0 & AGBData$Alive==1 & AGBData$Snapped==0,1,0)
	  #Includes snapped trees - for stem dynamics
	  AGBData$Survive<-ifelse(AGBData$Census.No>1 & AGBData$Recruit==0 & AGBData$Alive==1 ,1,0)
	  AGB.surv<-aggregate(Delta.AGB/PlotArea ~ PlotViewID + Census.No,  data = AGBData[AGBData$Survive2==1,], FUN=sum )
 	  IndSurv<-aggregate(Survive/PlotArea ~ PlotViewID + Census.No,  data = AGBData, FUN=sum )
	  #Calculate mean WD per census
	  WD<-aggregate(WD ~ PlotViewID + Census.No,  data = AGBData, FUN=function(x)mean(x,na.rm=T))

	  # Find Recruits
        Recruits<- AGBData[AGBData$Recruit==1,]
        # Removed this if statement because we need to discuss if this is necessary if(rec.meth==100){
	#AGBRec <- aggregate (AGWPRec/PlotArea ~ PlotViewID + Census.No,  data = Recruits, FUN=sum )
	#}else{
	AGBRec <- aggregate (AGBind/PlotArea ~ PlotViewID + Census.No,  data = Recruits, FUN=sum )
	#}
        colnames(AGBRec) <- c('PlotViewID','Census.No','AGBRec.PlotArea')
        IndRec <- aggregate (Recruit/PlotArea ~ PlotViewID + Census.No,  data = Recruits, FUN=sum )
     	
        #merge recruit information
        Recs<- merge(AGBRec, IndRec, by =  c('PlotViewID','Census.No'), all.x=TRUE)
        
        # Dead stems  get only stems that are dead and have an AGB_D, to count only dead trees ones
        DeadTrees <-AGBData[AGBData$Dead==1 & !is.na(AGBData$D1_D),]
        #Match in size class growth rate
	  IndDead <- aggregate (Dead/PlotArea ~ PlotViewID + Census.No,  data = DeadTrees, FUN=sum )
        AGBDe <-aggregate (AGBDead/PlotArea ~  PlotViewID + Census.No, data = AGBData, FUN=sum )
        #merge Dead trees
        Deads <- merge(AGBDe, IndDead, by = c('PlotViewID','Census.No'),all.x=TRUE)
     
	# Unobserved growth of dead trees. Estimated to mid-point between last census the tree
        #was recoded as alive and dead census.
	  growth.rate<-SizeClassGrowth(xdataset,dbh=dbh)
	  
	  dead2<-merge(DeadTrees,growth.rate,by="PlotViewID",all.x=T)
	  dead2$DBH.death<-ifelse(dead2$D4_D<200,(dead2$Delta.time * 0.5)*dead2$Class1,
	                          ifelse(dead2$D4_D<400,(dead2$Delta.time * 0.5)*dead2$Class2,
	                                 (dead2$Delta.time * 0.5)*dead2$Class3))
	  dead2$DBH.death<-dead2$DBH.death+dead2$D4_D
	  #Height at death considering unobserved growth
	  #dead2$Height.dead<-dead2$a_par*(1-exp(-dead2$b_par*(dead2$D4_D/10)^dead2$c_par))
	  dead2$Height.dead2<-dead2$a_par*(1-exp(-dead2$b_par*(dead2$DBH.death/10)^dead2$c_par))
	  #AGB at death
	  if(Chv14==F){
			dead2$AGB.death2<-(0.0509*dead2$WD * ((dead2$DBH.death/10)^2)* dead2$Height.dead2)/1000
		}else{
			dead2$AGB.death2<-(0.0673*(dead2$WD * ((dead2$DBH.death/10)^2)* dead2$Height.dead2)^0.976)/1000
	  }	
	 dead2$Unobs.dead<-dead2$AGB.death2-dead2$AGBDead
	 unobs.dead<-aggregate(Unobs.dead/PlotArea~PlotViewID + Census.No, data = dead2, FUN=sum )
        	


	# merge all summaries
        mergeAGBAlive <- merge (AGBAlive, IndAL, by = c('PlotViewID','Census.No'),all.x=TRUE)
        mergeB <-  merge(mergeAGBAlive, Recs, by = c('PlotViewID','Census.No'),all.x=TRUE)
        mergeC <- merge (mergeB, Deads, by=  c('PlotViewID','Census.No'),all.x=TRUE) 
	  mergeD<-merge (mergeC,AGB.surv,  by = c('PlotViewID','Census.No'),all.x=TRUE)
        mergeE<-merge (mergeD,IndSurv,  by = c('PlotViewID','Census.No'),all.x=TRUE)
      	mergeF<-merge(mergeE,unobs.dead, by = c('PlotViewID','Census.No'),all.x=TRUE)
	mergeF<-merge(mergeF,WD,by=c('PlotViewID','Census.No'),all.x=TRUE)
	#Correct names of merge F to be legal
	names(mergeF)<-sub("/",".",names(mergeF))	

	#Estimate growth of unobserved recruits - 1st merge growth rate data with allometric parameter data
	unobs.dat<-unique(AGBData[!is.na(AGBData$Delta.time),c("PlotViewID","Census.No","a_par","b_par","c_par","Delta.time")])
	unobs.dat<-merge(unobs.dat,growth.rate,by="PlotViewID",all.x=T)
	#Then merge with existing data to get number of recruits and deaths
	tmp<-merge(mergeF,unobs.dat,by = c('PlotViewID','Census.No'),all.x=TRUE)
	#Get number of stems in previous census
	tmp$PrevStems<-tmp[match(paste(tmp$PlotViewID,tmp$Census.No-1),paste(tmp$PlotViewID,tmp$Census.No)),"Alive.PlotArea"]
	#Estimate recruitment and mortality rates
	stems<-tmp$PrevStems
	death<-tmp$Dead.PlotArea
	recruits<-tmp$Recruit.PlotArea
	interval<-tmp$Delta.time
	tmp$rec.rate<-recruits/stems/interval
	tmp$mort.rate<-death/stems/interval
	tmp$rec.rate <- replace(tmp$rec.rate,is.na(tmp$rec.rate),0)													# CHANGE: added, otherwise function doesn't work as crashes on plotviews with 0 recruitment
	tmp$mort.rate <- replace(tmp$mort.rate,is.na(tmp$mort.rate),0)	
	#Get time weighted mean of rec and mort rates for each plot# Check if weighted mean is necessary
	a<-split(tmp,f=tmp$PlotViewID)
	rec1<-unlist(lapply(a,function(x)as.numeric(lm(x$rec.rate~1,weights=x$Delta.time)[1])))
	mort1<-unlist(lapply(a,function(x)as.numeric(lm(x$mort.rate~1,weights=x$Delta.time)[1])))
	rec.means<-data.frame("PlotViewID"=names(rec1),"Rec.mean"=as.numeric(rec1),stringsAsFactors=F)
	mort.means<-data.frame("PlotViewID"=names(mort1),"Mort.mean"=as.numeric(mort1),stringsAsFactors=F)
	plot.means<-merge(rec.means,mort.means,by="PlotViewID",all.x=T)
	tmp<-merge(tmp,plot.means,by="PlotViewID",all.x=T)
	#Growth of unobserved recruits
	unobs.D<-tmp$Class1*tmp$Delta.time*(1/3)
	#D at death
	D.death<-100+unobs.D
	#Get height at death
	height.death<-tmp$a_par*(1-exp(-tmp$b_par*(D.death/10)^tmp$c_par))
	#Height at point of recruitment
	height.rec<-tmp$a_par*(1-exp(-tmp$b_par*(100/10)^tmp$c_par))
	#WD_mean<-tmp$MeanWD # Commented out - mean across all censuses
	WD_mean<-tmp$WD # Use mean in plot and census
	#Estimate AGB at death
	if(Chv14==F){
		AGB.death<-(0.0509*WD_mean * ((D.death/10)^2)* height.death)/1000
	}else{
		AGB.death<-(0.0673*(WD_mean * ((D.death/10)^2)* height.death)^0.976)/1000
	}
	#Estimate AGB at point of recruitment
	if(Chv14==F){
		AGB.rec<-(0.0509*WD_mean * ((100/10)^2)* height.rec)/1000
	}else{
		AGB.rec<-(0.0673*(WD_mean * ((100/10)^2)* height.rec)^0.976)/1000
	}
	#Get number of unobserved recruits
	unobs.rec<-tmp$Delta.time*tmp$Rec.mean*tmp$Mort.mean*tmp$Alive.PlotArea
	#unobs.rec<-tmp$Delta.time*tmp$Rec.mean*tmp$Mort.mean*tmp$PrevStems
	#Get growth of unobs recruits
	#Option for recruits starting at 100
	if(rec.meth==100){
		unobs.growth<-(unobs.rec*(AGB.death-AGB.rec))
	}else{
		unobs.growth<-(unobs.rec*AGB.death)
	}
	tmp$UnobsGrowth<-unobs.growth
	tmp$UnobsRec<-unobs.rec
	#Simplyfy tmp and merge with MergeF
	tmp<-tmp[,c("PlotViewID","Census.No","rec.rate","mort.rate","UnobsGrowth","UnobsRec","Delta.time")]
	mergeG<-merge (mergeF,tmp,  by = c('PlotViewID','Census.No'),all.x=TRUE)

	#Create output file
        SummaryB <- mergeG

	
        SummaryB <- SummaryB[order(SummaryB$PlotViewID, SummaryB$Census.No, decreasing=FALSE), ]
	#Add AGWP per ha
	SummaryB$AGWP.PlotArea<-with(SummaryB,Delta.AGB.PlotArea+AGBRec.PlotArea+Unobs.dead.PlotArea+UnobsGrowth)
	SummaryB$AGWP.PlotArea.Year<-SummaryB$AGWP.PlotArea/SummaryB$Delta.time
	
	#Neaten up column names
	names(SummaryB)<-c("PlotViewID","Census.No","AGB.ha","Stems.ha","AGWPrec.ha","Recruit.ha","AGBmort.ha","Mortality.ha",
	                   "AGWPsurv.ha","SurvivingStems.ha","UnobsAGWPmort.ha","Mean.WD","Recruitment.stem.year","Mortality.stem.year","UnobsAGWPrec.ha","UnobsRecruits.ha","CensusInterval","AGWP.ha","AGWP.ha.year")
        SummaryB
        
}

