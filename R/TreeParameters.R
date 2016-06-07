#' @title Function to fit tree-level allometries.
#' @description This function solves the a parameter in Weibull height-diameter models to force the model through a measured tree height
#' @param xdataset Object returned by /code{local.heights}
#' @author Wannes Hubau

#' @export


TreeParameters<- function (xdataset){
         
height_data <- xdataset 

# Recategorize F5 Methods
height_data$Method<- ifelse(height_data$F5==1| height_data$F5==2| height_data$F5==3,1,6)

#DEVELOPMENT NOTE - do we want to allow clinometer measurements here????
# make subset with only laser heights
LaserHeightSubset<- height_data[(height_data$Method=="6"),]

x<-(aggregate(Height ~ TreeID + Census.No, data = LaserHeightSubset, FUN = length ))
x<- x[order(x$TreeID),] 

# make subset of TreeIDs with multiple LaserHeight censuses
for(i in 1:length (x[,1])){x[i,4] <- nrow(x[(x$TreeID==x[i,1]),])}
y <- x[!(x[,4]==1),]

# number of TreeIDs with multiple LaserHeight censuses
length(unique(y[,1]))

# per TreeID with multiple laserH, select last census
for(i in 1:length (y[,1])){ifelse(y[i,2]==max(y[(y$TreeID==y[i,1]),2])	,	y[i,4] <- "keep",	y[i,4] <- "drop"	)}
y <- y[(y[,4]=="keep"),]
nrow(y)

# make list of TreeIDs with selected LaserHeight census per TreeID
final <- rbind(y,x[(x[,4]==1),])
final <- final[,c("TreeID","Census.No")]
names(final)[2] <- "Census.No.selectedH"

# merge with data
height_data <- merge(height_data, final, by ='TreeID',all.x=TRUE,all.y=FALSE)

# create column with selected own Height
height_data$H_Own <- ifelse (height_data$Census.No.selectedH == height_data$Census.No,height_data$Height,NA)

# make list of selected H_own
tmp <- height_data[,c("TreeID","H_Own","D4","a_Best","b_Best","c_Best")]
tmp <- tmp[!is.na(tmp$H_Own),]
tmp$a_Best_tree <- NULL
tmp$treeallometry <- NULL

# per TreeID with selected own H : recalculate parameter a_Best based on selected measured own H and DBH1
for(i in 1:nrow(tmp)){
if(!is.na(tmp[i,c("c_Best")])){tmp[i,c("a_Best_tree")] <- tmp[i,c("H_Own")] /(1-exp(-tmp[i,c("b_Best")]*(tmp[i,c("D4")]/10)^tmp[i,c("c_Best")]))} 	# weibull
else{tmp[i,c("a_Best_tree")] <-log(tmp[i,c("H_Own")]) - tmp[i,c("b_Best")] * log(tmp[i,c("D4")]/10)}										# not sure if also good procedure for log-log
tmp$treeallometry <- "tree"
}

# merge with data
tmp <- tmp[,c("TreeID","treeallometry","a_Best_tree")]
height_data <- merge(height_data, tmp, by ='TreeID',all.x=TRUE)

# fill NA's
tmp1 <- height_data[is.na(height_data$a_Best_tree),]
tmp1$a_Best_tree <- tmp1$a_Best
tmp2 <- height_data[!is.na(height_data$a_Best_tree),]
height_data <- rbind(tmp1,tmp2)

# add other tree parameters
height_data$b_Best_tree <- height_data$b_Best
height_data$c_Best_tree <- height_data$c_Best

# add column allometry
height_data$allometry <- NULL
tmp_plot <- height_data[!is.na(height_data$a_Plot),]
tmp_plot$allometry <- "plot"
tmp_cluster <- height_data[is.na(height_data$a_Plot),]
tmp_cluster <- tmp_cluster[!is.na(tmp_cluster$a_ClustF),]
tmp_cluster$allometry <- "cluster"
tmp_bioregion <- height_data[is.na(height_data$a_ClustF),]
tmp_bioregion <- tmp_bioregion[!is.na(tmp_bioregion$a_BioRF),]
tmp_bioregion$allometry <- "bioregion"
tmp_continent <- height_data[is.na(height_data$a_BioRF),]
tmp_continent <- tmp_continent[!is.na(tmp_continent$a_ContF),]
tmp_continent$allometry <- "continent"
tmp_other <- height_data[is.na(height_data$a_ContF),]
tmp_other$allometry <- "none"
#nrow(height_data)-nrow(tmp_continent)-nrow(tmp_bioregion)-nrow(tmp_cluster)-nrow(tmp_plot)-nrow(tmp_other)
height_data <- rbind(tmp_other,tmp_continent,tmp_bioregion,tmp_cluster,tmp_plot)

tmp_tree <- height_data[!is.na(height_data$treeallometry),]
tmp_tree$allometry <- paste(tmp_tree$allometry,tmp_tree$treeallometry,sep="-")
tmp_other <- height_data[is.na(height_data$treeallometry),]
#nrow(height_data)-nrow(tmp_other)-nrow(tmp_tree)

height_data <- rbind(tmp_other,tmp_tree)

# clean Dataset
height_data$H_Own <- NULL ; height_data$Census.No.selectedH <- NULL ; height_data$Method <- NULL ; height_data$treeallometry <- NULL

#order
height_data <- height_data[order(height_data$TreeID),] 

height_data 
}


