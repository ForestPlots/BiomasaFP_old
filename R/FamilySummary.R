#' @title Function for summarising the abundance of different tree families by plot.
#' @description  Function to summarize information on tree families (based on APGIII classification) by Plot.
#' Output headers are: Country, PlotCode, PlotViewID, Family, No.sp (number of species), Frequency (number of individuals in a family).
#' 
#' 
#' @param xdataset a dataset  that has been merged using the function mergefp.
#' @author Gabriela Lopez-Gonzalez
#'
#' @export

SummaryFamilies <- function (xdataset){
        
        
        #1. Create Unique Dataset of treeIds by Families by PlotViewID        
        xdu <- xdataset[,c('PlotID','PlotCode','Country','PlotViewID','FamilyAPGID',
                         'Family','GenusID','Genus','SpeciesID','Species', 'TreeID'
                         )]
        #extract unique elemments, to create a list of indiviuals and determinations by Plotviewid
        udfile <-unique(xdu)
        
        Freq1 <-aggregate (TreeID ~  PlotViewID +Family, data = udfile, FUN=length)
        colnames(Freq1) <- c('PlotViewID', 'Family', 'Frequency')
        
       
        #2. Unique Dataset of families 
        xdu2 <- xdataset[,c('PlotID','PlotCode','Country','PlotViewID','FamilyAPGID',
                           'Family','GenusID','Genus','SpeciesID','Species'
        )]
        #extract unique elemments, to create a list of indiviuals and determinations by Plotviewid
        udfile2 <-unique(xdu2)
        
        Freq2 <-aggregate (SpeciesID ~ Country+ PlotCode + PlotViewID +Family, data = udfile2, FUN=length)
        colnames(Freq2) <- c('Country', 'PlotCode', 'PlotViewID','Family', 'No.sp')
        
        FamilySumm<-merge(Freq2, Freq1, by =c('PlotViewID', 'Family'), all=TRUE)
        FamilySumma<- FamilySumm[,c('Country','PlotCode', 'PlotViewID', 'Family','No.sp','Frequency')]
        FamilySummb <-FamilySumma[order(FamilySumma$PlotViewID, FamilySumma$Family, decreasing=FALSE), ]
        FamilySummb
        
}


