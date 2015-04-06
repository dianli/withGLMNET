###data preparation for ploting cross validation results. 

##############Graphs -- Chrismax Tree#############
cvplot <- function(){
  IQR=matrix(0,r,1)
  names(IQR)="IQR"
  for(j in 1:r){
    IQR[j,]=IQR(heiser[j,],na.rm=TRUE,type = 7)}
  median=median/IQR
  
  #######sort by range (max-min)####
  row=dim(median)[1]
  range=matrix(0,row,1)
  names(range)="Range"
  for(i in 1:row){
    range[i,]=max(median[i,],na.rm=TRUE)-min(median[i,],na.rm=TRUE)}
  
  median=cbind(median,range)
  median_range=median[order(median$range,decreasing=TRUE),]
  return(median_range)
}

##############Graphs for RNA VALI#######
cvplotVALI <- function(){
  
  #######sort by range (max-min)####
  row=dim(vali)[1]
  range=matrix(0,row,1)
  names(range)="Range"
  for(i in 1:row){
    range[i,]=max(vali[i,],na.rm=TRUE)-min(vali[i,],na.rm=TRUE)}
  
  vali=cbind(vali,range)
  vali_range=vali[order(vali$range,decreasing=TRUE),]
  return(vali_range)
}
