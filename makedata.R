###mkdata:GPs; mkdataMRM:MRM; mkdataRNA:RNA; mkdataRPPA:RPPA(include celllines in GP's plus ramdon samples to make totally 22 observations )

###### Make a data set with thresholds for Glycoprotein data #######
mkdata<-function(n){
  
  ###Input explanatory and response variables
  y0=data.o[,n]
  x0=data.o[,(no.drug+2):dim(data.o)[2]]
  
  ###Remove any NA rows from temporary dataset
  data0=cbind(y0,x0)
  data0=na.omit(data0)
  
  ###Remove any column with column sum less than "cutoff" from GPs' data
  trim_x=data0[,2:dim(data0)[2]]
  trim_x=trim_x[,colSums(trim_x)>cutoff]
  
  ###Take log transform of protein mass spec counts
  datax=log(trim_x+0.5,10)
  datay=matrix(data0[,1])
  colnames(datay)=drug
  
  ###Create a trimed dataset for further use
  data=cbind(datay,datax)
}

#################Make a data set for MRM data############
### no thresholds, logarithm of base ten 
mkdataMRM<-function(n){
  
  ###Input explanatory and response variables
  y0=data.o[,n]
  x0=data.o[,(no.drug+2):dim(data.o)[2]]
  
  ###Remove any NA rows from temporary dataset
  data0=cbind(y0,x0)
  data0=na.omit(data0)
  
  trim_x=data0[,2:dim(data0)[2]]
  
  ###Take log transform of protein mass spec counts
  datax=log(trim_x,10)
  datay=matrix(data0[,1])
  colnames(datay)=drug
  
  ###Create a trimed dataset for further use
  data=data.frame(cbind(datay,datax))
}

######Make a dataset for RNA data#######
### no cutoff, no logarithm
mkdataRNA<-function(n){
  
  ###Input explanatory and response variables
  y0=data.o[,n]
  x0=data.o[,(no.drug+2):dim(data.o)[2]]
  
  ###Remove any NA rows from temporary dataset
  data0=cbind(y0,x0)
  data0=na.omit(data0)
  
  ###Create a trimmed dataset  
  ### w/ log transform
  #trim_x=data0[,-1]
  #datax=log(2^trim_x,10)
  #datay=matrix(data0[,1])
  #colnames(datay)=drug
  #data=data.frame(cbind(datay,datax))
  
  ##########################
  ###w/ no log transform
  #datax=data0[,-1]
  #datay=matrix(data0[,1])
  #colnames(datay)=drug
  #data=cbind(datay,datax)
  ##########################
}

######Make a dataset for RPPA data#######
mkdataRPPA<-function(n){
  
  ###Input explanatory and response variables
  y0=data.o[,n]
  x0=data.o[,(no.drug+2):dim(data.o)[2]]
  
  ###Remove any NA rows from temporary dataset
  data0=cbind(y0,x0)
  row.names(data0)=row.names(data.o)
  data0=na.omit(data0)
  
  ###match to the 22 cell lines###
  selct=data0[row.names(data0) %in% cell.22,]
  rest=data0[!(row.names(data0) %in% cell.22),]
  randm=rest[sample(nrow(rest),(22-nrow(selct)),replace=F),]
  data=rbind(selct,randm)
  colnames(data)[1]=drug
  data=data.frame(data)
}
