############################## Lasso for crossvalidation ##################################
lassoCV <- function(n){
  
  ###Set the dataset as a matrix
  data=data.matrix(datar)
  
  ###Assign explantory and response variables
  y=data[,1]
  x=data[,2:dim(data)[2]]  
  dimx=dim(x)
  
  ###Create tables of approperiate size to store results
  stat=matrix(0,nrep,4,dimnames=list(c(1:nrep),c("df","lambda","resi","dev.ratio_R^2")))  
  coef0=matrix(0,(dimx[2]+1),(nrep+1),dimnames=list(c(0:dimx[2]),c(0:nrep)))
  
  ###Loop of running Lasso to get a final list of selected predictors
  for(i in 1:nrep){
    
    ###Run a cross validation of Lasso to obtain a lambda of the best fit
    cv.fit=cv.glmnet(x,y,alpha=1,grouped=FALSE)
    
    ###Run Lasso with the best lambda to fit the model
    fit=glmnet(x,y,lambda=cv.fit$lambda.min)
    
    ###Record the number of variables of the fitted model
    df=fit$df
    stat[i,1]=df
    
    ###Record the best lambda
    stat[i,2]=fit$lambda
    
    ###Record coeffitients of predictors
    coef0[1:(dimx[2]+1),1]=row.names(coef(fit))
    coef0[1:(dimx[2]+1),i+1]=as.matrix(coef(fit))
    
    ###Calculate the difference b/t observation data from Heiser and the prediction for the removed cell line by using the fitted model
    if(df==0){est=0}
    else{est=predict(fit,newx=newx,s=cv.fit$lambda.min,type="link")}
    if(est==0){resi="NA"}else{resi=testy-est}
    stat[i,3]=resi
    stat[i,4]=fit$dev.ratio
  }
  
  ###Write CSV files for statistics of all fitted models
  stat_file=paste("stat_",drug,"_",j,".csv",sep="")
  write.csv(stat, file=stat_file)
  #coef_file=paste("coef_",drug,"_",j,".csv",sep="")
  #write.csv(coef0, file=coef_file)
  residuals=stat[,3]
  
  ##################export output######################
  data2=coef0[2:(dim(coef0)[1]),2:dim(coef0)[2]]
  coef=matrix(as.numeric(data2),nrow=(dim(coef0)[1]-1),ncol=(dim(coef0)[2]-1))
  row.names(coef)=coef0[2:dim(coef0)[1],1]
  
  ###Count the number of selections with no predictor selected
  count_zero=data.matrix(apply(coef,2,sum))
  vr_zero=sum(count_zero==0)
  
  ###Extract the selected predictors and total numbers of being selected in a thousand of repeats
  coef[coef!=0]=1
  counts=as.matrix(sort(apply(coef,1,sum)))
  
  ###Write CSV files for statistics
  vr_file=paste("vr_",drug,"_",j,".csv",sep="")
  write.csv(counts, file=vr_file)
  
  vr_fun=function(z) z[z >0]
  vr=counts[(counts %in% apply(counts,2,vr_fun)),]
  vr=as.matrix(vr)
  
  ###Print the results
  print(drug)
  print(paste("j=",j,"remove row", rmrow))
  print(paste("vr_zero=", vr_zero))
  print(paste("predictors=", dim(vr)[1]))
  print(vr)
  cat("\n")
  return(residuals)
}

#################simplified CV which only returns predictors for each round#########
lassoCV2 <- function(n){  
  data=data.matrix(data_train)  
  y=data[,1]
  x=data[,2:dim(data)[2]]
  dimx=dim(x) 
  coef00=matrix(0,(dimx[2]+1),(nrep+1),dimnames=list(c(0:dimx[2]),c(0:nrep)))
  ###Loop of running Lasso to get a final list of selected predictors
  for(i in 1:nrep){
    cv.fit=cv.glmnet(x,y,alpha=1,grouped=FALSE)
    fit=glmnet(x,y,lambda=cv.fit$lambda.min)    
    ###Record coeffitients of predictors
    coef00[1:(dimx[2]+1),1]=row.names(coef(fit))
    coef00[1:(dimx[2]+1),i+1]=as.matrix(coef(fit))
  }  
  coef0=coef00
  data2=coef0[-1,-1]
  
  coef=matrix(as.numeric(data2),nrow=(dim(coef0)[1]-1),ncol=(dim(coef0)[2]-1))
  row.names(coef)=coef0[2:dim(coef0)[1],1]
  count_zero=data.matrix(apply(coef,2,sum))
  vr_zero=sum(count_zero==0)
  coef[coef!=0]=1
  counts=as.matrix(sort(apply(coef,1,sum)))
  vr_fun=function(z) z[z >0]
  vr=counts[(counts %in% apply(counts,2,vr_fun)),]
  vr=as.matrix(vr)
  print(drug)
  print(paste("j=",j,"remove row", rmrow))
  print(paste("vr_zero=", vr_zero))
  print(paste("predictors=", dim(vr)[1]))
  print(vr)
  cat("\n")
  predtr=row.names(vr)
  return(predtr)
}
