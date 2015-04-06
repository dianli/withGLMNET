#################################Run GLMNET with Lasso##################################
lasso <- function(n){
  
  ###Set the dataset as a matrix
  data=data.matrix(data)
  
  ###Assign explantory and response variables
  y=data[,1]
  x=data[,2:dim(data)[2]]
  dimx=dim(x)
  
  ###Create tables of approperiate size to store results
  stat=matrix(0,nrep,4,dimnames=list(c(1:nrep),c("df","lambda","MSE","dev.ratio_R^2")))  
  coef=matrix(0,(dimx[2]+1),(nrep+1),dimnames=list(c(0:dimx[2]),c(0:nrep)))
  
  ###Loop of running Lasso to get a final list of selected predictors
  for(i in 1:nrep){
    
    ###Run a cross validation of Lasso to obtain a lambda of best fit
    cv.fit=cv.glmnet(x,y,alpha=1,grouped=FALSE)
    
    ###Run Lasso with the best lambda to fit the model 
    fit=glmnet(x,y,lambda=cv.fit$lambda.min)
    
    ###Record the number of variables of the fitted model
    df=fit$df
    stat[i,1]=df
    
    ###Record the best lambda
    stat[i,2]=fit$lambda
    
    ###Record coeffitients of predictors
    coef[1:(dimx[2]+1),1]=row.names(coef(fit))
    coef[1:(dimx[2]+1),i+1]=as.matrix(coef(fit))
    
    ###Calculate the mean squared error of the fitted model
    if(df==0){est=0}
    else{est=predict(fit,newx=x,s=cv.fit$lambda.min,type="link")}
    if(est==0){mse="NA"}
    else{resi=y-est
         mse=sqrt(sum((resi)^2)/dimx[1])}
    stat[i,3]=mse
    stat[i,4]=fit$dev.ratio
  }   
  
  ###Write CSV files for statistics of all fitted models   
  stat_file <- paste("stat_",drug,".csv",sep="")
  write.csv(stat, file=stat_file)
  #coef_file=paste("coef_",drug,".csv",sep="")
  #write.csv(coef, file=coef_file)
  return(coef)
}
