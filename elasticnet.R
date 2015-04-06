#######################elastic net#####################
elasticnet <- function(n){
 
  ###Set the dataset as a matrix
  data=data.matrix(data)
  
  ###Assign explantory and response variables
  y=data[,1]
  x=data[,2:dim(data)[2]]
  dimx=dim(x)
  
  ###Create tables of approperiate size to store results  
  stat_ave=matrix(0,101,5,dimnames=list(c(0:100),c("alpha","MSE","Deviance","df","lambda.min")))
 
  #choose an alpha with minimal MSE 
  for(t in 0:100)
  { #choose an alpha, alpha=0 ridge, alhpa=1 lasso
    alf=t/100
    stat_ave[(t+1),1]=alf
    
    #reduce the un-stability, run a loop of (nrep) repeats for each alpha
    stat_loop=matrix(0,nrep,4,dimnames=list(c(1:nrep),c("MSE","Deviance","df","lambda.min")))
    for(k in 1:nrep){
    	#do 18-folds cross-validation to find the lambda with minimal mse
        cv.fit=cv.glmnet(x,y,alpha=alf,nfolds=18)
        
        #fit the model with selected lambda
        fit=glmnet(x,y,alpha=alf,lambda=cv.fit$lambda.min)
        df=fit$df   #degree of fredom of the fitted model
        
        #calculate the mse of the fitted model: if coefficients of the model are not all zero, then mse=sqrt(sum((y-y_hat)^2)/n)
        if(df==0){mse="NA"
               stat_loop[k,1]=mse
               stat_loop[k,2]="NA"
               stat_loop[k,3]="NA"
               stat_loop[k,4]="NA"}else{
                 est=predict(fit,newx=x,s=cv.fit$lambda.min,type="link")
                 resi=y-est
                 mse=sqrt(sum((resi)^2)/dimx[1])
                 stat_loop[k,1]=mse
                 stat_loop[k,2]=deviance(fit)
                 stat_loop[k,3]=df
                 stat_loop[k,4]=cv.fit$lambda.min}
    }
    
    #store stats:mean MSE, Deviance, df, lambda.min over nrep.
    stat_loop=na.omit(matrix(as.numeric(stat_loop),nrow=nrep,ncol=4))
    stat_ave[(t+1),2:5]=apply(stat_loop,2,mean)            
  }
  
  #save the stats for each drug
  stat_file=paste("stat_ave_",drug,".csv",sep="")
  write.csv(stat_ave, file=stat_file) 
  
  #plot MSE vs alpha
  alpha=stat_ave[,1]
  MSE=stat_ave[,2]
  plot_file=paste("mse_alfa_",drug,".png",sep="")
  png(plot_file)
  plot(MSE~alpha,pch=20,cex=1,main=drug)
  dev.off()
  
  #choose the alpha with minimal mean MSE and fit a model
  stat_ave=na.omit(stat_ave)
  temp=data.matrix(stat_ave[(stat_ave[,2] %in% apply(stat_ave,2,min)),1:3])
  stat[N,2:4]=temp[,1]   
  alf_selct=as.numeric(stat[N,2])
  cv.fit.el=cv.glmnet(x,y,alpha=alf_selct,nfolds=18)
  fit.el=glmnet(x,y,alpha=alf_selct,lambda=cv.fit.el$lambda.min)
  df.el=fit.el$df
  stat[N,7]=df.el  
  
  ###Calculate the mean squared error of the fitted model  
  if(df.el==0){mse.el="NA"
               stat[N,5]=mse.el
               stat[N:(N+dimx[1]-1),8]=0}else{
                 est.el=predict(fit.el,newx=x,s=cv.fit.el$lambda.min,type="link")      
                 resi.el=y-est.el
                 mse.el=sqrt(sum((resi.el)^2)/dimx[1])
                 stat[N,5]=mse.el
                 stat[N,6]=deviance(fit.el)
                 stat[N:(N+dimx[1]-1),8]=est.el
                 ###Record coeffitients of predictors
                 coef=matrix(0,dimx[2],1)
                 row.names(coef)=row.names(coef(fit.el))[-1]
                 coef[,]=as.matrix(coef(fit.el))[-1]  
                 prd_fun=function(z) z[z!=0]
                 prd=coef[(coef %in% apply(coef,1,prd_fun)),]
                 stat[N:(N+length(prd)-1),9]=names(prd)}  
}   
