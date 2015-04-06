######################validation for RNA############
validRNA <- function(n){
  
  validori[,1]=length(predtr)
  
  Hori=cbind(Hdrug[1:19,(n-1)],HRNA[1:19,])
  Hrest=cbind(Hdrug[20:45,(n-1)],HRNA[20:45,])
  
  Tori=na.omit(Hori)
  names(Tori)[1]=drug
  Trest=na.omit(Hrest)
  names(Trest)[1]=drug
  
  if(length(predtr)==1){
    ###Extract the list of selected predictors from Lasso's output
    selct=unlist(strsplit(capture.output(cat(drug,predtr)), " "))
    selct_set=data[selct]
    
    ###The explanatory and response variables
    y=data.matrix(selct_set[,1])
    names(selct_set)[1]="y"
    
    ###Fit the data into a linear model and calculate the R^2 
    
    fit=lm(y~.,selct_set)
    r2=1-sum(fit$residuals^2)/sum((y-ave(y))^2)
    
    ###Print the results
    print(drug)
    print("Best 1 predictor")
    print(names(selct_set)[2])
    print("R^2")
    print(r2)
    cat("\n")
    
    ###Validation
    new_x1=Tori[names(selct_set)[2]]         
    prd1=predict(fit,new_x1)
    resi1=Tori[,1]-prd1
    validori[,2:(length(resi1)+1)]=matrix(resi1)
    
    new_x2=Trest[names(selct_set)[2]]         
    prd2=predict(fit,new_x2)
    resi2=Trest[,1]-prd2
    validrest[,1:length(resi2)]=matrix(resi2)
    
  }
  
  ###If there is more than one predictor selected by Lasso, then
  if(length(predtr)>1){
    
    ###Extract the list of selected predictors from Lasso's output
    selct=unlist(strsplit(capture.output(cat(drug,predtr)), " "))
    selct_set=data[selct]
    
    ###The explanatory and response variables 
    y=data.matrix(selct_set[,1])
    x=data.matrix(selct_set[,-1])
    nn=names(selct_set[,-1])
    
    ###Run Leaps and Bound to obtain the best predictors, results sort by R^2
    M=leaps(x,y,nbest=1,method="r2",strictly.compatible=FALSE)
    
    ###Get the best one predictor w/ highest R^2
    nb1=data.matrix(nn[M$which[1,]])
    ###R^2 of best predictor
    r1=M$r2[1]
    
    ###Get the best group of two predictors w/ highest R^2 
    nb2=data.matrix(nn[M$which[2,]])
    ###R^2 of the best group of two predictors
    r2=M$r2[2]
    
    ###Print the results
    print(drug)
    print("Best 1 predictor")
    print(nb1)
    print("R^2")
    print(r1)
    print("Best pair of predictors")
    print(nb2)
    print("R^2")
    print(r2)
    
    ###If there are more than two predictors selected by Lasso, then
    if(length(predtr)>=3){          
      ### Get the best group of three predictors w/ highest R^2
      nb3=data.matrix(nn[M$which[3,]])
      r3=M$r2[3]
      print("Best group of three predictors")
      print(nb3)
      print("R^2")
      print(r3)
      
      ###the best fitted linear model
      xx=data.matrix(selct_set[nn[M$which[3,]]])
      datalm=data.frame(y,xx)
      fit=lm(y~.,data=datalm)
      
      ###validation
      new_x1=Tori[nn[M$which[3,]]]         
      prd1=predict(fit,new_x1)
      resi1=Tori[,1]-prd1
      validori[,2:(length(resi1)+1)]=matrix(resi1)
      
      new_x2=Trest[nn[M$which[3,]]]         
      prd2=predict(fit,new_x2)
      resi2=Trest[,1]-prd2
      validrest[,1:length(resi2)]=matrix(resi2)          
    }
    else{
      ###the best fitted linear model
      xx=data.matrix(selct_set[nn[M$which[2,]]])
      datalm=data.frame(y,xx)
      fit=lm(y~.,data=datalm)
      
      ###validation
      new_x1=Tori[nn[M$which[2,]]]         
      prd1=predict(fit,new_x1)
      resi1=Tori[,1]-prd1
      validori[,2:(length(resi1)+1)]=matrix(resi1)
      
      new_x2=Trest[nn[M$which[2,]]]         
      prd2=predict(fit,new_x2)
      resi2=Trest[,1]-prd2
      validrest[,1:length(resi2)]=matrix(resi2)
    }
  }        	    
  cat("\n")
  valid=cbind(validori,validrest)
  return(valid)
}
