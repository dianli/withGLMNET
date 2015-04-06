################################# Leaps and Bound ########################################
###best linear model with one, two, three predictors
###If there is only one predictor selected by Lasso 
  leapsbd <- function(n){
  	if(length(predtr)==1){
    selct=unlist(strsplit(capture.output(cat(drug,predtr)), " "))
    selct_set=data[selct]
    y=data.matrix(selct_set[,1])
    x=data.matrix(selct_set[,-1])
    fit=lm(y~x)
    r2=1-sum(fit$residuals^2)/sum((y-ave(y))^2)
    print(drug)
    print("Best 1 predictor")
    print(names(selct_set)[2])
    print("R^2")
    print(r2)
    cat("\n")
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
    
    ###Print the result
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
    if(length(predtr)>2){          
          ### Get the best group of three predictors w/ highest R^2
          nb3=data.matrix(nn[M$which[3,]])
          r3=M$r2[3]
          print("Best group of three predictors")
          print(nb3)
          print("R^2")
          print(r3)
          }   
    cat("\n")
    }  
}

###best linear model with one, two ... up to seven predictors
###If there is only one predictor selected by Lasso 
leapsbd7 <- function(n){
  if(length(predtr)==1){
    
    ###Extract the list of selected predictors from Lasso's output
    selct=unlist(strsplit(capture.output(cat(drug,predtr)), " "))
    selct_set=data[selct]
    
    ###The explanatory and response variables
    y=data.matrix(selct_set[,1])
    x=data.matrix(selct_set[,-1])
    
    ###Fit the data into a linear model and calculate the R^2 
    fit=lm(y~x)
    r2=1-sum(fit$residuals^2)/sum((y-ave(y))^2)
    
    ###Print the results
    print(drug)
    print("Best 1 predictor")
    print(names(selct_set)[2])
    print("R^2")
    print(r2)
    cat("\n")
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
    if(length(predtr)>2){          
      ### Get the best group of three predictors w/ highest R^2
      nb3=data.matrix(nn[M$which[3,]])
      r3=M$r2[3]
      print("Best group of three predictors")
      print(nb3)
      print("R^2")
      print(r3)
      if(length(predtr)>3){
        nb4=data.matrix(nn[M$which[4,]])
        r4=M$r2[4]
        print("Best group of four predictors")
        print(nb4)
        print("R^2")
        print(r4)
        if(length(predtr)>4){
          nb5=data.matrix(nn[M$which[5,]])
          r5=M$r2[5]
          print("Best group of five predictors")
          print(nb5)
          print("R^2")
          print(r5)
          if(length(predtr)>5){
            nb6=data.matrix(nn[M$which[6,]])
            r6=M$r2[6]
            print("Best group of six predictors")
            print(nb6)
            print("R^2")
            print(r6)
            if(length(predtr)>6){
              nb7=data.matrix(nn[M$which[7,]])
              r7=M$r2[7]
              print("Best group of seven predictors")
              print(nb7)
              print("R^2")
              print(r7)
            }
          }
        }
      }
    }    
    cat("\n")
  }  
}

################################leaps n bound CV################
lpsbdCV <- function(n){
  if(length(predtr)==1){
    selct=unlist(strsplit(capture.output(cat(drug,predtr)), " "))
    selct_set=data_train[selct]
    y=data.matrix(selct_set[,1])
    tx=data_train[predtr]
    fit=lm(y~.,tx)
    r2=1-sum(fit$residuals^2)/sum((y-ave(y))^2)    
    testx=data.frame(testx)
    testset=testx[predtr]
    y_hat=as.numeric(predict(fit,testset))
    e=(testy-y_hat)^2
    print(drug)
    print("Best 1 predictor")
    print(names(selct_set)[2])
    print("R^2")
    print(r2)
    cat("\n")
  }
  if(length(predtr)>1){
    
    ###Extract the list of selected predictors from Lasso's output
    selct=unlist(strsplit(capture.output(cat(drug,predtr)), " "))
    selct_set=data_train[selct]
    
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
    tx=data_train[as.character(nb1)]
    fit=lm(y~.,tx)
    testx=data.frame(testx)
    testset=testx[as.character(nb1)]
    y_hat=as.numeric(predict(fit,testset))
    e=(testy-y_hat)^2  
    ###Print the result
    print(drug)
    print("Best 1 predictor")
    print(nb1)
    print("R^2")
    print(r1)
    cat("\n")
  }
  return(e)
}

##############################leaps and bound on full predictor model
lpsbdfm <- function(n){
  if(length(predtr)==1){
    selct=unlist(strsplit(capture.output(cat(drug,predtr)), " "))
    selct_set=data[selct]
    y=data.matrix(selct_set[,1])
    x=data[predtr]
    fit=lm(y~.,x)
    r2=1-sum(fit$residuals^2)/sum((y-ave(y))^2)
    E2=sum(fit$residuals^2)/ncol
    print(drug)
    print("Best 1 predictor on full data model")
    print(names(selct_set)[2])
    print("R^2")
    print(r2)
    cat("\n")
  }      
  
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
    
    ###Print the result
    print(drug)
    print("Best 1 predictor on full data model")
    print(nb1)
    print("R^2")
    print(r1)
    cat("\n")
    
    tx=data[as.character(nb1)]
    fit=lm(y~.,tx)
    E2=sum(fit$residuals^2)/ncol   
  }
  return(E2) 
}  
