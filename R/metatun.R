##This is the "main" function
##The parameter method determines the type of regression method used. "ols"= least squares regression,
## "quant" for quantile regression, "irls" for IRLS (Iterated Reweighted Least Square) regression, 
## "lasso" for LASSO (Least Absolute Shrinkage and Selection Operator) regression and for "ridge" 
## for L2 regularization regression.  


MetaTun <-function(optpar=Inf,fvalueopt=Inf,Paramdata,instancesampled,order,budget,method="ols",tau=0.5,typeofresult="mean",ncores=1,packuser=NA,funcuser=NA){

  
  ###################Used only with simulated models########################
  if (fvalueopt!=Inf) {
    dist_eucli <- data.frame(exec=numeric(0),iter=numeric(0),dist=numeric(0))
    gapt_opt <- data.frame(exec=numeric(0),gap=numeric(0))
  }
  dist <- Inf
  ##########################################################################
  
  #set.seed(1234)
  Checkpackages()
  
  ##################################################Starting parallel environment######################################################
  library(doParallel)
  maxcoresmachine <- detectCores()-1
  
  if (is.character(ncores))          
    if (ncores=="MAX") no_cores <-maxcoresmachine
      else stop("Error! The unique character value accepted to ncores is 'MAX'")
  else 
    if (ncores%%1!=0) stop("Error! The number of cores must be an integer value >=1 or equal to 'MAX'")
       else 
         if (ncores<1) stop("The value of ncores must be an integer >=1")
           else 
             if (maxcoresmachine<ncores) no_cores <- maxcoresmachine
               else no_cores <- ncores
  
  cat("\n")
  cat("Number of cores used:")
  cat(no_cores)
  cat("\n")
  
  if ((is.numeric(ncores))&&(ncores%%1==0)&&(no_cores<ncores)){
    warning("The number of cores specified is greater than number-of-cores-in-the-machine-1. Using number-of-cores-in-the-machine-1")
  }
  if (no_cores==1) warning("Using only 1 core. This is equivalent to a sequential processing")
  
  
  ################################################registering the cluster of cores#########################################################
  cl <- makeCluster(no_cores,type="PSOCK")
  registerDoParallel(cl)
  clusterExport(cl,c("functuning"))                            ##########exporting "functuning" to the clusters#########
  if (!is.na(funcuser[1])){                                  ##########exporting other functions to the clusters######
    for (i in 1:length(funcuser)){
      namefunc <- funcuser[i]
      clusterExport(cl,c(as.character(namefunc)))
    }
  } 
  
  if (!is.na(packuser[1])) {                                   ############exporting not native R packages##############
    for (i in 1:length(packuser)) {
      namepackage <- packuser[i]
      clusterExport(cl,c("namepackage"),envir=environment())
      clusterEvalQ(cl, library(namepackage,character.only=TRUE))
    }
  }
  #########################################################################################################################################
  
  
  
  ##filenamepoints <- paste("points.csv",sep="")   ##arquivo de log dos candidatos
  
  nextid <-0
  
  if (length(instancesampled)<5){
    stop("Error!! MetaTun needs that the training instances set has at least 5 instances")
  }
  
  instancesvalues <- instancesampled
  instances <- sample(length(instancesvalues),length(instancesvalues))
  
  ####This code refers to the situation when is accepted repetition of instances
  # if (length(instancesvalues)<100) {
  #   instances <- sample(length(instancesvalues),length(instancesvalues))
  #   instances <- c(instances,sample(length(instancesvalues),100-(length(instances)),replace=TRUE))
  # }
  # else instances <- sample(length(instancesvalues),length(instancesvalues))
  
  print("Order of using instances (according to Instances IDs)")
  print(instances)
  
  datatuning <- ReadParametersData(Paramdata)
  
  initialcandidates <- (choose((length(datatuning$name)+order),order))*3
  compbudget <- budget
  
  if ((typeofresult!="mean") & (typeofresult!="sumrankings") & (typeofresult!="meanrankings")){
    stop("Error!! The parameter 'typeofresult' must be equal to 'mean' or 'sumrankings' or 'meanrankings'")
  }
  
  if ((initialcandidates*5)>budget){
    stop("Error!! The computational budget is not enough to perform the evaluation of initial candidates")
  }
  
  if ((method!="ols") & (method!="quant") & (method!="lasso") & (method!="ridge")){
    stop("Error!! The regression method must be: \"ols\", \"irls\", \"quant\", \"lasso\", or \"ridge\"  ")
  }
  
  firstcandidate <- 1
  firstinstance <- 1
  lastinstance <- 5
  instancespertest <- 1
  
  nbcandidates <- ceiling(initialcandidates*.2)
  nbeliminated <- nbcandidates                    ####number of candidates eliminated at each iteration
  eliminated <- numeric(0)                        ####vector of candidate IDs eliminated after each iteration
  
  it <- 1
  successful <- 1
  
  while(TRUE) {
    
        cat("\n")
        cat("Iteration nro. ")
        cat(it)
        cat("\n")
        
        if (it==1){
          
           validmodel <- FALSE
           ntrylhs <- 1
          
           while((!validmodel)&&(compbudget>=(initialcandidates*lastinstance))){
             allcandidates <- matrix(ncol=(length(datatuning$name)+1),nrow=0)
             allcandidates<-GenInitialCandidates(initialcandidates,datatuning,allcandidates)
             Results <- matrix(ncol=0, nrow=0)
             
             cat("\n")
             cat("Evaluating Initial candidates...")
             cat("\n")
             Results <- EvaluateCandidates(Results,allcandidates,instances,instancesvalues,c(firstcandidate:allcandidates[nrow(allcandidates),ncol(allcandidates)]),firstinstance,lastinstance)
             #print(Results)
             
             Resultsnorm <- NormalizeData(Results)
             
             cat("\n")
             cat("Evaluation of candidates after scaling results in an [0,1] scale")
             cat("\n")
             print(Resultsnorm)
             
             cat("\n")
             cat("\n")
             cat("Regression Modeling...")
             cat("\n")
             
             model<-RegModel(Resultsnorm,allcandidates,datatuning,typeofresult,order,method,tau,weights=NA)
             cat("...Done!")
             
             
                                   ##I am using summary(model, se="ker") instead of other methods when obtaining 
                                   ##coefficients standard errors in "quant" regression.
                                   ##This method achieved the best performance considering confidence intervals X
                                   ##runtime.
                                   
                                   #About a warning when using "nid" method in summary of quantile regression (rq function) 
                                   #for estimating standard errors. 
                                   #This explanation was found on rq  FAQ.   http://www.econ.uiuc.edu/~roger/research/rq/FAQ
                                   #"What does the message "Non-positive fis" mean?
                                   # When method ="nid" is used in summary local density estimates are made at
                                   # each x_i value, in some cases these estimates can be negative and if so
                                   # they are set to zero.  This is generally harmless, leading to a somewhat
                                   # conservative (larger) estimate of the standard errors, however if the 
                                   # reported number of non-positive fis is large relative to the sample size
                                   # then it is an indication of misspecification of the model.
             
             
             if (method=="quant")  modelsum <- summary(model, se="ker")  #modelsum <- summary(model, se="boot", bsmethod="wxy", R=500)  
             else if (method=="ols") modelsum <- summary(model)
             else modelsum <- model
             
             if (is.element(method,c("ols","quant"))) {
                 modelsum <- PruneModel(modelsum,method)
                
                 ##verifying if a model is a valid model
                 if ((length(modelsum[[2]])>1) || ((length(modelsum[[2]])==1)&(modelsum[[2]][1]!="(Intercept)")) )
                   validmodel <- TRUE
             }
             else if (method=="ridge") validmodel <- TRUE
                    else if (!is.na(modelsum[[1]])) validmodel <- TRUE
                         #if (((length(which(modelsum[[4]]==0)))<((length(modelsum[[4]]))-1)) || (((length(which(modelsum[[4]]==0)))==((length(modelsum[[4]]))-1))&&(modelsum[[4]][1]==0))) validmodel <- TRUE
                            ###if model is lasso, it is a valid model if at least one term different of intercept is different from zero
               
                ##print("modelo após validação - iteração 1")
                ##print(modelsum)
             
             compbudget <- budget -((lastinstance*ncol(Resultsnorm))*ntrylhs)
             ntrylhs <- ntrylhs + 1
           }
          
           if (validmodel==FALSE) {
                print("It was impossible to generate a first valid regression model using the computational budget provided and the regression modeling strategy. Try another kind of regression modeling.")
                successful <- 0
                break
           }
           else { 
              cat("\n")
              cat("Initial Candidates")
              cat("\n")
              print(allcandidates)
           }
        }
        else {
          
          budgetneeded <- (initialcandidates*instancespertest)
          
          if ((lastinstance<=length(instancesvalues))&&(compbudget>=budgetneeded)){
          
              cat("\n")
              cat("Evaluating all candidates on new instances...")
              cat("\n")
          
              Results <- EvaluateCandidates(Results,allcandidates,instances,instancesvalues,setdiff((allcandidates[1,ncol(allcandidates)]:allcandidates[nrow(allcandidates),ncol(allcandidates)]),eliminated),firstinstance,lastinstance)
          
              #print("Evaluation of candidates before scaling results in a [0,1] scale")
              #print(Results)
          
              Resultsnorm <- NormalizeData(Results)
              #Resultsnormchecked <- CheckPseudoreplication(Resultsnorm,instances,firstinstance,lastinstance)
          
              cat("\n")
              cat("Evaluation of candidates after scaling results in an [0,1] scale")
              cat("\n")
              print(Resultsnorm)
              compbudget <- compbudget - budgetneeded
              
              cat("\n")
              cat("------------------------------------------Computational Budget remaining-----------------------------------------")
              cat("\n")
              cat(compbudget)
              cat(" Experiments")
          }
          
          cat("\n")
          cat("\n")
          cat("Regression Modeling...")
          cat("\n")
          
          
          weights <- GetWeights(Resultsnorm)
          
          model<-RegModel(Resultsnorm,allcandidates,datatuning,typeofresult,order,method,tau,weights)
          cat("...Done.")
          
          
          if (method=="quant")  modelsum <- summary(model, se="ker") #modelsum <- summary(model, se="boot", bsmethod="wxy", R=500) 
            else if (method=="ols") modelsum <- summary(model)
                 else modelsum <- model
          
          ###if model is lasso, it is a valid model if at least one term different of intercept is different from zero
          ###In the case of not getting non-zero coefficients, the method have to be terminated.
          if (method=="lasso")
              #if (((length(which(modelsum[[4]]==0)))==(length(modelsum[[4]]))) || (((length(which(modelsum[[4]]==0)))==((length(modelsum[[4]]))-1))&&(modelsum[[4]][1]!=0))) { 
              if (is.na(modelsum[[1]])){
                 print("The lasso modeling generated a model with all zero coefficients or only the intercept non-zero.")
                 print("Maybe the data are somewhat bad behaved to use lasso. Please try to use the other options of regression modeling")
                 break
              }
        }
        
        cat("\n")
        cat("Original model obtained...")
        cat("\n")
        cat("Generating derivated models...")
        cat("\n")
        
        if (is.element(method,c("ols","quant")))
          if (it==1) models<-GenerateModels(modelsum[[1]],nbcandidates,method)
             else models <- GenerateModels(modelsum$coefficients[,1:2],nbcandidates,method)
        else models<-GenerateModels(modelsum[3:4],nbcandidates,method)
        
        cat("...Done!")
        
        #cat("\n")
        #cat("Original Model and derivated models")
        cat("\n")
        #print(models)
        
        newcandidates <- matrix(ncol=(length(datatuning$name)+1),nrow=0)
        
        if (is.element(method,c("ols","quant"))) 
           if (it==1) modeldescrip <- modelsum[[2]]
               else modeldescrip <- rownames(modelsum$coefficients)
        else modeldescrip <- modelsum[1:2]
        
        funcorigin <- BuildFunction(modeldescrip,datatuning,models[[1]],method)
        equationsforopt <- rep(funcorigin,length(models))
        
        for (l in 2:length(models)) {
          funcmodel <- BuildFunction(modeldescrip,datatuning,models[[l]],method)
          equationsforopt[l]<-funcmodel
        }
        
        ####if the amount of instances required until the next iteration exceeds the total of instances available, it will be sampled one to one at each 
        ####new iteration
        if ((lastinstance+instancespertest)>length(instancesvalues)) instancespertest <- 1
        
        if (lastinstance>length(instancesvalues)) budgetneeded <- (nbcandidates*length(instancesvalues)) else budgetneeded <- (nbcandidates*lastinstance) 
        
        ##old version, when candidates were not eliminated of evaluation phase and was allowed repetition of instances
        #budgetneeded <- ((nbcandidates*lastinstance) + ((nbcandidates+ncol(Resultsnorm))*instancespertest))
        
        if (compbudget>=budgetneeded) {
            cat("\n")
            cat("Optimizing Models and generating new candidates...")
            cat("\n")
            newcandidates <-as.matrix(Optimize(equationsforopt,datatuning))
            cat("...Done!")
            
            nextid<-initialcandidates +(it-1)*nbcandidates + 1
            firstcandidate <- nextid
            newcandidates[,(ncol(newcandidates))] <- c((nextid):(nextid+nrow(newcandidates)-1)) 
            allcandidates <- rbind(allcandidates,newcandidates)
    
            cat("\n")
            cat("all candidates after generating new candidates by Optimization")
            cat("\n")
            print(allcandidates)
            
            cat("\n")
            cat("Evaluating the new candidates on all instances sampled...")
            cat("\n")
            cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-----------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
            cat("\n")
            
            if (lastinstance>length(instancesvalues)) lastinstance <- length(instancesvalues) ####forces lastinstance to be the last available instance, if it is greater
            Results <- EvaluateCandidates(Results,allcandidates,instances,instancesvalues,c(firstcandidate:allcandidates[nrow(allcandidates),ncol(allcandidates)]),1,lastinstance)
            #print(Results)
            Resultsnorm <- NormalizeData(Results)
            
            eliminated <- c(eliminated,EliminateCandidates(allcandidates,Results,nbeliminated,eliminated))
            cat("\n")
            cat("Candidates not evaluated anymore")
            cat("\n")
            print(eliminated)
         
            compbudget <- compbudget - budgetneeded
               
            cat("\n")
            cat("------------------------------------------Computational Budget remaining-----------------------------------------")
            cat("\n")
            cat(compbudget)
            cat(" Experiments")
            
            firstinstance <- lastinstance+1
            lastinstance <- firstinstance+instancespertest-1
        }
        else {
               cat("\n")
               cat("The computational budget remaining is not sufficient to perform another iteration")
               cat("\n")
               cat("The method will be finished")
               cat("\n")
               
               Resultsnorm <- NormalizeData(Results)
               #Resultsnormchecked <- CheckPseudoreplication(Resultsnorm,instances,firstinstance,lastinstance)
               bestiter <- ReturnElite(Resultsnorm,1,allcandidates)
               
               
               #######Only used in the case of simulated algorithm########
               if (fvalueopt!=Inf) {
                 dists <- numeric(nrow(allcandidates))
                 for (l in 1:(nrow(allcandidates))) dists[l] <- DistEucli(allcandidates[l,1:((ncol(allcandidates))-1)],optpar) 
                 
                 if (min(dists)<dist) {
                   dist <- min(dists)  
                   closestcand <- allcandidates[(order(dists)[1]),1:((ncol(allcandidates))-1)]
                 }
                 
                 dist_eucli[nrow(dist_eucli)+1,] <- c(NA,it,dist)
                 
               }
               ###########################################################
               
               break
               
        }
        
        bestiter <- ReturnElite(Resultsnorm,1,allcandidates)
        
        #######Only used in the case of simulated algorithm########
        if (fvalueopt!=Inf) {
          dists <- numeric(nrow(allcandidates))
          for (l in 1:(nrow(allcandidates))) dists[l] <- DistEucli(allcandidates[l,1:((ncol(allcandidates))-1)],optpar) 
          
          if (min(dists)<dist) {
            dist <- min(dists)  
            closestcand <- allcandidates[(order(dists)[1]),1:((ncol(allcandidates))-1)]
          }
          
          dist_eucli[nrow(dist_eucli)+1,] <- c(NA,it,dist)
          
        }
        ###########################################################
        
        it <- it + 1
  }

  stopCluster(cl)
  if (successful) {
    
      #readline(prompt="Press [enter] to continue")
    
      e<-ReturnElite(Resultsnorm,3,allcandidates)
      best_par <- e[1,1:(ncol(e)-1)]
      colnames(e) <- c(as.vector(datatuning$name),"Id")
      best_par <- cbind(best_par)
      colnames(best_par)[ncol(best_par)] <- c("exec")
      best_par["exec"] <- NA
      best_par <-as.data.frame(t(best_par))
      
      
      #######Only used in the case of simulated algorithm########
      if (fvalueopt!=Inf) {
        
        valuefbest <- functuning(closestcand,0,exact=1)
        if (fvalueopt==0) gap <- abs(valuefbest-fvalueopt)
        else  gap <- (abs(valuefbest-fvalueopt)/abs(fvalueopt))
        gapt_opt[1,] <- c(NA,gap)
      }
      ############################################################
      
      cat("\n")
      cat("Final model obtained")
      cat("\n")
      cat("-====================================================================================================================================-")
      cat("\n")
      print(models[[1]])
      cat("-====================================================================================================================================-")
      cat("\n")
      
      
      cat("\n")
      cat("Best Candidates")
      cat("\n")
      cat("-====================================================================================================================================-")
      cat("\n")
      print(e)
      cat("-====================================================================================================================================-")
      cat("\n")

      cat("\n")
      cat("-====================================================================================================================================-")
      cat("\n")
   
      cat("\n")
      cat("The structure of parameters to be tuned. Describing the names, ranges, values(in case of categorical ones) and the type of parameters\n")
      cat("-====================================================================================================================================-")
      cat("\n")
      print(datatuning)
      cat("-====================================================================================================================================-")
      cat("\n")
      cat("\n")
      
      cat("Returning the best parameter values the last model")
      cat("\n")
      
      if(fvalueopt==Inf) return(list(best_par, funcorigin))
      else return((list(best_par,dist_eucli,gapt_opt,closestcand,funcorigin)))   ###used in case of simulated algorithms
  }
  else {
    print("Probably the model is not suitable to the data, or the data are somewhat bad behaved")
    print("Try to use a higher computacional budget")
    return(list(NA,NA,NA,NA,NA))
  }
}
