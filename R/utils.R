###################################Function that verifies if all required packages are installed.################################
Checkpackages <- function(){
  checking <- c(require("hqreg"), require("MASS"), require("quantreg"), require("lhs"), require("gtools"), require("doParallel"))
  if (!all(checking)){
    stop("Error!! Packages required not installed. Packages required: hqreg, MASS, quantreg, lhs, gtools and doParallel")
  }
}


#########function that returns all combinations of parameters described in datatuning, according to the order of regression######
#####################This is used as an auxiliar function when using ridge and lasso regression##################################
Termsregression <- function(datatuning,order){
  allcombs <- list()
  
  for(i in 1:order) {
    combs <- combinations(n=length(datatuning),r=i,v=datatuning,repeats.allowed = TRUE)
    allcombs[[i]] <- combs
  }
  
  return(allcombs)
}


#############This function returns a matrix with the values of form: [x11, x21,...,xn1, x11*x11, x11*x21,...]
#############                                                        [x12, x22,...,xn2, x12*x12, x12*x22,...]
#############                                                        [x1n, x2n,...,xnn, x1n*x1n, x1n*x2n,...]
#############according to the parameters described in datatuning and the terms that are valid in the ridge/lasso 
#############regression, according to "termsreg" parameter. This matrix is used by hqreg and cv.hqreg to obtain 
#############the ridge/lasso models
Buildridgelasso <- function(dataformodel, termsreg){
  
  nrowstermsreg <- 0
  for(i in 1:length(termsreg)) nrowstermsreg <- nrowstermsreg+nrow(termsreg[[i]])
  
  X <- matrix(ncol=nrowstermsreg,nrow=as.integer(nrow(dataformodel)))
  
  col <-1
  for(j in 1:length(termsreg)){
    for(i in 1:nrow(termsreg[[j]])){ 
         expr <- character(0)
         for(k in 1:ncol(termsreg[[j]])){
           expr <- paste0(expr,"dataformodel$")
           expr <- paste0(expr,termsreg[[j]][i,k],collapse=NULL)
           expr <- paste0(expr,"*",collapse=NULL)
         }
    
         expr <- substr(expr,1,(nchar(expr)-1))
         X[,col] <- eval(parse(text=expr))
         col <- col+1
    }
  }
  
  return(X)
}


##############This function build a string which is a definition of a function, according to the R sintax.##############
##############The parameter of this function are those defined by datatuning, and the equation is formed ###############
##########################based on the parameters modelterms, modelvalues and method####################################
BuildFunction <-function(modelterms,datatuning,modelvalues,method){
  
    if (is.element(method,c("ols","quant","irls"))){
       if (modelterms[1]=="(Intercept)") {
           expr <- as.character(modelvalues[1])
           indexestimator <- 2
       }
       else {
         expr <- as.character(0)
         indexestimator <- 1
       }
         
       for (i in indexestimator:length(modelterms)) modelterms[i] <- substr(modelterms[i], nchar(modelterms[i])-(length(datatuning$name)*2)+2, nchar(modelterms[i]))        
         
       expr <- paste0(expr,"+",collapse=NULL)  
       
       for (i in indexestimator:(length(modelterms))) {
         
         eachdescrip <- modelterms[i] 
         k <- 1
         for (j in seq(1,(nchar(eachdescrip)),by=2)){
           exponent <- substr(eachdescrip,j,j)
           if ((as.numeric(exponent))==0) expr <- paste0(expr,"1",collapse=NULL)
           else {
             if ((as.numeric(exponent))>1) {
               expr <- paste0(expr,datatuning$name[k],collapse=NULL)
               expr <- paste0(expr,"^",collapse=NULL)
               expr <- paste0(expr,as.numeric(exponent),collapse=NULL)
             }
             else expr <- paste0(expr,datatuning$name[k],collapse=NULL)
           }
           expr <- paste0(expr,"*",collapse=NULL)
           
           k <- k+1
         }
         expr <- paste0(expr,modelvalues[i],collapse=NULL)
         expr <- paste0(expr,"+",collapse=NULL)
       }
       expr <- substr(expr,1,(nchar(expr)-1))   
    }
    else {
      if (modelterms[[1]]!=0) expr <- as.character(modelvalues[1])
      else expr <- as.character(0)
      
      expr <- paste0(expr,"+",collapse=NULL)
      
      indexestimator <- 2
      
      for(j in 1:length(modelterms[[2]])){
          for(k in 1:ncol(modelterms[[2]][[j]])){
            #expr <- paste0(expr,"dataformodel$")
            expr <- paste0(expr,modelterms[[2]][[j]][1,k],collapse=NULL)
            expr <- paste0(expr,"*",collapse=NULL)
          }
          expr <- paste0(expr,as.character(modelvalues[indexestimator]),collapse=NULL)
          expr <- paste0(expr,"+",collapse=NULL)
          indexestimator <- indexestimator + 1   
      }
      expr <- substr(expr,1,(nchar(expr)-1))
    }
  
    #print("expressao") 
    #print(expr)
  
    func <- "f1 <- function(x){\n"
    
    p <- character(0)
    for(i in 1:(length(datatuning$name))){
      p <- paste0(p,datatuning$name[i],collapse=NULL)
      p <- paste0(p,"<- ",collapse=NULL)
      p <- paste0(p,"x[",collapse=NULL)
      p <- paste0(p,i,collapse=NULL)
      p <- paste0(p,"]\n")
    }
    
    func <- paste0(func,p,collapse=NULL)
    func <- paste0(func,"return(",collapse=NULL)
    func <- paste0(func,expr,collapse=NULL)
    func <- paste0(func,")\n}")
    
    return(func)
}

################This function calculates the Euclidian distance of a candidate to some target candidate##############
################It is used when using the parameter tuning method with simulated models##############################
#############################that represents the target-algorithm behavior###########################################
DistEucli <- function(candidate,target){
  return(sqrt(sum((candidate-target)^2)))
}


########This function identifies pseudoreplication and summarize the results of candidates on repeated instance#######
###########################using the mean of these candidates on the repeated instances###############################
CheckPseudoreplication <- function(Resultsnorm,instances,firstinstance,lastinstance){
  
  Resultsaux <- Resultsnorm
  rownames(Resultsaux) <- instances[1:lastinstance]
 
  Resultsaux <- aggregate(Resultsaux,by=list(rownames(Resultsaux)),mean)
  Resultsaux <- Resultsaux[,-1]
  
  Resultschecked <- as.matrix(Resultsaux)
  colnames(Resultschecked) <- c(1:ncol(Resultschecked))
  
  return(Resultschecked)
}
#######################################################################################################################


#####################################This function calculates the weights for each#####################################
#####################################candidate performance value when performing the###################################
#####################################################regression modeling###############################################
GetWeights <- function(Results,type=1){
  
  if (type==1)  {
    validresults <- apply(Results, function(Results) sum(!is.na(Results)),MARGIN=2)
    totalvalidresults <- sum(validresults)
    weights <- validresults/totalvalidresults
  }
  else {      ####antoher type of weights
    notvalidresults <- apply(Results, function(Results) sum(is.na(Results)),MARGIN=2)
    weights <- (nrow(Results)-notvalidresults)/nrow(Results)
  }
  
  return(weights)
}
#######################################################################################################################



