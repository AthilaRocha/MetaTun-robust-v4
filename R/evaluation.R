###############################The next two functions below have not been used for the last two versions, ###################################
#############################so they will be maintaned here as comments (they may be useful in the future) ##################################

#EvaluateOne <- function(candidate,FUN=objectivefun){
#  return(FUN(candidate))
#}

#EvaluateFunction <- function(FUN=func,allcandidates,fun="generated"){
  
#  points <- allcandidates[,1:(ncol(allcandidates)-1)]
#  if (fun=="generated")  fvalue <- apply(points[1:nrow(points),],FUN=FUN,MARGIN=1)
#  else fvalue <- apply(points[1:nrow(points),],FUN=FUN,instance=0,exact=1,MARGIN=1)
       
#  return(fvalue)
#}

################################This function evaluates the candidates according to the function "functuning"###################################
###########functuning must be a function which receives two arguments (candidate, instance), calls the target algorithm and return #############
#####################################################its objective function value###############################################################
#######The evauation of candidates on instances is made using parallel computation, if the initial parameter####################################
######################################################## "ncores" of MetaTun is > 1.############################################################
EvaluateCandidates<-function(Results,allcandidates,instances,instancesvalues,candidates,firstinstance,lastinstance){
  
  inst <- instancesvalues[instances[firstinstance:lastinstance]]
  cands <- vector("list",length(candidates))
  for(i in 1:length(cands)) {
    cands[[i]] <- c(allcandidates[candidates[i],(1:(ncol(allcandidates)))]) #This line will be used when testing a simulated model, because is needed the
                                                                            #ID of the candidate, which in this case is also a parameter of functuning
    #cands[[i]] <- c(allcandidates[candidates[i],(1:(ncol(allcandidates)-1))])
  }
  
  
  m <- foreach(i=inst,.export=ls(envir=globalenv()),.combine='rbind') %:% foreach(c=cands, .combine='cbind',.export=ls(envir=globalenv())) %dopar% {functuning(c,i)}
  
  if (ncol(Results)==0) {                                      ######Initial Evaluation of candidates on instances
    x <- rep(NA,(lastinstance-firstinstance+1))
    Results <- matrix(rep(x,nrow(allcandidates)), ncol = nrow(allcandidates))
    Results <- as.matrix(m)
    colnames(Results) <- c(candidates[1]:candidates[length(candidates)])
  }
  else{ 
    if ((allcandidates[nrow(allcandidates),ncol(allcandidates)])%in%(colnames(Results))){  ####Evaluation of valid candidates on new instances
      for (i in 1:nrow(m)) Results <- rbind(Results,NA,names(Results))
      Results[(nrow(Results)-length(inst)+1):nrow(Results),candidates] <- m
    }
    else {                                                              ###Evaluation of new candidates on instances already sampled
      oldcolumns <-ncol(Results)
      Results <- cbind(Results,m,names(Results))
      colnames(Results)[(oldcolumns+1):(ncol(Results))] <- c(candidates[1]:candidates[length(candidates)])  
    }
  }
  print(Results)
  return(Results)
}


################################Function that returns the Elite candidates, based on ranks of candidates#######################################
ReturnElite <- function(Results,nbcandidates,allcandidates){
  
  ranks <- matrix(ncol=ncol(Results),nrow=nrow(Results))
  for(i in 1:nrow(Results)) ranks[i,] <- rank(Results[i,])
  y <- colSums(ranks) 
  
  ##if the average of normalized results is used
  #avgs <- apply(Results,2,FUN=mean)
  #elite <- order(avgs[1:nbcandidates])
  
  elite <- order(y)[1:nbcandidates]
  return(allcandidates[elite,])
}


################################Function that scaled in the [0,1] interval the objective function values#######################################
############################of candidates on instances. For each instance, all results on that instance are transformed########################
##################################################### in values in the [0,1] interval##########################################################
NormalizeData <- function(Results,direction="rows"){
  
  R <- Results
  
  if (direction=="rows")
    for(i in 1:(nrow(R))) {
      
      minim <- min(Results[i,],na.rm=TRUE)
      maxi <- max(Results[i,],na.rm=TRUE)
      
      for (j in 1:(ncol(R))) if (!is.na(R[i,j])) R[i,j] <- (R[i,j] - minim)/(maxi-minim)
    }
  else
  {
    for(i in 1:(ncol(R))) {
      
      minim <- min(Results[,i],na.rm=TRUE)
      maxi <- max(Results[,i],na.rm=TRUE)
      
      for (j in 1:(nrow(R))) if (!is.na(R[j,i])) R[j,i] <- (R[j,i] - minim)/(maxi-minim)
    }
  }
  
  return(R)
}