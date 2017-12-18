###########################Generation of initial candidate configurations by LHS method######################################
#########################This function generates the initial candidates using an LHS approach################################
############This function gets a number of candidates and a data frame with the information about the parameters#############
##########################The candidates are stored into the allcandidates structure#########################################
GenInitialCandidates <- function(nbcandidates,datatuning,allcandidates){

  if ((as.integer(nbcandidates)>0) & (!is.na(as.integer(nbcandidates)))) {

    library(lhs)
    parameters<-length(datatuning$name)
    candidates <-randomLHS(nbcandidates, parameters)
    candidates <- cbind(candidates,NA)
    for (j in 1:parameters)
       if (datatuning$type[j]=="r")  candidates[,j] <- as.numeric(round((datatuning$min[j]  + (datatuning$max[j]-datatuning$min[j])*candidates[,j]), digits=2))
       else if (datatuning$type[j]=="i") candidates[,j] <- as.numeric(round(datatuning$min[j]  + (datatuning$max[j]-datatuning$min[j])*candidates[,j]))
              else {
                     valuesparam <- as.vector(datatuning$values[j])
                     valuesparam <- strsplit(valuesparam,",")
                     nbvalues <- length(valuesparam[[1]])
                     interval <- 1/nbvalues
                     candidates[,j] <-valuesparam[[1]][ceiling(candidates[,j]/interval)]
                   }
    candidates[,(parameters+1)] <- c((nrow(allcandidates)+1):(nrow(allcandidates)+nbcandidates))
    allcandidates <-rbind(allcandidates,candidates)
    return(allcandidates)
  }
  else {
    cat("Error, number of candidates invalid")
  }
}

###############################Function that eliminates a number of worst candidates, according to the their ranks#############################
EliminateCandidates <- function(allcandidates,Results,nbeliminated,alreadyeliminated){
  
  eliminated <- numeric(nbeliminated)
  
  if (length(alreadyeliminated>0)) R <- Results[,-(which((colnames(Results)%in%alreadyeliminated)==TRUE))]
  else R <- Results
  
  ranks <- matrix(ncol=ncol(R),nrow=nrow(R))
  colnames(ranks) <- colnames(R)
  for(i in 1:nrow(R)) ranks[i,] <- rank(R[i,])
  
  y <- colSums(ranks) 
  names(y) <- colnames(ranks)
  
  eliminated <- as.numeric(names(y[order(-y)[1:nbeliminated]]))
   
#   print("Positions of candidates to be eliminated")
#   print(eliminated)
  return(eliminated)
}