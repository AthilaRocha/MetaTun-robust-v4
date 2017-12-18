###########obtained from http://www.di.fc.ul.pt/~jpn/r/rbf/rbf.html###########
#######################returns a rbf model given the:#########################
##################### * observations x1, xN of dataset D  ####################
##################### * output value for each observation #################### 
##################### * number of centers                ##################### 
##################### * gamma value                      #####################
Rbf <- function(X, Output, K=10, typeofresult, gamma=1.0) {
  
  X <- as.data.frame(X)
  Y <- matrix(nrow=ncol(Output),ncol=1)
  
  
  if (typeofresult=="mean") for (j in 1:nrow(Y)) Y[j] <- mean(Output[,j])
  else {
    ranks <- matrix(ncol=ncol(Output),nrow=nrow(Output))
    for(i in 1:nrow(Output)) ranks[i,] <- rank(Output[i,])
    if (typeofresult=="sumrankings") Y[,1] <- colSums(ranks)
    else for (j in 1:ncol(ranks)) Y[j,1] <- mean(ranks[,j])
  }
  
  
  N     <- dim(X)[1] # number of observations
  ncols <- dim(X)[2] # number of variables
  
  
  if (K!=nrow(X)) {
    repeat {
      km <- kmeans(X, K)  # let's cluster K centers out of the dataset
      if (min(km$size)>0) # only accept if there are no empty clusters
      break
    }
    mus <- km$centers # the clusters points
  }
  else {
    mus <- X
  }
  
  Phi <- matrix(rep(NA,(K+1)*N), ncol=K+1)
  
#   if (K!=nrow(X)) Phi <- matrix(rep(NA,(K+1)*N), ncol=K+1)
#   else  {
#     Phi <- matrix(rep(NA,(K+1)*N), ncol=K+1)
#     #Phi <- matrix(rep(NA,(K)*N), ncol=K)
#   }
    
#   print(Phi)
#   print(Y)
  
  for (lin in 1:N) {
    Phi[lin,1] <- 1
    for (col in 1:K) {
      Phi[lin,col+1] <- exp( -gamma * norm(as.matrix(X[lin,]-mus[col,]),"F")^2 )
#       if (K!=nrow(X)) Phi[lin,col+1] <- exp( -gamma * norm(as.matrix(X[lin,]-mus[col,]),"F")^2 )
#       else {
#         Phi[lin,col+1] <- exp( -gamma * norm(as.matrix(X[lin,]-mus[col,]),"F")^2 )
#         #Phi[lin,col] <- exp( -gamma * norm(as.matrix(X[lin,]-mus[col,]),"F")^2 )
#       }
    }
  }
  
#print(Phi)
  
#w <- solve(t(Phi) %*% Phi) %*% t(Phi) %*% Y  # find RBF weights
  
library("MASS")
w <- ginv((t(Phi)%*%Phi), tol = sqrt(.Machine$double.eps)) %*% t(Phi) %*% Y
  
list(weights=w, centers=mus, gamma=gamma)  # return the rbf model
}



#############Function that return a prediction of a set X of observations, according to the Rbf model described by "model"##########
Rbf.predict <- function(model, X, classification=FALSE) {
  
  gamma   <- model$gamma
  centers <- model$centers
  w       <- model$weights
  N       <- dim(X)[1]    # number of observations
  
  pred <- rep(w[1],N)  # we need to init to a value, so let's start with the bias
  
  for (j in 1:N) {  
    # find prediction for point xj
    for (k in 1:length(centers[,1])) {
      # the weight for center[k] is given by w[k+1] (because w[1] is the bias)
      pred[j] <- pred[j] + w[k+1] * exp( -gamma * norm(as.matrix(X[j,]-centers[k,]),"F")^2 )
    }
  }
  
  if (classification) {
    pred <- unlist(lapply(pred, sign))
  }
  return(pred)
}
