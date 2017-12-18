###############Function that optimize a number of equations which refer to the original and perturbed models################# 
Optimize <- function(equations,datatuning){
  
  result <- matrix(ncol=(length(datatuning$name)),nrow=(length(equations)))
  minvalue <- datatuning$min
  maxvalue <- datatuning$max
  
  initialpoints <- numeric(length(datatuning$name))
  
  ui <- matrix(ncol=length(datatuning$name),nrow=2*(ncol=length(datatuning$name)))
  ci <- numeric(2*length(datatuning$name))
  
  k<-1
  
  #The function constrOptim used below requires a box constraint in a form: ui%*%theta >= ci. 
  #For instance, considering two parameters in a vector theta = (theta1  theta2), and the constraints
  #0<=theta1<=5, 0<=theta2<=5. In order to use constrOptim, it is required to build a matrix ui(4x2) in a form:
  #ui = [1 0; -1 0; 0 1; 0 -1] and a vector ci = (0  -5  0  -5). In this way, ui%*%theta>=ci
  # results in constraint inequations: theta1>=0; -theta1>=-5; theta2>=0; -theta2>=-5, 
  #which is equivalent to: theta1>=0; theta1<=5; theta2>=0; theta2<=5
  
  # #vector ci of constraint bounds, considering the parameter values not scaled in [0,1] scale
  # for (i in seq(from=1, to=length(ci), by=2)){
  #   ci[i] <- minvalue[k]
  #   ci[i+1] <- -maxvalue[k]
  #   k <- k+1
  # }
  
  #vector ci of constraint bounds, considering the parameter values scaled in [0,1] scale
  for (i in seq(from=1, to=length(ci), by=2)){
    ci[i] <- 0
    ci[i+1] <- -1
    k <- k+1
  }
  
  for(i in 1:nrow(ui)) ui[i,] <- rep(0,ncol(ui))
  
  k<-1
  for (i in seq(from=1, to=nrow(ui), by=2)){
    ui[i,k] <- 1
    ui[i+1,k] <- -1
    k <- k+1
  }
  
  
  ####Optimizing models using parallel processing
  m <- foreach(i=equations,.export=ls(envir=globalenv()),.combine='rbind') %dopar% {
        initialpoints <- runif(nrow(datatuning),0,1)   
        b <- constrOptim(initialpoints,f=eval(parse(text=i)),ui=ui,ci=ci,method="SANN")
        return(b$par)
  }
  
  #non-parallel version of models optimization
  #for (l in 1:(length(equations))){
      
  #    optvalue <- Inf
  #    xstar <- numeric(length(datatuning$name))
      
  #    for (k in 1:timesopt){
         ####an old version using optim 
            #b<-optim(fn=eval(parse(text=equations[l])),par=initialpoints,lower=minvalue, upper=maxvalue,control=list(trace=1),method="L-BFGS-B")
        
         ####initial points, considering the parameter values not scaled in a [0,1] scale
            #for(i in 1:nrow(datatuning)) initialpoints[i] <-runif(1,datatuning[i,2],datatuning[i,3])
         
         #initial points, considering the parameter values scaled in a [0,1] scale
  #       for(i in 1:nrow(datatuning)) initialpoints[i] <-runif(1,0,1)
        
  #       b <- constrOptim(initialpoints, f=eval(parse(text=equations[l])), ui=ui, ci=ci, method="SANN")
  
  #       if (b$value<optvalue){
  #          xstar <- b$par
  #          optvalue <- b$value
  #       }
  #    }
      
  #    result[l,] <- xstar
      
      ##rounding in case of integer parameter (considering not scaling them in a [0,1] scale)
      #for(i in 1:length(datatuning$name)) if (datatuning$type[i]=="i") result[l,i] <- round(result[l,i]) 
  #}
      
  result <- m
  rownames(result) <- NULL
  
  ##Scaling the best candidates to their original scale
  for(j in 1:ncol(result)){
         minim <- datatuning$min[j]
         maxi <- datatuning$max[j]

         for (i in 1:(nrow(result)))
              result[i,j] <- result[i,j]*(maxi-minim)+minim

         if (datatuning$type[j]=="i") result[,j] <- round(result[,j])
  }
  
  cat("\n")
  cat("Best values for parameters \n")
  resultforprint <- result
  colnames(resultforprint) <- datatuning$name
  print(resultforprint)
  cat("\n")
  result <- cbind(result,NA)
  return(result)
}



###########Function that optimize a number of Rbf equations which refer to the original and perturbed models################# 
OptimizeRadial <- function(rbfmodel,datatuning,timesopt){

    centers <- as.matrix(rbfmodel$centers)
    weights <- as.matrix(rbfmodel$weights)

    expr <- as.character(weights[1,1])
    expr <- paste0(expr,"+",collapse=NULL)

    for(i in 1:nrow(centers)){
      expr <- paste0(expr,weights[i+1,1],collapse=NULL)
      expr <- paste0(expr,"*",collapse=NULL)
      expr <- paste0(expr,"exp(",collapse=NULL)
      expr <- paste0(expr,"-",collapse=NULL)
      expr <- paste0(expr,rbfmodel$gamma,collapse=NULL)
      expr <- paste0(expr,"*",collapse=NULL)
      expr <- paste0(expr, "norm(as.matrix(c(",collapse=NULL)
  
    for(k in 1:length(datatuning$name)) {
       expr <- paste0(expr, datatuning$name[k],collapse=NULL)
       expr <- paste0(expr,",",collapse=NULL)
    }
  
    expr <- substr(expr,1,(nchar(expr)-1))
    expr <- paste0(expr,")",collapse=NULL)
    expr <- paste0(expr,"-",collapse=NULL)
    expr <- paste0(expr,"c(",collapse=NULL)
  
    for(k in 1:ncol(centers)){
      expr <- paste0(expr, centers[i,k],collapse=NULL)
      expr <- paste0(expr,",",collapse=NULL)
    }
  
    expr <- substr(expr,1,(nchar(expr)-1))
    expr <- paste0(expr,'),"F"',collapse=NULL)
    expr <- paste0(expr,")^2))",collapse=NULL)
    expr <- paste0(expr,"+",collapse=NULL)
  }

  expr <- substr(expr,1,(nchar(expr)-1))

  #print("expressao radial")
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

  #print("funcao")
  #print(func)

  result <- matrix(ncol=(length(datatuning$name)),nrow=1)
  minvalue <- datatuning$min
  maxvalue <- datatuning$max

  initialpoints <- numeric(length(datatuning$name))
  optvalue <- Inf
  xstar <- numeric(length(datatuning$name))
  
  ui <- matrix(ncol=length(datatuning$name),nrow=2*(ncol=length(datatuning$name)))
  ci <- numeric(2*length(datatuning$name))
  
  
 
  
  
  k<-1
  
  #vector ci of constraint bounds, considering the parameter values not scaled in [0,1] scale
  # for (i in seq(from=1, to=length(ci), by=2)){
  #   ci[i] <- minvalue[k]
  #   ci[i+1] <- -maxvalue[k]
  #   k <- k+1
  # }
  
  for(i in 1:nrow(ui)) ui[i,] <- rep(0,ncol(ui))
  
  k<-1
  for (i in seq(from=1, to=nrow(ui), by=2)){
    ui[i,k] <- 1
    ui[i+1,k] <- -1
    k <- k+1
  }
  
  
  for (k in 1:timesopt){
      for(i in 1:nrow(datatuning)) initialpoints[i] <-runif(1,datatuning[i,2],datatuning[i,3])
      #b<-optim(fn=eval(parse(text=func)),par=initialpoints,lower=minvalue, upper=maxvalue,control=list(trace=0),method="L-BFGS-B")
      
      b <- constrOptim(initialpoints, f=eval(parse(text=func)), ui=ui, ci=ci, method="SANN")
      
      if (b$value<optvalue){
        xstar <- b$par
        optvalue <- b$value
      }
  }
  
  #c<-optimx(fn=eval(parse(text=func)),par=initialpoints,lower=minvalue, upper=maxvalue,control=list(trace=1),method=c("L-BFGS-B")) 
  
  cat("\n")
  print(xstar)
  cat("\n")
  print(optvalue)
  #print(a)
  cat("\n")
  result[1,] <- xstar
  for(i in 1:length(datatuning$name)) if (datatuning$type[i]=="i") result[1,i] <- round(result[1,i]) 

  cat("\n")
  cat("Best values for parameters - radial case \n")
  print(result)
  cat("\n")
  
  result <- cbind(result,NA)
  return(result)
}
