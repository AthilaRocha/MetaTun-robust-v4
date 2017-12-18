set.seed(1234)

acrossinstance <- matrix(ncol=1000,nrow=100)

for (i in 1:100){
  for(j in 1:1000){
    acrossinstance[i,j] <- (rexp(1,rate=0.25)-4)                 ##muito ruido
    #matrixauxiliar[i,j] <- (rexp(1,rate=1)-1)
    #matrixauxiliar[i,j] <- (rexp(1,rate=sqrt(10))-(1/sqrt(10))) ##pouco ruido
  }
}

#set.seed(Sys.time())

functions <- character(5)

functions[1] <- 'function(candidate,instance,exact=0){
  param1 <- candidate[1]
  param2 <- candidate[2]
  
  term1 <- (3*(1-param1)^2*exp(-(param1^2)-(param2+1)^2)-10*(param1/5-param1^3-param2^5)*exp(-param1^2-param2^2)-1/3*exp(-(param1+1)^2-param2^2))
  if (exact==0) return(term1+acrossinstance[instance,candidate[3]]+(rexp(1,rate=1)-1))
  else return(term1)
}'

functions[2] <- 'function(candidate,instance,exact=0){
  param1 <- candidate[1]
  param2 <- candidate[2]
  param3 <- candidate[3]
  a <- 20
  b <- 0.2
  c <- 2*pi
  if (exact==0) return(-a*exp(-b*sqrt(1/3*(param1^2+param2^2+param3^2)))-exp(1/3*(cos(c*param1)+cos(c*param2)+cos(c*param3)))+a+exp(1)+acrossinstance[instance,candidate[4]]+(rexp(1,rate=1)-1))
  else return(-a*exp(-b*sqrt(1/3*(param1^2+param2^2+param3^2)))-exp(1/3*(cos(c*param1)+cos(c*param2)+cos(c*param3))))
}'

functions[3] <- 'function(candidate,instance,exact=0){
  param1 <- candidate[1]
  param2 <- candidate[2]
  param3 <- candidate[3]
  term1 <- 30
  term2 <- (param1^2-10*cos(2*pi*param1)) + (param2^2-10*cos(2*pi*param2)) + (param3^2-10*cos(2*pi*param3))
  if (exact==0) return(term1+term2+acrossinstance[instance,candidate[4]]+(rexp(1,rate=1)-1))
  else return(term1+term2)
}'

functions[4] <- 'function(candidate,instance,exact=0){
  param1 <- candidate[1]
  param2 <- candidate[2]
  param3 <- candidate[3]
  param4 <- candidate[4]
  param5 <- candidate[5]
  term1 <- 30
  term2 <- (param1^2-10*cos(2*pi*param1)) + (param2^2-10*cos(2*pi*param2)) + (param3^2-10*cos(2*pi*param3)) + (param4^2-10*cos(2*pi*param4)) + (param5^2-10*cos(2*pi*param5))
  if (exact==0) return(term1+term2+acrossinstance[instance,candidate[6]]+(rexp(1,rate=1)-1))
  else return(term1+term2)
}'

functions[5] <- 'function(candidate,instance,exact=0){
  param1 <- candidate[1]
  param2 <- candidate[2]
  param3 <- candidate[3]
  param4 <- candidate[4]
  param5 <- candidate[5]
  param6 <- candidate[6]
  param7 <- candidate[7]
  param8 <- candidate[8]
  term1 <- 30
  term2 <- (param1^2-10*cos(2*pi*param1)) + (param2^2-10*cos(2*pi*param2)) + (param3^2-10*cos(2*pi*param3)) + (param4^2-10*cos(2*pi*param4)) + (param5^2-10*cos(2*pi*param5))
  + (param6^2-10*cos(2*pi*param6)) + (param7^2-10*cos(2*pi*param7)) + (param8^2-10*cos(2*pi*param8))
  
  if (exact==0) return(term1+term2+acrossinstance[instance,candidate[9]]+(rexp(1,rate=1)-1))
  else return(term1+term2)
}'


names(functions) <- c("FunctionPeaks", "FunctionAckley", "FunctionRastrigin", "FunctionRastrigin5", "FunctionRastrigin8")
optvalues <- c(-6.5511,0,0,0,0)
bestpoints <- list(c(0.2283,-1.6255),c(0,0,0),c(0,0,0),c(0,0,0,0,0),c(0,0,0,0,0,0,0,0))
budgets <- c(2000,3000,3000,7000,15000)
methods <- c("ols","ridge","lasso")


for(i in 1:length(methods)){        
  for(j in 1:length(functions[j])){
    functuning <- eval(parse(text=functions[j]))
    
    filename <- paste0(methods[i],names(functions[j]))
    
    f <- data.frame(Exec = "Exec",Par = "Par")
    filename1 <-paste(filename,"-best_par.csv",sep="")
    write.table(f, file=filename1, append=T, row.names=F, col.names=F, sep=",")
    
    f <- data.frame(Exec = "Exec",Iter = "Iter",Dist = "Dist")
    filename2 <-paste(filename,"-dist_eucli.csv",sep="")
    write.table(f, file=filename2, append=T, row.names=F, col.names=F, sep=",")
    
    f <- data.frame(Exec = "Exec",Gap = "Gap")
    filename3 <-paste(filename,"-gap_opt.csv",sep="")
    write.table(f, file=filename3, append=T, row.names=F, col.names=F, sep=",")
    
    f <- data.frame(Exec = "Exec",Par = "Par")
    filename4 <-paste(filename,"-closest_cand.csv",sep="")
    write.table(f, file=filename4, append=T, row.names=F, col.names=F, sep=",")
    
    for (k in 1:30) {
      
      resultdata <- MetaTun(bestpoints[[j]],optvalues[j],names(functions[j]),c(1:20),4,budgets[j],methods[i],ncores=18)
    
      if (!is.na(resultdata[[1]][1])){
        
        resultdata[[1]]$exec <- k
        f <- data.frame(Exec = resultdata[[1]]$exec, Par = as.matrix(resultdata[[1]][1,(1:ncol(resultdata[[1]])-1)])) 
        write.table(f,file=filename1,append=T,row.names=F,col.names=F,sep=",")
        
        resultdata[[2]]$exec <- k
        f <- data.frame(Exec = resultdata[[2]]$exec, Iter = resultdata[[2]]$iter,Dist = resultdata[[2]]$dist) 
        write.table(f,file=filename2,append=T,row.names=F,col.names=F,sep=",")
        
        resultdata[[3]]$exec <-k
        f <- data.frame(Exec = resultdata[[3]]$exec,Gap = resultdata[[3]]$gap) 
        write.table(f,file=filename3,append=T,row.names=F,col.names=F,sep=",")
        
        resultdata[[4]]$exec <- k
        f <- data.frame(Exec = resultdata[[4]]$exec, Par = resultdata[[4]]) 
        write.table(f,file=filename4,append=T,row.names=F,col.names=F,sep=",")
      }
      else {
        resultdata[[1]]$exec <- k
        f <- data.frame(Exec = resultdata[[1]]$exec, Par = NA) 
        write.table(f,file=filename1,append=T,row.names=F,col.names=F,sep=",")
        
        resultdata[[2]]$exec <- k
        f <- data.frame(Exec = resultdata[[2]]$exec, Iter = NA,Dist = NA) 
        write.table(f,file=filename2,append=T,row.names=F,col.names=F,sep=",")
        
        resultdata[[3]]$exec <-k
        f <- data.frame(Exec = resultdata[[3]]$exec,Gap = NA) 
        write.table(f,file=filename3,append=T,row.names=F,col.names=F,sep=",")
        
        resultdata[[4]]$exec <- k
        f <- data.frame(Exec = resultdata[[4]]$exec, Par = NA) 
        write.table(f,file=filename4,append=T,row.names=F,col.names=F,sep=",")
      }
    }
  }
}
