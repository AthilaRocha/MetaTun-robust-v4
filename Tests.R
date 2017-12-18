XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
set.seed(1234)
instances <- c(1:20)

acrossinstance <- matrix(ncol=1000,nrow=100)
for(i in 1:100) for(j in 1:1000) acrossinstance[i,j] <- (rexp(1,rate=.25)-4)

####this code refers to function Ackley (3d), with across and within-instance noises
functuning <- function(candidate,instance){
  param1 <- candidate[1]
  param2 <- candidate[2]
  param3 <- candidate[3]
  a <- 20
  b <- 0.2
  c <- 2*pi
  
  ###candidate[4] is the ID of the candidate
  return(-a*exp(-b*sqrt(1/3*(param1^2+param2^2+param3^2)))-exp(1/3*(cos(c*param1)+cos(c*param2)+cos(c*param3)))+a+exp(1)+acrossinstance[instance,candidate[4]]+(rexp(1,rate=1)-1))
}

MetaTun(Inf,Inf,"FunctionAckley",instances,4,4000,ncores="MAX")  ##call of MetaTun, considering standard regression modeling (OLS)

###**obs: In the case of simulated algorithms, the code of the function 'EvaluateCandidates' in MetaTun is adapted to 
###get candidates with their IDs, which are used to get a value of across-instance noise from 'acrossinstance'
###matrix
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
set.seed(1234)
instances <- c(1:20)

acrossinstance <- matrix(ncol=1000,nrow=100)
for(i in 1:100) for(j in 1:1000) acrossinstance[i,j] <- (rexp(1,rate=.25)-4)

####this code refers to function Rastrigin (3d), with across and within-instance noises
functuning <- function(candidate,instance){   
  param1 <- candidate[1]
  param2 <- candidate[2]
  param3 <- candidate[3]
  term1 <- 30
  term2 <- (param1^2-10*cos(2*pi*param1)) + (param2^2-10*cos(2*pi*param2)) + (param3^2-10*cos(2*pi*param3))
  
  ###candidate[4] is the ID of the candidate
  return(term1+term2+acrossinstance[instance,candidate[4]]+(rexp(1,rate=1)-1))
}

MetaTun(Inf,Inf,"FunctionRastrigin",instances,4,4000,ncores="MAX")  ##call of MetaTun, considering standard regression modeling (OLS)

###**obs: In the case of simulated algorithms, the code of the function 'EvaluateCandidates' in MetaTun is adapted to 
###get candidates with their IDs, which are used to get a value of across-instance noise from 'acrossinstance'
###matrix
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX





XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
set.seed(1234)
instances <- c(1:20)

acrossinstance <- matrix(ncol=1000,nrow=100)
for(i in 1:100) for(j in 1:1000) acrossinstance[i,j] <- (rexp(1,rate=.25)-4)

####this code refers to function Rastrigin (5d), with across and within-instance noises
functuning <- function(candidate,instance){   
  param1 <- candidate[1]
  param2 <- candidate[2]
  param3 <- candidate[3]
  param4 <- candidate[4]
  param5 <- candidate[5]
  term1 <- 30
  term2 <- (param1^2-10*cos(2*pi*param1)) + (param2^2-10*cos(2*pi*param2)) + (param3^2-10*cos(2*pi*param3)) + (param4^2-10*cos(2*pi*param4)) + (param5^2-10*cos(2*pi*param5))
  
  ###candidate[6] is the ID of the candidate
  return(term1+term2+acrossinstance[instance,candidate[6]]+(rexp(1,rate=1)-1))
}

MetaTun(Inf,Inf,"FunctionRastrigin5",instances,4,7000,ncores="MAX")  ##call of MetaTun, considering standard regression modeling (OLS)

###**obs: In the case of simulated algorithms, the code of the function 'EvaluateCandidates' in MetaTun is adapted to 
###get candidates with their IDs, which are used to get a value of across-instance noise from 'acrossinstance'
###matrix
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
set.seed(1234)
instances <- c(1:20)

acrossinstance <- matrix(ncol=10000,nrow=100)
for(i in 1:100) for(j in 1:10000) acrossinstance[i,j] <- (rexp(1,rate=.25)-4)

####this code refers to function Rastrigin (5d), with across and within-instance noises
functuning <- function(candidate,instance){   
  param1 <- candidate[1]
  param2 <- candidate[2]
  param3 <- candidate[3]
  param4 <- candidate[4]
  param5 <- candidate[5]
  param6 <- candidate[6]
  param7 <- candidate[7]
  param8 <- candidate[8]
  term1 <- 30
  term2 <- (param1^2-10*cos(2*pi*param1)) + (param2^2-10*cos(2*pi*param2)) + (param3^2-10*cos(2*pi*param3)) + (param4^2-10*cos(2*pi*param4)) + (param5^2-10*cos(2*pi*param5)) + (param6^2-10*cos(2*pi*param1)) + (param7^2-10*cos(2*pi*param1)) + (param8^2-10*cos(2*pi*param1))
  
  ###candidate[9] is the ID of the candidate
  return(term1+term2+acrossinstance[instance,candidate[9]]+(rexp(1,rate=1)-1))
}

MetaTun(Inf,Inf,"FunctionRastrigin8",instances,4,15000,ncores="MAX")  ##call of MetaTun, considering standard regression modeling (OLS)

###**obs: In the case of simulated algorithms, the code of the function 'EvaluateCandidates' in MetaTun is adapted to 
###get candidates with their IDs, which are used to get a value of across-instance noise from 'acrossinstance'
###matrix
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX








XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
set.seed(1234)
instances <- c(1:20)

acrossinstance <- matrix(ncol=1000,nrow=100)
for(i in 1:100) for(j in 1:1000) acrossinstance[i,j] <- (rexp(1,rate=.25)-4)

####this code refers to function Peaks (2d), with across and within-instance noises
functuning <- function(candidate,instance){   
  param1 <- candidate[1]
  param2 <- candidate[2]
  
  term1 <- (3*(1-param1)^2*exp(-(param1^2)-(param2+1)^2)-10*(param1/5-param1^3-param2^5)*exp(-param1^2-param2^2)-1/3*exp(-(param1+1)^2-param2^2))
  
  ###candidate[3] is the ID of the candidate
  return(term1+acrossinstance[instance,candidate[3]]+(rexp(1,rate=1)-1))
}

MetaTun(Inf,Inf,"FunctionPeaks",instances,3,2000,ncores="MAX")  ##call of MetaTun, considering standard regression modeling (OLS)

###**obs: In the case of simulated algorithms, the code of the function 'EvaluateCandidates' in MetaTun is adapted to 
###get candidates with their IDs, which are used to get a value of across-instance noise from 'acrossinstance'
###matrix

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

instances <- c(2:20)

library("ExpDE")
###função que será avaliada e retornará valor da aplicaçao do algoritmo ExpDE, usando um candidato 'candidate'
###em uma instância 'instance', resolvendo a função "sphere" do pacote smoof. Neste caso 'instance' representa
###a dimensão da função sphere
functuning  <- function(candidate,instance){
 popsize <- candidate[1]                                                               
 selpars <- list(name="selection_standard")
 stopcrit <- list(names = "stop_maxiter",maxiter=100)
 recpars <- list(name = "recombination_blxAlphaBeta", alpha = candidate[2],beta=candidate[3])
 mutpars <- list(name = "mutation_rand", f=candidate[4])                                   
 probpars <- list(name="sphere",xmin=rep(-5.12,instance),xmax=rep(5.12,instance))
 return(ExpDE(popsize,mutpars,recpars,selpars,stopcrit,probpars)$Fbest)
}

MetaTun(Inf,Inf,"ExpDE",instances,3,2000,ncores="MAX",packuser="ExpDE") ##call of MetaTun

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

instances <- seq(2, 40, by=2)    ###The instances represent dimensions of MAKEBBOB(21,15) of smoof package.  
library("smoof")                 ###The list "fns" stores the codes of MAKEBBOB(21,15) function, considering  
library("ExpDE")                 ###each dimension from 2 to 40, by 2.

fn1 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(2,21,15),MARGIN=1))
}

fn2 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(4,21,15),MARGIN=1))
}

fn3 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(6,21,15),MARGIN=1))
}

fn4 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(8,21,15),MARGIN=1))
}

fn5 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(10,21,15),MARGIN=1))
}

fn6 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(12,21,15),MARGIN=1))
}

fn7 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(14,21,15),MARGIN=1))
}

fn8 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(16,21,15),MARGIN=1))
}

fn9 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(18,21,15),MARGIN=1))
}

fn10 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(20,21,15),MARGIN=1))
}

fn11 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(22,21,15),MARGIN=1))
}

fn12 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(24,21,15),MARGIN=1))
}

fn13 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(26,21,15),MARGIN=1))
}

fn14 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(28,21,15),MARGIN=1))
}

fn15 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(30,21,15),MARGIN=1))
}

fn16 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(32,21,15),MARGIN=1))
}

fn17 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(34,21,15),MARGIN=1))
}

fn18 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(36,21,15),MARGIN=1))
}

fn19 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(38,21,15),MARGIN=1))
}

fn20 <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,21,15),MARGIN=1))
}

fns <- list("fn1","fn2","fn3","fn4","fn5","fn6","fn7","fn8","fn9","fn10","fn11","fn12","fn13","fn14","fn15","fn16","fn17","fn18","fn19","fn20")

functuning  <- function(candidate,instance){
  popsize <- as.integer(candidate[1])
  selpars <- list(name="selection_standard")
  stopcrit <- list(names = "stop_maxiter",maxiter=100)
  recpars <- list(name = "recombination_blxAlphaBeta", alpha = as.numeric(candidate[2]),beta=as.numeric(candidate[3]))
  mutpars <- list(name = "mutation_rand", f=as.numeric(candidate[4]))                                   
  probpars <- list(name=as.character(fns[instance/2]),xmin=rep(-5,instance),xmax=rep(5,instance))
  return(ExpDE(popsize,mutpars,recpars,selpars,stopcrit,probpars)$Fbest)
}

MetaTun(Inf,Inf,"ExpDE",instances,4,6000,ncores="MAX",packuser = c("ExpDE","smoof"),funcuser = c("fn1","fn2","fn3","fn4","fn5","fn6","fn7","fn8","fn9","fn10","fn11","fn12","fn13","fn14","fn15","fn16","fn17","fn18","fn19","fn20"))
##In this case two not-native R packages have to be exported to all nodes of processing clusters: "ExpDE" and "smoof" and all user-defined functions: "fn1" to "fn20"

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


###protocol of testing MetaTun (4 versions: ols, quantile, ridge and lasso regression)
###The training instance functions are BBOB functions of the family 21, id 15, with dimensions from 2 to 40 (20 instances: dimensions 2-4-6-...40)
###The validation instance functions are of the same BBOB functions family, with dimensions from 3-5-7,...to 39 (19 instances)
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
set.seed(1234)
Methods <- c("ols","quant","lasso","ridge")

resulteachbest <- numeric(19)

                                 ###The instances represents dimensions of MAKEBBOB(21,15) of smoof package.  
library("smoof")                 ###The list "fns" stores the codes of MAKEBBOB(21,15) function, considering  
library("ExpDE")                 ###each dimension from 2 to 40, by 2.

fn1_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(2,21,15),MARGIN=1))
}

fn2_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(4,21,15),MARGIN=1))
}

fn3_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(6,21,15),MARGIN=1))
}

fn4_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(8,21,15),MARGIN=1))
}

fn5_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(10,21,15),MARGIN=1))
}

fn6_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(12,21,15),MARGIN=1))
}

fn7_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(14,21,15),MARGIN=1))
}

fn8_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(16,21,15),MARGIN=1))
}

fn9_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(18,21,15),MARGIN=1))
}

fn10_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(20,21,15),MARGIN=1))
}

fn11_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(22,21,15),MARGIN=1))
}

fn12_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(24,21,15),MARGIN=1))
}

fn13_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(26,21,15),MARGIN=1))
}

fn14_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(28,21,15),MARGIN=1))
}

fn15_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(30,21,15),MARGIN=1))
}

fn16_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(32,21,15),MARGIN=1))
}

fn17_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(34,21,15),MARGIN=1))
}

fn18_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(36,21,15),MARGIN=1))
}

fn19_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(38,21,15),MARGIN=1))
}

fn20_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,21,15),MARGIN=1))
}

fns_training <- list("fn1_training","fn2_training","fn3_training","fn4_training","fn5_training","fn6_training","fn7_training",
                     "fn8_training","fn9_training","fn10_training","fn11_training","fn12_training","fn13_training","fn14_training",
                     "fn15_training","fn16_training","fn17_training","fn18_training","fn19_training","fn20_training")

  

fn1_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(3,21,15),MARGIN=1))
}

fn2_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(5,21,15),MARGIN=1))
}

fn3_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(7,21,15),MARGIN=1))
}

fn4_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(9,21,15),MARGIN=1))
}

fn5_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(11,21,15),MARGIN=1))
}

fn6_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(13,21,15),MARGIN=1))
}

fn7_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(15,21,15),MARGIN=1))
}

fn8_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(17,21,15),MARGIN=1))
}

fn9_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(19,21,15),MARGIN=1))
}

fn10_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(21,21,15),MARGIN=1))
}

fn11_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(23,21,15),MARGIN=1))
}

fn12_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(25,21,15),MARGIN=1))
}

fn13_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(27,21,15),MARGIN=1))
}

fn14_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(29,21,15),MARGIN=1))
}

fn15_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(31,21,15),MARGIN=1))
}

fn16_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(33,21,15),MARGIN=1))
}

fn17_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(35,21,15),MARGIN=1))
}

fn18_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(37,21,15),MARGIN=1))
}

fn19_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(39,21,15),MARGIN=1))
}

fns_validation <- list("fn1_validation","fn2_validation","fn3_validation","fn4_validation","fn5_validation","fn6_validation","fn7_validation",
                     "fn8_validation","fn9_validation","fn10_validation","fn11_validation","fn12_validation","fn13_validation","fn14_validation",
                     "fn15_validation","fn16_validation","fn17_validation","fn18_validation","fn19_validation")


for(i in 1:length(Methods)){
  
  filename <- as.character(Methods[i])
  f <- data.frame(Exec = "Exec", AvgPerf = "AvgPerf")
  filename <- paste(filename,".csv",sep="")
  write.table(f, file=filename, append=T, row.names=F, col.names=F, sep=",")
  
  filename1 <- as.character(Methods[i])
  f <- data.frame(Exec = "Exec", PopSize = "Popsize", Alpha="Alpha", Beta="Beta", f="f")
  filename1 <-paste(filename1,"-bests.csv",sep="")
  write.table(f, file=filename1, append=T, row.names=F, col.names=F, sep=",")
  
  filename2 <- as.character(Methods[i])
  f <- data.frame(Exec = "Exec", Time = "Time")
  filename2 <- paste(filename2,"-time.csv",sep="")
  write.table(f, file=filename2, append=T, row.names=F, col.names=F, sep=",")
  

  filename3 <- as.character(Methods[i])
  f <- data.frame(Exec="Exec",Model="Model")
  filename3 <- paste(filename3,"-model.csv",sep="")
  write.table(f, file=filename3, append=T, row.names=F, col.names=F, sep=",")
  
  for (j in 1:30){
  
    instances_training <- seq(2, 40, by=2)
    
    functuning <- function(candidate,instance){
      
     popsize <- as.integer(candidate[1])
     selpars <- list(name="selection_standard")
     stopcrit <- list(names = "stop_maxiter",maxiter=100)
     recpars <- list(name = "recombination_blxAlphaBeta", alpha = as.numeric(candidate[2]),beta=as.numeric(candidate[3]))
     mutpars <- list(name = "mutation_rand", f=candidate[4])                                   
     probpars <- list(name=as.character(fns_training[as.integer(instance)/2]),xmin=rep(-5,as.integer(instance)),xmax=rep(5,as.integer(instance)))
     return(ExpDE(popsize,mutpars,recpars,selpars,stopcrit,probpars)$Fbest)
    }
    
    ptm <- proc.time()
    tuning <- MetaTun(Inf,Inf,"ExpDE",instances_training,4,6000,method=Methods[i],ncores="MAX",packuser=c("ExpDE","smoof"),
                      funcuser=c("fn1_training","fn2_training","fn3_training","fn4_training","fn5_training","fn6_training","fn7_training",
                                 "fn8_training","fn9_training","fn10_training","fn11_training","fn12_training","fn13_training","fn14_training",
                                 "fn15_training","fn16_training","fn17_training","fn18_training","fn19_training","fn20_training"))
    timerun <- proc.time() - ptm
    
    if (!is.na(tuning[1])){
        best <- numeric(4)
        best[1] <- tuning[[1]][1,1]
        best[2] <- tuning[[1]][1,2]
        best[3] <- tuning[[1]][1,3]
        best[4] <- tuning[[1]][1,4]
    
        f <- data.frame(Exec=j, Popsize=best[1], Alpha=best[2], Beta=best[3], f=best[4])
        write.table(f,file=filename1,append=T,row.names=F,col.names=F, sep=",")
    
        model <- tuning[[2]]
        
        f <- data.frame(Exec=j, Model=model)
        write.table(f,file=filename3,append=T,row.names=F,col.names=F, sep=",")
        
        instances_validation <- seq(3, 39, by=2)
        instances_validation <- instances_validation[sample.int(length(instances_validation),length(instances_validation))]
        
        for (k in 1:19){
           candidate <- best
           
           instance <- instances_validation[k]
           
           popsize <- candidate[1]
           selpars <- list(name="selection_standard")
           stopcrit <- list(names = "stop_maxiter",maxiter=100)
           recpars <- list(name = "recombination_blxAlphaBeta", alpha = candidate[2],beta=candidate[3])
           mutpars <- list(name = "mutation_rand", f=candidate[4])                                   
           probpars <- list(name=as.character(fns_validation[as.integer(instance/2)]),xmin=rep(-5,instance),xmax=rep(5,instance))
           res <- ExpDE(popsize,mutpars,recpars,selpars,stopcrit,probpars)$Fbest
           resulteachbest[k] <- res
        }
    
        avgresult <- mean(resulteachbest)
    
        f <- data.frame(Exec = j, AvgPerf = avgresult) 
        write.table(f,file=filename,append=T,row.names=F,col.names=F,sep=",")
    
        f <- data.frame(Exec = j, Time = timerun[3]) 
        write.table(f,file=filename2,append=T,row.names=F,col.names=F,sep=",")
    }
    else {
        f <- data.frame(Exec=j, Popsize=NA, Alpha=NA, Beta=NA, f=NA)
        write.table(f,file=filename1,append=T,row.names=F,col.names=F, sep=",")
        
        f <- data.frame(Exec = j, AvgPerf = NA) 
        write.table(f,file=filename,append=T,row.names=F,col.names=F,sep=",")
        
        f <- data.frame(Exec = j, Time = timerun[3]) 
        write.table(f,file=filename2,append=T,row.names=F,col.names=F,sep=",")
        
        f <- data.frame(Exec = j, Model = NA) 
        write.table(f,file=filename3,append=T,row.names=F,col.names=F,sep=",")
    }
  }
}
