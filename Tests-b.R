## Tests with different functions of the same dimension (40 dimensions)
set.seed(1234)
functionid <- sample(24,20)
instanceid <- sample(100,20)

Methods <- c("ols","quant","lasso","ridge")

resulteachbest <- numeric(20)

###The instances represent dimensions of MAKEBBOB(21,15) of smoof package.  
library("smoof")                 ###The list "fns" stores the codes of MAKEBBOB(21,15) function, considering  
library("ExpDE")                 ###each dimension from 2 to 40, by 2.

fn1_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[1],instanceid[1]),MARGIN=1))
}

fn2_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[2],instanceid[2]),MARGIN=1))
}

fn3_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[3],instanceid[3]),MARGIN=1))
}

fn4_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[4],instanceid[4]),MARGIN=1))
}

fn5_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[5],instanceid[5]),MARGIN=1))
}

fn6_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[6],instanceid[6]),MARGIN=1))
}

fn7_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[7],instanceid[7]),MARGIN=1))
}

fn8_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[8],instanceid[8]),MARGIN=1))
}

fn9_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[9],instanceid[9]),MARGIN=1))
}

fn10_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[10],instanceid[10]),MARGIN=1))
}

fn11_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[11],instanceid[11]),MARGIN=1))
}

fn12_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[12],instanceid[12]),MARGIN=1))
}

fn13_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[13],instanceid[13]),MARGIN=1))
}

fn14_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[14],instanceid[14]),MARGIN=1))
}

fn15_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[15],instanceid[15]),MARGIN=1))
}

fn16_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[16],instanceid[16]),MARGIN=1))
}

fn17_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[17],instanceid[17]),MARGIN=1))
}

fn18_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[18],instanceid[18]),MARGIN=1))
}

fn19_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[19],instanceid[19]),MARGIN=1))
}

fn20_training <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[20],instanceid[20]),MARGIN=1))
}

fns_training <- list("fn1_training","fn2_training","fn3_training","fn4_training","fn5_training","fn6_training",
                     "fn7_training","fn8_training","fn9_training","fn10_training","fn11_training","fn12_training",
                     "fn13_training","fn14_training","fn15_training","fn16_training","fn17_training","fn18_training",
                     "fn19_training","fn20_training")

functionid <- sample(24,20)
instanceid <- sample(100,20)

fn1_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[1],instanceid[1]),MARGIN=1))
}

fn2_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[2],instanceid[2]),MARGIN=1))
}

fn3_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[3],instanceid[3]),MARGIN=1))
}

fn4_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[4],instanceid[4]),MARGIN=1))
}

fn5_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[5],instanceid[5]),MARGIN=1))
}

fn6_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[6],instanceid[6]),MARGIN=1))
}

fn7_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[7],instanceid[7]),MARGIN=1))
}

fn8_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[8],instanceid[8]),MARGIN=1))
}

fn9_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[9],instanceid[9]),MARGIN=1))
}

fn10_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[10],instanceid[10]),MARGIN=1))
}

fn11_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[11],instanceid[11]),MARGIN=1))
}

fn12_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[12],instanceid[12]),MARGIN=1))
}

fn13_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[13],instanceid[13]),MARGIN=1))
}

fn14_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[14],instanceid[14]),MARGIN=1))
}

fn15_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[15],instanceid[15]),MARGIN=1))
}

fn16_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[16],instanceid[16]),MARGIN=1))
}

fn17_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[17],instanceid[17]),MARGIN=1))
}

fn18_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[18],instanceid[18]),MARGIN=1))
}

fn19_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[19],instanceid[19]),MARGIN=1))
}

fn20_validation <- function(x){
  return(apply(x,FUN=makeBBOBFunction(40,functionid[20],instanceid[20]),MARGIN=1))
}


fns_validation <- list("fn1_validation","fn2_validation","fn3_validation","fn4_validation","fn5_validation","fn6_validation","fn7_validation",
                       "fn8_validation","fn9_validation","fn10_validation","fn11_validation","fn12_validation","fn13_validation","fn14_validation",
                       "fn15_validation","fn16_validation","fn17_validation","fn18_validation","fn19_validation","fn20_validation")

instances_training <- fns_training

for (i in 1:length(Methods)) {

  filename <- as.character(Methods[i])
  f <- data.frame(Exec = "Exec", AvgPerf = "AvgPerf")
  filename <- paste(filename,"-b.csv",sep="")
  write.table(f, file=filename, append=T, row.names=F, col.names=F, sep=",")
  
  filename1 <- as.character(Methods[i])
  f <- data.frame(Exec = "Exec", PopSize = "Popsize", Alpha="Alpha", Beta="Beta", f="f")
  filename1 <-paste(filename1,"-bests-b.csv",sep="")
  write.table(f, file=filename1, append=T, row.names=F, col.names=F, sep=",")
  
  filename2 <- as.character(Methods[i])
  f <- data.frame(Exec = "Exec", Time = "Time")
  filename2 <- paste(filename2,"-time-b.csv",sep="")
  write.table(f, file=filename2, append=T, row.names=F, col.names=F, sep=",")
  
  filename3 <- as.character(Methods[i])
  f <- data.frame(Exec="Exec",Model="Model")
  filename3 <- paste(filename3,"-model-b.csv",sep="")
  write.table(f, file=filename3, append=T, row.names=F, col.names=F, sep=",")
  
  for (j in 1:30){
    
    functuning <- function(candidate,instance){
      popsize <- candidate[1]
      selpars <- list(name="selection_standard")
      stopcrit <- list(names = "stop_maxiter",maxiter=100)
      recpars <- list(name = "recombination_blxAlphaBeta", alpha = candidate[2],beta=candidate[3])
      mutpars <- list(name = "mutation_rand", f=candidate[4])                                   
      probpars <- list(name=as.character(instance),xmin=rep(-5,40),xmax=rep(5,40))
      return(ExpDE(popsize,mutpars,recpars,selpars,stopcrit,probpars)$Fbest)
    }
    
    ptm <- proc.time()
    tuning <- MetaTun(Inf,Inf,"ExpDE",instances_training,4,6000,method=Methods[i],ncores="MAX",packuser=c("ExpDE","smoof"),
                      funcuser=c("fn1_training","fn2_training","fn3_training","fn4_training","fn5_training","fn6_training","fn7_training",
                                 "fn8_training","fn9_training","fn10_training","fn11_training","fn12_training","fn13_training","fn14_training",
                                 "fn15_training","fn16_training","fn17_training","fn18_training","fn19_training","fn20_training","functionid","instanceid"))
    timerun <- proc.time() - ptm
    
    if (!is.na(tuning[1])){
      best <- numeric(4)
      best[1] <- tuning[[1]][1,1]
      best[2] <- tuning[[1]][1,2]
      best[3] <- tuning[[1]][1,3]
      best[4] <- tuning[[1]][1,4]
      
      f <- data.frame(Exec=j, Popsize=best[1], Alpha=best[2], Beta=best[3], f=best[4])
      write.table(f,file=filename1,append=T,row.names=F,col.names=F, sep=",")
      
      model <- tuning[[5]]
      
      f <- data.frame(Exec=j, Model=model)
      write.table(f,file=filename3,append=T,row.names=F,col.names=F, sep=",")
      
      instances_validation <- c(1:20)
      
      for (k in 1:20){
        candidate <- best
        
        instance <- instances_validation[k]
        
        popsize <- candidate[1]
        selpars <- list(name="selection_standard")
        stopcrit <- list(names = "stop_maxiter",maxiter=100)
        recpars <- list(name = "recombination_blxAlphaBeta", alpha = candidate[2],beta=candidate[3])
        mutpars <- list(name = "mutation_rand", f=candidate[4])                                   
        probpars <- list(name=as.character(fns_validation[instance]),xmin=rep(-5,40),xmax=rep(5,40))
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
