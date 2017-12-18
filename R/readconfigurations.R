#####################This function gets the descriptions of parameters to be tuned (names, ranges and types) and #####################
##################a csv file with information of parameters have to be read, according the following example: ########################
#name,min,max,values,type
#Mutation,1,10,NA,r
#Crossover,0.5,5,NA,r
#Selection,2,110,NA,i
#LocalSearch,NA,NA,"search1,search2,search3",c
ReadParametersData <-function(Function){
  file <- paste0(Function,".csv",collapse=NULL)
  dataparameters <- read.csv(file,header=TRUE)
  return(dataparameters)
}
