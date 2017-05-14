############# CLEAN GLOBAL ENVIRONMENT #########
rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)


############# FUNCTIONS ###############
defineEnvironment <- function(file_directory="C:\\Users\\",file_name ){
  #Working directory
  setwd(file_directory)
  #read and discover the data 
  read_file = read.csv(file_name)

  return(read_file)
}

printResults <- function(dataSet,message=""){
  print(message)
  print(c("CLASS", class(dataSet)))
  print(c("Total rows", nrow(dataSet)))
  print(c("Total Columns", ncol(dataSet)))
  print(c("Number of complete rows", length(which(complete.cases(dataSet)==TRUE))))
  print(c("Number of rows with missing values", length(which(complete.cases(dataSet)==FALSE))))
  #summary(dataSet)
}

removeEmptyRows <- function(dataSet){
  #Remove the missing rows from the dataset
  dataSetClean <- dataSet[rowSums(is.na(dataSet)) != ncol(dataSet)-1, ]
  #check how many variables are missing in total 
  print(c("Empty rows deleted:",nrow(dataSet) - nrow(dataSetClean)))
  return(dataSetClean)
}

generateIdColumn <- function(dataSet){
  #Clean the ID column
  dataSet$ID = 1:nrow(dataSet)
  return(dataSet)
}

imputeMissingValues <- function(dataSet,method="pmm"){
  #Imputing missing values with pmm method and with R package of Mice
  library(mice)
  
  #The data set with different imputations of missing values 
  TempMiss = mice(dataSet, m=10, method = "pmm", maxit = 50, seed = 500 )
  
  #some variable imputations (each with 10 imputation values for each missing value)
  TempMiss$imp$IMAG1
  TempMiss$imp$IMAG2
  
  #Imputed bank data set with the first imputation among the 10
  ImputedDataSet = complete(TempMiss)
  ImputedDataSet = ImputedDataSet[,c(2:ncol(ImputedDataSet))]
  return(ImputedDataSet)
  
}

buildModel <- function(values){
  newModel = matrix(values, ncol=2, byrow = TRUE)
  colnames(newModel) = c("Source", "Target")
  print(newModel)
  return(newModel)
}

matrixInitial <- function(rows,cols,name_rows,name_cols){
  newMatrix = matrix(c(rep(0,rows*cols)), nrow = rows, ncol = cols)
  colnames(newMatrix) = name_rows
  rownames(newMatrix) = name_cols
  print(newMatrix)
  return(newMatrix)
}

zeroInitialize <- function(rows,cols=1){
  newVector = c(rep(0,rows))
  return(newVector)
}

######## GLOBAL PARAMETERS ########

file = "bank.csv"
file_directory = "C:\\Users\\Ricardo\\Sync\\NOVA\\Second semester\\Descriptive analytics - Estadistica l\\Final project"

###################################

bank = defineEnvironment(file_directory ,file )
printResults(bank,"First Bank dataset")

bank2 = removeEmptyRows(bank)
printResults(bank2,"Without empty rows Bank dataset")

bank2 = generateIdColumn(bank2)
printResults(bank2,"Final")

ImputedBank = imputeMissingValues(bank2,"pmm")
printResults(ImputedBank,"ImputedBank")
#Standardize the data
ImputedBankStd = scale(ImputedBank)

printResults(ImputedBankStd,"ImputedBankStd")


# Building the Inner Model
InModel = buildModel(c("Image", "Loyalty", "Image", "Expectations", "Image",
                       "Satisfaction", "Expectations", "Quality", "Expectations", "Value", "Expectations", "Satisfaction", "Quality",
                       "Satisfaction", "Quality", "Value", "Value", "Satisfaction", "Satisfaction", "Loyalty"))

# Building the Outer Model
OutModel = buildModel(c("Image", "IMAG1", "Image", "IMAG2","Image", "IMAG3",	"Image", "IMAG4","Image", "IMAG5"
                       , "Expectations","EXPE1",	"Expectations", "EXPE2", "Expectations", "EXPE3",
                       "Quality", "QUAL1",	"Quality", "QUAL2",	"Quality", "QUAL3", "Quality", "QUAL4",	"Quality", "QUAL5",
                       "Quality", "QUAL6", "Quality", "QUAL7", "Quality", "QUAL8", "Quality","QUAL9", "Value",	"VALU1","Value","VALU2",
                       "Satisfaction","SATI1", "Satisfaction", "SATI2" ,"Satisfaction", "SATI3", "Loyalty", "LOYA1", "Loyalty",	"LOYA2"))


# Preaparing the Model
LatentVariables = unique(OutModel[,1])
ManifestVariables = OutModel[,2]
NbLatentVariables = length(LatentVariables)
NbManifestVariables = length(ManifestVariables)

#Building Pls wheights and paths coefficient Matrices
PlsWeights =  matrixInitial(NbManifestVariables,NbLatentVariables,LatentVariables,ManifestVariables)
PathCoeffs = matrixInitial(NbLatentVariables,NbLatentVariables,LatentVariables,LatentVariables)

#Represent the connections between the latent variables
LatentConnections = PathCoeffs 
for(i in 1:nrow(InModel)){
      a = which(LatentVariables==InModel[i,1])
      b = which(LatentVariables==InModel[i,2])
      LatentConnections[a,b] = 1
      LatentConnections[b,a] = 1
}

#Verctor of occurences of each latentVariable
NbManifestPerLatent = zeroInitialize(NbLatentVariables)
for(i in 1:length(LatentVariables)){
  NbManifestPerLatent[i] = c(length(grep(LatentVariables[i], OutModel[,1])));
}


#Corresponding data set for each Latent Variable
BankSetPerLatent = list()
minElement = 1
for(i in 1:NbLatentVariables){
  maxElement = minElement + NbManifestPerLatent[i] -1
  BankSetPerLatent[[i]] = ImputedBankStd[,c(minElement:maxElement)]
  minElement = minElement + NbManifestPerLatent[i]
}

#Temporary Vector of weights
WeightsTemp = c(rep(1/24,24))


#************For****************************************

Iteration = 0

repeat{


  #List of Weights Per Latent variable
  minElement = 1
  WeightsPerLatent = list()
  for(i in 1:NbLatentVariables){
    maxElement = minElement + NbManifestPerLatent[i] -1
    WeightsPerLatent[[i]] = WeightsTemp[minElement:maxElement]
    minElement = minElement + NbManifestPerLatent[i]
  }
  
  #Build Y1...Yn
  Y = list()
  for(i in 1:length(BankSetPerLatent)){
    Y[[i]] = zeroInitialize(nrow(ImputedBank))
    for(j in 1:length(WeightsPerLatent[[i]])){
      Y[[i]] = Y[[i]] + WeightsPerLatent[[i]][j]*BankSetPerLatent[[i]][,j]
    }
  }
  
  InModel
  
  #Build the inner weights ei
  e=c()
  for(i in 1:length(Y)){
    for(j in 1:length(Y)){
      if(i!=j){
        if(LatentConnections[i,j]==1){
          e = c(e,cor(Y[[i]],Y[[j]]))
        }
        else{
          e=c(e,0)
        }
      }
      
    }
  }
  #Build the Zeds z
  z=list()
  for(i in 1:length(BankSetPerLatent)){
    z[[i]] = c(rep(0,nrow(ImputedBank)))
  }
  
  
  aux=1
  for(i in 1:length(Y)) {
    for(j in 1:length(Y)){
      if(j!=i){
        z[[i]]=z[[i]]+Y[[j]]*e[aux]
        aux=aux+1
      }
    }
  }
  
  #UpdateWeights
  WeightsTempNew = WeightsTemp
  WeightNewPerLatent = WeightsPerLatent
  k=0
  for(i in 1:length(LatentVariables)){
    for(j in 1:NbManifestPerLatent[i]){
      k=k+1
      WeightNewPerLatent[[i]][j] = cov(z[[i]],BankSetPerLatent[[i]][,j])
      WeightsTempNew[k] = cov(z[[i]],BankSetPerLatent[[i]][,j])
    }
  }
  
  epsilon = sqrt(sum((WeightsTempNew - WeightsTemp)^2))
  WeightsTemp = WeightsTempNew
  Iteration = Iteration +1
  print(Iteration)
  if(epsilon <= 2){
    break
  }
}
#**********************************For****************************


