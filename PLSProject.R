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
  ImputedDataSet = dataSet
  if (method == "remove"){
  ImputedDataSet =  dataSet[complete.cases(dataSet),]
    }
   if (method == "pmm"){
  # #Imputing missing values with pmm method and with R package of Mice
   library(mice)
  # 
  # #The data set with different imputations of missing values 
   TempMiss = mice(dataSet, m=10, method = "pmm", maxit = 50, seed = 500 )
  # 
  # #some variable imputations (each with 10 imputation values for each missing value)
   TempMiss$imp$IMAG1
   TempMiss$imp$IMAG2
  # 
  # #Imputed bank data set with the first imputation among the 10
   ImputedDataSet = complete(TempMiss)
   
   }
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

calculateCronbach  <- function(dataSet,numberVariables){
  for(i in 1:numberVariables){
    print(colnames(dataSet[[i]]))
    res = cor(dataSet[[i]])
    diag(res) =  zeroInitialize(ncol(dataSet[[i]]))
    correl  = sum(res)
    p = ncol(dataSet[[i]])
    alpha = (correl/(p + correl)) * (p / (p - 1))
    print (c("Alpha",colnames(dataSet[[i]]),alpha))
    print(cronbach(dataSet[[i]]))
  }
}



calculateDillonGoldstein  <- function(dataSet,numberVariables){
  for(i in 1:numberVariables){
  firstComponent = princomp(dataSet[[i]])$scores[,1]
  
  up = colSums(cor(dataSet[[i]],firstComponent))^2
  down = up + colSums(1 - cor(dataSet[[i]],firstComponent)^2)
  dg = up / down
  print (c("DillonGoldstein",colnames(dataSet[[i]]),dg))
  print(rho(dataSet[[i]]))
  }
  }

######## GLOBAL PARAMETERS ########

file = "bank.csv"
file_directory = "C:\\Users\\Ricardo\\Sync\\NOVA\\Second semester\\Descriptive analytics - Estadistica l\\Final project"

###################################

bank = defineEnvironment(file_directory ,file )
printResults(bank,"First Bank dataset")

bank2 = removeEmptyRows(bank)
#printResults(bank2,"Without empty rows Bank dataset")

bank2 = generateIdColumn(bank2)
#printResults(bank2,"Final")

ImputedBank = imputeMissingValues(bank2,"remove")
#printResults(ImputedBank,"ImputedBank")
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

#Use standarized data
calculateCronbach(BankSetPerLatent, NbLatentVariables)
calculateDillonGoldstein(BankSetPerLatent, NbLatentVariables)
#Temporary Vector of weights
WeightsTemp = c(rep(1,24))

#************For****************************************

Iteration = 0

repeat{

  #List of Weights Per Latent variable
  #Build W's
  minElement = 1
  WeightsPerLatent = list()
  for(i in 1:NbLatentVariables){
    maxElement = minElement + NbManifestPerLatent[i] -1
    WeightsPerLatent[[i]] = WeightsTemp[minElement:maxElement]
    minElement = minElement + NbManifestPerLatent[i]
  }
  
  #Build Y1...Yn
  # Yi = W1X1 + ... + WiXi
  Y = list()
  Ystd = c(rep(0,NbLatentVariables))
  for(i in 1:length(BankSetPerLatent)){
    Y[[i]] = zeroInitialize(nrow(ImputedBank))
    for(j in 1:length(WeightsPerLatent[[i]])){
      Y[[i]] = Y[[i]] + WeightsPerLatent[[i]][j]*BankSetPerLatent[[i]][,j]
    }
    Ystd[i] = sd(Y[[i]])
    Y[[i]] = scale(Y[[i]])
  }
  

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
    z[[i]] = zeroInitialize(nrow(ImputedBank))
  }
  
  
  aux=1
  for(i in 1:length(Y)) {
    for(j in 1:length(Y)){
      if(j!=i){
        z[[i]]=z[[i]]+Y[[j]]*e[aux]
        aux=aux+1
      }
    }
    z[[i]] = scale(z[[i]])
  }
  
  #UpdateWeights
  WeightsTempNew = WeightsTemp
  WeightNewPerLatent = WeightsPerLatent
  k=0
  for(i in 1:length(LatentVariables)){
    for(j in 1:NbManifestPerLatent[i]){
      k=k+1
      
      WeightNewPerLatent[[i]][j] = cov(z[[i]],BankSetPerLatent[[i]][,j])
      
      #WeightNewPerLatent[[i]][j] = WeightNewPerLatent[[i]][j] / Ystd[i]
      
      #List format
      WeightsTempNew[k] = WeightNewPerLatent[[i]][j]
    }
  }
  print(Iteration)
  #print(WeightsTemp)
  print(WeightsTempNew)
  epsilon = sum((abs(WeightsTempNew) - abs(WeightsTemp))^2)
  
  print( Ystd)
  print(WeightsTempNew)
  #epsilon = sqrt(sum((WeightsTempNew - WeightsTemp)^2))
  print(epsilon)
  WeightsTemp = WeightsTempNew
  Iteration = Iteration +1

  if(epsilon <= 0.00001 | Iteration > 100){
    
    break
  }
}

#**********************************For****************************

#####Estimate the OLS Coefficients of the Latent Variables#####
BetaList = list()
for(i in 1:length(LatentVariables)){
  InModelSubset = subset(InModel, InModel[,2]==LatentVariables[i])
  X=c()
  if(nrow(InModelSubset)!=0){
    for(j in 1:nrow(InModelSubset)){
      X = cbind (X,Y[[which(LatentVariables==InModelSubset[j,1])]]) #Look for the index in Latent vars
    }
    XY = t(X)%*%Y[[i]]
    XXInv = solve(t(X)%*%X)
    Beta = XXInv %*% XY
    Beta = cbind(Beta,InModelSubset[,1])
    BetaList[[i]] = Beta
  }
}

######Report the OLS Coefficients in the PathCoeffs Matrix#####
for(i in 1:length(BetaList)){
  if(length(BetaList[[i]])!=0){
    for(j in 1:nrow(BetaList[[i]])){
      coeffIndex = which(LatentVariables==BetaList[[i]][j,2])
      PathCoeffs[coeffIndex, i] = BetaList[[i]][j,1]
    }
  }
}

######Loadings######
resultsMatrix =  matrixInitial(sum(NbManifestPerLatent),4,c("weight","loading","communality","redundancy"),ManifestVariables) 

k=0
Loadings = matrixInitial(NbLatentVariables,sum(NbManifestPerLatent),ManifestVariables,LatentVariables) 
for(i in 1:NbLatentVariables){
    for(j in 1:NbManifestPerLatent[i]){
    Loadings[i,j + k] = cor(ImputedBankStd[,j+k],Y[[i]])
    resultsMatrix[j+k,2] = Loadings[i,j + k]
    }
  k = k + NbManifestPerLatent[i]
}

### WEIGHT ADJUST FOR STD ######
k=1
Weights = matrixInitial(NbLatentVariables,NbManifestVariables,ManifestVariables,LatentVariables) 
for(i in 1:NbLatentVariables){
  max = k + length(WeightNewPerLatent[[i]]) -1
  Weights[i,k:max] =  (WeightNewPerLatent[[i]] / Ystd[i])
  resultsMatrix[k:max,1] = Weights[i,k:max]
  k = k + NbManifestPerLatent[i]
}


##### PRESENT RESULTS ########

results = list(w = rowSums(Weights), W = t(Weights), loadings = t(Loadings), path_coefficients = t(PathCoeffs), matrix=resultsMatrix)

results$loadings
results$W
results$path_coefficients
results$matrix




################### TEST OUTPUT FROM ORIGINAL FUNCTION #########
library(plspm)
sat_blocks <- list(1:5, 6:8, 9:17, 18:19, 20:22, 23:24)
sat_modes <- rep("A", NbLatentVariables) 
LatentConnectionsTest = LatentConnections
LatentConnectionsTest[upper.tri(LatentConnectionsTest)] <- 0

plsres = plspm(ImputedBank, LatentConnectionsTest,sat_blocks,modes = sat_modes,
               scheme = "centroid", maxiter = 100, tol = 0.000001, scaled= TRUE)
plsres$outer_model
plsres$inner_model
plsres$path_coefs
plsres$scores
plsres$crossloading
plsres$inner_summary
plsres$effects
plsres$unidim
plsres$gof
plsres$boot
plsres$data
