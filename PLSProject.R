
rm(list=ls())

# Set the directory
setwd("C:\\Users\\Ricardo\\Sync\\NOVA\\Second semester\\Descriptive analytics - Estadistica l\\Final project")

#read and discover the data 
bank = read.csv("bank.csv")
summary(bank)

#Remove the missing rows from the dataset
bank2 <- bank[rowSums(is.na(bank)) != ncol(bank)-1, ]
summary(bank2)
#check how many variables are missing in total 
nrow(bank) - nrow(bank2)

#Clean the ID column
bank2$ID = 1:nrow(bank2)

#Imputing missing values with pmm method and with R package of Mice
install.packages("mice")
library(mice)

#The data set with different imputations of missing values 
TempMiss = mice(bank2, m=10, method = "pmm", maxit = 50, seed = 500 )

#some variable imputations (each with 10 imputation values for each missing value)
TempMiss$imp$IMAG1
TempMiss$imp$IMAG2

#Imputed bank data set with the first imputation among the 10
ImputedBank = complete(TempMiss)
ImputedBank = ImputedBank[,c(2:25)]
summary(ImputedBank)

# Building the Inner Model
InModel = matrix(c("Image", "Loyalty", "Image", "Expectations", "Image",
                   "Satisfaction", "Expectations", "Quality", "Expectations", "Value", "Expectations", "Satisfaction", "Quality",
                   "Satisfaction", "Quality", "Value", "Value", "Satisfaction", "Satisfaction", "Loyalty"), ncol=2, byrow = TRUE)
InModel

colnames(InModel) = c("Source", "Target")

# Building the Outer Model
OutModel = matrix(c("Image", "IMAG1", "Image", "IMAG2","Image", "IMAG3",	"Image", "IMAG4","Image", "IMAG5"
                    , "Expectations","EXPE1",	"Expectations", "EXPE2", "Expectations", "EXPE3",
                    "Quality", "QUAL1",	"Quality", "QUAL2",	"Quality", "QUAL3", "Quality", "QUAL4",	"Quality", "QUAL5",
                    "Quality", "QUAL6", "Quality", "QUAL7", "Quality", "QUAL8", "Quality","QUAL9", "Value",	"VALU1","Value","VALU2",
                    "Satisfaction","SATI1", "Satisfaction", "SATI2" ,"Satisfaction", "SATI3", "Loyalty", "LOYA1", "Loyalty",	"LOYA2"),ncol=2,byrow = TRUE)
OutModel
colnames(OutModel) = c("Source", "Target")

# Preaparing the Model
LatentVariables = unique(OutModel[,1])
ManifestVariables = OutModel[,2]
NbLatentVariables = length(LatentVariables)
NbManifestVariables = length(ManifestVariables)

#Building Pls wheights and paths coefficient Matrices
PlsWeights = matrix(rep(0,NbManifestVariables*NbLatentVariables), nrow = NbManifestVariables, ncol = NbLatentVariables)
colnames(PlsWeights) = LatentVariables
rownames(PlsWeights) = ManifestVariables

PathCoeffs = matrix(rep(0,NbLatentVariables*NbLatentVariables), nrow = NbLatentVariables, ncol = NbLatentVariables)
colnames(PathCoeffs) = LatentVariables
rownames(PathCoeffs) = LatentVariables


#Vector of occurences of each latentVariable
NbManifestPerLatent = c(rep(0,length(LatentVariables)))
for(i in 1:length(LatentVariables)){
  NbManifestPerLatent[i] = c(length(grep(LatentVariables[i], OutModel[,1])));
}

#Temporary Vector of weights
WeightsTemp = c(rep(1/24,24))


#List of Weights Per Latent variable
WeightsTemp2 = WeightsTemp
WeightsPerLatent = list()
for(i in 1:NbLatentVariables){
  WeightsPerLatent[[i]] = WeightsTemp2[1:NbManifestPerLatent[i]]
  WeightsTemp2 = WeightsTemp2[-c(1:NbManifestPerLatent[i])]
}

#Corresponding data set for each Latent Variable
BankSetPerLatent = list()
CopyImputedBank = ImputedBank
for(i in 1:NbLatentVariables){
  BankSetPerLatent[[i]] = CopyImputedBank[,c(1:NbManifestPerLatent[i])]
  CopyImputedBank = CopyImputedBank[-c(1:NbManifestPerLatent[i])]
}

#Build Y1...Yn
Y = list()
for(i in 1:length(BankSetPerLatent)){
  Y[[i]] = c(rep(0,nrow(ImputedBank)))
  for(j in 1:length(WeightsPerLatent[[i]])){
    Y[[i]] = Y[[i]] + WeightsPerLatent[[i]][j]*BankSetPerLatent[[i]][,j]
  }
}

#Build the inner weights ei
e=c()
for(i in 1:length(Y)){
  for(j in 1:length(Y)){
    if(i!=j){
      e = c(e,cor(Y[[i]],Y[[j]]))
    }
  }
}

#Build the Zeds z
Z = list();
Hide=1
for(i in 1:length(Y)){
  if (i != Hide){
    
  }
}


