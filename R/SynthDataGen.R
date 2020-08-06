#'@title SynthDataGen
#'
#'@description This function provides a data generation, while using existing data and imputation algorithms.

#'@param a_OriginalData The data, which is available and should be used for the data generation, in form of a data frame
#'@param n_OutputData A integer indicating the number of cases, which should be there at the and of generation.
#'@param c_methode The method, which should be used for data generation, currently only the amelia algorithm is available

#'@return The output is a array, which contains the original and generated data

#'@import Amelia
#'@export

SynthDataGen  <- function(a_OriginalData, i_OutputData, c_method = "amelia"){

  n_CasesOriginalData <- nrow(a_OriginalData)

  n_OutputCases <- i_OutputData

  n_DifferenceImputOutputCases <- n_OutputCases - n_CasesOriginalData

  if(n_DifferenceImputOutputCases < 0){
    print("Number of Output Cases have to be bigger than the number of Cases provided in the original imput data")
  }

  n_ParametersOriginalData <- ncol(a_OriginalData)

  ResMatrix <- matrix(NA, ncol = n_ParametersOriginalData, nrow = i_OutputData)

  for (i in 1:i_OutputData){

    SelectedCase <- sample(1:n_CasesOriginalData, 1)

    SelectedParameters <- sort(sample(1:n_ParametersOriginalData, ceiling(n_ParametersOriginalData/2)))

    ResMatrix[i,SelectedParameters] <- as.matrix(a_OriginalData[SelectedCase, SelectedParameters])

  }

  c_ClassesOriginal_Data <- sapply(a_OriginalData, class)

  ResDataFrame <- as.data.frame(ResMatrix)

  for (i in 1:n_ParametersOriginalData){

    if((c_ClassesOriginal_Data[i] == "numeric") == T){
      ResDataFrame[,i] <- as.numeric(ResMatrix[,i])
    }

    if((c_ClassesOriginal_Data[i] == "factor") == T){
      ResDataFrame[,i] <- as.factor(ResMatrix[,i])
    }

    if((c_ClassesOriginal_Data[i] == "character") == T){
      ResDataFrame[,i] <- as.character(ResMatrix[,i])
    }
  }

  c_NomialVariables <-names(which(c_ClassesOriginal_Data == "factor"))
  c_CharacterVariables <- names(which(c_ClassesOriginal_Data == "character"))

  colnames(ResDataFrame) <- colnames(a_OriginalData)

  a_dataSynthesized <- amelia(ResDataFrame, noms = c(c_NomialVariables, c_CharacterVariables), parallel = "multicore", ncpus = 4)

  return(a_dataSynthesized)
}
