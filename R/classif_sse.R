#'@title Classif_SSE
#'
#'@description This function provides a simulation for the behavior of a defined measure of a learners performance with increasing numbers of cases

#'@param a_data A data set object, containing oberservations of the parameters
#'@param nseq_SampleSizeToTest A sequence of integers, defining the numbers of sample sizes, which should be tested
#'@param c_learner The name of learner from the mlr3 package
#'@param c_measurment The name of a measure from the mlr3 package
#'@param n_bootstrap The number of bootstraps, how often the learning task should be repeated
#'@param c_classificationTarget A parameter of the data object, which should be used as target for the classification
#'
#'@return The output can be used for plotting in the *plotting function
#'@export



Classif_SSE <- function(a_data, nseq_SampleSizeToTest, c_learner, c_measurment, n_bootstraps, c_classificationTarget){

  #Variables Collection
  c_Usedlearner <- lrn(c_learner)
  c_Usedmeasure <- msr(c_measurment)

  m_ResMatrix <- matrix(NA, nrow = length(nseq_SampleSizeToTest), ncol = n_bootstraps)

  #Data synthesize, if needed
  n_MaxSampleSizeToTest <- max(nseq_SampleSizeToTest)
  n_RowData <- nrow(a_data)

  if (n_MaxSampleSizeToTest > n_RowData){

    n_MissingValues <- n_MaxSampleSizeToTest - n_RowData

    a_dataForSynthesis <- a_data[sample(nrow(a_data), n_MissingValues),]

    l_ClassesData <- sapply(a_data, class)

    l_NomialVariables <-names(which(l_ClassesData == "factor"))
    l_CharacterVariables <- names(which(l_ClassesData == "character"))

    for (i in (1:nrow(a_dataForSynthesis))){
      a_dataForSynthesis[i,sample(1:ncol(a_dataForSynthesis),round(col(a_dataForSynthesis)))] <- NA
    }

    a_dataSynthesized <- amelia(a_dataForSynthesis, noms = c(l_NomialVariables, l_CharacterVariables), parallel = "multicore", ncpus = 4)

    a_dataComplete <- rbind(a_data, a_dataSynthesized$imputations$imp1)
  }else{

    a_dataComplete <- a_data
  }


  for (i in nseq_SampleSizeToTest){

    Run <- which(i == nseq_SampleSizeToTest)

    a_dataUsedForAccuracyCalculation <- a_dataComplete[sample(nrow(a_dataComplete), i),]

    task <- TaskClassif$new(id = "Classif", backend = a_dataUsedForAccuracyCalculation, target = c_classificationTarget)


    for (b in 1:n_bootstraps){

      # Data harvesting

      train_set <- sample(task$nrow, 0.8 * task$nrow)
      test_set <- setdiff(seq_len(task$nrow), train_set)

      c_Usedlearner$train(task, row_ids = train_set)
      prediction <- c_Usedlearner$predict(task, row_ids = test_set)
      m_ResMatrix[Run,b] <- prediction$score(c_Usedmeasure)
    }
  }


  #plot(x = nseq_SampleSizeToTest, y = rowMeans(m_ResMatrix), type = "l", col ="blue", main = "Accuracy", ylab = c_measurment, xlab = "N Sample")

  setClass("Simulation Sample Size Estimation", slots=list(MachineLearningType="character",Learner="character",Measurement="character",Bootstrap="numeric",SampleSizesTested="numeric",MeasuredMatrix="matrix"))

  OutputObject <- new(Class = "Simulation Sample Size Estimation", MachineLearningType="Classification",Learner=c_learner,Measurement=c_measurment,Bootstrap=n_bootstraps,SampleSizesTested=nseq_SampleSizeToTest,MeasuredMatrix=m_ResMatrix)

  return(OutputObject)
}

