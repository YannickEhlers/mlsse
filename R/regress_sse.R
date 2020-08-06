#'@title Regress_SSE
#'
#'@description This function provides a simulation for the behavior of a defined measure of a learners performance with increasing numbers of cases

#'@param a_data A data set object, containing oberservations of the parameters
#'@param nseq_SampleSizeToTest A sequence of integers, defining the numbers of sample sizes, which should be tested
#'@param c_learner The name of a regression learner from the mlr3 package
#'@param c_measurment The name of a measure for regressiors from the mlr3 package
#'@param n_bootstrap The number of bootstraps, how often the learning task should be repeated
#'@param c_classificationTarget A parameter of the data object, which should be used as target for the regression
#'
#'@return The output can be used for plotting in the *plotting function
#'@import mlr3
#'@import mlr3learners
#'@import mlr3measures
#'@export

Regress_SSE <- function(a_data, nseq_SampleSizeToTest, c_learner, c_measurment, n_bootstraps, c_RegressionTarget){

  c_Usedlearner <- lrn(c_learner)
  c_Usedmeasure <- msr(c_measurment)

  m_ResMatrix <- matrix(NA, nrow = length(nseq_SampleSizeToTest), ncol = n_bootstraps)

  for (i in nseq_SampleSizeToTest){

    Run <- which(i == nseq_SampleSizeToTest)

    a_dataUsedForAccuracyCalculation <- a_data[sample(nrow(a_data), i),]

    task <- TaskRegr$new(id = "Regress", backend = a_dataUsedForAccuracyCalculation, target = c_RegressionTarget)

    for (b in 1:n_bootstraps){

      train_set <- sample(task$nrow, 0.8 * task$nrow)
      test_set <- setdiff(seq_len(task$nrow), train_set)

      c_Usedlearner$train(task, row_ids = train_set)
      prediction <- c_Usedlearner$predict(task, row_ids = test_set)
      m_ResMatrix[Run,b] <- prediction$score(c_Usedmeasure)
    }
  }

  setClass("Simulation Sample Size Estimation", slots=list(MachineLearningType="character",Learner="character",Measurement="character",Bootstrap="numeric",SampleSizesTested="numeric",MeasuredMatrix="matrix"))

  OutputObject <- new(Class = "Simulation Sample Size Estimation", MachineLearningType="Regression",Learner=c_learner,Measurement=c_measurment,Bootstrap=n_bootstraps,SampleSizesTested=nseq_SampleSizeToTest,MeasuredMatrix=m_ResMatrix)

  return(OutputObject)
}
