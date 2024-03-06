#This is code for measuring the attribute disclosure risk of synthetic datasets,
#as provided by https://arxiv.org/pdf/2103.09805.pdf

#Installation of standard packages:
#install.packages(c("rstanarm", "brms", "synthpop", "arrayhelpers", "ggplot2", "arrayhelpers", "matrixStats", "progress"))
require(synthpop)
library(devtools)
require(rstanarm)
require(brms)
source("PlotFunctions.R")

#Installation of packages provided by Ryan Hornby:
install_github("https://github.com/RyanHornby/IdentificationRiskCalculation")
require(IdentificationRiskCalculation)
install_github("https://github.com/RyanHornby/AttributeRiskCalculation")
require(AttributeRiskCalculation)

#Using Synthpop to make a fully synthetic dataset of CEdata
#syn_data <- syn(CEdata, m = 20)

#Synthesizing continuous LogExpenditure and LogIncome sequentially with the stan_glm() function
syn_normal_brms = function(orig_data,         #Real dataset
                           syn_data,          #Synthetic dataset
                           model_brms,        #Formula to be used for synthesis
                           chains = 1,        #Number of chains to be used in regression model
                           iterations = 1000, #Iterations used to fit regression model
                           m = 20,            #Number of iterations over samples
                           thin = 5           #the thinning interval (i.e., the number of iterations to skip between samples)
                           ) 
  {
  
  ff = as.formula(model_brms)
  
  #Provide a summary of model
  utils::str(model <- model.frame(ff, syn_data)  #model: a data frame of variables specified in the formula, using columns of syn_data that match the variable names in the formula.
             ) 
  X = model.matrix(ff, model)                    #X: a matrix of the predictor variables used in the model
  
  print("Fitting stan_glm")
  
  #Fit a generalized linear model on real data
  fit = stan_glm(
    model_brms,                              #formula used for synthesis
    data = orig_data,                        
    family = gaussian(),                     #function to be used in fitting the generalized linear model
    prior = normal(0, 2, autoscale = FALSE), #prior: a normal distribution with mean = 0, deviation = 2
    refresh = 0,                             #how often to print updates when sampling
    chains = chains,                         #Number of MCMC chains to estimate the posterior distribution
    iter = iterations                        #Number of samples per chain
  )
  
  
  #### Synthesis ####
  print("Synthesising")
  
  N = length(orig_data[,1])                  #N: Length of first column in the real dataset (number of observations)
  draws = as.data.frame(fit)                 #extract the intercept (expected value of the response variable when all predictor variables are equal to zero) and sigma (represents the variability of the response variable) samples from the posterior distribution obtained during the MCMC sampling process
  start = length(draws[,1]) - thin * (m - 1) #declare start index 
  syndata = vector("list", m)                #instantiate vector to contain synthetic data
  
  for (i in 1:m){
    indx = start + thin * (i - 1)                                 #Skip iterations
    draws_exp_mean = as.matrix(X) %*%                             #find expected mean of the synthetic data based on the posterior samples of the model parameters.
      t(draws[indx, !names(draws) %in% c("sigma")])                  #multiply the matrix of predictor variables (X) by the transpose of the selected posterior samples, excluding the "sigma" parameter.
    draws_sd = draws[indx, "sigma"]                               #draws_sd: the standard deviation of the synthetic data
    syndata[[i]] = rnorm(N, mean = draws_exp_mean, sd = draws_sd) #generate synthetic data each iteration with the previously calculated mean and std. deviation
  }
  return(list(syndata, draws))
}

#Assign data variables
CEData_cut <- CEdata[1:200, ] 
CEdata_syn_cont = CEData_cut
draws_cont = list()

synthesis_cont = syn_normal_brms(CEData_cut, 
                                 CEdata_syn_cont,
                                 bf(LogExpenditure ~ 1),
                                 m = 1
                                 )
CEdata_syn_cont$LogExpenditure = synthesis_cont[[1]][[1]]

synthesis_cont2 = syn_normal_brms(CEData_cut, 
                                  CEdata_syn_cont,
                                  bf(LogIncome ~ LogExpenditure), 
                                  m = 1
                                  )
CEdata_syn_cont$LogIncome = synthesis_cont2[[1]][[1]]

draws_cont[[1]] = synthesis_cont[[2]]
draws_cont[[2]] = synthesis_cont2[[2]]

CEdata_syn_cont = list(CEdata_syn_cont)


#G: Number of guesses  (the true confidential value plus 10 guesses in the neighborhood within a 20% range of the true confidential value)
#   Can be calculated as:
#     y_i_guesses = seq(y_i*0.9, y_i*1.1, length.out = 11)
#G <- 11 

#H: The number of posterior parameter draws in the importance sampling step.
#H <- 50

print("Measuring AttributeDisclosureRisk")

#Synthesis of Race given LogIncomee,the confidential dataset
#(CEdata), the synthetic dataset (CEdata_syn_cat), and MCMC draws (draws_cat).
#We use c("multinom") for the synthesizer type for categorical Race and the default value of H (H = 50).
Two_Cont = AttributeRisk(modelFormulas = list(bf(LogExpenditure ~ 1),
                                              bf(LogIncome ~ LogExpenditure)),
                         origdata = CEData_cut,
                         syndata = CEdata_syn_cont,
                         posteriorMCMCs = draws_cont,
                         syntype = c("norm", "norm"),
                         G = c(11, 11),
                         H = 50
                         )

#' Produces a graph of the probabilities of each guess with a line indicating 
#' the chance of randomly guessing the confidential value(s) from among the 
#' guesses.
#' 
#' Density of the joint posterior probability of correctly guessing the true
#' value of both LogIncome and LogExpenditure. The vertical line shows the prior probability of 1/121.
print("Making plots")
randomGuessPlot(Two_Cont)

#' The rank of posterior probability of the true pair of values 
#' (of LogIncome and LogExpenditure being guessed correctly, among 121 (11*11) guesses)
posteriorRankPlot(Two_Cont)

#' Density of the marginal posterior probabilities of correctly guessing the
#' true value (of LogIncome and LogExpenditure, respectively). The vertical line shows
#' the prior probability (of 1/11).
marginalPosteriorProbabilitiesPlot(Two_Cont)

#' Density of the absolute difference between the true value and the
#' guessed value with the largest marginal
#' posterior probability (for both LogIncome and LogExpenditure).
absoluteDifferencePlot(Two_Cont)

