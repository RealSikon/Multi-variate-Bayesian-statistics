#This is code for measuring the attribute disclosure risk of synthetic datasets,
#as provided by https://arxiv.org/pdf/2103.09805.pdf

#Installation of standard packages:
#Install.packages(c("rstanarm", "brms", "synthpop", "arrayhelpers", "ggplot2", "arrayhelpers", "matrixStats", "progress"))
library(synthpop)
library(devtools)
library(rstanarm)
library(brms)
library(ggplot2)
library(jsonlite)
#Auxiliary functions
source("PlotFunctions.R")
source("ModelFitting.R")

#Installation of packages provided by Ryan Hornby:
#install_github("https://github.com/RyanHornby/IdentificationRiskCalculation")
require(IdentificationRiskCalculation)
#install_github("https://github.com/RyanHornby/AttributeRiskCalculation")
require(AttributeRiskCalculation)

#Set directory
current_directory <- getwd()
parent_directory  <- dirname(current_directory)
data_directory    <- file.path(parent_directory, "data")
plot_directory    <- file.path(parent_directory, "plots")


# Load JSON data, Contains: [meta, attribute description, Bayesian network, conditional probabilities]
privbayesModel           <- fromJSON(file.path(data_directory, "description.json"))
meta                     <- privbayesModel[[1]] #[#tuples],[#attributes],[#attributesInBN],[attributeList],[candidateKeyList],[nonCategoricalStringAttributeList],[attributesInBN]]
attributeDescription     <- privbayesModel[[2]] #unused
bayesianNetwork          <- privbayesModel[[3]] #[[child], [[parent_1], [parent_2], ... [parent_k]]]
conditionalProbabilities <- privbayesModel[[4]] #unused

#Load synthetic data, and data description and cut real data 

#Might not be an issue but syn_data is a list with a dataframe and not "just" a dataframe
real_data <- read.csv(file.path(data_directory, "CEData.csv"))[1:200, ]
syn_data  <- read.csv(file.path(data_directory, "synthetic_data.csv"))[1:200, ]

#List of formulars
formulars <- list()
    nodes <- list()
# Add the root conditionals(node + parent) to the list formulars (0 parents)
nodes[[1]] = bayesianNetwork[[1]][[2]]

formulars[[1]] = paste("bf(", nodes[[1]], " ~ 1)", sep="")
# Add other conditionals  | APPair = attribute parent pair | TODO: add support for k number of children
for (APPair in 1:length(bayesianNetwork)){
  nodes[[APPair+1]] = bayesianNetwork[[APPair]][[1]]
  formulars[[APPair+1]] = paste("bf(", nodes[[APPair+1]], " ~ ", bayesianNetwork[[APPair]][[2]][[1]], sep="")
  if (length(bayesianNetwork[[APPair]][[2]]) > 1){
    formulars[[APPair+1]] = paste(formulars[[APPair+1]], " + ", bayesianNetwork[[APPair]][[2]][[2]], sep="")
  }
  formulars[[APPair+1]] = paste(formulars[[APPair+1]], ")", sep="")
}

# Models from ModelFitting.R that can be used for synthesising and/or fitting:
#   syn_normal_brms()      : Fits a stan_glm to the data ("norm" distribution) 
#   syn_multinomial_brms() : Fits a brm to the data ("multinom" distribution)
#   syn_pois_brms()        : Fits a brm to the data ("pois" distribution)


#Root node LogIncome
synthesis_list <- list()
#Synthesise based on the formulars
formula_str <- paste("synthesis_list[[1]] = syn_normal_brms(real_data, syn_data, ", formulars[[1]], ", m = 1)", sep="")
eval(parse(text = formula_str))
for (APPair in 2:(length(formulars))){
  formula_str <- paste("synthesis_list[[", APPair, "]] = syn_normal_brms(real_data, syn_data, ", formulars[[APPair]], ", m = 1)", sep="")
  eval(parse(text = formula_str))
}

syn_data = list(syn_data)

#G: Number of guesses  (the true confidential value plus 10 guesses in the neighborhood within a 20% range of the true confidential value)
#G <- 11 

#H: The number of posterior parameter draws in the importance sampling step.
#H <- 50

print("Measuring AttributeDisclosureRisk")

draws_conts <- list()

#Store the draws for synthesis. 
#If we have multiple formulas in AttributeRisk():
for (synthesis in 1:(length(synthesis_list))){
  draws_conts[[synthesis]] <- synthesis_list[[synthesis]][[2]] 
}

risk_list <- list()

for (formular in 1:length(formulars)){
  risk_list[[formular]] <- AttributeRisk(
                                           modelFormulas = eval(parse(text = paste("list(", formulars[[formular]], ")", sep=""))),
                                                origdata = real_data,
                                                 syndata = syn_data,
                                          posteriorMCMCs = draws_conts[formular],
                                                 syntype = c("norm", "norm"),
                                                       G = c(11, 11),
                                                       H = 1
                                        )
}

print("Generating plots")

# randomGuessPlot(risks[[i]])
#' Produces a graph of the probabilities of each guess with a line indicating 
#' the chance of randomly guessing the confidential value(s) from among the 
#' guesses.
#' Density of the joint posterior probability of correctly guessing the true
#' value of both LogIncome and LogExpenditure. The vertical line shows the prior probability of 1/121.
for (risk in 1:length(risk_list)){
  plotName <- paste("randomGuess_", nodes[[risk]], ".png", sep = "")
  ggsave(plotName, device = "png", randomGuessPlot(risk_list[[risk]]), path = plot_directory)
}
 

# posteriorRankPlot(risks[[i]]):
#' The rank of posterior probability of the true pair of values 
#' (of LogIncome and LogExpenditure being guessed correctly, among 121 (11*11) guesses)
for (risk in 1:length(risk_list)){
  plotName <- paste("posteriorRank_", nodes[[risk]], ".png", sep = "")
  ggsave(plotName, device = "png", posteriorRankPlot(risk_list[[risk]]), path = plot_directory)
}

#Print first two plots for all formulas


#marginalPosteriorProbabilitiesPlot(risks[[i]]):
#' Density of the marginal posterior probabilities of correctly guessing the
#' true value (of LogIncome and LogExpenditure, respectively). The vertical line shows
#' the prior probability (of 1/11).
#' NEEDS TWO FORMLUAS

#plotName <- paste("marginalPosteriorProbabilitiesPlot1.png")
#ggsave(plotName, device = "png", marginalPosteriorProbabilitiesPlot(risk_list[[1]]), path = plot_directory)
# absoluteDifferencePlot
#' Density of the absolute difference between the true value and the
#' guessed value with the largest marginal
#' posterior probability (for both LogIncome and LogExpenditure).
#' NEEDS TWO FORMLUAS
#plotName <- paste("absoluteDifferencePlot1.png")
#ggsave(plotName, device = "png", absoluteDifferencePlot(risk_list[[1]]), path = plot_directory)