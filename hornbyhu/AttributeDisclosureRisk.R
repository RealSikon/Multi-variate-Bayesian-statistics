# Installation of standard packages:
# Install.packages(c("rstanarm", "brms", "synthpop", "arrayhelpers", "ggplot2", "arrayhelpers", "matrixStats", "progress", "here"))
library(synthpop)
library(devtools)
library(rstanarm)
library(brms)
library(ggplot2)
library(jsonlite)
library(progress)
library(matrixStats)
library(arrayhelpers)
library(here)

# This is code for measuring the attribute disclosure risk of synthetic datasets,
# as provided by https://arxiv.org/pdf/2103.09805.pdf

# Obtains path from python script
args = commandArgs(trailingOnly=TRUE)

# Enables execution both in python and Rstudio
if (length(args) == length(character(0))){
  args[1] = here()
  args[2] = "CEDataR.csv"
}

path = args[1]

# Set directory
current_directory <- path
parent_directory  <- dirname(current_directory)
data_directory    <- file.path(parent_directory, "data")
plot_directory    <- file.path(parent_directory, "plots/hornbyhu")


# Auxiliary functions
source(paste(path, "/AttributeRiskCalculation/R/multiproduct.R", sep = ""))
source(paste(path, "/Functions.R",     sep = ""))
source(paste(path, "/ModelFitting.R",  sep = ""))
source(paste(path, "/PlotFunctions.R", sep = ""))

# Installation of packages provided by Ryan Hornby:
# install_github("https://github.com/RyanHornby/IdentificationRiskCalculation")
require(IdentificationRiskCalculation)
# install_github("https://github.com/RyanHornby/AttributeRiskCalculation")
#require(AttributeRiskCalculation)

useknowngood <- FALSE
if (useknowngood) {
  print("Using known good")
  #data_loader(file_name, cut) cut needs to be less or equal to real dataset
  real_data <- data_loader("kgreal.csv", 100)
  syn_data  <- data_loader("kgsyn.csv", 100)
  
  # Load JSON data, Contains: [meta, attribute description, Bayesian network, conditional probabilities]
  meta             <- desc_loader("kgdesc.json")[[1]] # [#tuples],[#attributes],[#attributesInBN],[attributeList],[candidateKeyList],[nonCategoricalStringAttributeList],[attributesInBN]]
  bayesian_network <- desc_loader("kgdesc.json")[[3]] # [[child], [[parent_1], [parent_2], ... [parent_k]]]
} else {
  
  # Load JSON data, Contains: [meta, attribute description, Bayesian network, conditional probabilities]
  meta             <- desc_loader("description.json")[[1]] # [#tuples],[#attributes],[#attributesInBN],[attributeList],[candidateKeyList],[nonCategoricalStringAttributeList],[attributesInBN]]
  bayesian_network <- desc_loader("description.json")[[3]] # [[child], [[parent_1], [parent_2], ... [parent_k]]]
  #data_loader(file_name, cut) cut needs to be less or equal to real dataset
  real_data <-  as.data.frame(data_loader(args[2], 5126))
  syn_data  <-  as.data.frame(data_loader("synthetic_data.csv", 5126))
  
}

# List of formulars
formulars <- list()
nodes     <- list()
# Add the root conditionals(node + parent) to the list formulars (0 parents)
nodes[[1]] <- bayesian_network[[1]][[2]]

formulars[[1]] <- paste("bf(", nodes[[1]], " ~ 1)", sep = "")
# Add other conditionals  | APPair = attribute parent pair | TODO: add support for k number of children
for (APPair in seq_along(bayesian_network)) {
  nodes[[APPair + 1]] <- bayesian_network[[APPair]][[1]]
  formulars[[APPair + 1]] <- paste("bf(", nodes[[APPair + 1]], " ~ ", bayesian_network[[APPair]][[2]][[1]], sep = "")
  if (length(bayesian_network[[APPair]][[2]]) > 1) {
    formulars[[APPair + 1]] <- paste(formulars[[APPair + 1]], " + ", bayesian_network[[APPair]][[2]][[2]], sep = "")
  }
  formulars[[APPair + 1]] <- paste(formulars[[APPair + 1]], ")", sep = "")
}

# Models from ModelFitting.R that can be used for synthesising and/or fitting:
#   syn_normal_brms()      : Fits a stan_glm to the data ("norm" distribution)
#   syn_multinomial_brms() : Fits a brm to the data ("multinom" distribution)
#   syn_pois_brms()        : Fits a brm to the data ("pois" distribution)


# Root node LogIncome
synthesis_list <- list()
# Synthesise based on the formulars
synthesis_list[[1]] <- string_parser(paste("synthesis_list[[1]] = syn_normal_brms(real_data, syn_data, ", formulars[[1]], ", m = 1)", sep = ""))
for (APPair in 2:(length(formulars))) {
  synthesis_list[[APPair]] <- string_parser(paste("synthesis_list[[", APPair, "]] = syn_normal_brms(real_data, syn_data, ", formulars[[APPair]], ", m = 1)", sep = ""))
}

syn_data <- list(syn_data)


# G: Number of guesses  (the true confidential value plus 10 guesses in the neighborhood within a 20% range of the true confidential value)
# G <- 11

# H: The number of posterior parameter draws in the importance sampling step.
# H <- 50
index_list <- list()
for (i in 1:length(nodes)) {
  index_list[i] <- which(nodes[i] == meta["all_attributes"][[1]])
}

guess_list = list()
for (i in 1:length(real_data)){
  if (length(unique(real_data[[i]])) <= 20){
    guess_list[i] = length(unique(real_data[[i]]))
  }
   else{
    guess_list[i] = 11
   }
}

print("Measuring AttributeDisclosureRisk")

risk_list <- list()

for (i in seq_along(formulars)) {
  print(string_parser(paste("list(", formulars[[i]], ")", sep = "")))
  risk_list[[i]] <- AttributeRisk(
    modelFormulas = string_parser(paste("list(", formulars[[i]], ")", sep = "")),
    origdata = real_data,
    syndata = syn_data,
    posteriorMCMCs = synthesis_list[[i]][2], # Draws
    syntype = "norm",
    H = 1,
    G = guess_list[[index_list[[i]]]],
    additiveBounds = NULL, 
    bounds = NULL,
    guesses = NULL,
    simplePrior = NULL,
    categorical = NULL #c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )
}

print("Generating plots")
#' Produces a graph of the probabilities of each guess with a line indicating
#' the chance of randomly guessing the confidential value(s) from among the
#' guesses.
for (risk in seq_along(risk_list)) {
  #' Density of the joint posterior probability of correctly guessing the true
  #' value of both LogIncome and LogExpenditure. The vertical line shows the prior probability of 1/121.
  ggsave(paste("randomGuessPlot", nodes[[risk]], ".png", sep = ""),
         device = "png",
         randomGuessPlot(risk_list[[risk]]),
         path = plot_directory
  )
  
  #' The rank of posterior probability of the true pair of values
  #' (of LogIncome and LogExpenditure being guessed correctly, among 121 (11*11) guesses)
  ggsave(paste("posteriorRankPlot", nodes[[risk]], ".png", sep = ""),
         device = "png",
         posteriorRankPlot(risk_list[[risk]]),
         path = plot_directory
  )
}



#for (v in seq_along(risk_list)) {
#}


# marginalPosteriorProbabilitiesPlot(risks[[i]]):
#' Density of the marginal posterior probabilities of correctly guessing the
#' true value (of LogIncome and LogExpenditure, respectively). The vertical line shows
#' the prior probability (of 1/11).
#' NEEDS TWO FORMLUAS

# plotName <- paste("marginalPosteriorProbabilitiesPlot1.png")
# ggsave(plotName, device = "png", marginalPosteriorProbabilitiesPlot(risk_list[[1]]), path = plot_directory)
# absoluteDifferencePlot
#' Density of the absolute difference between the true value and the
#' guessed value with the largest marginal
#' posterior probability (for both LogIncome and LogExpenditure).
#' NEEDS TWO FORMLUAS
# plotName <- paste("absoluteDifferencePlot1.png")
# ggsave(plotName, device = "png", absoluteDifferencePlot(risk_list[[1]]), path = plot_directory)