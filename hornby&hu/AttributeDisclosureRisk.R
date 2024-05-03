# This is code for measuring the attribute disclosure risk of synthetic datasets,
# as provided by https://arxiv.org/pdf/2103.09805.pdf

# Installation of standard packages:
# Install.packages(c("rstanarm", "brms", "synthpop", "arrayhelpers", "ggplot2", "arrayhelpers", "matrixStats", "progress"))
library(synthpop)
library(devtools)
library(rstanarm)
library(brms)
library(ggplot2)
library(jsonlite)
# Auxiliary functions
source("PlotFunctions.R")
source("ModelFitting.R")
source("Functions.R")

# Installation of packages provided by Ryan Hornby:
# install_github("https://github.com/RyanHornby/IdentificationRiskCalculation")
require(IdentificationRiskCalculation)
# install_github("https://github.com/RyanHornby/AttributeRiskCalculation")
require(AttributeRiskCalculation)

# Set directory
current_directory <- getwd()
parent_directory  <- dirname(current_directory)
data_directory    <- file.path(parent_directory, "data")
plot_directory    <- file.path(parent_directory, "plots")

real_data_name <- "CEData.csv"
syn_data_name  <- "synthetic_data.csv"

real_data <- data_loader(real_data_name, 200)
syn_data  <- data_loader(syn_data_name, 200)

# Load JSON data, Contains: [meta, attribute description, Bayesian network, conditional probabilities]
privbayes_model <- fromJSON(file.path(data_directory, "description.json"))
meta <- privbayes_model[[1]] # [#tuples],[#attributes],[#attributesInBN],[attributeList],[candidateKeyList],[nonCategoricalStringAttributeList],[attributesInBN]]
attribute_description <- privbayes_model[[2]] # unused
bayesian_network <- privbayes_model[[3]] # [[child], [[parent_1], [parent_2], ... [parent_k]]]
conditional_probabilities <- privbayes_model[[4]] # unused


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
string_parser(paste("synthesis_list[[1]] = syn_normal_brms(real_data, syn_data, ", formulars[[1]], ", m = 1)", sep = ""))
for (APPair in 2:(length(formulars))) {
  string_parser(paste("synthesis_list[[", APPair, "]] = syn_normal_brms(real_data, syn_data, ", formulars[[APPair]], ", m = 1)", sep = ""))
}

syn_data <- list(syn_data)

# G: Number of guesses  (the true confidential value plus 10 guesses in the neighborhood within a 20% range of the true confidential value)
# G <- 11

# H: The number of posterior parameter draws in the importance sampling step.
# H <- 50

print("Measuring AttributeDisclosureRisk")

draws_conts <- list()

# Store the draws for synthesis.
# If we have multiple formulas in AttributeRisk():
for (synthesis in seq_along(synthesis_list)) {
  draws_conts[[synthesis]] <- synthesis_list[[synthesis]][[2]]
}

risk_list <- list()

for (formular in seq_along(formulars)) {
  risk_list[[formular]] <- AttributeRisk(
    modelFormulas = string_parser(paste("list(", formulars[[formular]], ")", sep = "")),
    origdata = real_data,
    syndata = syn_data,
    posteriorMCMCs = draws_conts[formular],
    syntype = c("norm", "norm"),
    G = c(11, 11),
    H = 1
  )
}

print("Generating plots")

#' Produces a graph of the probabilities of each guess with a line indicating
#' the chance of randomly guessing the confidential value(s) from among the
#' guesses.
#' Density of the joint posterior probability of correctly guessing the true
#' value of both LogIncome and LogExpenditure. The vertical line shows the prior probability of 1/121.
for (risk in seq_along(risk_list)) {
  plot_name <- paste("randomGuess_", nodes[[risk]], ".png", sep = "")
  ggsave(plot_name, device = "png", randomGuessPlot(risk_list[[risk]]), path = plot_directory)
}

#' The rank of posterior probability of the true pair of values
#' (of LogIncome and LogExpenditure being guessed correctly, among 121 (11*11) guesses)
for (risk in seq_along(risk_list)) {
  plot_name <- paste("posteriorRank_", nodes[[risk]], ".png", sep = "")
  ggsave(plot_name, device = "png", posteriorRankPlot(risk_list[[risk]]), path = plot_directory)
}

# Print first two plots for all formulas


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
