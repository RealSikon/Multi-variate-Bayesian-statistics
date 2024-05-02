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
#real_Data <- read.csv(file.path(data_directory, "synthetic_data_origin.csv"))[1:meta[[1]], ]
#syn_Data  <- read.csv(file.path(data_directory, "synthetic_data.csv"))

# Put data into dataframes
#Might not be an issue but syn_data is a list with a dataframe and not "just" a dataframe
real_data <- subset(CEdata[1:200, ], )
syn_data  <- read.csv(file.path(data_directory, "synthetic_data_origin.csv"))[1:200, ]


#List of formulars
formulars = list()
# Add the root conditionals(node + parent) to the list formulars (0 parents)
formulars[[1]] = paste("bf(", bayesianNetwork[[1]][[2]], " ~ 1)", sep="")
# Add other conditionals  | APPair = attribute parent pair | TODO: add support for k number of children
for (APPair in 1:length(bayesianNetwork)){
  formulars[[APPair+1]] = paste("bf(", bayesianNetwork[[APPair]][[1]], " ~ ", bayesianNetwork[[APPair]][[2]][[1]], sep="")
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
formular_parameters = list()

synthesis_list <- list()
synthesis_name <- list()
#Synthesise based on the formulars
formula_str <- paste("synthesis_list[[1]] = syn_normal_brms(real_data, syn_data, ", formulars[[1]], ", m = 1)", sep="")
synthesis_name[[1]] <- paste("syn_normal_brms(real_data, syn_data, ", formulars[[1]], ", m = 1)", sep="")
eval(parse(text = formula_str))
for (APPair in 2:(length(formulars))){
  formula_str <- paste("synthesis_list[[", APPair, "]] = syn_normal_brms(real_data, syn_data, ", formulars[[APPair]], ", m = 1)", sep="")
  synthesis_name[[APPair]] <- paste("syn_normal_brms(real_data, syn_data, ", formulars[[APPair]], ", m = 1)", sep="")
  eval(parse(text = formula_str))
}

#synthesis_LogIncome = syn_normal_brms(CEData_cut, 
#                                           CEdata_syn_cut,
#                                           bf(LogIncome ~ 1),
#                                           m = 1
#)

#LogExpenditure has parents ['LogIncome'].
#synthesis_LogExpenditure = syn_normal_brms(CEData_cut, 
#                                           CEdata_syn_cut,
#                                           bf(LogExpenditure ~ LogIncome),
#                                           m = 1
#)

#KidsCount      has parents ['LogExpenditure', 'LogIncome'].
#synthesis_KidsCount = syn_normal_brms(CEData_cut, 
#                                           CEdata_syn_cut,
#                                           bf(KidsCount ~ LogExpenditure + LogIncome),
#                                           m = 1
#)

#Race           has parents ['LogExpenditure', 'LogIncome'].
#synthesis_Race = syn_normal_brms(CEData_cut, 
#                                      CEdata_syn_cut,
#                                      bf(Race ~ LogExpenditure + LogIncome),
#                                      m = 1
#)

#UrbanRural     has parents ['Race', 'LogIncome'].
#synthesis_UrbanRural = syn_normal_brms(CEData_cut, 
#                                 CEdata_syn_cut,
#                                 bf(UrbanRural ~ Race + LogIncome),
#                                 m = 1
#)

syn_data = list(syn_data)

#G: Number of guesses  (the true confidential value plus 10 guesses in the neighborhood within a 20% range of the true confidential value)
#G <- 11 

#H: The number of posterior parameter draws in the importance sampling step.
#H <- 50

print("Measuring AttributeDisclosureRisk")

draws_cont  = list()
draws_cont1 = list()
draws_cont2 = list()
draws_cont3 = list()
draws_cont4 = list()

#Store the draws for synthesis. 
#If we have multiple formulas in AttributeRisk():
#for (synthesis in 1:(length(synthesis_list))){
#  draws_cont[[synthesis]] <- synthesis_list[[synthesis]][[2]] 
#}

draws_cont[[1]] <- synthesis_list[[1]][[2]]
draws_cont[[2]] <- synthesis_list[[2]][[2]]
#If we use individual formulas in AttributeRisk()
draws_cont1[[1]] <- synthesis_list[[2]][[2]]
draws_cont2[[1]] <- synthesis_list[[3]][[2]]
draws_cont3[[1]] <- synthesis_list[[4]][[2]]
draws_cont4[[1]] <- synthesis_list[[5]][[2]]
#This does not work:
# Two_Cont = AttributeRisk(modelFormulas = list(bf(LogIncome ~ 1),
#                                               bf(LogExpenditure ~ LogIncome),
#                                               bf(KidsCount ~ LogExpenditure + LogIncome),
#                                               bf(Race ~ LogExpenditure + LogIncome),
#                                               bf(UrbanRural ~ Race + LogIncome)
#                                               ),
#                         origdata = CEData_cut,
#                         syndata = CEdata_syn_cut,
#                         posteriorMCMCs = draws_cont,
#                         syntype = c("norm", "norm", "norm", "norm", "norm"),
#                         G = 11,
#                         H = 1
#                         # categorical = c(0, 0, 1, 1, 0)
#                         )
#Estimate attribute disclosure risk of LogIncome and LogExpenditure
#Something is going wrong here

risk1 = AttributeRisk(modelFormulas = list(bf(LogIncome ~ 1),
                                              bf(LogExpenditure ~ LogIncome)
                                              ),
                         origdata = real_data,
                         syndata = syn_data,
                         posteriorMCMCs = draws_cont,
                         syntype = c("norm", "norm"),
                         G = c(11, 11),
                         H = 1
                         )
print("twocontLELI")

risk2 = AttributeRisk(modelFormulas = list(bf(LogExpenditure ~ LogIncome)
                                                  ),
                        origdata = real_data,
                        syndata = syn_data,
                        posteriorMCMCs = draws_cont1,
                        syntype = c("norm"),
                        G = c(11, 11),
                        H = 1
                        )



print("twocontKCLELI")
risk3 = AttributeRisk(modelFormulas = list(bf(KidsCount ~ LogExpenditure + LogIncome)
                                               ),
                         origdata = real_data,
                         syndata = syn_data,
                         posteriorMCMCs = draws_cont2,
                         syntype = c("norm"),
                         G = c(11, 11),
                         H = 1
                         )
print("twocontRLELI")
risk4 = AttributeRisk(modelFormulas = list(bf(Race ~ LogExpenditure + LogIncome)),
                          origdata = real_data,
                          syndata = syn_data,
                          posteriorMCMCs = draws_cont3,
                          syntype = c("norm"),
                          G = c(11, 11),
                          H = 1
                          )
print("twocontURRLI")
risk5 = AttributeRisk(modelFormulas = list(bf(UrbanRural ~ Race + LogIncome)),
                          origdata = real_data,
                          syndata = syn_data,
                          posteriorMCMCs = draws_cont4,
                          syntype = c("norm"),
                          G = c(11, 11),
                          H = 1
)


print("Making plots")
risk_list <- list(risk1, risk2, risk3, risk4, risk5)

# randomGuessPlot(risks[[i]])
#' Produces a graph of the probabilities of each guess with a line indicating 
#' the chance of randomly guessing the confidential value(s) from among the 
#' guesses.
#' Density of the joint posterior probability of correctly guessing the true
#' value of both LogIncome and LogExpenditure. The vertical line shows the prior probability of 1/121.
for (j in 1:length(risk_list)){
  plotName <- paste("randomguessPlot", j, ".png", sep = "")
  ggsave(plotName, device = "png", randomGuessPlot(risk_list[[j]]), path = plot_directory)
}
 

# posteriorRankPlot(risks[[i]]):
#' The rank of posterior probability of the true pair of values 
#' (of LogIncome and LogExpenditure being guessed correctly, among 121 (11*11) guesses)
for (j in 1:length(risk_list)){
  plotName <- paste("posteriorRankPlot", j, ".png", sep = "")
  ggsave(plotName, device = "png", posteriorRankPlot(risk_list[[j]]), path = plot_directory)
}

#Print first two plots for all formulas


#marginalPosteriorProbabilitiesPlot(risks[[i]]):
#' Density of the marginal posterior probabilities of correctly guessing the
#' true value (of LogIncome and LogExpenditure, respectively). The vertical line shows
#' the prior probability (of 1/11).
#' NEEDS TWO FORMLUAS

plotName <- paste("marginalPosteriorProbabilitiesPlot1.png")
ggsave(plotName, device = "png", marginalPosteriorProbabilitiesPlot(risk_list[[1]]), path = plot_directory)
# absoluteDifferencePlot
#' Density of the absolute difference between the true value and the
#' guessed value with the largest marginal
#' posterior probability (for both LogIncome and LogExpenditure).
#' NEEDS TWO FORMLUAS
plotName <- paste("absoluteDifferencePlot1.png")
ggsave(plotName, device = "png", absoluteDifferencePlot(risk_list[[1]]), path = plot_directory)
