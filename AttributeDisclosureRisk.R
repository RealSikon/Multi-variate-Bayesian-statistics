#This is code for measuring the attribute disclosure risk of synthetic datasets,
#as provided by https://arxiv.org/pdf/2103.09805.pdf

#Installation of standard packages:
#install.packages(c("rstanarm", "brms", "synthpop", "arrayhelpers", "ggplot2", "arrayhelpers", "matrixStats", "progress"))
require(synthpop)
library(devtools)
require(rstanarm)
require(brms)
require(ggplot2)
source("PlotFunctions.R")
source("ModelFitting.R")

#Installation of packages provided by Ryan Hornby:
install_github("https://github.com/RyanHornby/IdentificationRiskCalculation")
require(IdentificationRiskCalculation)
install_github("https://github.com/RyanHornby/AttributeRiskCalculation")
require(AttributeRiskCalculation)

#Using synthesised data made using PrivBayes on CEData, where TotalIncomeLastYear and TotalExpLastQ has been removed.
CEData_cut = subset(CEdata[1:200, ], select = -c(TotalIncomeLastYear, TotalExpLastQ))
#Need to have a synthetic dataset (synthetic_data.csv) in current directory
file <- "synthetic_data.csv"
CEdata_syn = read.csv(file)
#Take only first 200 rows
CEdata_syn_cut <- CEdata_syn[1:200, ]
draws_cont = list()
draws_cont1 = list()
draws_cont2 = list()
draws_cont3 = list()
draws_cont4 = list()

# Models from ModelFitting.R that can be used for synthesising and/or fitting:
#   syn_normal_brms()      : Fits a stan_glm to the data ("norm" distribution) 
#   syn_multinomial_brms() : Fits a brm to the data ("multinom" distribution)
#   syn_pois_brms()        : Fits a brm to the data ("pois" distribution)

#Root node LogIncome
synthesis_LogIncome = syn_normal_brms(CEData_cut, 
                                           CEdata_syn_cut,
                                           bf(LogIncome ~ 1),
                                           m = 1
)

# LogExpenditure has parents ['LogIncome'].
synthesis_LogExpenditure = syn_normal_brms(CEData_cut, 
                                           CEdata_syn_cut,
                                           bf(LogExpenditure ~ LogIncome),
                                           m = 1
)

# KidsCount      has parents ['LogExpenditure', 'LogIncome'].
synthesis_KidsCount = syn_normal_brms(CEData_cut, 
                                           CEdata_syn_cut,
                                           bf(KidsCount ~ LogExpenditure + LogIncome),
                                           m = 1
)

# Race           has parents ['LogExpenditure', 'LogIncome'].
synthesis_Race = syn_normal_brms(CEData_cut, 
                                      CEdata_syn_cut,
                                      bf(Race ~ LogExpenditure + LogIncome),
                                      m = 1
)

# UrbanRural     has parents ['Race', 'LogIncome'].
synthesis_UrbanRural = syn_normal_brms(CEData_cut, 
                                 CEdata_syn_cut,
                                 bf(UrbanRural ~ Race + LogIncome),
                                 m = 1
)

CEdata_syn_cut = list(CEdata_syn_cut)

#G: Number of guesses  (the true confidential value plus 10 guesses in the neighborhood within a 20% range of the true confidential value)
#G <- 11 

#H: The number of posterior parameter draws in the importance sampling step.
#H <- 50

print("Measuring AttributeDisclosureRisk")

#Store the draws for synthesis. 
#If we have multiple formulas in AttributeRisk():
draws_cont[[1]] = synthesis_LogIncome[[2]]
draws_cont[[2]] = synthesis_LogExpenditure[[2]]
#If we use individual formulas in AttributeRisk()
draws_cont1[[1]] = synthesis_LogExpenditure[[2]]
draws_cont2[[1]] = synthesis_KidsCount[[2]]
draws_cont3[[1]] = synthesis_Race[[2]]
draws_cont4[[1]] = synthesis_UrbanRural[[2]]

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
risk1 = AttributeRisk(modelFormulas = list(bf(LogIncome ~ 1),
                                              bf(LogExpenditure ~ LogIncome)
                                              ),
                         origdata = CEData_cut,
                         syndata = CEdata_syn_cut,
                         posteriorMCMCs = draws_cont,
                         syntype = c("norm", "norm"),
                         G = c(11, 11),
                         H = 1
                         )
print("twocontLELI")
risk2 = AttributeRisk(modelFormulas = list(bf(LogExpenditure ~ LogIncome)
                                                  ),
                        origdata = CEData_cut,
                        syndata = CEdata_syn_cut,
                        posteriorMCMCs = draws_cont1,
                        syntype = c("norm"),
                        G = c(11, 11),
                        H = 1
                        )



print("twocontKCLELI")
risk3 = AttributeRisk(modelFormulas = list(bf(KidsCount ~ LogExpenditure + LogIncome)
                                               ),
                         origdata = CEData_cut,
                         syndata = CEdata_syn_cut,
                         posteriorMCMCs = draws_cont2,
                         syntype = c("norm"),
                         G = c(11, 11),
                         H = 1
                         )
print("twocontRLELI")
risk4 = AttributeRisk(modelFormulas = list(bf(Race ~ LogExpenditure + LogIncome)),
                          origdata = CEData_cut,
                          syndata = CEdata_syn_cut,
                          posteriorMCMCs = draws_cont3,
                          syntype = c("norm"),
                          G = c(11, 11),
                          H = 1
                          )
print("twocontURRLI")
risk5 = AttributeRisk(modelFormulas = list(bf(UrbanRural ~ Race + LogIncome)),
                          origdata = CEData_cut,
                          syndata = CEdata_syn_cut,
                          posteriorMCMCs = draws_cont4,
                          syntype = c("norm"),
                          G = c(11, 11),
                          H = 1
)


print("Making plots")
risk_list <- list(risk1, risk2, risk3, risk4, risk5)
x <- length(risk_list)

# randomGuessPlot(risks[[i]])
#' Produces a graph of the probabilities of each guess with a line indicating 
#' the chance of randomly guessing the confidential value(s) from among the 
#' guesses.
#' Density of the joint posterior probability of correctly guessing the true
#' value of both LogIncome and LogExpenditure. The vertical line shows the prior probability of 1/121.
for (j in 1:x){
  plotName <- paste("randomguessPlot", j, ".png", sep = "")
  ggsave(plotName, device = "png", randomGuessPlot(risk_list[[j]]), path = "plots")
}
 

# posteriorRankPlot(risks[[i]]):
#' The rank of posterior probability of the true pair of values 
#' (of LogIncome and LogExpenditure being guessed correctly, among 121 (11*11) guesses)
for (j in 1:x){
  plotName <- paste("posteriorRankPlot", j, ".png", sep = "")
  ggsave(plotName, device = "png", posteriorRankPlot(risk_list[[j]]), path = "plots")
}

#Print first two plots for all formulas


# marginalPosteriorProbabilitiesPlot(risks[[i]]):
#' Density of the marginal posterior probabilities of correctly guessing the
#' true value (of LogIncome and LogExpenditure, respectively). The vertical line shows
#' the prior probability (of 1/11).
#' NEEDS TWO FORMLUAS

plotName <- paste("marginalPosteriorProbabilitiesPlot1.png")
ggsave(plotName, device = "png", marginalPosteriorProbabilitiesPlot(risk_list[[1]]), path = "plots")
# absoluteDifferencePlot
#' Density of the absolute difference between the true value and the
#' guessed value with the largest marginal
#' posterior probability (for both LogIncome and LogExpenditure).
#' NEEDS TWO FORMLUAS
plotName <- paste("absoluteDifferencePlot1.png")
ggsave(plotName, device = "png", absoluteDifferencePlot(risk_list[[1]]), path = "plots")
