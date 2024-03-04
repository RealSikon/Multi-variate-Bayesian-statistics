#This is code for measuring the attribute disclosure risk of synthetic datasets,
#as provided by https://arxiv.org/pdf/2103.09805.pdf

#Installation of standard packages:
#install.packages(c("rstanarm", "brms", "synthpop"))

#Installation of packages provided by Ryan Hornby:
#install_github("RyanHornby/AttributeRiskCalculation")
#devtools::install_github("https://github.com/RyanHornby/IdentificationRiskCalculation")

#require(IdentificationRiskCalculation)
#require(synthpop)
library(devtools)
require(rstanarm)
require(brms)
require(AttributeRiskCalculation)

#Using Synthpop to make a fully synthetic dataset of CEdata
#syn_data <- syn(CEdata, m = 20)

#Synthesizing continuous Expenditure and Income sequentially with the stan_glm() function
syn_normal_brms = function(orig_data, syn_data,
                           model_brms = brmsformula(outcome ~ 1),
                           chains = 1, iterations = 1000, m = 20, thin = 5) {
  ff = as.formula(model_brms)
  utils::str(model <- model.frame(ff, syn_data))
  X = model.matrix(ff, model)

  fit = stan_glm(
    model_brms,
    data = orig_data,
    family = gaussian(),
    prior = normal(0, 2, autoscale = FALSE),
    refresh = 0,
    chains = chains, iter = iterations
  )

  #### Synthesis ####
  N = length(orig_data[,1])
  draws = as.data.frame(fit)
  start = length(draws[,1]) - thin * (m - 1)
  syndata = vector("list", m)
  for (i in 1:m){
    indx = start + thin * (i - 1)
    draws_exp_mean = as.matrix(X) %*%
      t(draws[indx, !names(draws) %in% c("sigma")])
    draws_sd = draws[indx, "sigma"]
    syndata[[i]] = rnorm(N, mean = draws_exp_mean, sd = draws_sd)
  }
  return(list(syndata, draws))
}

CEdata_syn_cont = CEdata
draws_cont = list()
synthesis_cont = syn_normal_brms(CEdata, CEdata_syn_cont,
                                 bf(Expenditure ~ 1), m = 1)
CEdata_syn_cont$Expenditure = synthesis_cont[[1]][[1]]
synthesis_cont2 = syn_normal_brms(CEdata, CEdata_syn_cont,
                                  bf(Income ~ Expenditure), m = 1)
CEdata_syn_cont$Income = synthesis_cont2[[1]][[1]]
draws_cont[[1]] = synthesis_cont[[2]]
draws_cont[[2]] = synthesis_cont2[[2]]
CEdata_syn_cont = list(CEdata_syn_cont)


#G: Number of guesses  (the true confidential value plus 10 guesses in the neighborhood within a 20% range of the true confidential value)
#   Can be calculated as:
#     y_i_guesses = seq(y_i*0.9, y_i*1.1, length.out = 11)
#G <- 11 

#H: The number of posterior parameter draws in the importance sampling step.
#H <- 50

#Synthesis of Race given Incomee,the confidential dataset
#(CEdata), the synthetic dataset (CEdata_syn_cat), and MCMC draws (draws_cat).
#We use c("multinom") for the synthesizer type for categorical Race and the default value of H (H = 50).
Two_Cont = AttributeRisk(modelFormulas = list(bf(Expenditure ~ 1),
                                              bf(Income ~ Expenditure)),
                         origdata = CEdata,
                         syndata = CEdata_syn_cont,
                         posteriorMCMCs = draws_cont,
                         syntype = c("norm", "norm"),
                         G = c(11, 11),
                         H = 50)



