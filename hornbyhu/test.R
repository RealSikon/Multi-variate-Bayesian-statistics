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

require(AttributeRiskCalculation)
CEdata <- AttributeRiskCalculation::CEdata
CEdata[ ,c("TotalIncomeLastYear", "TotalExpLastQ")] <- NULL
CEdata_cut <- CEdata
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
  #### synthesis ####
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
#CEdata_cut <- CEdata[1:1000,]
CEdata_syn_cont = CEdata_cut

synthesis_cont = syn_normal_brms(CEdata_cut, CEdata_syn_cont,
                                 bf(LogExpenditure ~ 1), m = 1)
CEdata_syn_cont$LogExpenditure = synthesis_cont[[1]][[1]]
synthesis_cont2 = syn_normal_brms(CEdata_cut, CEdata_syn_cont,
                                  bf(LogIncome ~ LogExpenditure), m = 1)
CEdata_syn_cont$LogIncome = synthesis_cont2[[1]][[1]]
# synthesis_cont3 = syn_normal_brms(CEdata_cut, CEdata_syn_cont,
#                                   bf(TotalIncomeLastYear ~ LogIncome + LogExpenditure), m = 1)
#CEdata_syn_cont$TotalIncomeLastYear = synthesis_cont3[[1]][[1]]
draws_cont1 <- list()
draws_cont2 <- list()
draws_cont <- list()
draws_cont1[[1]] = synthesis_cont[[2]]
draws_cont2[[1]] = synthesis_cont2[[2]]
draws_cont[[1]] <- synthesis_cont[[2]]
draws_cont[[2]] <- synthesis_cont[[2]]

CEdata_syn_cont = list(CEdata_syn_cont)
contLE = AttributeRisk(modelFormulas = list(bf(LogExpenditure ~ 1)),
                       origdata = CEdata_cut,
                       syndata = CEdata_syn_cont,
                       posteriorMCMCs = draws_cont1,
                       syntype = c("norm"),
                       G = c(11),
                       H = 50)
contLI = AttributeRisk(modelFormulas = list(bf(LogIncome ~ LogExpenditure)),
                       origdata = CEdata_cut,
                       syndata = CEdata_syn_cont,
                       posteriorMCMCs = draws_cont2,
                       syntype = c("norm"),
                       G = c(11),
                       H = 50)
# one_cont = AttributeRisk(modelFormulas = list(bf(TotalIncomeLastYear ~ LogIncome + LogExpenditure)),
#                          origdata = CEdata_cut,
#                          syndata = CEdata_syn_cont,
#                          posteriorMCMCs = draws_cont1,
#                          syntype = c("norm"),
#                          G = c(11),
#                          H = 20)
Two_Cont = AttributeRisk(modelFormulas = list(bf(LogExpenditure ~ 1),
                                              bf(LogIncome ~ LogExpenditure)),
                         origdata = CEdata_cut,
                         syndata = CEdata_syn_cont,
                         posteriorMCMCs = draws_cont,
                         syntype = c("norm", "norm"),
                         G = c(11, 11),
                         H = 20)

count <- 0
for (i in 1:length(contLE)){
  if (contLE[[i]]$RankTrue == 1){
    count = count + 1
  }
}
print(count)