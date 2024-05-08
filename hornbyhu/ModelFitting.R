###Synthesisation methods###

#Synthesizing continuous variables sequentially with the stan_glm() function
syn_normal_brms = function(orig_data,         #Real dataset
                           syn_data,          #Synthetic dataset
                           model_brms,        #Formula to be used for synthesis
                           chains = 1,        #Number of chains to be used in regression model
                           iterations = 20, #Iterations used to fit regression model | original 1000
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



#Synthesizing categorical variables sequentially with the multinomian brm() function
syn_multinomial_brms = function(orig_data, syn_data,
                                model_brms = bf(outcome ~ 1), chains = 1, iterations = 1000,
                                c = 0.95, thresh = 1.00, m = 20, thin = 5) {
  ff = stats::as.formula(model_brms)
  model = stats::model.frame(ff, orig_data)
  X = data.frame(stats::model.matrix(ff, model))
  
  multi_logistic_fit = brms::brm(data = orig_data,
                                 family = categorical(link="logit"),
                                 model_brms,
                                 iter = iterations,
                                 chains = chains)
  post_multi_logistic = brms::posterior_samples(multi_logistic_fit)
  
  n = length(orig_data[,1])
  C = length(levels(orig_data[, paste(text = model_brms$formula[[2]])]))
  
  start = length(post_multi_logistic[,1]) - thin * (C - 1)
  log_p_allC = matrix(NA, nrow = n, ncol = C)
  for (c in 2:C) {
    name_Intercept_c = paste0("b_mu", c, "_Intercept")
    name_LogIncome_c = paste0("b_mu", c, "_LogIncome")
    index = start + thin * (c - 2)
    log_p_c = as.matrix(X) %*% t(post_multi_logistic[index, c(name_Intercept_c, name_LogIncome_c)])
    log_p_allC[, c] = log_p_c
  }
  
  log_p_allC[, 1] = rep(0, n)
  p_allC = exp(log_p_allC) / (1 + exp(log_p_allC))
  syndata = vector("list", m)
  for (i in 1:m){
    synthetic_Y = rep(NA, n)
    for (j in 1:n){
      synthetic_Y[j] = which(rmultinom(1, size = 1,
                                       prob = p_allC[j, ]) == 1)
    }
    syndata[[i]] = synthetic_Y
  }
  return(list(syndata, p_allC))
}


#Synthesizing categorical variables sequentially with the poisson brm() function
syn_pois_brms = function(orig_data, syn_data,
                         model_brms = bf(outcome ~ 1), chains = 1, iterations = 1000,
                         c = 0.95, thresh = 1.00, m = 20, thin = 5) {
  ff = as.formula(model_brms)
  utils::str(model <- model.frame(ff, syn_data))
  X = model.matrix(ff, model)
  fit = stan_glm(
    model_brms,
    data = orig_data,
    family = poisson(link = "log"),
    prior = normal(0, 2, autoscale = FALSE),
    refresh = 0,
    chains = chains, iter = iterations
  )
  #### synthesis ####
  N = length(orig_data[,1])
  draws = as.data.frame(fit)
  start_draws = length(draws[,1]) - thin * (m - 1)
  start_data = length(syn_data[,1]) - thin * (m - 1)
  syndata = vector("list", m)
  for (i in 1:m){
    indx = start_draws + thin * (i - 1)
    draws_exp_mean = exp(as.matrix(X) %*% t(draws[indx, ]))
    syndata[[i]] = rpois(N, lambda = draws_exp_mean)
  }
  return(list(syndata, draws))
}
