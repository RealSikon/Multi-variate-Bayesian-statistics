###Functions for plotting results###


#' Make a plot of the rank of posterior probability of the true pair of values 
#' (of LogIncome and LogExpenditure being guessed correctly, among 121 (11*11) guesses)
posteriorRankPlot <- function(risks){
  true_ranks = c()
  for (i in 1:length(risks)) {
    true_ranks[i] = risks[[i]]$RankTrue
  }

  plotdf = as.data.frame(true_ranks)
  
  plt = ggplot(data = plotdf) + geom_histogram(aes(abs(true_ranks))) + 
    labs(x = "Rank of true value", y = "Count") +
    ggtitle("posteriorRankPlot")
  
  plt
  return(plt)
}



#' Density of the marginal posterior probabilities of correctly guessing the
#' true value (of LogIncome and LogExpenditure, respectively). The vertical line shows
#' the prior probability (of 1/11).
marginalPosteriorProbabilitiesPlot = function(risks) {
  
  true_risks = c()
  for (i in 1:length(risks)) {
    true_marginals[i] = risks[[i]]$TrueMarginals
  }
  
  
  plotdf = as.data.frame(true_marginals)
  
  plt = ggplot(data = plotdf) + geom_density(aes(abs(true_marginals)), size = .5) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey")) + 
    labs(x = "Probability of guessing correctly", y = "Density") +
    ggtitle("marginalPosteriorProbabilitiesPlot")

  plt = plt + geom_vline(aes(xintercept = 1/length(risks$FullProb[, 1])), col = "red", size = 1)
  
  plt
  return(plt)
}

#' Density of the absolute difference between the true value and the
#' guessed value with the largest marginal
#' posterior probability (for both LogIncome and LogExpenditure).
absoluteDifferencePlot = function(risks) {

  marginal_diff <- c()
  for (i in 1:length(risks)) {
    marginal_diff[i] = risks[[i]]$MarginalAbsDiff
  }
  
  plotdf = as.data.frame(marginal_diff)
  
  plt = ggplot(data = plotdf) + geom_density(aes(abs(marginal_diff)), size = .5) + 
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey")) + 
    labs(x = "Absolute difference", y = "Density") +
    ggtitle("absoluteDifferencePlot")
  
  plt
  return(plt)
}

# Density plot
density_plot = function(real_att, syn_att) {
  
  both <- c()
  both$real <- real_att
  both$syn <- syn_att
  
  labels <- c("real", "synthetic")
  
  plotdf <- as.data.frame(both)
  
  plt <- ggplot(data = plotdf, aes(real_att), group = labels) + 
    geom_density(aes(abs(both$real), color = "real"), size = .5) +
    geom_density(aes(abs(both$syn), color = "synthetic"), size = .5) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey")) + 
    labs(x = "Value", y = "Density") +
    ggtitle("DensityPlot")
  
  plt
}
