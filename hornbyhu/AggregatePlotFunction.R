avgRiskForEpsilon <- function(all_risks, cont_list, risk_name){
  avg_list <- rep(0, length(all_risks[[1]]))
  
  for (run in 1:3){
    for (f in 1:length(all_risks[[run]])){
      if (cont_list[[f]] == 1){
        for (risk in 1:length(all_risks[[run]][[f]][[risk_name]])){
          avg_list[[f]] <- avg_list[[f]] + all_risks[[run]][[f]][[risk_name]][[risk]]
        }
        avg_list[[f]] <- avg_list[[f]] / length(all_risks[[run]][[f]][[risk_name]])
      }
    }
  }
  return(avg_list)
}

avgRiskForRuns <- function(all_risks, cont_list, risk_name){
  avg_list <- c()
  i <- 1
  
  for (f in 1:length(all_risks[[1]])){
    run1 <- c()
    run2 <- c()
    run3 <- c()
    if (cont_list[[f]] == 1){
      for (risk in 1:length(all_risks[[1]][[f]][[risk_name]])){
        run1 <- append(run1, all_risks[[1]][[f]][[risk_name]][[risk]])
        run2 <- append(run2, all_risks[[2]][[f]][[risk_name]][[risk]])
        run3 <- append(run3, all_risks[[3]][[f]][[risk_name]][[risk]])
      }
      
      run_list <- list(run1, run2, run3)
      avg_list[[i]] <- rowMeans(simplify2array(run_list))
      i <- i+1
    }
  }
  return(avg_list)
}

avgADRPlot = function(risks, cont_list, cont_name_list){
  epsilon_values <- c(0, 0.05, 0.1, 0.2, 0.5, 1, 5, 10, 100)
  
  #Load the TrueMarginal values
  avg_TM <- c()
  for (epsilon in 1:length(risks)){
    avg_TM[[epsilon]] <- avgRiskForEpsilon(risks[[epsilon]], cont_list, 'TrueMarginals')
  }
  
  #Remove 0 values (non continuous attributes)
  true_avgs <- c()
  for (i in 1:length(epsilon_values)){
    true_avgs[i] <- lapply(avg_TM[i], function(x) {x[x!=0]})
  }
  
  avgs <- c(c())
  for (j in 1:length(true_avgs[[i]])){
    list <- c()
    for (i in 1:length(true_avgs)){
      list <- append(list, true_avgs[[i]][j])
    }
    avgs[[j]] <- list
  }
  i <- 1
  name_list <- list()
  for (j in 1:length(cont_list)){
    if (cont_list[[j]] == 1){
      name_list <- append(name_list, cont_name_list[[i]])
      i <- i + 1
    }
  }
  
  names(avgs) <- name_list
  
  prior_prob <- 1/41
  
  row <- data.frame(LogIncome = prior_prob, LogExpenditure = prior_prob, epsilon = 0)
  
  row <- data.frame(Age =prior_prob, Height = prior_prob, 
                    Weight = prior_prob, NCP = prior_prob,
                    CAEC = prior_prob, CH2O = prior_prob,
                    FAF = prior_prob, epsilon = 0)
  
  row <- data.frame(AlcoholLevel =prior_prob, HeartRate = prior_prob, 
                    BloodOxygenLevel = prior_prob, BodyTemperature = prior_prob,
                    Weight = prior_prob, MRIDelay = prior_prob,
                    Age = prior_prob, epsilon = 0)
  
  df <- data.frame(avgs,
                   epsilon = epsilon_values)
  inf_eps <- df[1, ]
  inf_eps$epsilon <- Inf
  df <- df[-1, ]
  new_df <- rbind(row, df)
  plotdf <- rbind(new_df, inf_eps)
  #Plot for CEData:
  plot <- ggplot(data = plotdf) + geom_line(aes(x = epsilon, y = LogIncome, colour="LogIncome"), size=1) + 
    geom_line(aes(x = epsilon, y = LogExpenditure, colour="LogExpenditure"), size=1) + 
    scale_x_continuous(trans='log2') + expand_limits(x=c(0, 90), y=c(0.0238,0.0245)) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey")) +
    labs(x = "epsilon", y = "avgProb")
  
  plot
  
  #Plot for DPHP:
  plot <- ggplot(data = plotdf) + geom_line(aes(x = epsilon, y = MRIDelay, colour="MRIDelay"), size=1) + 
    geom_line(aes(x = epsilon, y = AlcoholLevel, colour="AlcoholLevel"), size=1) +
    geom_line(aes(x = epsilon, y = Weight, colour="Weight"), size=1) +
    geom_line(aes(x = epsilon, y = HeartRate, colour="HeartRate"), size=1) +
    geom_line(aes(x = epsilon, y = Age, colour="Age"), size=1) +
    geom_line(aes(x = epsilon, y = BloodOxygenLevel, colour="BloodOxygenLevel"), size=1) +
    geom_line(aes(x = epsilon, y = BodyTemperature, colour="BodyTemperature"), size=1) + 
    scale_x_continuous(trans='log2') + expand_limits(x=c(0, 90), y=c(0.0235,0.025)) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey")) +
    labs(x = "epsilon", y = "avgProb")
  
  plot
  
  #Plot obesity:
  plot <- ggplot(data = plotdf) + geom_line(aes(x = epsilon, y = Age, colour="Age"), size=1) + 
    geom_line(aes(x = epsilon, y = Height, colour="Height"), size=1) +
    geom_line(aes(x = epsilon, y = Weight, colour="Weight"), size=1) +
    geom_line(aes(x = epsilon, y = NCP, colour="NCP"), size=1) +
    geom_line(aes(x = epsilon, y = CAEC, colour="CAEC"), size=1) +
    geom_line(aes(x = epsilon, y = CH2O, colour="CH2O"), size=1) +
    geom_line(aes(x = epsilon, y = FAF, colour="FAF"), size=1) + 
    scale_x_continuous(trans='log2') + expand_limits(x=c(0, 90), y=c(0.0235,0.025)) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey")) +
    labs(x = "epsilon", y = "avgProb")
  
  ggsave(paste("avgADRPlot.pdf", sep = ""),
         device = "pdf",
         width = 10, height = 3, dpi = 1000,
         plot,
         path = obesity_plot_directory
  )
  
  
  return(plot)
}


aggRandomGuessPlot = function(risks, cont_list, cont_name_list){
  epsilon_values <- c(0, 0.05, 0.1, 0.2, 0.5, 1, 5, 10, 100)
  true_risks <- list()
  
  for (epsilon in 1:length(risks)){
    true_risks[[epsilon]] <- avgRiskForRuns(risks[[epsilon]], cont_list, 'TrueMarginals')
  }
  i <- 1
  name_list <- list()
  for (j in 1:length(cont_list)){
    if (cont_list[[j]] == 1){
      name_list <- append(name_list, cont_name_list[[i]])
      i <- i+1
    }
  }
  
  for (epsilon in 1:length(true_risks)){
    names(true_risks[[epsilon]]) <- name_list
  }
  
  plot_risks <- c(c())
  for (i in 1:length(true_risks[[1]])){
    list <- c()
    for (j in 1:length(true_risks)){
      list <- append(list, true_risks[[j]][[i]])
    }
    plot_risks[[i]] <- list
  }
  str_eps <- c("inf", "0.05", "0.1", "0.2", "0.5", "1", "5", "10", "100")
  eps_for_risks <- c()
  for (i in 1:length(true_risks)){
    eps_list <- rep(str_eps[[i]], length(true_risks[[1]][[1]]))
    eps_for_risks <- append(eps_for_risks, eps_list)
  }
  names(plot_risks) <- name_list
  plotdf = data.frame(plot_risks, epsilo = eps_for_risks)
  #sortedlist = plotdf[order(plotdf$LogExpenditure, decreasing = TRUE),]
  for (i in 1:length(true_risks[[1]])){
    tmp <- data.frame(a = plotdf[[cont_name_list[[i]]]], epsilon = plotdf$epsilo)
    plt = ggplot(data = tmp, aes(x = a, colour = epsilon), size=1) +
      scale_color_manual(breaks = c("inf", "0.05", "0.1", "0.2", "0.5", "1", "5", "10", "100"), 
                         values = c("darkred", "red", "pink", "purple", "darkblue", "blue", "cyan", "green", "orange" )) +
      geom_density() + 
      theme(panel.background = element_rect(fill = "white", color = "black"),
            panel.grid.major = element_line(color = "grey")) +
      #xlim(0.02354, 0.02445) +
      labs(x = "Probability of guessing correctly", y = "Density") +
      geom_vline(aes(xintercept = 1.0/41), col = "red", size = 1)
    
    ggsave(paste("aggRandomGuessPlot", cont_name_list[[i]], ".pdf", sep = ""),
           device = "pdf",
           plt,
           path = obesity_plot_directory
    )
    
    plt
    #plot_list[i] <- plt
  }
  return(plt)
  
}


density_plot = function(dataset, attribute){
  library(here)
  epsilon_str_values <- c("0", "005", "01", "02", "05", "1", "5", "10", "100")
  #C:\Users\Frederik Trudslev\Documents\GitHub\Multi-variate-Bayesian-statistics\data\allDatasets
  
  path = here()
  current_directory <- path
  parent_directory  <- dirname(current_directory)
  data_directory    <- file.path(parent_directory, "data/allDatasets")
  
  #name_list <- c("e0r1", "e0r2", "e0r3", "e0.05r1", "e0.05r2", "e0.05r3", "e0.1r1", "e0.1r2", "e0.1r3", "e0.2r1", "e0.2r2", "e0.2r3","e0.5r1", "e0.5r2", "e0.5r3", "e1r1", "e1r2", "e1r3", "e5r1", "e5r2", "e5r3","e10r1", "e10r2", "e10r3", "e100r1", "e100r2", "e100r3")
  syn_data_attributes <- c(c())
  
  run <- 1
  if (dataset == "CEDataR" | dataset == "CEDataR_in_syn" | dataset == "CEDataR_out_syn"){
    if (dataset == "CEDataR"){
      real_data_dir     <- paste(data_directory, "/", dataset, "/e0/run1/", dataset, "2k.csv", sep = "")
    }
    if (dataset == "CEDataR_in_syn"){
      real_data_dir     <- paste(data_directory, "/", dataset, "/e0/run1/CEdataR2k_in.csv", sep = "")
    }
    if (dataset == "CEDataR_out_syn"){
      real_data_dir     <- paste(data_directory, "/", dataset, "/e0/run1/CEdataR2k_out.csv", sep = "")
    }
    
    real_data <-  as.data.frame(read.csv(real_data_dir))
    for (epsilon in 1:length(epsilon_str_values)){
      #for (run in 1:3){
      syn_data_dir <- file.path(data_directory, paste(dataset, "/e", epsilon_str_values[[epsilon]], "/run", run,"/synthetic_data", epsilon_str_values[[epsilon]], "_", run, ".csv", sep = ""))
      syn_data <- as.data.frame(read.csv(syn_data_dir))
      attribute_values <- eval(parse(text = paste("syn_data$", attribute, sep="")))
      syn_data_attributes <- cbind(syn_data_attributes, attribute_values)
      #}
    }
  }
  
  if (dataset == "DPHP" | dataset == "DPHP_in_syn" | dataset == "DPHP_out_syn"){
    if (dataset == "DPHP"){
      real_data_dir     <- paste(data_directory, "/DPHP/e0/run1/DPHP.csv", sep = "")
    }
    if (dataset == "DPHP_in_syn"){
      real_data_dir     <- paste(data_directory, "/", dataset, "/e0/run1/DPHP_in.csv", sep = "")
    }
    if (dataset == "DPHP_out_syn"){
      real_data_dir     <- paste(data_directory, "/", dataset, "/e0/run1/DPHP_out.csv", sep = "")
    }
    
    real_data <-  as.data.frame(read.csv(real_data_dir))
    for (epsilon in 1:length(epsilon_str_values)){
      #for (run in 1:3){
      syn_data_dir <- file.path(data_directory, paste(dataset, "/e", epsilon_str_values[[epsilon]], "/run", run,"/synthetic_data", epsilon_str_values[[epsilon]], "_", run, ".csv", sep = ""))
      syn_data <- as.data.frame(read.csv(syn_data_dir, nrows = 1000))
      attribute_values <- eval(parse(text = paste("syn_data$", attribute, sep="")))
      syn_data_attributes <- cbind(syn_data_attributes, attribute_values)
      #}
    }
  }
  
  if (dataset == "obesity_syn" | dataset == "obesity_in_syn" | dataset == "obesity_out_syn"){
    if (dataset == "obesity_syn"){
      real_data_dir     <- paste(data_directory, "/", dataset, "/e0/run1/obesity.csv", sep = "")
    }
    if (dataset == "obesity_in_syn"){
      real_data_dir     <- paste(data_directory, "/", dataset, "/e0/run1/obesity_in.csv", sep = "")
    }
    if (dataset == "obesity_out_syn"){
      real_data_dir     <- paste(data_directory, "/", dataset, "/e0/run1/obesity_out.csv", sep = "")
    }
    
    real_data <-  as.data.frame(read.csv(real_data_dir))
    for (epsilon in 1:length(epsilon_str_values)){
      #for (run in 1:3){
      syn_data_dir <- file.path(data_directory, paste(dataset, "/e", epsilon_str_values[[epsilon]], "/run", run,"/synthetic_data", epsilon_str_values[[epsilon]], "_", run, ".csv", sep = ""))
      syn_data <- as.data.frame(read.csv(syn_data_dir))
      attribute_values <- eval(parse(text = paste("syn_data$", attribute, sep="")))
      syn_data_attributes <- cbind(syn_data_attributes, attribute_values)
      #}
    }
  }
  
  df <- data.frame(syn_data_attributes, real = eval(parse(text = paste("real_data$", attribute, sep=""))))
  
  plot_risks <- c()
  list <- c()
  for (i in 1:length(df)){
    list <- c(list, df[[i]])
  }
  
  name_list <- c("inf", "0.05", "0.1", "0.2", "0.5", "1", "5", "10", "100", "real")
  #name_list <- c("e0r1", "e0r2", "e0r3", "e0.05r1", "e0.05r2", "e0.05r3", "e0.1r1", "e0.1r2", "e0.1r3", "e0.2r1", "e0.2r2", "e0.2r3","e0.5r1", "e0.5r2", "e0.5r3", "e1r1", "e1r2", "e1r3", "e5r1", "e5r2", "e5r3","e10r1", "e10r2", "e10r3", "e100r1", "e100r2", "e100r3", "real")
  
  eps_for_risks <- c()
  for (i in 1:length(name_list)){
    eps_list <- rep(name_list[[i]], length(df[[1]]))
    eps_for_risks <- append(eps_for_risks, eps_list)
  }
  
  plot_risks <- data.frame(values = list, epsilon = eps_for_risks)
  
  plt <- ggplot(data = plot_risks, aes(x=values, color=epsilon)) + geom_density(size=1) + 
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey")) + 
    labs(x = attribute, y = "Density") +
    ggtitle("DensityPlot")
  plt
  return(plt)
}
library(here)
path <- here()
current_directory <- path
parent_directory  <- dirname(current_directory)
data_directory    <- file.path(parent_directory, "data")
plot_directory    <- file.path(parent_directory, "plots/ours")

#CEData plots:
CE_plot_directory    <- file.path(plot_directory, "CEData")
CE_in_plot_directory    <- file.path(plot_directory, "CEData_in")
CE_out_plot_directory    <- file.path(plot_directory, "CEData_out")
CEData_continuous_list <- c(1, 1, 0, 0, 0)
CEData_cont <- c("LogIncome", "LogExpenditure")

ggsave(paste("aggRandomGuessPlot.pdf", sep = ""),
       device = "pdf",
       aggRandomGuessPlot(CEDataR_risks, CEData_continuous_list),
       path = CE_plot_directory
)
ggsave(paste("aggRandomGuessPlot.pdf", sep = ""),
       device = "pdf",
       aggRandomGuessPlot(CEDataR_i_risks, CEData_continuous_list),
       path = CE_in_plot_directory
)
ggsave(paste("aggRandomGuessPlot.pdf", sep = ""),
       device = "pdf",
       aggRandomGuessPlot(CEDataR_o_risks, CEData_continuous_list),
       path = CE_out_plot_directory
)

for (att in CEData_cont){
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         density_plot("CEDataR", att),
         path = CE_plot_directory
  )
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         density_plot("CEDataR_in_syn", att),
         path = CE_in_plot_directory
  )
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         density_plot("CEDataR_out_syn", att),
         path = CE_out_plot_directory
  )
}


avgADRPlot(CEDataR_risks, CEData_continuous_list)
avgADRPlot(CEDataR_i_risks, CEData_continuous_list)
avgADRPlot(CEDataR_o_risks, CEData_continuous_list)


DPHP_plot_directory    <- file.path(plot_directory, "DPHP")
DPHP_in_plot_directory    <- file.path(plot_directory, "DPHP_in")
DPHP_out_plot_directory    <- file.path(plot_directory, "DPHP_out")
DPHP_continuous_list <- c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
DPHP_cont <- c("AlcoholLevel", "HeartRate", "BloodOxygenLevel", "BodyTemperature", "Weight", "MRIDelay", "Age")

for (att in DPHP_cont){
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         density_plot("DPHP", att),
         path = DPHP_plot_directory
  )
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         density_plot("DPHP_in_syn", att),
         path = DPHP_in_plot_directory
  )
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         density_plot("DPHP_out_syn", att),
         path = DPHP_out_plot_directory
  )
  
}


obesity_plot_directory    <- file.path(plot_directory, "obesity")
obesity_in_plot_directory    <- file.path(plot_directory, "obesity_in")
obesity_out_plot_directory    <- file.path(plot_directory, "obesity_out")
obesity_continuous_list <- c(1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0)
obesity_cont <- c("Age", "Height", "Weight", "NCP", "CAEC", "CH2O", "FAF")

for (att in obesity_cont){
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         density_plot("obesity_syn", att),
         path = obesity_plot_directory
  )
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         density_plot("obesity_in_syn", att),
         path = obesity_in_plot_directory
  )
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         density_plot("obesity_out_syn", att),
         path = obesity_out_plot_directory
  )
}
for (att in obesity_cont){
  ggsave(paste("aggRandomGuessPlot", att, ".pdf", sep = ""),
         device = "pdf",
         aggRandomGuessPlot("obesity_syn", obesity_continuous_list, obesity_cont),
         path = obesity_plot_directory
  )
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         aggRandomGuessPlot("obesity_in_syn", obesity_continuous_list, obesity_cont),
         path = obesity_in_plot_directory
  )
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         aggRandomGuessPlot("obesity_out_syn", obesity_continuous_list, obesity_cont),
         path = obesity_out_plot_directory
  )
}
#obesity plots:


aggRandomGuessPlot(obesity_risks, obesity_continuous_list)

avgADRPlot(obesity_risks, obesity_continuous_list)
avgADRPlot(obesity_i_risks, obesity_continuous_list)
avgADRPlot(obesity_o_risks, obesity_continuous_list)

ggsave(density_plot("obesity_syn", "Age"), device = "pdf" )

#DPHP plots:
aggRandomGuessPlot(DHPH_risks, DPHP_continuous_list)
avgADRPlot(DHPH_risks, DPHP_continuous_list)
avgADRPlot(DHPH_i_risks, DPHP_continuous_list)
avgADRPlot(DHPH_o_risks, DPHP_continuous_list)

density_plot("DPHP", "MRIDelay")
density_plot("DPHP", "AlcoholLevel")




# aggPosteriorRankPlot <- function(risks){
#   epsilon_values <- c(0, 0.05, 0.1, 0.2, 0.5, 1, 5, 10, 100)
#   true_ranks <- list()
#   
#   for (epsilon in 1:length(risks)){
#     true_ranks[[epsilon]] <- avgRiskForRuns(risks[[epsilon]], cont_list, 'RankTrue')
#   }
#   
#   plotdf = as.data.frame(true_ranks)
#   
#   plt = ggplot(data = plotdf) + geom_histogram(aes(abs(true_ranks))) +
#     labs(x = "Rank of true value", y = "Count") +
#     ggtitle("posteriorRankPlot")
#   
#   plt
#   return(plt)
# }





# marginalPosteriorProbabilitiesPlot = function(risks) {
# 
#   true_risks = c()
#   for (i in 1:length(risks)) {
#     true_marginals[i] = risks[[i]]$TrueMarginals
#   }
# 
# 
#   plotdf = as.data.frame(true_marginals)
# 
#   plt = ggplot(data = plotdf) + geom_density(aes(abs(true_marginals)), size = .5) +
#     theme(panel.background = element_rect(fill = "white", color = "black"),
#           panel.grid.major = element_line(color = "grey")) +
#     labs(x = "Probability of guessing correctly", y = "Density") +
#     ggtitle("marginalPosteriorProbabilitiesPlot")
# 
#   plt = plt + geom_vline(aes(xintercept = 1/length(risks$FullProb[, 1])), col = "red", size = 1)
# 
#   plt
#   return(plt)
# }
# 



density_plot_V = function(dataset, attribute){
  library(here)
  epsilon_str_values <- c("0", "005", "1", "100")
  #C:\Users\Frederik Trudslev\Documents\GitHub\Multi-variate-Bayesian-statistics\data\allDatasets
  
  path = here()
  current_directory <- path
  parent_directory  <- dirname(current_directory)
  data_directory    <- file.path(parent_directory, "data/allDatasets/variance")
  
  #name_list <- c("e0r1", "e0r2", "e0r3", "e0.05r1", "e0.05r2", "e0.05r3", "e0.1r1", "e0.1r2", "e0.1r3", "e0.2r1", "e0.2r2", "e0.2r3","e0.5r1", "e0.5r2", "e0.5r3", "e1r1", "e1r2", "e1r3", "e5r1", "e5r2", "e5r3","e10r1", "e10r2", "e10r3", "e100r1", "e100r2", "e100r3")
  syn_data_attributes <- c(c())
  
  if (dataset == "CEDataV"){
  
    for (epsilon in 1:length(epsilon_str_values)){
      #for (run in 1:3){
        syn_data_dir <- file.path(data_directory, paste("cedata/e", epsilon_str_values[[epsilon]], "/run", run,"/synthetic_data", epsilon_str_values[[epsilon]], "_", run, ".csv", sep = ""))
        syn_data <- as.data.frame(read.csv(syn_data_dir))
        attribute_values <- eval(parse(text = paste("syn_data$", attribute, sep="")))
        syn_data_attributes <- cbind(syn_data_attributes, attribute_values)
      #}
    }
  }
  
  
  if (dataset == "obesityV"){
    
    for (epsilon in 1:length(epsilon_str_values)){
      #for (run in 1:3){
        syn_data_dir <- file.path(data_directory, paste("obesity/e", epsilon_str_values[[epsilon]], "/run", run,"/synthetic_data", epsilon_str_values[[epsilon]], "_", run, ".csv", sep = ""))
        syn_data <- as.data.frame(read.csv(syn_data_dir))
        attribute_values <- eval(parse(text = paste("syn_data$", attribute, sep="")))
        syn_data_attributes <- cbind(syn_data_attributes, attribute_values)
      #}
    }
  }
  
  df <- data.frame(syn_data_attributes)
  
  plot_risks <- c()
  list <- c()
  for (i in 1:length(df)){
    list <- c(list, df[[i]])
  }
  
  name_list <- c("inf", "0.05", "1", "100")
  #name_list <- c("e0r1", "e0r2", "e0r3", "e0.05r1", "e0.05r2", "e0.05r3", "e0.1r1", "e0.1r2", "e0.1r3", "e0.2r1", "e0.2r2", "e0.2r3","e0.5r1", "e0.5r2", "e0.5r3", "e1r1", "e1r2", "e1r3", "e5r1", "e5r2", "e5r3","e10r1", "e10r2", "e10r3", "e100r1", "e100r2", "e100r3", "real")
  
  eps_for_risks <- c()
  for (i in 1:length(name_list)){
    eps_list <- rep(name_list[[i]], length(df[[1]]))
    eps_for_risks <- append(eps_for_risks, eps_list)
  }
  
  plot_risks <- data.frame(values = list, epsilon = eps_for_risks)
  
  plt <- ggplot(data = plot_risks, aes(x=values, color=epsilon)) + geom_density(size=1) + 
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey")) + 
    labs(x = attribute, y = "Density") +
    ggtitle("DensityPlot")
  plt
  return(plt)
}


library(here)
path <- here()
current_directory <- path
parent_directory  <- dirname(current_directory)
data_directory    <- file.path(parent_directory, "data")
plot_directory    <- file.path(parent_directory, "plots/ours")

#CEData plots:
CE_plot_directory    <- file.path(plot_directory, "CEData")
CE_in_plot_directory    <- file.path(plot_directory, "CEData_in")
CE_out_plot_directory    <- file.path(plot_directory, "CEData_out")
CEData_continuous_list <- c(1, 1, 0, 0, 0)
CEData_cont <- c("LogIncome", "LogExpenditure")

ggsave(paste("aggRandomGuessPlot.pdf", sep = ""),
       device = "pdf",
       aggRandomGuessPlot(CEDataR_risks, CEData_continuous_list),
       path = CE_plot_directory
)
ggsave(paste("aggRandomGuessPlot.pdf", sep = ""),
       device = "pdf",
       aggRandomGuessPlot(CEDataR_i_risks, CEData_continuous_list),
       path = CE_in_plot_directory
)
ggsave(paste("aggRandomGuessPlot.pdf", sep = ""),
       device = "pdf",
       aggRandomGuessPlot(CEDataR_o_risks, CEData_continuous_list),
       path = CE_out_plot_directory
)

for (att in CEData_cont){
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         density_plot("CEDataR", att),
         path = CE_plot_directory
  )
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         density_plot("CEDataR_in_syn", att),
         path = CE_in_plot_directory
  )
  ggsave(paste("Density_Plot", att, ".pdf", sep = ""),
         device = "pdf",
         density_plot("CEDataR_out_syn", att),
         path = CE_out_plot_directory
  )
}

run <- 3
for (att in CEData_cont){
  ggsave(paste("Density_Plot", att, run, ".pdf", sep = ""),
         device = "pdf",
         density_plot_V("CEDataV", att),
         path = CE_plot_directory
  )
}
for (att in obesity_cont){
  ggsave(paste("Density_Plot", att, run, ".pdf", sep = ""),
         device = "pdf",
         density_plot_V("obesityV", att),
         path = obesity_plot_directory
  )
}
