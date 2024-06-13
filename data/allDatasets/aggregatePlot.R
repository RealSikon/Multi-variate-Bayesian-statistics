#set working directory to multi-variate-bayesian-statistics

#set this to the dataset(s) you want to load
run_list <- list(1,2,3,4,5,6,7,8,9)

#---------------------------

#                    1          2             3           4          5            6          7        8         9
datasets <- list("CEDataR", "CEDataR_i", "CEDataR_o", "obesity", "obesity_i", "obesity_o", "DHPH", "DHPH_i", "DHPH_o")


cur_dir <- paste(getwd(), "/allDatasets/", sep= "")

datasets_dir <- list(paste(cur_dir, "CEDataR/", sep= ""), paste(cur_dir, "CEDataR_in_syn/", sep= ""), paste(cur_dir, "CEDataR_out_syn/", sep= ""),
                     paste(cur_dir, "obesity_syn/", sep= ""), paste(cur_dir, "obesity_in_syn/", sep= ""), paste(cur_dir, "obesity_out_syn/", sep= ""),
                     paste(cur_dir, "DPHP/", sep= ""),    paste(cur_dir, "DPHP_in_syn/", sep= ""),    paste(cur_dir, "DPHP_out_syn/", sep= ""))

eps_lst  <- list(value = list(0, 0.05, 0.1, 0.2, 0.5, 1, 5, 10, 100), 
                 string = list('0', '005', '01', '02', '05', '1', '5', '10', '100'))


# list creator
for (d in run_list) {
  eval(parse(text = paste(datasets[[d]], "_risks <- list()", sep="")))


  # For gathering nodes
  load(paste(datasets_dir[[d]], "e0/run1/e0-r1.Rdata", sep=""))
  for (e in seq_along(eps_lst$string)) {
    eval(parse(text = paste(datasets[[d]], "_risks = append(", datasets[[d]], "_risks, list(e", eps_lst$string[[e]], " = list()))", sep="")))
    for (r in 1:3){
      eval(parse(text = paste(datasets[[d]],"_risks[[e]] = append(", datasets[[d]], "_risks[[e]], list(run", r, " = list()))", sep="")))
      for (n in nodes){
        eval(parse(text = paste(datasets[[d]], "_risks[[", e, "]][[", r, "]] = append(", datasets[[d]], "_risks[[", e, "]][[", r, "]], list(", n, " = list(TrueValProb = list(), TrueMarginals = list(), RankTrue = list())))", sep = "")))
      }
    }
  }
  
  #Load RData
  for (e in seq_along(eps_lst$string)){
    for (r in seq_along(1:3)) {
      load(paste(datasets_dir[[d]], "e", eps_lst$string[[e]], "/run", r, "/e", eps_lst$string[[e]], "-r", r, ".Rdata", sep=""))
      print(paste(datasets_dir[[d]], "e", eps_lst$string[[e]], "/run", r, "/e", eps_lst$string[[e]], "-r", r, ".Rdata", sep=""))
      for (f in seq_along(risk_list)) {
        for (v in seq_along(risk_list[[f]])) {
          eval(parse(text = paste(datasets[[d]], "_risks[[e]][[r]]$", nodes[[f]],  "$TrueValProb[[v]]    <- risk_list[[f]][[v]]$TrueValProb",   sep="")))
          eval(parse(text = paste(datasets[[d]], "_risks[[e]][[r]]$", nodes[[f]],  "$TrueMarginals[[v]]  <- risk_list[[f]][[v]]$TrueMarginals",   sep="")))
          eval(parse(text = paste(datasets[[d]], "_risks[[e]][[r]]$", nodes[[f]],  "$RankTrue[[v]]       <- risk_list[[f]][[v]]$RankTrue",   sep="")))
        }
      }
    }
  }
}