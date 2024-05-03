#String evaluator
string_parser <- function(evalstring) {
  return(eval(parse(text = evalstring)))
}

# Data Loader
data_loader <- function(data, cut) {
  return(read.csv(file.path(data_directory, data))[1:cut, ])
}
