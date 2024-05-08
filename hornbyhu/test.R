f1 <- function(x,y){
  print (x)
  print (y)
}
args = commandArgs(trailingOnly=TRUE)
f1(args[1], args[2])