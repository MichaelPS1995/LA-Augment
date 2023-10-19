

# ./generate 1 2 1 -v Sample-Input/TWC.tsv out.tsv

generate_array <- function(design_path=NULL, infile=NULL, outfile="out.tsv", d=1, t=2, delta=1){
  if(is.null(design_path) || is.null(infile)){
    stop("There was no executable or input file given.")
  }
  library(Rcpp)
  library(RcppArmadillo)
  command <- paste(design_path, d, t, delta, infile, outfile,  sep=" ")
  print(command)
  system(command, intern=TRUE)
  
}

generate_array("C:/Users/micha/OneDrive/Desktop/function/generate", "C:/Users/micha/OneDrive/Desktop/function/TWC.tsv", "C:/Users/micha/OneDrive/Desktop/function/out_test.tsv")

