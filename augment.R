

augment_design <- function(design_path=NULL, design_params=NULL, flag=0, aug_percent=0.25, aug_fix=0.5){
  # flag = 0 will augment by randomly selecting some # rows based on aug_percent
  # flag = 1 will augment by adding D-Optimal rows based on aug_perecent
  # TODO :: flag = 2 will add rows based on separation in LA design
  
  #need this package for some of the augment procedure and gen.factorial function
  library(AlgDesign)
  # check if both design and parameters are missing
  if(is.null(design_path) && is.null(design_params)){
    stop("No design, or design parameters given.")
  }
  # check if design is missing
  if(is.null(design_path)){
    stop("No design given.")
  }
  # check if parameters are missing
  if(is.null(design_params)){
    stop("No design parameters given.")
  }
  
  # read in design
  design = read.table(file=design_path, sep="\t",header=FALSE)
  
  # Isaacs code generates TSV where each line is ended with a tab, 
  # which creates N/A values so remove that column from design
  design <- design[, -ncol(design)]
  
  # change column/row names to match gen.factorial() method name scheme
  new_column_names <- paste("X", 1:ncol(design), sep="")
  colnames(design) <- new_column_names
  
  
  # map all the LA values to factorial
  for(i in 1:nrow(design)){
    for(j in 1:ncol(design)){
      design[i,j] <- design[i,j] + 1
    }
  }
  # print(design)
  
  #suppressing warning here because the file is not "proper" table
  suppressWarnings({params <- read.table(file=design_params, sep="\t",header=FALSE)})
  
  # read in the factor levels and convert to a vector of numbers to generate full factorial
  num_params <- as.numeric(params$V1[1])               
  row2_string <- as.character(params[2, 1]) 
  values_list <- unlist(strsplit(row2_string, "\\s+"))
  
  # read in number of factors
  numeric_values <- as.numeric(values_list)
  
  # generate factorial design
  factorial <- gen.factorial(levels=numeric_values, nVars=num_params[1], center=FALSE)
  
  # add % of base design rows randomly selected from factorial to LA
  # check for overflow error edge case and reset augment amount
  rows_to_add <-as.integer(nrow(design) * aug_percent)
  if((rows_to_add + nrow(design)) > nrow(factorial)){
    rows_to_add = as.integer((nrow(factorial)-nrow(design))* aug_fix)
  }
  
  if(flag == 0){

    for (i in 1:rows_to_add) {
      rand_index = sample(nrow(factorial), 1)
      while(anyDuplicated(rbind(design, factorial[rand_index, ]))){
        rand_index = sample(nrow(factorial), 1)
      }
      print(rand_index)
      design <- rbind(design, factorial[rand_index,])
    }
    # this just renames the rows, they assign weird numbers if they choose
    # say row 24 from factorial where row 24 (the name) already exists in design
    rownames(design) <- 1:nrow(design)
    return(design)
    
  }
  
  else if(flag == 1){
    
    trials <- rows_to_add + nrow(design)
    design_rows <- nrow(design)
    design <- rbind(design,factorial)
    opt <- optFederov(data = design, nTrials=trials, augment=TRUE, criterion = "D", rows=1:design_rows)
    design <- opt$design
    rownames(design) <- 1:nrow(design)
    return(design)
    
  }
  
}

augment_design("C:\\Users\\micha\\OneDrive\\Desktop\\function\\out.tsv", "C:\\Users\\micha\\OneDrive\\Desktop\\function\\TWC.tsv")

augment_design("C:\\Users\\micha\\OneDrive\\Desktop\\function\\out.tsv", "C:\\Users\\micha\\OneDrive\\Desktop\\function\\TWC.tsv", flag=1)
