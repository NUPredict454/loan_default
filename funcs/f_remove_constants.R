f_remove_constants <- function(df){
  # Do:      remove any column with a single unique value
  # inputs:  dataframe object
  # Outputs: dataframe object
  
  for (f in names(df)) {
    if (length(unique(df[[f]])) == 1) {
      cat(f, "is constant. We delete it.\n")
      df[[f]] <- NULL
    }
  }
  df
}