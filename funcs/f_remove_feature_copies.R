f_remove_feature_copies <- function(df){
  # Do:      Remove redundant columns
  # inputs:  dataframe object
  # Outputs: dataframe object
  features_pair <- combn(names(df), 2, simplify = F)
  toRemove <- c()
  for(pair in features_pair) {
    f1 <- pair[1]
    f2 <- pair[2]
    
    if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
      if (all(df[[f1]] == df[[f2]])) {
        cat(f1, "and", f2, "are equals.\n")
        toRemove <- c(toRemove, f2)
      }
    }
  }
  
  feature.names <- setdiff(names(df), toRemove)
  
  df <- df[, feature.names]
  df
}