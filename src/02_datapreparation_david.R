

raw_continuous_vars_selection <- function(data)
{
  categorical_vars <- c("id", "date", "waterfront", "view", "yr_built", "yr_renovated", "zipcode", "lat", "long")
  removed_vars <- names(data) %in% categorical_vars
  data <- data[,!removed_vars]
  
  names(data)[1] <- "target"
  
  return(data)
}