#'@title
#'Standardise a variable where lower values are more 'valuable'
#'
#'@description
#'The transformed value represents the number of standard deviations the variable 
#'lies from the mean. 
#'
#'@details
#'The mean is subtracted from the variable, then it is divided by the standard deviation
#'and multiplied by (-1).
#'
#'@param x The variable to be standardised.
#'@export
min_standardise <- function(x){
  return(x = (-(x-mean(x))/sd(x)))
}