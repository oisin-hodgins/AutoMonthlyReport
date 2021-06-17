#'@title
#'Standardise a variable where higher values are more 'valuable'
#'
#'@description
#'Default method of standardisation. The transformed value represents the number of
#'standard deviations the variable lies from the mean.
#'
#'@details
#'The mean is subtracted from the variable, then it is divided by the standard deviation.
#'
#'@param x The variable to be standardised.
#'@export
max_standardise <- function(x){
  return(x = ((x-mean(x))/sd(x)))
}