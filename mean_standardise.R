#'@title
#'Standardise a variable where values closer to the mean are more 'valuable'
#'
#'@description
#'The transformed value represents the number of standard deviations the variable
#'lies from the mean.
#'
#'@details
#'The mean is subtracted from the variable and then it is divided by the standard
#'deviation. We take the absolute value of the result, multiplied by (-1).
#'
#'@param x The variable to be standardised.
#'@export
mean_standardise <- function(x){
  return(x = (-abs((x-mean(x))/sd(x))))
}
