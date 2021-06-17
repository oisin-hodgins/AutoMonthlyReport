#'@title
#'Performs Min-Max normalization on a variable
#'
#'@description
#'The output is rounded to 2 deicimel places
#'
#'@details
#'Scales the variable between [0,1] where 0 is the minimum value and 1 is the maximum
#'value.
#'
#'@param x The variable to be normalized.
#'@export
normalize <- function(x){
  return(x = round(((x - min(x))/(max(x)-min(x))),2))
}
