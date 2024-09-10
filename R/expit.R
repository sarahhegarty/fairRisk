#' expit
#'
#' Calculate the expit of a quantity
#'
#' @param x input value

#' @return expit
#' 
#' @export

expit <- function(x){
  z <- exp(x)/(1 + exp(x))
  
  return(z)
}