#' logit
#'
#' Calculate the logit given a probability
#'
#' @param p probability

#' @return logit
#' 
#' @export

logit <- function(p){
  z <- log(p/(1-p))
  
  return(z)
}