#' odds
#'
#' Calculate the odds given a probability
#'
#' @param p probability

#' @return odds
#' 
#' @export

odds <- function(p){
  odds <- p/(1-p)
  
  return(odds)
}