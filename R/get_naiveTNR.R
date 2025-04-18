#' Standard True Negative Rate (TNR)
#'
#' Calculate the true negative rate
#'
#' @param data a data.frame containing the original and re-calibrate risks, density ratio estimate and group label
#' @param risk the data.frame column representing the (original) risk score under evaluation
#' @param response the response variable
#' @param groupvar the group column
#' @param taus a vector containing the decision thresholds of interest
#'
#' @return a list with taus and TNRs 
#' 
#' @import dplyr
#' @importFrom stats approx coef runif rnorm rbinom
#'
#' @export
 
get_naiveTNR <-  function(data
                          , risk
                          , response 
                          , groupvar
                          , taus = seq(0.1,0.9,0.1)){
  
  TNR <- NULL 
  
  for(t in taus){
    
    TNR.t <- data %>%
      dplyr::mutate(tau = t
                    ,highrisk = if_else({{risk}} > t,1,0)) %>%
      dplyr::group_by(.data$tau, {{groupvar}}) %>%
      dplyr::summarise(num = mean((1-{{response}}) * (1-.data$highrisk))
                       ,denom = mean((1-{{response}}))) %>%
      dplyr::mutate(TNR = .data$num/.data$denom) %>%
      dplyr::select({{groupvar}},.data$tau, .data$TNR)
    
    TNR <- TNR %>%
      dplyr::bind_rows(TNR.t)
  }
  
  return(TNR)
}

