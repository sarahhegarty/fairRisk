#' Standard True Positive Rate (TPR)
#'
#' Calculate the true positive rate
#'
#' @param data a data.frame containing the original and re-calibrate risks, density ratio estimate and group label
#' @param risk the data.frame column representing the (original) risk score under evaluation
#' @param response the response variable
#' @param groupvar the group column
#' @param taus a vector containing the decision thresholds of interest
#'
#' @return a list with taus and aTPRs 
#' 
#' @import dplyr
#' @importFrom stats approx coef runif rnorm rbinom
#'
#' @export
 
get_naiveTPR <-  function(data
                          , risk
                          , response 
                          , groupvar
                          , taus = seq(0.1,0.9,0.1)){
  
  TPR <- NULL 
  
  for(t in taus){
    
    TPR.t <- data %>%
      dplyr::mutate(tau = t
                    ,highrisk = if_else({{risk}} > t,1,0)) %>%
      dplyr::group_by(.data$tau, {{groupvar}}) %>%
      dplyr::summarise(num = mean({{response}}* .data$highrisk)
                       ,denom = mean({{response}})) %>%
      dplyr::mutate(TPR = .data$num/.data$denom) %>%
      dplyr::select({{groupvar}},.data$tau, .data$TPR)
    
    TPR <- TPR %>%
      dplyr::bind_rows(TPR.t)
  }
  
  return(TPR)
}

