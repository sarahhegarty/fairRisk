#' Standard Negative Predictive Value (NPV)
#'
#' Calculate the negative predictive value
#'
#' @param data a data.frame containing the original and re-calibrate risks, density ratio estimate and group label
#' @param risk the data.frame column representing the (original) risk score under evaluation
#' @param response the response variable
#' @param groupvar the group column
#' @param taus a vector containing the decision thresholds of interest
#'
#' @return a list with taus and NPVs 
#' 
#' @import dplyr
#' @importFrom stats approx coef runif rnorm rbinom
#'
#' @export
 
get_naiveNPV <-  function(data
                          , risk
                          , response 
                          , groupvar
                          , taus = seq(0.1,0.9,0.1)){
  
  NPV <- NULL 
  
  for(t in taus){
    
    NPV.t <- data %>%
      dplyr::mutate(tau = t
                    ,highrisk = if_else({{risk}} > t,1,0)) %>%
      dplyr::group_by(.data$tau, {{groupvar}}) %>%
      dplyr::summarise(num = mean((1 - {{response}}) * (1 - .data$highrisk))
                       ,denom = mean(1 - .data$highrisk)) %>%
      dplyr::mutate(NPV = .data$num/.data$denom) %>%
      dplyr::select({{groupvar}},.data$tau, .data$NPV)
    
    NPV <- NPV %>%
      dplyr::bind_rows(NPV.t)
  }
  
  return(NPV)
}

