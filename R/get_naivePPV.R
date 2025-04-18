#' Standard Positive Predictive Value (PPV)
#'
#' Calculate the positive predictive value
#'
#' @param data a data.frame containing the original and re-calibrate risks, density ratio estimate and group label
#' @param risk the data.frame column representing the (original) risk score under evaluation
#' @param response the response variable
#' @param groupvar the group column
#' @param taus a vector containing the decision thresholds of interest
#'
#' @return a list with taus and PPVs 
#' 
#' @import dplyr
#' @importFrom stats approx coef runif rnorm rbinom
#'
#' @export
 
get_naivePPV <-  function(data
                          , risk
                          , response 
                          , groupvar
                          , taus = seq(0.1,0.9,0.1)){
  
  PPV <- NULL 
  
  for(t in taus){
    
    PPV.t <- data %>%
      dplyr::mutate(tau = t
                    ,highrisk = if_else({{risk}} > t,1,0)) %>%
      dplyr::group_by(.data$tau, {{groupvar}}) %>%
      dplyr::summarise(num = mean(({{response}}) * (.data$highrisk))
                       ,denom = mean(.data$highrisk)) %>%
      dplyr::mutate(PPV = .data$num/.data$denom) %>%
      dplyr::select({{groupvar}},.data$tau, .data$PPV)
    
    PPV <- PPV %>%
      dplyr::bind_rows(PPV.t)
  }
  
  return(PPV)
}

