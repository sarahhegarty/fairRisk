#' Adjusted True Negative Rate (aTNR)
#'
#' Calculate the adjusted true negative rate with respect to a specified reference group.
#'
#' @param data a data.frame containing the original and re-calibrate risks, density ratio estimate and group label
#' @param orig_risk the data.frame column representing the (original) risk score under evaluation
#' @param cal_risk the data.frame column representing the re-calibrated risk score 
#' @param dens_ratio the data.frame column representing the density ratio of the recalibrated risk scores
#' @param groupvar the group column
#' @param taus a vector containing the decision thresholds of interest
#'
#' @return a list with taus and aTNRs 
#' 
#' @import dplyr
#' @importFrom stats approx coef runif rnorm rbinom
#'
#' @export
 
get_aTNR <- function(data
                     , orig_risk
                     , cal_risk
                     , dens_ratio
                     , groupvar
                     , taus = seq(0.1,0.9,0.1)){

  aTNR <- NULL
  
  for(t in taus){
    
    aTNR.t <- data %>%
              dplyr::mutate(highrisk = if_else({{orig_risk}} > t,1,0)
                     ,tau = t) %>%
              dplyr::group_by({{groupvar}},.data$tau) %>%
              dplyr::summarise(num = mean((1-{{cal_risk}}) * (1-.data$highrisk) * {{dens_ratio}})
                               ,denom = mean((1-{{cal_risk}}) * {{dens_ratio}})) %>%
              dplyr::mutate(aTNR = .data$num/.data$denom)  %>%
              dplyr::select({{groupvar}},.data$tau, .data$aTNR)
    
    aTNR <- aTNR %>%
      dplyr::bind_rows(aTNR.t)
  }
  
  return(aTNR)
}

