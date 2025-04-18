#' Adjusted Negative Predictive Values (aNPV)
#'
#' Calculate the adjusted negative predictive value with respect to a specified reference group.
#'
#' @param data a data.frame containing the original and re-calibrate risks, density ratio estimate and group label
#' @param orig_risk the data.frame column representing the (original) risk score under evaluation
#' @param cal_risk the data.frame column representing the re-calibrated risk score 
#' @param dens_ratio the data.frame column representing the density ratio of the recalibrated risk scores
#' @param groupvar the group column
#' @param taus a vector containing the decision thresholds of interest
#'
#' @return a list with taus and aNPVs 
#' 
#' @import dplyr
#' @importFrom stats approx coef runif rnorm rbinom
#'
#' @export
 
get_aNPV <- function(data
                     , orig_risk
                     , cal_risk
                     , dens_ratio
                     , groupvar
                     , taus = seq(0.1,0.9,0.1)){

  aNPV <- NULL
  
  for(t in taus){
    
    aNPV.t <- data %>%
              dplyr::mutate(highrisk = if_else({{orig_risk}} > t,1,0)
                     ,tau = t) %>%
              dplyr::group_by({{groupvar}},.data$tau) %>%
              dplyr::summarise(num = mean((1 - {{cal_risk}}) * (1 -.data$highrisk) * {{dens_ratio}})
                               ,denom = mean((1 - .data$highrisk) * {{dens_ratio}})) %>%
              dplyr::mutate(aNPV = .data$num/.data$denom)  %>%
              dplyr::select({{groupvar}},.data$tau, .data$aNPV)
    
    aNPV <- aNPV %>%
      dplyr::bind_rows(aNPV.t)
  }
  
  return(aNPV)
}

