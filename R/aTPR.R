#' Calibrate risk 
#'
#' Calculate the re-calibrated risk within each group.
#'
#' @param data a data.frame containing the original and re-calibrate risks, density ratio estimate and group label
#' @param groupvar the group column
#' @param ref reference level
#' @param response response variable
#' @param risk the data.frame column representing the (original) risk score under evaluation
#' @param taus a vector containing the decision thresholds of interest
#' @param calmethod method to be used for calibration, default "logit"
#' @param cal.args additional arguments for calibration method; with "logit" degree of polynomial 
#' @param drmethod method for estimating density ratio
#' @param dr.args arguments for density ratio estimation method
#' @param cv logical indicating whether MSPE should be estimated by cross-validation
#'
#' @return a list with taus and aTPRs 
#' 
#' @import dplyr
#' @importFrom stats glm predict poly
#'
#' @export

aTPR <- function(data
                 , groupvar
                 , ref
                 , response
                 , risk
                 , taus
                 , calmethod = c('logit')
                 , cal.args = list(2)
                 , drmethod = c('logit')
                 , dr.args = list(2)
                 , cv = FALSE
                 ){
  
  # check for appropriate options for estimation steps
  if(!(tolower(calmethod)  %in% c('logit'))){stop("Calibration method (cm) not recognized, please choose from the following list: llogit, qlogit")}
  if(!(tolower(drmethod)  %in% c('logit','none'))){stop("Density ratio method (dm) not recognized, please choose from the following list: llogit, qlogit")}


  # Step 1 & 2: Calibrate risk score
  dfcal <- calibrateRiskCV(data = {{data}}
                          , groupvar = {{groupvar}}
                          , response = {{response}}
                          , risk = {{risk}}
                          , transform = TRUE
                          , method = calmethod
                          , args = cal.args
                          , cv = cv
                          , k = 5 )
  
  # Step 3: Estimate density ratio
  #df_atpr <- dfcal %>% mutate(w_s = 1)
  df_atpr <- estDensityRatio(train = dfcal
                             ,test = dfcal
                             ,method = drmethod
                             ,args = dr.args
                             ,groupvar = .data$s
                             ,refgp = ref
                             ,calrisk = .data$rs.gX)
  
  # Step 4: Calculate adjuted TPR
  aTPR <- get_aTPR(data = df_atpr
                   , orig_risk = .data$gX
                   , cal_risk = .data$rs.gX
                   , dens_ratio = .data$w_s
                   , groupvar = .data$s
                   , taus = taus)
    
    return(aTPR)
}
