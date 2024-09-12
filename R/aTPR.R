#' aTPR
#'
#' Calculate the adjusted true positive rate with respect to a specified reference group.
#'
#' @param data a data.frame containing the original and re-calibrate risks, density ratio estimate and group label
#' @param groupvar the group column
#' @param ref reference level
#' @param response response variable
#' @param risk the data.frame column representing the (original) risk score under evaluation
#' @param taus a vector containing the decision thresholds of interest
#' @param calmethod method to be used for calibration, default "logit"
#' @param cal.args additional arguments for calibration method; with "logit" degree of polynomial and if risk score should be transformed with logit
#' @param drmethod method for estimating density ratio
#' @param dr.args arguments for density ratio estimation method
#' @param cv logical indicating whether MSPE should be estimated by cross-validation
#' @param se.boot logical indicating whether bootstrapping should be used to estimate standard error
#' @param bootsize number of bootstrapped data sets to sample, default = 500
#' @param alpha confidence level for bootstrap quantiles
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
                 , cal.args = list(2,TRUE)
                 , drmethod = c('logit')
                 , dr.args = list(2)
                 , cv = FALSE
                 , se.boot = FALSE
                 , bootsize = 500
                 , alpha = 0.05
                 ){
  
  # check for appropriate options for estimation steps
  if(!(tolower(calmethod)  %in% c('logit'))){stop("Calibration method (cm) not recognized, please choose from the following list: llogit, qlogit")}
  if(!(tolower(drmethod)  %in% c('logit','none'))){stop("Density ratio method (dm) not recognized, please choose from the following list: llogit, qlogit")}

  # Step 1 & 2: Calibrate risk score
  dfcal <- calibrateRiskCV(data = {{data}}
                          , groupvar = {{groupvar}}
                          , response = {{response}}
                          , risk = {{risk}}
                          , transform = cal.args[[2]]
                          , method = calmethod
                          , args = cal.args[[1]]
                          , cv = cv
                          , k = 5 )
  
  # Step 3: Estimate density ratio
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
  
  if(se.boot == TRUE){
    
    aTPR.stack <- NULL 
    
    for(b in 1:bootsize){
      # sample with replacement within group strata
       boot.b <- data %>%
          dplyr::group_by({{groupvar}}) %>%
          dplyr::sample_frac(size = 1, replace = TRUE) %>%
          dplyr::ungroup()
       
       # Step 1 & 2: Calibrate risk score
       dfcal.b <- calibrateRiskCV(data = boot.b
                                , groupvar = {{groupvar}}
                                , response = {{response}}
                                , risk = {{risk}}
                                , transform = cal.args[[2]]
                                , method = calmethod
                                , args = cal.args[[1]]
                                , cv = FALSE)
       
       # Step 3: Estimate density ratio
       df_atpr.b <- estDensityRatio(train = dfcal.b
                                  ,test = dfcal.b
                                  ,method = drmethod
                                  ,args = dr.args
                                  ,groupvar = .data$s
                                  ,refgp = ref
                                  ,calrisk = .data$rs.gX)
       
       # Step 4: Calculate adjuted TPR
       aTPR.b <- get_aTPR(data = df_atpr.b
                        , orig_risk = .data$gX
                        , cal_risk = .data$rs.gX
                        , dens_ratio = .data$w_s
                        , groupvar = .data$s
                        , taus = taus)
       
       # stack this bootstrap with previous
       aTPR.stack <- aTPR.stack %>%
                bind_rows(aTPR.b) 
    }
    
    aTPR.boot <- aTPR.stack %>%
              dplyr::group_by(.data$s,.data$tau) %>%
              dplyr::summarise(aTPR.bootmean = mean(.data$aTPR)
                               ,aTPR.boot.lower = quantile(.data$aTPR,alpha/2)
                               ,aTPR.boot.upper = quantile(.data$aTPR,1-alpha/2)
                               ,aTPR.bootse = sd(.data$aTPR)) 
              
    
    aTPR <- aTPR %>%
              left_join(aTPR.boot, by = join_by(s, tau)) 
  }
    
    return(aTPR)
}
