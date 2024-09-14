#' aTPR
#'
#' Calculate the adjusted true positive rate with respect to a specified reference group.
#'
#' @param data a data.frame containing the original and re-calibrated risks, density ratio estimate and group label
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
#' @param quietly suppress messages, default = TRUE
#'
#' @return a list containing a data.frama of aTPR estimates for each tau and group and separate data.frame of bootstrapped estimates if se.boot = TRUE
#' 
#' @import dplyr
#' @importFrom stats glm predict poly sd quantile
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
                 , quietly = TRUE
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
                          , k = 5 
                          , quietly = quietly)
  
  # Step 3: Estimate density ratio
  df_atpr <- estDensityRatioCV(data = dfcal[[1]]
                                ,method = drmethod
                                ,args = dr.args
                                ,groupvar = .data$s
                                ,refgp = ref
                                ,calrisk = .data$rs.gX
                                ,cv = cv
                                ,k = 5
                                ,quietly = quietly)
  
  # Step 4: Calculate adjuted TPR
  aTPR <- get_aTPR(data = df_atpr[[1]]
                   , orig_risk = .data$gX
                   , cal_risk = .data$rs.gX
                   , dens_ratio = .data$w_s
                   , groupvar = .data$s
                   , taus = taus)
  
   if(se.boot == TRUE){
    
    aTPR.boot <- NULL 
    
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
                                , cv = FALSE
                                , quietly = TRUE)
       
       # Step 3: Estimate density ratio
       df_atpr.b <- estDensityRatioCV(data = dfcal.b[[1]]
                                      ,method = drmethod
                                      ,args = dr.args
                                      ,groupvar = .data$s
                                      ,refgp = ref
                                      ,calrisk = .data$rs.gX
                                      ,cv = FALSE
                                      ,quietly = TRUE)
       
       # Step 4: Calculate adjuted TPR
       aTPR.b <- get_aTPR(data = df_atpr.b[[1]]
                        , orig_risk = .data$gX
                        , cal_risk = .data$rs.gX
                        , dens_ratio = .data$w_s
                        , groupvar = .data$s
                        , taus = taus)
       
       # stack this bootstrap with previous
       aTPR.boot <- aTPR.boot %>%
                bind_rows(aTPR.b %>%
                            mutate(bootrep = b)) 
    }
    
    aTPR.boot.sum <- aTPR.boot %>%
              dplyr::group_by(.data$s,.data$tau) %>%
              dplyr::summarise(aTPR.bootmean = mean(.data$aTPR)
                               ,aTPR.boot.lower = quantile(.data$aTPR,alpha/2)
                               ,aTPR.boot.upper = quantile(.data$aTPR,1-alpha/2)
                               ,aTPR.bootse = sd(.data$aTPR)
                               ,n = n()) 
              
    
    aTPR <- aTPR %>%
              left_join(aTPR.boot.sum, by = join_by(s, tau)) 
  }
    
   if(se.boot == TRUE){
     out <- list(aTPR = aTPR, boot = aTPR.boot) 
   }else{
     out <- list(aTPR = aTPR)
   }
  
    return(out)
}
