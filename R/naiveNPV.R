#' Standard Negative Predictive Value (NPV)
#'
#' Calculate the negative predictive value
#'
#' @param data a data.frame containing a risk score, response label and group label
#' @param risk the data.frame column representing the (original) risk score under evaluation
#' @param response the response variable
#' @param groupvar the group column
#' @param taus a vector containing the decision thresholds of interest
#' @param se.boot logical indicating whether bootstrapping should be used to estimate standard error
#' @param bootsize number of bootstrapped data sets to sample, default = 500
#' @param alpha confidence level for bootstrap quantiles

#' @return a list with a data.frame of NPVs by tau and group and data.frame of bootstrapped estimates if se.boot = TRUE 
#' 
#' @import dplyr
#' @importFrom stats quantile sd
#'
#' @export

naiveNPV <- function(data
                     , risk
                     , response 
                     , groupvar
                     , taus = seq(0.1,0.9,0.1)
                     , se.boot = FALSE
                     , bootsize = 500
                     , alpha = 0.05
                     ){
  
  NPV <- get_naiveNPV(data = {{data}}
               , risk = {{risk}}
               , response = {{response}}
               , groupvar = {{groupvar}}
               , taus = taus)
  
  if(se.boot == TRUE){
    
    NPV.boot <- NULL 
    
    for(b in 1:bootsize){
      # sample with replacement within group strata
      boot.b <- data %>%
        dplyr::group_by({{groupvar}}) %>%
        dplyr::sample_frac(size = 1, replace = TRUE) %>%
        dplyr::ungroup()
      
      # get NPV for this bootstrapped sample
      NPV.b <- get_naiveNPV(data = boot.b
                          , risk = {{risk}}
                          , response = {{response}}
                          , groupvar = {{groupvar}}
                          , taus = taus)
      
      # stack this bootstrap with previous
      NPV.boot <- NPV.boot %>%
                    bind_rows(NPV.b %>% mutate(bootrep = b))
    }
    
    NPV.boot.sum <- NPV.boot %>%
      dplyr::group_by({{groupvar}},.data$tau) %>%
      dplyr::summarise(NPV.bootmean = mean(.data$NPV)
                       ,NPV.boot.lower = quantile(.data$NPV,alpha/2)
                       ,NPV.boot.upper = quantile(.data$NPV,1-alpha/2)
                       ,NPV.bootse = sd(.data$NPV)
                       ,n = n()) 
    
    
    NPV <- NPV %>%
      left_join(NPV.boot.sum, by = join_by({{groupvar}}, .data$tau)) 
  }
  
  if(se.boot == TRUE){
    out <- list(NPV = NPV, boot = NPV.boot) 
  }else{
    out <- list(NPV = NPV)
  }
  
  return(out)
}