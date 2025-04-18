#' Standard True Negative Rate (TNR)
#'
#' Calculate the true negative rate
#'
#' @param data a data.frame containing a risk score, response label and group label
#' @param risk the data.frame column representing the (original) risk score under evaluation
#' @param response the response variable
#' @param groupvar the group column
#' @param taus a vector containing the decision thresholds of interest
#' @param se.boot logical indicating whether bootstrapping should be used to estimate standard error
#' @param bootsize number of bootstrapped data sets to sample, default = 500
#' @param alpha confidence level for bootstrap quantiles

#' @return a list with a data.frame of TNRs by tau and group and data.frame of bootstrapped estimates if se.boot = TRUE 
#' 
#' @import dplyr
#' @importFrom stats quantile sd
#'
#' @export

naiveTNR <- function(data
                     , risk
                     , response 
                     , groupvar
                     , taus = seq(0.1,0.9,0.1)
                     , se.boot = FALSE
                     , bootsize = 500
                     , alpha = 0.05
                     ){
  
  TNR <- get_naiveTNR(data = {{data}}
               , risk = {{risk}}
               , response = {{response}}
               , groupvar = {{groupvar}}
               , taus = taus)
  
  if(se.boot == TRUE){
    
    TNR.boot <- NULL 
    
    for(b in 1:bootsize){
      # sample with replacement within group strata
      boot.b <- data %>%
        dplyr::group_by({{groupvar}}) %>%
        dplyr::sample_frac(size = 1, replace = TRUE) %>%
        dplyr::ungroup()
      
      # get TNR for this bootstrapped sample
      TNR.b <- get_naiveTNR(data = boot.b
                          , risk = {{risk}}
                          , response = {{response}}
                          , groupvar = {{groupvar}}
                          , taus = taus)
      
      # stack this bootstrap with previous
      TNR.boot <- TNR.boot %>%
                    bind_rows(TNR.b %>% mutate(bootrep = b))
    }
    
    TNR.boot.sum <- TNR.boot %>%
      dplyr::group_by({{groupvar}},.data$tau) %>%
      dplyr::summarise(TNR.bootmean = mean(.data$TNR)
                       ,TNR.boot.lower = quantile(.data$TNR,alpha/2)
                       ,TNR.boot.upper = quantile(.data$TNR,1-alpha/2)
                       ,TNR.bootse = sd(.data$TNR)
                       ,n = n()) 
    
    
    TNR <- TNR %>%
      left_join(TNR.boot.sum, by = join_by({{groupvar}}, .data$tau)) 
  }
  
  if(se.boot == TRUE){
    out <- list(TNR = TNR, boot = TNR.boot) 
  }else{
    out <- list(TNR = TNR)
  }
  
  return(out)
}