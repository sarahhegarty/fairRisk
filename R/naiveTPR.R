#' Standard True Positive Rate (TPR)
#'
#' Calculate the true positive rate
#'
#' @param data a data.frame containing a risk score, response label and group label
#' @param risk the data.frame column representing the (original) risk score under evaluation
#' @param response the response variable
#' @param groupvar the group column
#' @param taus a vector containing the decision thresholds of interest
#' @param se.boot logical indicating whether bootstrapping should be used to estimate standard error
#' @param bootsize number of bootstrapped data sets to sample, default = 500
#' @param alpha confidence level for bootstrap quantiles

#' @return a list with a data.frame of TPRs by tau and group and data.frame of bootstrapped estimates if se.boot = TRUE 
#' 
#' @import dplyr
#' @importFrom stats quantile sd
#'
#' @export

naiveTPR <- function(data
                     , risk
                     , response 
                     , groupvar
                     , taus = seq(0.1,0.9,0.1)
                     , se.boot = FALSE
                     , bootsize = 500
                     , alpha = 0.05
                     ){
  
  TPR <- get_naiveTPR(data = {{data}}
               , risk = {{risk}}
               , response = {{response}}
               , groupvar = {{groupvar}}
               , taus = taus)
  
  if(se.boot == TRUE){
    
    TPR.boot <- NULL 
    
    for(b in 1:bootsize){
      # sample with replacement within group strata
      boot.b <- data %>%
        dplyr::group_by({{groupvar}}) %>%
        dplyr::sample_frac(size = 1, replace = TRUE) %>%
        dplyr::ungroup()
      
      # get TPR for this bootstrapped sample
      TPR.b <- get_naiveTPR(data = boot.b
                          , risk = {{risk}}
                          , response = {{response}}
                          , groupvar = {{groupvar}}
                          , taus = taus)
      
      # stack this bootstrap with previous
      TPR.boot <- TPR.boot %>%
                    bind_rows(TPR.b %>% mutate(bootrep = b))
    }
    
    TPR.boot.sum <- TPR.boot %>%
      dplyr::group_by({{groupvar}},.data$tau) %>%
      dplyr::summarise(TPR.bootmean = mean(.data$TPR)
                       ,TPR.boot.lower = quantile(.data$TPR,alpha/2)
                       ,TPR.boot.upper = quantile(.data$TPR,1-alpha/2)
                       ,TPR.bootse = sd(.data$TPR)
                       ,n = n()) 
    
    
    TPR <- TPR %>%
      left_join(TPR.boot.sum, by = join_by({{groupvar}}, tau)) 
  }
  
  if(se.boot == TRUE){
    out <- list(TPR = TPR, boot = TPR.boot) 
  }else{
    out <- list(TPR = TPR)
  }
  
  return(out)
}