#' Standard Positive Predictive Value (PPV)
#'
#' Calculate the positive predictive value
#'
#' @param data a data.frame containing a risk score, response label and group label
#' @param risk the data.frame column representing the (original) risk score under evaluation
#' @param response the response variable
#' @param groupvar the group column
#' @param taus a vector containing the decision thresholds of interest
#' @param se.boot logical indicating whether bootstrapping should be used to estimate standard error
#' @param bootsize number of bootstrapped data sets to sample, default = 500
#' @param alpha confidence level for bootstrap quantiles

#' @return a list with a data.frame of PPVs by tau and group and data.frame of bootstrapped estimates if se.boot = TRUE 
#' 
#' @import dplyr
#' @importFrom stats quantile sd
#'
#' @export

naivePPV <- function(data
                     , risk
                     , response 
                     , groupvar
                     , taus = seq(0.1,0.9,0.1)
                     , se.boot = FALSE
                     , bootsize = 500
                     , alpha = 0.05
                     ){
  
  PPV <- get_naivePPV(data = {{data}}
               , risk = {{risk}}
               , response = {{response}}
               , groupvar = {{groupvar}}
               , taus = taus)
  
  if(se.boot == TRUE){
    
    PPV.boot <- NULL 
    
    for(b in 1:bootsize){
      # sample with replacement within group strata
      boot.b <- data %>%
        dplyr::group_by({{groupvar}}) %>%
        dplyr::sample_frac(size = 1, replace = TRUE) %>%
        dplyr::ungroup()
      
      # get PPV for this bootstrapped sample
      PPV.b <- get_naivePPV(data = boot.b
                          , risk = {{risk}}
                          , response = {{response}}
                          , groupvar = {{groupvar}}
                          , taus = taus)
      
      # stack this bootstrap with previous
      PPV.boot <- PPV.boot %>%
                    bind_rows(PPV.b %>% mutate(bootrep = b))
    }
    
    PPV.boot.sum <- PPV.boot %>%
      dplyr::group_by({{groupvar}},.data$tau) %>%
      dplyr::summarise(PPV.bootmean = mean(.data$PPV)
                       ,PPV.boot.lower = quantile(.data$PPV,alpha/2)
                       ,PPV.boot.upper = quantile(.data$PPV,1-alpha/2)
                       ,PPV.bootse = sd(.data$PPV)
                       ,n = n()) 
    
    
    PPV <- PPV %>%
      left_join(PPV.boot.sum, by = join_by({{groupvar}}, .data$tau)) 
  }
  
  if(se.boot == TRUE){
    out <- list(PPV = PPV, boot = PPV.boot) 
  }else{
    out <- list(PPV = PPV)
  }
  
  return(out)
}