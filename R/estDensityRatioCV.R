#' Estimate density ratio
#'
#' Calculate the re-calibrated risk within each group.
#'
#' @param data a data.frame containing the risk score, observed response and group label
#' @param method method to be used to calibrate the risks, options: 'logit' for logistic regression
#' @param args optional odditional arguments to be used for the method. e.g., when method = 'logit', the degree of the polynomial can be specified
#' @param groupvar group variable
#' @param refgp reference group level
#' @param calrisk calibrated risk score
#' @param k number of folds for cross-validation
#'
#' @return a list with taus and aTPRs 
#' 
#' @import dplyr
#' @importFrom stats glm predict poly
#'
#' @export


estDensityRatioCV <- function(data
                            ,method = 'logit'
                            ,args = list(1)
                            ,groupvar = .data$s
                            ,refgp = '1'
                            ,calrisk = .data$rs.gX
                            ,k = 5
                            ){
  
  
  # use k-fold cross-validation to get MSPE
  cat("Mean squared prediction error of density ratio models computed using",k,"-fold cross-validation \n")
  
  folddf <- data
  colnames(folddf)

  # randomly assign fold
  folddf$fold <- sample(1:k, size=nrow(data), replace=TRUE)
  
  
  # loop over folds
  for(f in 1:k){
    
    # Calibrate risk according to method
    fold.k <- estDensityRatio(train = folddf %>% dplyr::filter(.data$fold != f)
                              ,test = folddf %>% dplyr::filter(.data$fold == f)
                              ,method 
                              ,args 
                              ,groupvar = .data$s
                              ,refgp
                              ,calrisk = .data$rs.gX )
    
    # stack results by fold
    if(f == 1){
      foldout <- fold.k
    }else{
      foldout <- foldout %>%
        dplyr::bind_rows(fold.k)
    }     

  } # end fold loop
  # get MSPE for group s 
  MSPE <- foldout %>%
    dplyr::group_by(.data$s) %>%
    dplyr::mutate(diff.sq = (.data$refind - .data$w_s)**2) %>%
    dplyr::summarize(MSPE = mean(.data$diff.sq)) %>%
    dplyr::pull(.data$MSPE)
  
  cat("The mean square prediction error in group is \n")   
  print(MSPE)
  return(foldout)
}