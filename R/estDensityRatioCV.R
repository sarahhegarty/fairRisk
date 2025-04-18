#' Estimate density ratio
#'
#' Calculate the density ratio of the re-calibrated risk between each group and reference group.
#'
#' @param data a data.frame containing the risk score, observed response and group label
#' @param method method to be used to calibrate the risks, options: 'logit' for logistic regression
#' @param args optional odditional arguments to be used for the method. e.g., when method = 'logit', the degree of the polynomial can be specified
#' @param groupvar group variable
#' @param refgp reference group level
#' @param calrisk calibrated risk score
#' @param transform  use logit of calibrated risk (TRUE) or calibrated risk on probability scale (FALSE), default
#' @param cv logical indicating if cross-validation should be performed to estimate MSPE
#' @param k number of folds for cross-validation
#' @param quietly suppress messages, default = TRUE
#'
#' @return a list with data.frame with estimated density ratio weights and MSPE if cv options selected
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
                            ,transform = FALSE
                            ,cv = TRUE
                            ,k = 5
                            ,quietly = TRUE
                            ){
  # overall estimate 
  drout <- estDensityRatio(train = data
                           ,test = data
                           ,method = method
                           ,args = args
                           ,groupvar = .data$s
                           ,refgp = refgp
                           ,calrisk = .data$rs.gX
                           ,transform = transform
                           ,quietly = quietly)
  
  if(cv == TRUE){
    # use k-fold cross-validation to get MSPE
    if(quietly !=TRUE){
      cat("Mean squared prediction error of density ratio models computed using",k,"-fold cross-validation \n")
    }
    
    folddf <- data
    colnames(folddf)
  
    # randomly assign fold
    folddf$fold <- sample(1:k, size=nrow(folddf), replace=TRUE)
    
    
    # loop over folds
    for(f in 1:k){
      
      # Calibrate risk according to method
      fold.k <- estDensityRatio(train = folddf %>% dplyr::filter(.data$fold != f)
                                ,test = folddf %>% dplyr::filter(.data$fold == f)
                                ,method 
                                ,args 
                                ,groupvar = .data$s
                                ,refgp
                                ,calrisk = .data$rs.gX
                                ,transform = transform
                                ,quietly = quietly)
      
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
    
    if(quietly != TRUE){ 
      cat("The mean square prediction error for the density ratio model in each group is \n") 
      print(MSPE)
    }
  }
  
  if(cv == TRUE){
    out <- list(drout = drout, MSPE =MSPE)
  }else{out <- list(drout= drout)}
  
  
  return(out)
}