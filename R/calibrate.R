#' calibrate
#'
#' Inner function for re-calibrated risk within a group.
#'
#' @param train a data.frame containing the risk score, observed response and group label
#' @param test label, categorical
#' @param method method to be used to calibrate the risks, options: 'logit' for logistic regression
#' @param args optional odditional arguments to be used for the method. e.g., when method = 'logit', the degree of the polynomial can be specified
#'
#' @return data.frame with the calibrated risk results
#' 
#' @import dplyr
#' @importFrom stats glm predict poly
#'
#' @export

calibrate <- function(train
                      ,test
                      ,method
                      ,args
                      ){
  
  if(tolower(method) == 'logit'){
    # get polynomial degree
    deg <- args[[1]]
    
    # ------------ fit rs.gX stratified by s ------------- #
    r.s <- glm(y ~ poly(lp.gX, degree=deg) , data = train, family ='binomial')
    
    # get predictions from s-concordant model
    rs.gX <- predict(r.s, newdata = test, type = 'response')
    test <- test %>%
              bind_cols(rs.gX = rs.gX)

  } else {stop("unknown calibration method specified")}
  
  
    return(test)
}