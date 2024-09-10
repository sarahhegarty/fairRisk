#' Calibrate risk 
#'
#' Calculate the re-calibrated risk within each group.
#'
#' @param data a data.frame containing the risk score, observed response and group label
#' @param groupvar the group label, categorical
#' @param response the observed outcome or response variable, should be binary 0/1
#' @param risk the (original) risk score under evaluation, scalar
#' @param transform apply a logit transformation to the risk score, converts probability to a linear predictor
#' @param method method to be used to calibrate the risks, options: 'logit' for logistic regression
#' @param args optional odditional arguments to be used for the method. e.g., when method = 'logit', the degree of the polynomial can be specified
#'
#' @return data.frame with the calibrated risk results
#' 
#' @import dplyr
#' @importFrom stats glm predict poly
#'
#' @export

calibrateRisk <- function(data 
                          , groupvar
                          , response
                          , risk
                          , transform = TRUE
                          , method = "qlogit"
                          , args = list(2)
                          ){
  
  
  # check data types
  check.resp <- data %>%
                dplyr::filter(!({{response}} %in% c(0,1))) %>%
                nrow() 
  if(check.resp != 0){stop("response must be 0/1")}
  
  check.risk <- data %>%
                dplyr::select({{risk}}) %>%
                range() 
  if(transform == TRUE && (check.risk[1] < 0 || check.risk[2] > 1) ){stop("risk must be between 0 and 1")}
  
  check.group <- data %>%
                    dplyr::group_by({{groupvar}}) %>%
                    dplyr::summarise(n = n()) %>%
                    nrow()
  if(check.group > 10){print("Warning: grouping variable has more than 10 distinct levels")}
  
  # check method
  if(!(tolower(method) %in% c('logit'))){stop("calibration method not recognized, please select from the following choices: 'logit'")}
  
  # construct dataset
  s <- data %>% dplyr::select({{groupvar}}); names(s) ='s'
  y <- data %>% dplyr::select({{response}}); names(y) = 'y'
  gX <- data %>% dplyr::select({{risk}}); names(gX) = 'gX'
  
  if(transform == TRUE){
    # get linear predictor version of risk using logit transfomation
    lp.gX <- logit(gX)  ; names(lp.gX) = 'lp.gX'
  }else{
    lp.gX <- gX; names(lp.gX) = 'lp.gX'
  }
  
  df <- data.frame(s, y, gX, lp.gX)
  
  # Determine group levels 
  slist <-  df %>%
    dplyr::arrange(.data$s) %>%
    dplyr::pull(.data$s) %>%
    unique()
  
  ns = length(slist)
  
  cat("There are ",ns," groups: ",slist,"\n")
 
  # loop over groups
  for(i.s in 1:ns){
    
    # Get group subset
    df.s <- df %>% dplyr::filter(.data$s == slist[i.s])
    
 
    
    if(tolower(method) == 'logit'){
      # get polynomial degree
      deg <- args[[1]]
      
      # ------------ fit rs.gX stratified by s ------------- #
      r.s <- stats::glm(y ~ poly(lp.gX,degree=deg) , data = df.s, family ='binomial')
      
      # get predictions from s-concordant model
      df.s$rs.gX <- stats::predict(r.s, newdata = df.s, type = 'response')
      
    } else{print("unknown calibration method specified")}
    
    # stack results by s level
    if(i.s == 1){
      caldf <- df.s
    }else{
      caldf <- caldf %>%
        dplyr::bind_rows(df.s)
    }
  } # end loop over s
  
  cat("The mean square prediction error is: XX \n")
  return(caldf)
}