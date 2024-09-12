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
#' @param cv logical indicating if cross-validation estimate of MSPE should be performed
#' @param k number of folds for cross-validation, default = 5
#'
#' @return data.frame with the calibrated risk results
#' 
#' @import dplyr
#' @importFrom stats glm predict poly
#'
#' @export

calibrateRiskCV <- function(data 
                          , groupvar
                          , response
                          , risk
                          , transform = TRUE
                          , method = "logit"
                          , args = list(2)
                          , cv = TRUE
                          , k = 5
                          ){
  
  
   # check data types
  check.resp <- data %>%
                dplyr::filter(!({{response}} %in% c(0,1))) %>%
                nrow() 
  if(check.resp != 0){stop("response must be 0/1")}
  
  check.risk <- data %>%
                dplyr::select({{risk}}) %>%
                range() 
  if(transform == TRUE && (check.risk[1] < 0 || check.risk[2] > 1) ){print(check.risk); stop("risk must be between 0 and 1")}
  
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
    lp.gX <- logit(gX)   ; names(lp.gX) = 'lp.gX'
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
    
    # Calibrate risk according to method
    df.s <- calibrate(train = df.s
              ,test = df.s
              ,method = method
              ,args = args)
    
    # stack results by s level
    if(i.s == 1){
      caldf <- df.s
    }else{
      caldf <- caldf %>%
        dplyr::bind_rows(df.s)
    }
  } # end loop over s
  
  # use k-fold cross-validation to get MSPE
  if(cv == TRUE){
    cat("Mean squared prediction error computed using",k,"-fold cross-validation \n")
    # loop over groups
    for(i.s in 1:ns){
      
      # Get group subset
      folddf.s <- df %>% filter(.data$s == slist[i.s])
      
      # randomly assign fold
      folddf.s$fold <- sample(1:k, size=nrow(folddf.s), replace=TRUE)
      
      # loop over folds
      for(f in 1:k){
        
        # Calibrate risk according to method
        fold.k <- calibrate(train = folddf.s %>% dplyr::filter(.data$fold != f)
                          ,test = folddf.s %>% dplyr::filter(.data$fold == f)
                          ,method = method
                          ,args = args)
        
        # stack results by s level
        if(f == 1){
          fold.s <- fold.k
        }else{
          fold.s <- fold.s %>%
            dplyr::bind_rows(fold.k)
        }
        
      } # end loop over fold
      # get MSPE for group s 
      MSPE <- fold.s %>%
                dplyr::mutate(diff.sq = (.data$y - .data$rs.gX)**2) %>%
                dplyr::summarize(MSPE = mean(.data$diff.sq)) %>%
                dplyr::pull(.data$MSPE)
      cat("The mean square prediction error in group ",slist[i.s],"is",MSPE," \n")   
    } # end loop over group
    
  }
 
  return(caldf)
}