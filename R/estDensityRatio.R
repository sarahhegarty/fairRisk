#' Estimate density ratio
#'
#' Calculate the density ratio of the re-calibrated risk between each group and reference group.
#'
#' @param train a data.frame containing the risk score, observed response and group label
#' @param test  data.frame containing the re-calibrated risk score, and group label
#' @param method method to be used to calibrate the risks, options: 'logit' for logistic regression
#' @param args optional odditional arguments to be used for the method. e.g., when method = 'logit', the degree of the polynomial can be specified
#' @param groupvar group variable
#' @param refgp reference group level
#' @param calrisk re-calibrated risk score
#' @param transform use logit of calibrated risk (TRUE) or calibrated risk on probability scale (FALSE), default
#' @param quietly logical for suppression of output messages, default = TRUE
#'
#' @return a data.frame with the estimated density ratio weights
#' 
#' @import dplyr
#' @importFrom stats glm predict poly
#'
#' @export


estDensityRatio <- function(train
                            ,test
                            ,method = 'logit'
                            ,args = list(1)
                            ,groupvar = .data$s
                            ,refgp = '1'
                            ,calrisk = .data$rs.gX
                            ,transform=FALSE
                            ,quietly = TRUE
                            ){
  
  if(quietly != TRUE){
    cat("Estimating density ratio using reference label",deparse(substitute(groupvar)),"=",refgp,"\n")
  }
  
  # make group zero-one 
  traindf <- {{train}} %>%
        dplyr::mutate(refind = if_else({{groupvar}} == refgp, 1, 0)
               ,rs.gX = {{calrisk}}
               ,lp = logit(.data$rs.gX))
 
  testdf <- {{test}} %>%
        dplyr::mutate(refind = if_else({{groupvar}} == refgp, 1, 0)
               ,rs.gX = {{calrisk}}
               ,lp = logit(.data$rs.gX))
  
  # Determine non-ref group levels 
  slist <-  {{train}} %>%
    dplyr::filter({{groupvar}} != refgp) %>%
    dplyr::arrange({{groupvar}}) %>%
    dplyr::pull({{groupvar}}) %>%
    unique()
 
    ns = length(slist)
  
  train.ref <- traindf %>% filter(.data$refind == 1)
  n_ref <- nrow(train.ref)
  
  # ----------------  set reference group ------------------- #
  outdf <- testdf %>%
              dplyr::filter(.data$refind == 1) %>%
              dplyr::mutate(w_s = 1)
  
  # ------------ loop over non-reference groups ------------- #
  for(i.s in 1:ns){
  
    train.s <- traindf %>% dplyr::filter({{groupvar}} == slist[i.s]) 
    
    # get non-ref group counts
    n_s <- train.s %>% nrow()
    
    # stack train set for non ref and ref group       
    train.s <- train.ref %>%
                  dplyr::bind_rows(train.s)
    
    # test set for non-ref group only
    test.s <- testdf %>% dplyr::filter({{groupvar}} == slist[i.s]) 
    
    # ------------ estimate density ratio ------------- #
    if(method == 'logit'){
      deg = args[[1]]
      if(transform == FALSE){
      dr.fit <- stats::glm(refind ~ poly(rs.gX ,deg), data = train.s, family = 'binomial') 
      }else{
        dr.fit <- stats::glm(refind ~ poly(lp ,deg), data = train.s, family = 'binomial') 
      }
      ps_ref <- stats::predict(dr.fit, newdata = test.s, type='response')
      test.s$w_s <- ps_ref/(1-ps_ref)*n_s/n_ref
      
    }else if(method == 'none'){
      test.s$w_s <- 1
    }else(print("specified density ratio estimation method is not recognized"))
    
    # append current group 
    outdf <- outdf %>%  dplyr::bind_rows(test.s)
  }
  
  return(outdf)
}