#' pred_r_squared evaluates R sq (Pred)
#' 
#' @param linear.model is a lm 
#' @return pred.r.squared is a numeric variable 
#' @export 

pred_r_squared <- function(linear.model) {
  lm.anova <- anova(linear.model)
  tss <- sum(lm.anova$'Sum Sq')
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}

#' model_fit_stats
#' @param linear.model
#' @return return.df
#' @export 

model_fit_stats <- function(linear.model) {
  r.sqr <- summary(linear.model)$r.squared
  adj.r.sqr <- summary(linear.model)$adj.r.squared
  pre.r.sqr <- pred_r_squared(linear.model)
  PRESS <- PRESS(linear.model)
  return.df <- data.frame(r.squared = r.sqr, adj.r.squared = adj.r.sqr, pred.r.squared = pre.r.sqr, press = PRESS)
  return(return.df)
}

#' PRESS
#' 
#' @param linear.model
#' @return PRESS
#' @export 
#' 

PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}