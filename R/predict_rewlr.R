#' predicting the rare event weighted logistic regreression
#'
#' @description preditct method for class "rewlr".
#' @param model an object of class "rewlr"
#' @param newdata a newdata to be predicted
#' @return return the predicted by the model in probablity form.
#' @export predict.rewlr

predict.rewlr <- function(model, newdata = NULL){
  if(is.null(newdata)) newdata = model$x
  pr <- 1/(1 + exp(-as.matrix(newdata) %*% model$B))
  return(pr)
}
