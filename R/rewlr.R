#' Fiting the Rare Event Weighted Logistic Regression
#'
#' @description rewlr is used to fitting the Rare Event Weighted Logistic Regression to handle the imbalanced or unbalanced response variabel in binary classification
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted
#' @param data  a dataframe or matrix (tibble is also supported)
#' @param weight0 (1 - proportion of events in the sample) devided by (1 - proportion of events in the population)
#' @param weight1 proportion of events in the sample devided by proportion of events in the population
#' @param tol positive convergence tolerance ε; the iterations converge when \emph{|dev - dev_old|/(|dev| + 0.1) < ε}'
#' @param iter an integer that giving maximum iteration for parameter estimation.
#' @param lambda a regularization (penalty) term to obtain better estimation. If the value is missing, lamda will be calculated by 1/sd(y)
#' @return rewlr returns output like glm, use function summary() to obtain the summary coefficients and others. The detail are shown in the following list:
#' \itemize{
#' \item coefficients - a named vector of coefficients.
#' \item fitted.values - return the prediction using the training data resulting probablity.
#' \item deviance - up to a constant, minus twice the maximized log-likelihood. Where sensible, the constant is chosen so that a saturated model has deviance zero.
#' \item AIC - A version of Akaike's An Information Criterion, minus twice the maximized log-likelihood plus twice the number of parameter.
#' \item null.deviance - The deviance for the null model, comparable with deviance. The null model will include the offset, and an intercept if there is one in the model.
#' \item df.residual - the residual degrees of freedom.
#' \item df.null - the residual degrees of freedom for the null model.
#' \item auc - an area under ROC curve
#' }
#' @references Maalouf M, Siddiqi M. (2014) emph{Weight logistic regression for large-scale imbalanced and rare events data}. emph{Knowledge-Based System}, strong{59}, 142-148.
#' @seealso \code{\link{summary.rewlr}} for summarises the model that has been built. Also use \code{\link{predict.rewlr}} to predict model to testing or new data.
#' @examples library(rewlr)
#' data(National_exam_id)
#' #data$Species <- ifelse(data$Species == "setosa",0,1)
#' #Supposed that current sample data has 9 percent of rare event data, and the population has 2 percent of those rare event data.
#' (weight0 = (1 - 0.09)/(1-0.02))
#' (weight1 = (0.09)/(0.02))
#' iter = 1000; tol = 0.00001
#'
#' fit <- rewlr(y~., data = National_exam_id, weights0 = weight0, weights1 = weight1)
#' summary(fit)
#' p <- predict(fit, newdata = National_exam_id)
#' @export rewlr
rewlr <- function(formula, data, weights0,
                  weights1, tol = 0.0001, iter = 1000, lambda = NULL) {
  if(any(is.na(data))) {
    stop("data contain missing value")
  }
  df <- model.frame(formula, data =data)
  y <- df[, 1]
  if(length(unique(y)) != 2) {
    stop("Please input binary response") }
   x <- as.matrix(cbind(Intercept = 1, df[, -1]))
  if(!(is.numeric(y))) {
    bn <- to_binary(y)
    base_char <- bn$base_char
    y <- bn$y
    rm(bn); invisible(gc())
  }

  if (is.null(lambda)) {
    lambda <- 1/sd(y) } else { lambda }

  p <-   rewlr_fit (y, x, lambda, weights0, weights1, iter, tol)

  class(p) <- "rewlr"

  p

  }







