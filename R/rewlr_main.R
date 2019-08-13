#data <- readr::read_delim("/home/zaenal/Documents/UN OLAH ok.csv", delim = ";")
#data$Species <- ifelse(data$Species == "setosa",0,1)

#formula <- "y ~ ."
#weights0 =NULL
#weights1 = NULL
#na.action = NULL

#iter <- 1000
#tol <- 0.00001
#e1 <- 0.005
#iter1 <- 200
#w0 <- 0.02/0.01
#w1 <- (1-0.02)/(1-0.01)
#lambda <- NULL


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







