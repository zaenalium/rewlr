#rewlr_fit <- function (y ,x, lambda, w0, w1, iter, tol) {
#  B <- matrix(rep(0, ncol(x)))
#  p <- c(); v <- c(); w <- c(); z <- c()
#  dev <- 0
#  dev1 <- 1
#  cx <- 0
#  n <- nrow(x)
#  pq <- ncol(x)
#  rnm <- colnames(x)
#  result <- list()
#  while(abs((dev - dev1) / dev1) > tol & cx <= iter)  {
#    dev <- dev1
#    for(i in 1:n) {
#      p[i] <- 1/(1 + exp(-x[i, ] %*% B))
#      v[i] <- p[i] * (1 - p[i])
#      w[i] <- w1 * y[i] + w0 * (1 - y[i])
#      z_part <- (y[i] - p[i])/(p[i] * (1 - p[i]))
#      z_part <- ifelse(is.nan(z_part), 0, ifelse(is.infinite(z_part) & z_part > 0, 99999,
#                       ifelse(is.infinite(z_part) & z_part < 0, -99999,  z_part)))
#      z[i] <- x[i, ] %*% B + z_part
#    }
#    D <- diag(c(w * p))
#    Q <- x %*% solve(t(x) %*% D %*% x +  lambda * diag(pq)) %*% t(x)
#    Qii <- diag(Q)
#
#    Xik <- c()
#    for(k in 1:n){
#      Xik[k] <- 1/2 * Qii[i] * ((1 + w1) * p[k] - w1)
#    }
#
#    #-----Rewl_algorithm 2----
#    A = t(x) %*% D %*% x +  lambda * diag(pq)
#    b = t(x) %*% D %*% as.matrix(z)
#
#
#    B <- cg_cpp(A, b, B)
#
#    #-----Rewl_algorithm 3----
#    A1 = t(x) %*% D %*% x +  lambda * diag(pq)
#    b1 = t(x) %*% (D %*% Xik)
#
#    B_bias_x <- matrix(runif(pq))
#    B_bias <- cg_cpp(A1, b1, B_bias_x)
#
#    ll_temp <- c()
#    for(j in 1:length(y)) {
#      ll_temp_1 <- w[j] * log(exp(y[j] * c(x[j, ] %*% B)) /(1 + exp(c(x[j, ] %*% B))))
#      ll_temp[j] <-  ifelse(is.nan(ll_temp_1), 0, ifelse(is.infinite(ll_temp_1) & ll_temp_1 > 0, 99999,
#                                                   ifelse(is.infinite(ll_temp_1) & ll_temp_1 < 0, -99999,  ll_temp_1)))
#    }
#
#    LogL <- sum(ll_temp) - lambda * (norm(B)^2) / 2
#    dev1 <- - 2 * LogL
#    cx <- cx + 1
#
#  }
#  ll_temp_null <- c()
#  null_model <- log(mean(y) / (1 - mean(y)))
#  for(j in 1:length(y)) {
#    ll_temp_null[j] <- w[j] * log(exp(y[j] * null_model) /
#                                 (1 + exp(null_model)))
#  }
#
#  LogLNULL <- sum(ll_temp_null) #- lambda * (norm(B)^2) / 2
#  result$B  <- B - B_bias
#  rownames(result$B) <- rnm
#  cov_B <- solve(-(-t(x) %*% D %*% x - lambda * diag(pq)))
#  result$x <- x
#  result$y <- y
#  result$fitted <- 1 / (1 + exp(-x %*% result$B))
#  result$std_error <- (sqrt(diag(cov_B)))
#  result$wald <- result$B/result$std_error
#  result$wald_all <- t(B) %*% cov_B %*% B
# # result$p <- 1 / (1 + exp(-x %*% result$B ))
#  result$aic <- 2 * (pq - 1) - sum(ll_temp)
#  result$null_dev <- 2 * (1 - LogLNULL)
#  result$res_dev <- 2 * (1 - sum(ll_temp))
#  result$df_null <- n - 1
#  result$df_res <- n - (pq - 1)
#  result$PseudoR2 <- 1 - (sum(ll_temp)  / LogLNULL)
#  result$auc <- as.numeric(roc(y, result$fitted)$auc)
#  return(result)
#  }
