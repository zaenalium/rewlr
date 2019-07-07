lr_tr_irs_fit <- function (y ,x, lambda, iter, tol) {
  B <- matrix(rep(0, ncol(x)))
  p <- c(); v <- c(); z <- c()
  dev <- 0
  dev1 <- 1
  cx <- 0
  n <- nrow(x)
  pq <- ncol(x)
  rnm <- colnames(x)

  result <- list()
  while(abs((dev - dev1) / dev1) > tol & cx <= iter)  {
    dev <- dev1
    for(i in 1:n) {
      p[i] <- 1/(1 + exp(-x[i, ] %*% B))
      v[i] <- p[i] * (1 - p[i])
      z_part <- (y[i] - p[i])/(p[i] * (1 - p[i]))
      z_part <- ifelse(is.nan(z_part), 0, ifelse(is.infinite(z_part) & z_part > 0, 99999,
                                                 ifelse(is.infinite(z_part) & z_part < 0, -99999,  z_part)))
      z[i] <- x[i, ] %*% B + z_part
    }

    V <- diag(c(v))

    #-----algorithm 2----
    A = t(x) %*% V %*% x +  lambda * diag(pq)
    b = t(x) %*% V %*% as.matrix(z)

    B <- cg_cpp(A, b, B)
    rownames(B) <- rnm

    ll_temp <- c()
    for(j in 1:length(y)) {
      ll_temp_1 <- y[j] * log(exp( c(x[j, ] %*% B)) /(1 + exp(c(x[j, ] %*% B)))) +
                  (1 - y[j]) *  log(exp( c(x[j, ] %*% B)) /(1 + exp(c(x[j, ] %*% B))))
      ll_temp[j] <-  ifelse(is.nan(ll_temp_1), 0,
                            ifelse(is.infinite(ll_temp_1) & ll_temp_1 > 0, 99999,
                            ifelse(is.infinite(ll_temp_1) & ll_temp_1 < 0, -99999,  ll_temp_1)))
    }

    LogL <- sum(ll_temp) - (lambda / 2) * (norm(B))^2
    dev1 <- - 2 * LogL
    cx <- cx + 1

  }
  null_model <- log(mean(y) / (1 - mean(y)))
  ll_temp_null <- c()
  for(j in 1:length(y)) {
    ll_temp_null[j] <- log(exp(y[j] * null_model) /
                                    (1 + exp(null_model)))
  }

  LogLNULL <- sum(ll_temp_null) #- lambda * (norm(B)^2) / 2

  cov_B <- solve(-(-t(x) %*% V %*% x))
  result$std_error <- (sqrt(diag(cov_B)))
  result$B  <- B
  result$fitted <- 1 / (1 + exp(-x %*% result$B))
  result$std_error <- (sqrt(diag(cov_B)))
  result$wald <- result$B/result$std_error
  result$wald_all <- t(result$B) %*% cov_B %*% result$B
  result$p <- 1 / (1 + exp(-x %*% result$B ))
  result$aic <- 2 * (pq - 1) - sum(ll_temp)
  result$null_dev <- 2 * (1 - LogLNULL)
  result$res_dev <- 2 * (1 - sum(ll_temp))
  result$df_null <- n - 1
  result$df_res <- n - (pq - 1)
  result$PseudoR2 <- 1 - (sum(ll_temp)  / LogLNULL)
  result$auc <- as.numeric(roc(y, result$fitted)$auc)
  return(result)
  }



