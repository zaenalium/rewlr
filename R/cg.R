cg <- function(A, b, B) {
  ci = 0
  r <- b - A %*% B
  d <- r

  while ((norm(r, "2"))^2 > 0.0005 & ci > 200 ) {
    s <- c((t(r) %*% r) / (t(d) %*% A %*% d))
    B <- B +  s * d
    r1 <- r - s * A %*% d
    zeta  <- c((t(r1) %*% r1) / (t(r) %*% r)) #1
    d1 <- r1 + (zeta * d)
    r <- r1
    d <- d1
    ci <- ci + 1
  }
  return(B)
}
