#' @export print.rewlr


print.rewlr<- function(model, digits = max(3L, getOption("digits") - 3L), ...){
  B <- model$B
  nm <- row.names(B)
  out <- c(t(model$B))
  names(out) <- nm
  cat("Coefficients:\n")
  print.default(format(out, digits = digits), print.gap = 2L,
                quote = FALSE)
  cat("\nDegrees of Freedom:", model$df_null, "Total (i.e. Null); ",
      model$df_res, "Residual\n")
  #if (nzchar(mess <- naprint(x$na.action)))
  #  cat("  (", mess, ")\n", sep = "")
  cat("Null Deviance:\t   ", format(signif(model$null_dev,
    digits)), "\nResidual Deviance:", format(signif(model$res_dev,
    digits)), "\tAIC:", format(signif(model$aic, digits)))
  cat("\n")
  invisible(model)

}


