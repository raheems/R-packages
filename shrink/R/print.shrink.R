print.shrink <-
function(x, ...) {
  cat("Call: \n")
  print(x$call)
  cat("\n")
  cat("Full Model: \n")
  print(x$formula)
  cat("\n")
  if (!is.null(x$formula2)) {
    cat("Sub-Model: \n")
    print(x$formula2)
    cat("\n")
  }
  cat("Estimator: \n")
  print(x$est)
  cat("\n")
  cat("Coefficients:\n")
  print(x$coefficients)
}

