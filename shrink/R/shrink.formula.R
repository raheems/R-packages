shrink.formula <-
function(formula, formula2 = NULL, data=list(), 
                           method = "AIC", est = "ps", test = "F", ... ) {
  est0 <- shrinkage(formula = formula, data = data, formula2 = formula2,
                    method = method, est = est, test = test, ... )
  if (!is.null(formula2))
    out <- list(call = match.call(), estimate = est, formula=formula, formula2 = formula2)    
  else
    out <- list(call = match.call(), estimate = est, formula = formula)
  if (est == "ur") {
    out <- c(out, list(coefficients = est0$beta.ur,
                       fitted.values = est0$fitted.ur,
                       y = est0$y.ur,
                       residuals = est0$y.ur - est0$fitted.ur))
  }
  else if (est == "res") {
    out <- c(out, list(coefficients = est0$beta.res,
                       fitted.values = est0$fitted.res,
                       y = est0$y.res,
                       residuals = est0$y.res - est0$fitted.res))
  }
  else if (est == "s") {
    out <- c(out, list(coefficients= est0$beta.s,
                       fitted.values=est0$fitted.s,
                       y=est0$y.s,
                       residuals=est0$y.s-est0$fitted.s))
  }
  else if (est =="ps") {
    out <- c(out, list(coefficients =  est0$beta.ps,
                       fitted.values = est0$fitted.ps,
                       y = est0$y.ps,
                       residuals = est0$y.ps - est0$fitted.ps))
  }
  class(out) <- "shrink"
  out
}

