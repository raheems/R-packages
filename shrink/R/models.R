models <-
function(data, method ="AIC", ...)
{
  ## require(MASS)
  ## The first column of the matrix or data frame must contain the response variable
  x <- data
  sub.model <- "No sub-model returned"
  n <- nrow(x)
  if (!is.data.frame(x)) x <- as.data.frame(x)
  ##  if((class(data) == "data.frame") & !is.null(names(data)))
  M <-  as.formula(paste(paste(names(x)[1], "~", sep=""),
                         paste(names(x)[2 : ncol(x)], collapse = "+")))
  if (method == "BIC") {
    find.model <- stepAIC(model.lm <- lm(M, data = x), trace = FALSE, k = log(n))
    if (length(find.model[1]$coefficients) > 1) {
      sub.model <- as.formula(paste(paste(names(x)[1], "~", sep = ""), paste(attr(terms(find.model), "term.labels"),  collapse = "+")))
    }
    if (length(find.model[1]$coefficients) == 1) {
      sub.model <- "No sub-model returned"
      find.model <- stepAIC(model.lm <- lm(M, data = x), trace = FALSE)
      if (length(find.model[1]$coefficients) > 1) {
        sub.model <- as.formula(paste(paste(names(x)[1], "~", sep = ""), paste(attr(terms(find.model), "term.labels"),  collapse = "+")))
      }
      if (length(find.model[1]$coefficients) == 1)
        warning ("BIC, AIC selection failed")
    }
  }
  else  if (method == "AIC") {
    find.model <- stepAIC(model.lm <- lm(M, data = x), trace = FALSE)
    if (length(find.model[1]$coefficients) > 1) {
      sub.model <- as.formula(paste(paste(names(x)[1], "~", sep = ""), paste(attr(terms(find.model), "term.labels"),  collapse = "+")))
    }
    else if (length(find.model[1]$coefficients) == 1)
      warning ("AIC selection failed")
  }
  list (call = match.call(), method = method, full = M, sub = sub.model)
}

