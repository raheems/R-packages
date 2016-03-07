predict.shrink <-
function(object, newdata = NULL, ...) {
  if(is.null(newdata))
    y <- fitted(object)
  else {
    if (!is.null(object$formula)) {
      ## Model fitted via shrink.formula() interface
      x <- model.matrix(object$formula, newdata)
    }
    else {
      x <- newdata
    }
    y <- as.vector(x%*%coef(object))
  }
  y
}

