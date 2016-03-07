plot.shrink <- function(x, which = c(1L : 2L), main = "", sub = "", ...) {
  if (!inherits(x, "shrink"))
    stop("use only with \"shrink\" objects")
  show <- rep(FALSE, 2)
  show[which] <- TRUE
  if (show[1L]) {
    l.x <- "Fitted Values"
    l.y <- "Residuals"
  }
    if (show[2L]) {
    l.x <- "QQ"
    l.y <- ""
  }
  if (show[1L]) {
    x.val <- x$fitted
    y.val <- x$residuals
    plot(x.val, y.val, xlab = l.x, ylab = l.y, main = main)
  }
  if (show[2L]) {
    qqnorm(x$residuals)
    qqline(x$residuals)
  }
}
