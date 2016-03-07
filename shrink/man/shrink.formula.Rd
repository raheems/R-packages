\name{shrink.formula}
\alias{shrink.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fits linear models
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
shrink.formula(formula, formula2 = NULL, data = list(), method = "AIC", est = "ps", test = "F", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{formula2}{
%%     ~~Describe \code{formula2} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{est}{
%%     ~~Describe \code{est} here~~
}
  \item{test}{
%%     ~~Describe \code{test} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
