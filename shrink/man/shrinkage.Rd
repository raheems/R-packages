\name{shrinkage}
\alias{shrinkage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal function
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
shrinkage(formula, formula2 = NULL, data = list(), method = "AIC", est = "ps", test = "F", ...)
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
function(formula,  formula2 = NULL, data=list(), method="AIC", est="ps", test="F", ... )
{
  call <- match.call()
  if (!is.data.frame(data)) data <- as.data.frame(data)
  mf <- model.frame(formula=formula, data=data)
  x <- model.matrix(attr(mf, "terms"), data=mf)
  y <- model.response(mf)
  M <-  lm(formula, data=data)
  if (!is.null(formula2)) ## Formula2: sub-model is providel
    mfmla <- formula2
  else  if (is.null(formula2)) { ## formula2 is not provided,  fit AIC or BIC
    if (method =="AIC") {                    
      maic <- stepAIC(M, trace=FALSE) ## gives sub-model
      mfmla <- as.formula(paste(paste(names(mf)[1], "~", sep=""),
                               paste(attr(terms(maic), "term.labels"), collapse="+")))
    }
    else if (method=="BIC") {
      mbic <- stepAIC(M, trace=FALSE, k=log(nrow(x)))
      mfmla <-as.formula(paste(paste(names(mf)[1], "~", sep=""),
                               paste(attr(terms(mbic), "term.labels"), collapse="+") ))
    }
  } else stop("Either formula or formula2 must be provided")
  m <- lm(mfmla, data)
  mcoef <- c(M$coef[c(match(names(m$coefficients),
                            names(M$coefficients)))]) ## res coef
  Mcoef <- c(M$coef[c(match(names(m$coeff), names(M$coeff)))],
             M$coef[-c(match(names(m$coeff), names(M$coeff)))]) ## ur coef
  rhs.formula <- as.formula(paste("~ ", paste(paste(names(Mcoef)[-1]),
                                              collapse= "+")))
  update(formula, rhs.formula)
  mf <- model.frame(formula = update(formula, rhs.formula), data=data)
  x <- model.matrix(attr(mf, "terms"), data=mf)
  p <- ncol(mf) - 1
  n <- nrow(mf)
  p2 <- p -(length(mcoef) - 1)
  if (!(p2 >= 2)) stop("No. restriction < 2")
  opt.c <- (p2 - 2)*(n - p) / (p2 * (n - p + 2)) ## Saleh (2006), p. 344
  if (test == "F") {
    tstat <- anova(m, M, test = "F")$F[2]
  }
  else if (test == "Chisq") {
    anova.test <- anova(m, M, test="Chisq")
    tstat <- qchisq(anova.test$P[2], anova.test$Df[2], lower.tail = F)
  }
  beta.ur <- Mcoef  
  beta.res <- c(mcoef, rep(0, p2))
  names(beta.res) <- c(names(beta.ur))  
  out <- list(call = call)
  if (any(est =="all" | est =="ur")) {
    out <- c(out, list(beta.ur = beta.ur,
                       y.ur = as.numeric(y),
                       fitted.ur = as.vector(x\%*\%beta.ur)))
  }
  if (any(est =="all" | est =="res")) {
    out <- c(out, list(beta.res = beta.res,
             y.res = as.numeric(y),
             fitted.res = as.vector(x\%*\%beta.res)))
  }
  if (any(est =="all" | est =="s")) {
    beta.s = beta.ur - (opt.c/tstat)*(beta.ur - beta.res)
    out <- c(out, list(beta.s = beta.s,
                       y.s = as.numeric(y),
                       fitted.s = as.vector(x\%*\%beta.s)))
  }
  if (any(est =="all" | est =="ps")) {
    beta.ps = beta.res + max(0, (1 - opt.c / tstat)) * (beta.ur - beta.res)
    out <- c(out, list(beta.ps = beta.ps,
                       y.ps = as.numeric(y),
                       fitted.ps = as.vector(x\%*\%beta.ps)))
  }
  out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
