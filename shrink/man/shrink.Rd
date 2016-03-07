\name{shrink}
\alias{shrink}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
James-Stein shrinkage estimation in linear regression
}
\description{
Fits linear regression and computes unrestricted (ur), 
 restricted (res), shrinkage (s), and positive shrinkage (ps) 
 estimates of slope parameters
}
\usage{
shrink(formula, formula2 = NULL, data = list(), method = "AIC", 
 est = "ps", test = "F", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
    A full model formula in the form of \code{y ~ x1+x2}, 
 returns an error if not supplied. 
}
  \item{formula2}{
A sub-model formula in the form of \code{y ~ x1+x2}. Can be omitted; 
 in that case variables will be selected automatically using AIC criterion
}
  \item{data}{
A data frame or matrix containing the response and the covariates. 
 The first column of the matrix or data frame will be treated as 
 response for the regression model
}
  \item{method}{
Sub-model selection criterion, either "AIC" or "BIC". Default is "AIC"
}
  \item{est}{
Estimators to be calculated, one of \code{"ur"}, \code{"res"}, 
 \code{"s"}, \code{"ps"}. Default is \code{"ps"} 
}
  \item{test}{
Test statistic; either \code{"Chisq"} or \code{"F"}; default is \code{"F"}
}
  \item{\dots}{
Other arguments that can be passed to \code{shrink}
}
}
\details{
If unrestricted estimates are required, use \code{est="ur"}, 
 use \code{est="res"} for restricted estimates, 
 \code{est="s"} for shrinkage, and \code{est="ps"} 
 for positive-shrinkage estimates. Default is \code{est="ps"}
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
TBA
}
\author{
Enayetur Raheem, with suggestions from S. Ejaz Ahmed
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(bodyfat)
body <- bodyfat[-1] ## Remove the variable "density"
ss.ur <- shrink(bodyfat~., data=body, est="ur")
ss.ps <- shrink(bodyfat~., data=body, est="ps")
par(mfrow=c(2,2))
plot(ss.ur, which=1, main="UR")
plot(ss.ur, which=2)
plot(ss.ps, which=1, main="PS")
plot(ss.ps, which=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
