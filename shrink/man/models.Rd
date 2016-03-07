\name{models}
\alias{models}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Provides full- and sub-model for 'shrink'
}
\description{
Automatically selects sub-model using \code{"AIC"} or \code{"BIC"} criterion. Default is \code{"AIC"}. A lasso and adaptive lasso-based sub-model selection
 will be added in next release.
}
\usage{
models(data, method = "AIC", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A matrix or data frame whose first column is the response for the linear
 regression fit
}
  \item{method}{
Method for stepwise variable selection, either \code{"AIC"} or \code{"BIC"}. Default is \code{"AIC"}
}
  \item{\dots}{
Other arguments that can be passed to \code{models}
}
}
\details{
TBA
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{full }{The unrestricted model or the full model. If no model 
 is specified in \code{formula=} then the first column of \code{data}
 is the response and all other columns are considered regressors }
  \item{sub }{Model selected by criterion as set in \code{method}.
 Also known as the sub-model or restricted model }
}
\references{
First_reference_(2010) TBA

Second_reference_(2009) Title, journal, page. 

}
%%\author{
%%Enayetur Raheem, with suggestions from S. Ejaz
%%}
%%\note{
%%%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{stepAIC}}, \code{\link{step}}
}
%%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%%}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
