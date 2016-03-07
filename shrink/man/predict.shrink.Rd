\name{predict.shrink}
\alias{predict.shrink}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
predict method for object of class 'shrink'
}
\description{
Provides predicted values from the fitted model. If \code{newdata}
 is not supplied, 'fitted' values will be returned.
}
\usage{
predict.shrink(object, newdata = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object of class 'shrink', usually output of \code{shrink()}
}
  \item{newdata}{
New data for which predicted values are required. \code{newdata} must
 have the same number of columns as the original data.
}
  \item{\dots}{
Other arguments that can be passed to \code{predict}
}
}
\details{
TBA
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
TBA
}
\author{
TBA
}
\note{
TBA
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{predict.lm}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
