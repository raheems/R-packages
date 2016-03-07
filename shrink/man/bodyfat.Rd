\name{bodyfat}
\alias{bodyfat}
\docType{data}
\title{
Bodyfat data
}
\description{
Lists estimates of the percentage of body fat determined by underwater
 weighing and various body circumference measurements for 252 men.
}
\usage{data(bodyfat)}
\format{
  A data frame with 252 observations on the following 15 variables.
  \describe{
    \item{\code{density}}{a numeric vector}
    \item{\code{bodyfat}}{a numeric vector}
    \item{\code{age}}{a numeric vector}
    \item{\code{weight}}{a numeric vector}
    \item{\code{height}}{a numeric vector}
    \item{\code{neck}}{a numeric vector}
    \item{\code{chest}}{a numeric vector}
    \item{\code{abdomen}}{a numeric vector}
    \item{\code{hip}}{a numeric vector}
    \item{\code{thigh}}{a numeric vector}
    \item{\code{knee}}{a numeric vector}
    \item{\code{ankle}}{a numeric vector}
    \item{\code{biceps}}{a numeric vector}
    \item{\code{forearm}}{a numeric vector}
    \item{\code{wrist}}{a numeric vector}
  }
}
\details{
A variety of popular health books suggest that the readers assess their
 health, at least in part, by estimating their percentage of body fat. In
 Bailey (1994), for instance, the reader can estimate body fat from tables
 using their age and various skin-fold measurements obtained by using a
 caliper. Other texts give predictive equations for body fat using body 
 circumference measurements (e.g. abdominal circumference) and/or skin-fold
 measurements. See, for instance, Behnke and Wilmore (1974), pp. 66-67;
 Wilmore (1976), p. 247; or Katch and McArdle (1977), pp. 120-132).

Percentage of body fat for an individual can be estimated once body density
 has been determined. Folks (e.g. Siri (1956)) assume that the body consists
 of two components - lean body tissue and fat tissue. Letting

D = Body Density (gm/cm^3)
A = proportion of lean body tissue
B = proportion of fat tissue (A+B=1)
a = density of lean body tissue (gm/cm^3)
b = density of fat tissue (gm/cm^3)

we have

D = 1/[(A/a) + (B/b)]

solving for B we find

B = (1/D)*[ab/(a-b)] - [b/(a-b)].

Using the estimates a=1.10 gm/cm^3 and b=0.90 gm/cm^3 (see Katch and McArdle
 (1977), p. 111 or Wilmore (1976), p. 123) we come up with "Siri's equation":

Percentage of Body Fat (i.e. 100*B) = 495/D - 450.

Volume, and hence body density, can be accurately measured a variety of ways.
 The technique of underwater weighing "computes body volume as the difference
 between body weight measured in air and weight measured during water
 submersion. In other words, body volume is equal to the loss of weight in
 water with the appropriate temperature correction for the water's density"
 (Katch and McArdle (1977), p. 113). Using this technique,

Body Density = WA/[(WA-WW)/c.f. - LV]

where

WA = Weight in air (kg)
WW = Weight in water (kg)
c.f. = Water correction factor (=1 at 39.2 deg F as one-gram of water
       occupies exactly one cm^3 at this temperature, =.997 at 76-78 deg F)
LV = Residual Lung Volume (liters)

(Katch and McArdle (1977), p. 115). Other methods of determining body volume
 are given in Behnke and Wilmore (1974), p. 22 ff.

}
\source{
http://lib.stat.cmu.edu/datasets/bodyfat
More TBA
}
\references{
%%  ~~ possibly secondary sources and usages ~~
TBA
}
\examples{
data(bodyfat)
## maybe str(bodyfat) ; plot(bodyfat) ...
}
\keyword{datasets}
