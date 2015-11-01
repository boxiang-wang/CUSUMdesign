\name{getH}
\alias{getH}
\title{compute decision interval (H) for CUSUM charts}
\description{Compute decision intervals for CUSUM charts.}
\usage{
getH(distr=NULL, ARL=NULL, ICmean=NULL, ICsd=NULL, 
    OOCmean=NULL, OOCsd=NULL, ICprob=NULL, OOCprob=NULL, 
    ICvar=NULL, IClambda=NULL, samp.size=NULL, 
    ref=NULL, winsrl=NULL, winsru=NULL, 
    type=c("zero start", "fast initial response", "steady state"))
}
\arguments{
  \item{distr}{Integer valued from 1 to 6: 1 refers to ``normal mean", 2 refers to ``normal variance", 3 refers to ``Poisson", 4 refers to ``binomial", 5 refers to ``negative binomial", 6 refers to ``inverse Gaussian mean". }
  \item{ARL}{An integer for in control average run length.}
  \item{ICmean}{In-control mean, which has to be provided when distr = 1 (normal mean), 3 (Poisson), 5 (negative binomial), and 6 (inverse Gaussian mean). The value has to be positive when distr = 3, distr = 5, or distr = 6. }
  \item{ICsd}{In-control standard deviation, which has to be provided when distr = 1 (normal mean) and 2 (normal variance). The value has to be positive. } 
  \item{OOCmean}{Out-of-control mean, which has to be provided when distr = 1 (normal mean), 3 (Poisson), 5 (negative binomial), and 6 (Inverse Gaussian mean). When distr = 3, 5, or 6, the value has to be positive. }
  \item{OOCsd}{Out-of-control standard deviation, which has to be provided when distr = 2 (normal variance). The value has to be positive. } 
  \item{ICprob}{In-control success probability, which has to be provided when distr = 4 (binomial); 0 < prob <= 1. } 
  \item{OOCprob}{Out-of-control success probability, which has to be provided when distr = 4 (binomial); 0 < prob <= 1. }
  \item{ICvar}{In-control variance, which has to be provided when distr = 5 (negative binomial). The value has to be larger than the in-control mean 'ICmean'. } 
  \item{IClambda}{In-control shape parameter for inverse Gaussian distribution. The argument 'IClambda' has to be provided when distr = 6 (inverse Gaussian mean). } 
  \item{samp.size}{Sample size, an integer which has to be provided when distr = 2 (normal variance) or distr = 4 (binomial). } 
  \item{ref}{Optional reference value. }
  \item{winsrl}{Lower Winsorizing constant. Use NULL or -999 if Winsorization is not needed. } 
  \item{winsru}{Upper Winsorizing constant. Use NULL or 999 if Winsorization is not needed. } 
  \item{type}{A string for CUSUM type: "Z" for zero-start CUSUM, "F" for fast-initial-response CUSUM, and "S" for steady-state CUSUM. Default is "F". }
}


\details{Computes the decision interval H when the reference value and the average run length are given. For each case, the necessary parameters are listed as follows. \cr
\cr
Normal mean (distr = 1): \code{ICmean}, \code{ICsd}, \code{OOCmean}. \cr
Normal variance (distr = 2): \code{samp.size}, \code{ICsd}, \code{OOCsd} \cr
Poisson (distr = 3): \code{ICmean}, \code{OOCmean}. \cr
Binomial (dist = 4): \code{samp.size}, \code{ICprob}, \code{OOCprob}. \cr
Negative binomial (distr = 5): \code{ICmean}, \code{Icvar}, \code{OOCmean}. \cr
Inverse Gaussian mean (distr = 6): \code{ICmean}, \code{IClambda}, \code{OOCmean.}
}

\value{
A list including three variables:
  \item{DI}{Decision interval.}
  \item{IC_ARL}{In-control average run length.}
  \item{OOCARL_Z}{Out-of-control average run length for the zero-start CUSUM.}
  \item{OOCARL_F}{Out-of-control  average run length for the fast-initial-response (FIR) CUSUM.}
  \item{OOCARL_S}{Out-of-control  average run length for the steady-state CUSUM.}
}


\author{Douglas M. Hawkins, David H. Olwell, and Boxiang Wang\cr
Maintainer: Boxiang Wang  \email{boxiang@umn.edu}}
\references{
Hawkins, D. M. and Olwell, D. H. (1998)
``Cumulative Sum Charts and Charting for Quality Improvement (Information Science and Statistics)", Springer, New York. \cr
}

\seealso{\code{\link{getARL}}}
\examples{
# normal mean
getH(distr=1, ICmean=10, ICsd=2, OOCmean=15, ARL=1000, type="F")

# normal variance
getH(distr=2, ICsd=2, OOCsd=4, samp.size=5, ARL=1000, type="F")

# Poission
getH(distr=3, ICmean=2, OOCmean=3, ARL=100, type="F")

# Binomial
getH(distr=4, ICprob=0.2, OOCprob=0.6, samp.size=100, ARL=1000, type="F")

# Negative binomial
getH(distr=5, ICmean=1, ICvar=3, OOCmean=2, ARL=100, type="F")

# Inverse Gaussian mean
getH(distr=6, ICmean=1, IClambda=0.5, OOCmean=2, ARL=1000, type="F")
}

\keyword{Decision interval}
\keyword{CUSUM}
\keyword{Quality control}