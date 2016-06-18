\name{getARL}
\alias{getARL}
\title{compute average run length (ARL) for CUSUM charts}
\description{Compute average run lengths for CUSUM charts based on the Markov chain algorithm.}
\usage{
getARL(distr=NULL, K=NULL, H=NULL,
    Mean=NULL, std=NULL, prob=NULL, Var=NULL, mu=NULL, lambda=NULL, 
    samp.size=NULL, is.upward=NULL, winsrl=NULL, winsru=NULL)
}
\arguments{
  \item{distr}{Integer valued from 1 to 6: 1 refers to ``normal mean", 2 refers to ``normal variance", 3 refers to ``Poisson", 4 refers to ``binomial", 5 refers to ``negative binomial", and 6 refers to ``inverse Gaussian mean". }
  \item{K}{A reference value, which is given by \code{\link{getH}}. }
  \item{H}{A given decision interval, which is given by \code{\link{getH}}. }
  \item{Mean}{Mean value, which has to be provided when distr = 1 (normal mean), 3 (Poisson), and 5 (negative binomial). The value must be positive when distr = 3 or distr = 5. }
  \item{std}{Standard deviation, which has to be provided when distr = 1 (normal mean) and 2 (normal variance). The value must be positive. } 
  \item{prob}{Success probability, which has to be provided when distr = 4 (binomial); 0 < prob <= 1. } 
  \item{Var}{Variance, which has to be provided when distr = 5 (negative binomial). The value has to be larger than Mean when distr = 5. } 
  \item{mu}{A positive value representing the mean of inverse Gaussian distribution. The argument 'mu' has to be provided when distr = 6 (inverse Gaussian mean). } 
  \item{lambda}{A positive value representing the shape parameter for inverse Gaussian distribution. The argument 'lambda' has to be provided when distr = 6 (inverse Gaussian mean). } 
  \item{samp.size}{Sample size, an integer which has to be provided when distr = 2 (normal variance) or distr = 4 (binomial). } 
  \item{is.upward}{Logical value, whether to depict a upward or downward CUSUM. }
  \item{winsrl}{Lower Winsorizing constant. Use NULL or -999 if Winsorization is not needed. } 
  \item{winsru}{Upper Winsorizing constant. Use NULL or 999 if Winsorization is not needed. } 
}

\details{Computes ARL when the reference value and decision interval are given. For each case, the necessary parameters are listed as follows. \cr
\cr
Normal mean (distr = 1): \code{Mean}, \code{std}, \code{K}, \code{H}. \cr
Normal variance (distr = 2): \code{samp.size}, \code{std}, \code{K}, \code{H}. \cr
Poisson (distr = 3): \code{Mean}, \code{K}, \code{H}. \cr
Binomial (dist = 4): \code{samp.size}, \code{prob}, \code{K}, \code{H}. \cr
Negative binomial (distr = 5): \code{Mean}, \code{Var}, \code{K}, \code{H}. \cr
Inverse Gaussian mean (distr = 6): \code{mu}, \code{lambda}, \code{K}, \code{H}.
}

\value{
A list including three variables:
  \item{ARL_Z}{The computed zero-start average run length for CUSUM.}
  \item{ARL_F}{The computed fast-initial-response (FIR) average run length for CUSUM.}
  \item{ARL_S}{The computed steady-state average run length for CUSUM.}
}

\author{Douglas M. Hawkins, David H. Olwell, and Boxiang Wang\cr
Maintainer: Boxiang Wang  \email{boxiang@umn.edu}}
\references{
Hawkins, D. M. and Olwell, D. H. (1998)
``Cumulative Sum Charts and Charting for Quality Improvement (Information Science and Statistics)", Springer, New York. \cr
}

\seealso{\code{\link{getH}}}
\examples{
# normal mean
getARL(distr=1, K=11, H=5, Mean=10, std=2)

# normal variance
getARL(distr=2, K=3, H=1, std=2, samp.size=5, is.upward=TRUE)

# Poission
getARL(distr=3, K=3, H=1, std=2, Mean=5, is.upward=TRUE)

# Binomial
getARL(distr=4, K=0.8, H=1, prob=0.2, samp.size=100, is.upward=TRUE)

# Negative binomial
getARL(distr=5, K=3, H=6, Mean=2, Var=5, is.upward=TRUE)

# Inverse Gaussian mean
getARL(distr=6, K=2, H=4, mu=3, lambda=0.5, is.upward=TRUE)
}
\keyword{ARL}
\keyword{CUSUM}
\keyword{Quality control}