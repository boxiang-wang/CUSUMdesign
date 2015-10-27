\name{CUSUMchart}
\alias{CUSUMchart}
\title{plot CUSUM charts}
\description{plot CUSUM charts using given reference values (K) and decision interval (H). }
\usage{
CUSUMchart(summand, kup, kdn, hup, hdn, nameof, ylab)
}
\arguments{
  \item{summand}{Summand in CUSUM charts. }
  \item{kup}{Reference value for the upward CUSUM. }
  \item{kdn}{Reference value for the downward CUSUM. }
  \item{hup}{Decision interval for the upward CUSUM. }
  \item{hdn}{Decision interval for the downward CUSUM. }
  \item{nameof}{An overall title. }
  \item{ylab}{A title for the y-axis. }
}

\details{Utility to draw cusum charts.}

\author{Douglas M. Hawkins}

\examples{
dat = c(10, 8, 11, 8, 8, 7, 9, 14, 9, 14, 13, 9, 13, 11, 14, 12, 12)
CUSUMchart(dat, kup=2.5, kdn=-2.5, hup=3.5, hdn=-3.5, 
  nameof="CUSUM chart", ylab="weight")
}

\keyword{CUSUM}
\keyword{Quality control}