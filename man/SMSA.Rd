\name{SMSA}
\alias{SMSA}
\docType{data}
\title{
Standard Metropolitan Statistical Areas
}
\description{
The properties of Standard Metropolitan Statistical Areas (a standard Census Bureau designation of the region around a city) in the United States, collected from a variety of sources. The data include information on the social and economic conditions in these areas, on their climate, and some indices of air pollution potentials. The dataset has 59 observations on 15 variables.
}
\usage{data("SMSA")}
\format{
  A data frame with 59 observations on the following 15 variables.
  \describe{
    \item{\code{JanTemp}}{Mean January temperature (in degrees Farenheit)}
    \item{\code{JulyTemp}}{Mean July temperature (in degrees Farenheit)}
    \item{\code{RelHum}}{Relative Humidity}
    \item{\code{Rain}}{Annual rainfall (in inches)}
    \item{\code{Mortality}}{Age adjusted mortality}
    \item{\code{Education}}{Median education}
    \item{\code{PopDensity}}{Population density}
    \item{\code{PerNonWhite}}{Percentage of non whites}
    \item{\code{PerWC}}{Percentage of white collour workers}
    \item{\code{pop}}{Population}
    \item{\code{popPerhouse}}{Population per household}
    \item{\code{income}}{Median income}
    \item{\code{HCPot}}{HC pollution potential}
    \item{\code{S02Pot}}{Sulfur Dioxide pollution potential}
    \item{\code{NOx}}{Nitrous Oxide (target variable)}
  }
}
\source{
https://www.openml.org/d/1091
}
\references{
U.S. Department of Labour Statistics Authorization: free use
}
\examples{
data(SMSA)
## maybe str(SMSA) ; plot(SMSA) ...
}
\keyword{SMSA}