\name{bike_share}
\alias{bike_share}
\docType{data}
\title{
Bike Sharing data
}
\description{
Bike Sharing data
}
\usage{data("bike_share")}
\format{
  A data frame with 17379 observations on the following 13 variables.
  \describe{
    \item{\code{date}}{a Date}
    \item{\code{season}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}
    \item{\code{hour}}{a numeric vector}
    \item{\code{holiday}}{a numeric vector}
    \item{\code{day_of_week}}{a numeric vector}
    \item{\code{working_day}}{a numeric vector}
    \item{\code{weather_type}}{a numeric vector}
    \item{\code{temp_F}}{a numeric vector}
    \item{\code{temp_feels_F}}{a numeric vector}
    \item{\code{wind_speed}}{a numeric vector}
    \item{\code{casual_users}}{a numeric vector}
    \item{\code{registerd_users}}{a numeric vector}
    \item{\code{total_users}}{a numeric vector}
  }
}
\details{
Bike Sharing data
}
\source{
https://data.world/data-society/capital-bikeshare-2011-2012
}
\references{
https://data.world/data-society/capital-bikeshare-2011-2012
}
\examples{
data(bike_share)
## maybe str(bike_share) ; plot(bike_share) ...
}
\keyword{datasets}
