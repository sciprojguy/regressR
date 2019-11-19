\name{bike_traffic}
\alias{bike_traffic}
\docType{data}
\title{
Bike Traffic Data
}
\description{
Bike Traffic Data
}
\usage{data("bike_traffic")}
\format{
  A data frame with 515688 observations on the following 6 variables.
  \describe{
    \item{\code{date}}{a POSIXct}
    \item{\code{crossing}}{a factor with levels \code{39th Ave NE Greenway at NE 62nd St} \code{Broadway Cycle Track North Of E Union St} \code{Burke Gilman Trail} \code{Elliot Bay Trail} \code{MTS Trail} \code{NW 58th St Greenway at 22nd Ave} \code{Sealth Trail}}
    \item{\code{direction}}{a character vector}
    \item{\code{bike_count}}{a numeric vector}
    \item{\code{ped_count}}{a numeric vector}
    \item{\code{directon}}{a factor with levels \code{East} \code{North} \code{South} \code{West}}
  }
}
\details{
Tidy Tuesday, 4/2/19
}
\source{
https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-04-02
}
\references{
https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-04-02
}
\examples{
data(bike_traffic)
## maybe str(bike_traffic) ; plot(bike_traffic) ...
}
\keyword{datasets}
