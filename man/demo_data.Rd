% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demo_data.R
\docType{data}
\name{demo_data}
\alias{demo_data}
\title{Demographics data}
\format{
A tibble with 8 variables:
\describe{
\item{geo}{nuts code of the administrative region. Values are available at different NUTS levels, up to NUTS3}
\item{geo_label}{Label of the administrative region considered}
\item{age}{Age group. Groups considered range from 20-24 years to 60-64 years}
\item{pop}{Population, both sexes, number}
\item{pop_share}{Share of the population of each age group on total population for each area considered}
\item{deaths}{Number of deaths, number}
\item{death_rate}{Death rate calculated dividing annual deaths by annual average population}
\item{life_exp}{Life expectancy for each agro group and geographical area}
}
}
\source{
{https://ec.europa.eu/eurostat/data/database}
}
\usage{
demo_data
}
\description{
A dataset containing population, death rate and life expectancy data for Italian administrative regions
and provinces. Data are the average of the last 5 years, starting from the latest available data.
}
\keyword{datasets}
