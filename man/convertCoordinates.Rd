% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cppFun.R
\name{convertCoordinates}
\alias{convertCoordinates}
\title{Convert Coordinates Between Different Systems}
\usage{
convertCoordinates(latitude, longitude, from, to)
}
\arguments{
\item{latitude}{Numeric value representing the latitude of the point to convert.}

\item{longitude}{Numeric value representing the longitude of the point to convert.}

\item{from}{A character string indicating the source coordinate system.
Options include "WGS-84", "GCJ-02", and "BD-09".}

\item{to}{A character string indicating the target coordinate system.
Options include "WGS-84", "GCJ-02", and "BD-09".}
}
\value{
A numeric vector of length 2, containing the converted latitude and longitude.
}
\description{
This function converts geographical coordinates between different coordinate systems,
including WGS-84, GCJ-02 (Chinese National Standard), and BD-09 (Baidu Map).
}
\examples{
# Convert WGS-84 coordinates to GCJ-02
convertCoordinates(39.90105, 116.42079, from = "WGS-84", to = "GCJ-02")

# Convert GCJ-02 coordinates to BD-09
convertCoordinates(39.90245, 116.42702, "GCJ-02", "WGS-84")

# Convert WGS-84 coordinates to BD-09
convertCoordinates(39.90105, 116.42079, "WGS-84", "BD-09")
}
