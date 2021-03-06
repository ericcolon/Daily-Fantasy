% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betterExtractFunction.R
\name{extractSpecific}
\alias{extractSpecific}
\title{A function to extract specific players from your player model}
\usage{
extractSpecific(playerList, model = mlbModel)
}
\arguments{
\item{playerList}{includes the names of all the players you desire}

\item{model}{the model to pull data from}
}
\value{
subsetted model
}
\description{
A function to extract specific players from your player model
}
\examples{
extractSpecific(playerList = playerNames, model = mlbModel)
}

