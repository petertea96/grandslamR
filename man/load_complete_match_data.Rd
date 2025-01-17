% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_data.R
\name{load_complete_match_data}
\alias{load_complete_match_data}
\title{For a single match, load all its PBP and ball trajectory data.}
\usage{
load_complete_match_data(
  match_id,
  data_path = "/Users/petertea/Documents/courtvision_data/data/"
)
}
\arguments{
\item{match_id}{str identifying match of interest.}

\item{data_path}{str for saved data URL.}
}
\value{
A Dataframe for all recorded events corresponding to match ID.
}
\description{
This function loads and aggregates multiple datafiles as a dataframe.
}
\examples{
Eg: Load 2019 Roland Garros match between Thiem and Nadal
load_complete_match_data(match_id = 'atp_roland_garros_2019_SM001')
}
