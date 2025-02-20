% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_dhs_microdata.R
\name{download_dhs_microdata}
\alias{download_dhs_microdata}
\title{Download DHS microdata}
\usage{
download_dhs_microdata(survey_id, topics_regex = character(0))
}
\arguments{
\item{survey_id}{(\code{character(1)}) The text ID of the survey from which data and GPS
will be downloaded. Can be found via StatCompiler.}

\item{topics_regex}{(\code{character(N)}) The list of topics that should be considered for
analysis from the survey files. Multiple topics can be passed - if so, all topics
will be included. The default, NULL, pull all topics.}
}
\value{
A list of \link{data.table}s containing survey datasets.
}
\description{
Download a subset of DHS microdata for a given country and year
}
\details{
This function requires credentials to access the DHS API, requested via
href{https://dhsprogram.com}{the DHS Program website} and loaded into the environment
with \link[rdhs:set_rdhs_config]{rdhs}. If you are unable to access some files via the
DHS API but have manual access via the website downloads, you can save them in the
proper ZIP file formats within the RDHS datasets folder instead.
}
