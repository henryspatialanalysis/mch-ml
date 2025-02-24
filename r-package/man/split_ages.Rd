% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/birth_history_preparation.R
\name{split_ages}
\alias{split_ages}
\title{Split full birth history data into age groups}
\usage{
split_ages(data, age_groups, max_timediff_months, child_id_columns)
}
\arguments{
\item{data}{(\link[data.table:data.table]{data.table}) Full birth history data containing
at least the fields 'c_birth_cmc', 'c_alive', 'c_died_age_months', 'int_month_cmc'}

\item{age_groups}{(\code{list(N)}) Named list corresponding to each age group. The name of
each item is the age group name, and the item itself contains two integers
representing \emph{inclusive} months of age in that group. For example \verb{6mo = c(6, 12)}
represents the age group between 6 and \emph{11} months of completed age.}

\item{max_timediff_months}{(\code{integer(1)}) Number of months prior to the interview
month to consider when splitting the data. Used to derive years of analysis.}

\item{child_id_columns}{(\code{character(N)}) Fields in \code{data} that uniquely identify
individual children.}
}
\value{
Dataset summarized by age group and year cohorts. In addition to the original
columns, the returned dataset will contain the fields 'year', 'age_group', and
'died'. Individual children not falling into any of the summary year data will be
excluded from analysis.
}
\description{
Split full birth history data into age groups
}
