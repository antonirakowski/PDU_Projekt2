% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions.R
\name{wyodrebnij}
\alias{wyodrebnij}
\title{Extract given date part}
\usage{
wyodrebnij(
  df,
  name = "Date",
  what = c("\%H"),
  from = "\%d-\%m-\%Y \%H:\%M:\%S",
  new = TRUE,
  newName = c("Hour")
)
}
\arguments{
\item{df}{Main data frame}

\item{name}{Name of column with date.In default "Date".}

\item{what}{Part of date to be extracted .}

\item{from}{Format of date in name column.}

\item{new}{Boolean, if FALSE name column will be overwriten.}

\item{newName}{Name of new column.}
}
\value{
Returns data frame with extracted parts of date.
}
\description{
Extracting given part of date and making new column with those values.
}
\details{
Length of what and newName vector must be the same in order to make function work properly.
Use only when date format is the same across all column.
Changing "new" parameter is not recommended.
}
\examples{
  #Extracting month from date and making new column
  wyodrebnij(df, what=c("\%m"), newName=c("Month"))
  #Extracting multiple data
  wyodrebnij(df, name="Time", what=c("H","m"), newName=c("Hour","Month"))
}
\references{
Gagolewski M. (2024), Deep R Programming, Melbourne
}
\seealso{
\link{czas} for changing time format in data frame
}
