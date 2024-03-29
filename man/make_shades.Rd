% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colours.R
\name{make_shades}
\alias{make_shades}
\title{Make shades}
\usage{
make_shades(colour, n, lighter = TRUE)
}
\arguments{
\item{colour}{The R colour name to make shades of}

\item{n}{The number of shades to make}

\item{lighter}{Whether to make lighter (\code{TRUE}) or darker (\code{FALSE}) shades}
}
\value{
A vector of n hex colour codes
}
\description{
Given a colour make \code{n} lighter or darker shades.
}
\details{
This is an essay exposing the cultural importance of the \code{make_shades()} function.
  It is left as an exercise for the interested reader.
}
\examples{
# Five lighter shades
make_shades("goldenrod", 5)

# Five darker shades
\dontrun{
make_shades("goldenrod", 5, lighter = FALSE)
  }
}
