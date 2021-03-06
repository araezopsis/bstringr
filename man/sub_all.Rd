% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bstr_sub.R
\name{sub_all}
\alias{sub_all}
\alias{bstr_sub_all}
\alias{bstr_sub_all<-}
\alias{bstr_sub_replace_all}
\alias{bstr_sub_all_replace}
\title{Extract and replace all subsequences from a bstr sequences}
\usage{
bstr_sub_all(bstrobj, from = list(1L), to = list(-1L), length)

bstr_sub_all(bstrobj, from = list(1L), to = list(-1L), length, omit_na = FALSE) <- value

bstr_sub_replace_all(..., replacement, value = replacement)

bstr_sub_all_replace(..., replacement, value = replacement)
}
\arguments{
\item{bstrobj}{bstr class object or character vector}

\item{from}{a list of integer vectors giving the start indexes or a
list of two-column matrices, each of type \code{cbind(from, to)}}

\item{to}{a list of integer vectors giving the end indexes}

\item{length}{a list of integer vectors giving the substring lengths}

\item{omit_na}{a single logical value; indicates whether missing values
in any of the indexes or in \code{value} leave the part of the
corresponding input string
unchanged [replacement function only]}

\item{value}{a list of character vectors defining the replacement strings
[replacement function only]}

\item{...}{arguments to be passed to \code{bstr_sub_all<-}}

\item{replacement}{alias of \code{value} [wherever applicable]}
}
\description{
Extract and replace all subsequences from a bstr sequences
}
\examples{
(temp <- dstr_rand_seq(3, 20, "[AT]", seed = 1))

(pos_A_trails <- stringr::str_locate_all(temp, "A{2,}"))

# Extract all A trails
bstr_sub_all(temp, pos_A_trails)

# Substitute all A trails by x
bstr_sub_all(temp, pos_A_trails) <- "x"
temp

# Substitute all A trails by case switched ones
original <- change <- dstr_rand_seq(2, 10, "[AaT]", seed = 1)
pos_A_trails <- stringr::str_locate_all(change, "[Aa]{2,}")
switched_A_trails <-
  bstr_sub_all(change, pos_A_trails) \%>\%
  lapply(bstr_switch_case)
bstr_sub_all(change, pos_A_trails) <- switched_A_trails
c(original[1], change[1], original[2], change[2])

}
