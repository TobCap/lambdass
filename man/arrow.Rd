% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrow.r
\name{arrow}
\alias{arrow}
\alias{\%->\%}
\title{Syntax sugar of making an anonymous function}
\usage{
lhs \%->\% rhs
}
\arguments{
\item{lhs}{formal parameters: any R parsable expression is acceptable,
but \code{f(x, y)} or \code{{x; y}} is recommended.}

\item{rhs}{body for lambda}
}
\description{
arrow function can check arguments class.
}
\examples{
{} \%->\% {x + 2}
x \%->\% {x + 1}
{x; y} \%->\% {x + y}
{x = 1L; y = 2L} \%->\% {x + y}

{x:character; e:environment} \%->\% {get(x, envir = e, inherits = FALSE)}
# see more examples in https://gist.github.com/TobCap/6826123
}
