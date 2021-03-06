% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lambdass.R
\name{lambdass}
\alias{lambdass}
\alias{as.formals}
\alias{is.formals}
\alias{f.r}
\alias{f.}
\title{Syntax sugar of making an anonymous function}
\usage{
as.formals(x, value = list(quote(expr = )))

is.formals(y)

f.r(..., env_ = parent.frame())

f.(...)
}
\arguments{
\item{x}{coerced to character (internally STRSXP).}

\item{value}{a default value for making formal parameters.}

\item{y}{expected to be a pairlist object which is all different named.}

\item{...}{If one argument is passed, that is parsed as lambda's body.
If two or more argument is passed, the last part is parsed as lambda's body
and the rest are parsed as formal parameters for lambda.}

\item{env_}{an environment in which call object is evaluated.}
}
\description{
Syntax sugar of making an anonymous function
}
\examples{
 f.(x, x + 1) # => function(x) x + 1
 f.(x, f.(y, x + y)) # => semantics is: function(x) function(y) x + y
 Reduce(f.(x, y, x + y), 1:10) # => 55
 Filter(f.(x, x \%\% 2 == 0), 1:10) # => c(2L, 4L, 6L, 8L, 10L)
}
