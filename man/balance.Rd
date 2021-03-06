% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balance.R
\name{balance}
\alias{balance}
\title{Balance Check}
\usage{
balance(outcome, covar, family = "gaussian", verbose = TRUE)
}
\arguments{
\item{outcome}{Outcome data, as a numeric vector. (Can be treatment assignment)}

\item{covar}{Dataframe of covariates. Each column should be a separate covariate.}

\item{family}{Family for GLM}

\item{verbose}{TRUE by default}
}
\value{
Summary of GLM model object and anova
}
\description{
Calculate balance for binary outcomes.
}
\examples{
data(iris)
mm <- balance(outcome = sample(0:1, nrow(iris), replace = TRUE),
              covar = iris)
}
