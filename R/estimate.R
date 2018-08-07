#' Estimate Outcome
#'
#' @param outcome Outcome data, as a numeric vector
#' @param covar Dataframe of covariates. Each column should be a separate covariate.
#' @param treat Treatment assignment
#' @param family Family for GLM
#' @param type `full` or `sparse`. `full` is a model with all covariates. `sparse` is a model with no covariates.
#' @param verbose Boolean. Should model output be printed?
#' @return GLM model object
#' @export
#'
#' @examples
#' data(iris)
#' mm <- estimate(outcome = iris$Sepal.Length,
#'                covar = iris[, -1],
#'                treat = sample(0:1, nrow(iris), replace = TRUE))
estimate <- function(
  outcome,
  covar,
  treat,
  family = 'gaussian',
  type = 'full',
  verbose = TRUE
){

  checkmate::assert_data_frame(covar, any.missing = FALSE)
  checkmate::assert_numeric(outcome, len = nrow(covar))
  checkmate::assert_choice(type, choices = c('full', 'sparse'))
  checkmate::assert_vector(treat, len = nrow(covar))
  checkmate::assert_character(family, len = 1, any.missing = FALSE)
  checkmate::assert_logical(verbose, len = 1)

  if(any(! outcome %in% 0:1))
    warning('Outcome data is not binary!')

  df <- data.frame(outcome = outcome, treat = treat, covar,
                   stringsAsFactors = FALSE)

  if(type == 'full'){
    mod <- glm(formula = outcome ~ ., family = family, data = df,
               model = F, x = F, y = F)
  }

  if(type == 'sparse'){

    mod <- glm(formula = outcome ~ treat, family = family, data = df,
               model = F, x = F, y = F)

  }

  if(verbose)
    cat(capture.output(summary(mod)), sep = '\n')

  mod

}
