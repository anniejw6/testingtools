#' Balance Check
#'
#' Calculate balance for binary outcomes.
#'
#' @param outcome Outcome data, as a numeric vector. (Can be treatment assignment)
#' @param covar Dataframe of covariates. Each column should be a separate covariate.
#' @param family Family for GLM
#'
#' @return Summary of GLM model object and anova
#' @export
#'
#' @examples
#' data(iris)
#' mm <- balance(outcome = sample(0:1, nrow(iris), replace = TRUE),
#'               covar = iris)
balance <- function(
  outcome,
  covar,
  family = 'gaussian',
  verbose = TRUE
){

  checkmate::assert_data_frame(covar, any.missing = FALSE)
  checkmate::assert_numeric(outcome, len = nrow(covar), lower = 0, upper = 1,
                            any.missing = FALSE)
  checkmate::assert_character(family, len = 1, any.missing = FALSE)
  checkmate::assert_logical(verbose, len = 1)

  df <- data.frame(outcome = outcome, covar, stringsAsFactors = FALSE)

  # Fit full model
  mod <- glm(formula = outcome ~ ., family = family, data = df,
             model = F, x = F, y = F)

  # Fit null model
  mod_null <- glm(formula = outcome ~ 1, family = family, data = df,
                  model = F, x = F, y = F)

  # Get output
  lrt <- anova(mod_null, mod, test = "LRT")
  mod <- summary(mod)


  if(verbose)
    cat(c(capture.output(mod), capture.output(lrt)), sep = '\n')

  list(mod = mod, lrt = lrt)

}
