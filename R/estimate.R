#' Estimate Outcome
#'
#' @param outcome character, outcome column.
#' @param covar character, columns for covariates
#' @param treat character, treatment assignment
#' @param family Family for glm
#' @param type `top` or `sub` based on topline or subgroup.
#' @param sub_var character, subgroup variable
#' @param data data frame to use for analysis
#' @param ... other paramters passed to glm
#' @param verbose Boolean. Should model output be printed?
#'
#' @return GLM model object
#' @export
#' @importFrom stats glm gaussian as.formula
#' @importFrom utils capture.output

#'
#' @examples
#' data(iris)
#' iris$treat <- sample(0:1, nrow(iris), replace = TRUE)
#' mm <- estimate(
#'   outcome = 'Sepal.Length',
#'   covar = 'Sepal.Width',
#'   treat = 'treat', data = iris)
estimate <- function(
  outcome,
  covar,
  treat,
  data,
  sub_var = NULL,
  family = gaussian,
  type = 'top',
  verbose = TRUE,
  ...
){

  checkmate::assert_character(outcome, len = 1)
  checkmate::assert_character(covar, any.missing = FALSE)
  checkmate::assert_character(treat, len = 1)
  checkmate::assert_character(sub_var, len = 1, null.ok = TRUE)
  checkmate::assert_data_frame(data)
  checkmate::assert_choice(type, choices = c('top', 'sub'))
  checkmate::assert_logical(verbose, len = 1)

  if(any(! outcome %in% 0:1))
    warning('Outcome data is not binary!')

  covar <- paste0(covar, collapse = "+")

  if(type == "top"){
    form <- sprintf("%s ~ %s + %s", outcome, treat, covar)
  } else if(type == "sub"){
    form <- sprintf("%s ~ %s * %s + %s", outcome, treat, sub_var, covar)
  }

  form <- as.formula(form)

  print("Distribution of Outcome Data")
  print(table(data[[outcome]], useNA = 'ifany'))
  mod <- glm(form, data = data, x = FALSE, y = FALSE, family = family, ...)

  if(verbose)
    cat(capture.output(summary(mod)), sep = '\n')

  mod

}
