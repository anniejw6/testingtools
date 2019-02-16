#' Estimate Discrete Subgroup Effect
#'
#' @param outcome Outcome data, as a numeric vector
#' @param subgroup Must be character vector
#' @param treat Treatment assignment
#' @param family Family for GLM
#' @param verbose Boolean. Should model output be printed?
#' @return GLM model object
#' @export
#'
#' @examples
#' data(iris)
#' mm <- estimate(outcome = iris$Sepal.Length,
#'                subgroup = iris$Species,
#'                treat = sample(0:1, nrow(iris), replace = TRUE))
subgroup <- function(
  outcome,
  subgroup,
  treat,
  family = 'gaussian',
  verbose = TRUE
){

  if(is.factor(subgroup)) subgroup <- as.character(subgroup)

  checkmate::assert_character(subgroup, any.missing = FALSE)
  checkmate::assert_numeric(outcome, len = nrow(subgroup))
  checkmate::assert_vector(treat, len = nrow(subgroup))
  checkmate::assert_character(family, len = 1, any.missing = FALSE)
  checkmate::assert_logical(verbose, len = 1)

  if(any(! outcome %in% 0:1))
    warning('Outcome data is not binary!')

  df <- data.frame(outcome = outcome, treat = treat,
                   subgroup = subgroup,
                   stringsAsFactors = FALSE)

  mod <- glm(formula = outcome ~ treat * subgroup,
             family = family, data = df,
             model = F, x = F, y = F)

  if(verbose)
    cat(capture.output(summary(mod)), sep = '\n')

  mod

}
