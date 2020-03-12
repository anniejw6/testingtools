#' Randomization Functions
#'
#' @param covariates data.frame or matrix of covariates
#' @param treatments
#'
#' @return
#' @export
#'
#' @examples
#' cov_df <- data.frame(
#'   a = sample(1:5, 1000, replace = T),
#'   b = sample(letters[1:10], 1000, replace = T))
#' rand(cov_df,
#'      treatments = c('Control', 'Treat'),
#'      probs = c(0.25, 0.75),
#'      blk_size = 4)
#' rand(cov_df,
#'      treatments = c('Control', 'Treat'))
rand <- function(covariates, treatments, probs = NULL, blk_size = NULL){

  requireNamespace("quickblock", quietly = TRUE)
  requireNamespace("randomizr", quietly = TRUE)

  # Check Inputs
  stopifnot(is.data.frame(covariates) || is.matrix(covariates))
  checkmate::assert_vector(probs, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_vector(treatments, any.missing = FALSE, min.len = 2)
  checkmate::assert_count(blk_size, null.ok = TRUE)

  if(is.data.frame(covariates)){
    checkmate::check_data_frame(covariates, any.missing = FALSE)
    covariates <- model.matrix(~ ., covariates)
  } else {
    checkmate::check_matrix(covariates, any.missing = FALSE)
  }

  # Handle Missing Values
  if(is.null(probs)){
    probs <- rep(1/length(treatments), length(treatments))
    print(sprintf("Assigning treatment probabilities of %s",
                  paste(probs, collapse = ", ")))
  } else {
    stopifnot(sum(probs) == 1)
  }
  if(is.null(blk_size)){
    blk_size <- length(treatments)
    print(sprintf("Assigning min block size of %s", blk_size))
  }

  # Randomize
  blks <- quickblock::quickblock(
    distances = distances::distances(covariates),
    size_constraint = blk_size
  )
  rand <- randomizr::block_ra(
    blocks = blks,
    prob_each = probs, conditions = treatments)

  rand

}
