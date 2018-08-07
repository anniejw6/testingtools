#' Graph Bayesian Updating
#'
#' Graphs posterior, prior (Distribution 1), and likelihood (Distribution 2). Requires ggplot2.
#'
#' @param est_1 Point estimate of Distribution 1
#' @param se_1 Standard error on point estimate of Distribution 2
#' @param est_2 Point estimate of Distribution 2
#' @param se_2 Standard error on point estimate of Distribution 2
#' @param est_pooled Point estimate of posterior, defaults to NULL (calculate automatically)
#' @param se_pooled Standard error of posterior, defaults to NULL (calculate automatically)
#' @param x_min Minimum value on x axis.
#' @param x_max Maximum value on x axis.
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' graph_bayes(0, 0.01, 0.05, 0.01)
graph_bayes <- function(est_1, se_1,
                        est_2, se_2,
                        est_pooled = NULL, se_pooled = NULL,
                        x_min = -0.075,
                        x_max = 0.075){

  if(! requireNamespace("ggplot2", quietly = TRUE))
    stop('ggplot2 must be installed to use this function')

  if(any(is.null(est_pooled), is.null(se_pooled))){
    output <- bayes_updater(est_1, se_1, est_2, se_2, graph = FALSE)
    est_pooled <- output[1]
    se_pooled <- output[2]
  }

  ggplot2::ggplot(data.frame(x = c(x_min, x_max)), ggplot2::aes(x)) +
    ggplot2::geom_vline(xintercept = 0, color = 'LightGrey') +
    ggplot2::stat_function(
      fun = dnorm, args = list(mean = est_1, sd = se_1),
      ggplot2::aes(colour = "0 - Prior"), size = 1) +
    ggplot2::stat_function(
      fun = dnorm, args = list(mean = est_2, sd = se_2),
      ggplot2::aes(colour = "1 - Likelihood"), size = 1) +
    ggplot2::stat_function(
      fun = dnorm, args = list(mean = est_pooled, sd = se_pooled),
      ggplot2::aes(colour = "2 - Posterior"), size = 1.5) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())

}

#' Bayesian Updating of Experimental Results
#'
#' Generates posterior based on prior (Distribution 1) and likelihood (Distribution 2).
#'
#' @param est_1 Point estimate of Distribution 1
#' @param se_1 Standard error on point estimate of Distribution 2
#' @param est_2 Point estimate of Distribution 2
#' @param se_2 Standard error on point estimate of Distribution 2
#' @param graph Boolean, should a graph be generated
#' @param ... Any additional parameters that should be passed to ?graph_bayes
#'
#' @return vector of the posterior point estimate and SE. Optionally, graph
#' @export
#'
#' @examples
#' bayes_updater(0, 0.1, 2.5, 0.1, x_min = -5, x_max = 5)
bayes_updater <- function(est_1, se_1, est_2, se_2,
                          graph = TRUE, ...){

  est_pooled <- weighted.mean(c(est_1, est_2), w = 1/(c(se_1, se_2)^2))

  se_pooled <- sqrt(1/((1/se_1^2) + (1/se_2^2)))

  if(graph){
    print( graph_bayes(est_1 = est_1, se_1 = se_1,
                       est_2 = est_2, se_2 = se_2,
                       est_pooled = est_pooled, se_pooled = se_pooled, ...)
    )
  }

  c(est_pooled, se_pooled)
}
