library(testingtools)
context("Rand")

test_that("Randomization", {

  cov_df <- data.frame(
    a = sample(1:5, 1000, replace = T),
    b = sample(letters[1:10], 1000, replace = T))

  testthat::expect_error(
    rand(cov_df,
         treatments = c('Control', 'Treat', NA))
  )

  testthat::expect_error(
    rand(cov_df,
         treatments = c('Control', 'Treat'),
         probs = c(0.25, 0.75, NA),
         blk_size = 4)
  )

  testthat::expect_error(
    rand(cov_df,
         treatments = c('Control', 'Treat'),
         probs = c(0.3, 0.75),
         blk_size = 4)
  )

})
