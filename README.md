testingtools
================

This is an extremely basic package to randomize and analyze simple experiments.

# Install

```r
devtools::install_github('anniejw6/testingtools')
```

# Usage

```r
# Generate Covariates
cov_df <- data.frame(
  a = sample(1:5, 1000, replace = T),
  b = sample(letters[1:10], 1000, replace = T))

# Randomize
rr <- testingtools::rand(
  cov_df, treatments = c('Control', 'Treat'),
  probs = c(0.25, 0.75), blk_size = 4)

table(rr)
prop.table(table(rr))

# Balance Check
testingtools::balance(
  outcome = as.numeric(rr == "Treat"), covar = cov_df)

# Estimation
out <- sample(0:1, 1000, replace = T)
testingtools::estimate(
  outcome = out,
  covar = cov_df,
  treat = rr)

# Subgroup estimation
testingtools::subgroup(
  outcome = out, subgroup = as.character(cov_df$a),
  treat = rr)
# can use package `modmarg` for more precision on estimate
```
