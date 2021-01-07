library(testingtools)
library(dplyr)
context("Margin Calculations")

data(margex, package = 'modmarg')
margex$treatment <- factor(margex$treatment)
margex$yc <- factor(margex$yc)

test_that("Topline Margins", {

  # Want to test that none of the data-munging changes the underlying values

  # Model 1: Binomial, No Controls ----
  mod <- glm(outcome ~ treatment, data = margex, family = 'binomial')

  # Raw Margins Estimation
  raw_lvl <- modmarg::marg(mod, var_interest = 'treatment')[[1]]
  raw_eff <- modmarg::marg(mod, var_interest = 'treatment', type = 'effects')[[1]]

  # Toplines function
  top <- topmarg(mod, 'treatment')

  testthat::expect_equal(raw_lvl$Margin, top$level)
  testthat::expect_equal(raw_lvl$Standard.Error, top$lvl_se)
  testthat::expect_equal(raw_lvl$P.Value, top$lvl_pval)
  testthat::expect_equal(raw_lvl$`Lower CI (95%)`, top$lvl_ci_low)
  testthat::expect_equal(raw_lvl$`Upper CI (95%)`, top$lvl_ci_high)

  testthat::expect_equal(raw_eff$Margin, top$margin)
  testthat::expect_equal(raw_eff$Standard.Error, top$eff_se)
  testthat::expect_equal(raw_eff$P.Value, top$eff_pval)
  testthat::expect_equal(raw_eff$`Lower CI (95%)`, top$eff_ci_low)
  testthat::expect_equal(raw_eff$`Upper CI (95%)`, top$eff_ci_high)

  # Model 2: Gaussian, With Controls ----
  mod <- glm(outcome ~ treatment * yc + distance,
             data = margex, family = 'gaussian')

  # Raw Margins Estimation
  raw_lvl <- modmarg::marg(mod, var_interest = 'treatment')[[1]]
  raw_eff <- modmarg::marg(mod, var_interest = 'treatment', type = 'effects')[[1]]

  # Toplines function
  top <- topmarg(mod, 'treatment')

  testthat::expect_equal(raw_lvl$Margin, top$level)
  testthat::expect_equal(raw_lvl$Standard.Error, top$lvl_se)
  testthat::expect_equal(raw_lvl$P.Value, top$lvl_pval)
  testthat::expect_equal(raw_lvl$`Lower CI (95%)`, top$lvl_ci_low)
  testthat::expect_equal(raw_lvl$`Upper CI (95%)`, top$lvl_ci_high)

  testthat::expect_equal(raw_eff$Margin, top$margin)
  testthat::expect_equal(raw_eff$Standard.Error, top$eff_se)
  testthat::expect_equal(raw_eff$P.Value, top$eff_pval)
  testthat::expect_equal(raw_eff$`Lower CI (95%)`, top$eff_ci_low)
  testthat::expect_equal(raw_eff$`Upper CI (95%)`, top$eff_ci_high)

})


test_that("Subgroup Margins", {

  # Want to test that none of the data-munging changes the underlying values

  # Model 1: Binomial, No Controls ----
  mod <- glm(outcome ~ treatment, data = margex, family = 'binomial')

  # Raw Margins Estimation
  raw_lvl_yc0 <- modmarg::marg(
    mod, var_interest = 'treatment', data = subset(margex, yc == 0))[[1]]
  raw_lvl_yc1 <- modmarg::marg(
    mod, var_interest = 'treatment', data = subset(margex, yc == 1))[[1]]
  raw_eff_yc0 <- modmarg::marg(
    mod, var_interest = 'treatment', type = 'effects',
    data = subset(margex, yc == 0))[[1]]
  raw_eff_yc1 <- modmarg::marg(
    mod, var_interest = 'treatment', type = 'effects',
    data = subset(margex, yc == 1))[[1]]

  # Toplines function
  sub <- submarg(mod, subgrp_var = 'yc', treat_var = 'treatment')

  testthat::expect_equal(raw_lvl_yc0$Margin,
                         sub %>% filter(yc == 0) %>% pull(level))
  testthat::expect_equal(raw_lvl_yc0$Margin,
                         sub %>% filter(yc == 0) %>% pull(level))
  testthat::expect_equal(raw_lvl_yc0$Standard.Error,
                         sub %>% filter(yc == 0) %>% pull(lvl_se))
  testthat::expect_equal(raw_lvl_yc0$P.Value,
                         sub %>% filter(yc == 0) %>% pull(lvl_pval))
  testthat::expect_equal(raw_lvl_yc0$`Lower CI (95%)`,
                         sub %>% filter(yc == 0) %>% pull(lvl_ci_low))
  testthat::expect_equal(raw_lvl_yc0$`Upper CI (95%)`,
                         sub %>% filter(yc == 0) %>% pull(lvl_ci_high))

  testthat::expect_equal(raw_eff_yc0$Margin,
                         sub %>% filter(yc == 0) %>% pull(margin))
  testthat::expect_equal(raw_eff_yc0$Standard.Error,
                         sub %>% filter(yc == 0) %>% pull(eff_se))
  testthat::expect_equal(raw_eff_yc0$P.Value,
                         sub %>% filter(yc == 0) %>% pull(eff_pval))
  testthat::expect_equal(raw_eff_yc0$`Lower CI (95%)`,
                         sub %>% filter(yc == 0) %>% pull(eff_ci_low))
  testthat::expect_equal(raw_eff_yc0$`Upper CI (95%)`,
                         sub %>% filter(yc == 0) %>% pull(eff_ci_high))

  testthat::expect_equal(raw_lvl_yc1$Margin,
                         sub %>% filter(yc == 1) %>% pull(level))
  testthat::expect_equal(raw_lvl_yc1$Margin,
                         sub %>% filter(yc == 1) %>% pull(level))
  testthat::expect_equal(raw_lvl_yc1$Standard.Error,
                         sub %>% filter(yc == 1) %>% pull(lvl_se))
  testthat::expect_equal(raw_lvl_yc1$P.Value,
                         sub %>% filter(yc == 1) %>% pull(lvl_pval))
  testthat::expect_equal(raw_lvl_yc1$`Lower CI (95%)`,
                         sub %>% filter(yc == 1) %>% pull(lvl_ci_low))
  testthat::expect_equal(raw_lvl_yc1$`Upper CI (95%)`,
                         sub %>% filter(yc == 1) %>% pull(lvl_ci_high))

  testthat::expect_equal(raw_eff_yc1$Margin,
                         sub %>% filter(yc == 1) %>% pull(margin))
  testthat::expect_equal(raw_eff_yc1$Standard.Error,
                         sub %>% filter(yc == 1) %>% pull(eff_se))
  testthat::expect_equal(raw_eff_yc1$P.Value,
                         sub %>% filter(yc == 1) %>% pull(eff_pval))
  testthat::expect_equal(raw_eff_yc1$`Lower CI (95%)`,
                         sub %>% filter(yc == 1) %>% pull(eff_ci_low))
  testthat::expect_equal(raw_eff_yc1$`Upper CI (95%)`,
                         sub %>% filter(yc == 1) %>% pull(eff_ci_high))

  # Model 2: Binomial, Interaction & Controls ----
  mod <- glm(outcome ~ treatment * yc + distance,
             data = margex, family = 'binomial')

  # Raw Margins Estimation
  raw_lvl_yc0 <- modmarg::marg(
    mod, var_interest = 'treatment', data = subset(margex, yc == 0))[[1]]
  raw_lvl_yc1 <- modmarg::marg(
    mod, var_interest = 'treatment', data = subset(margex, yc == 1))[[1]]
  raw_eff_yc0 <- modmarg::marg(
    mod, var_interest = 'treatment', type = 'effects',
    data = subset(margex, yc == 0))[[1]]
  raw_eff_yc1 <- modmarg::marg(
    mod, var_interest = 'treatment', type = 'effects',
    data = subset(margex, yc == 1))[[1]]

  # Toplines function
  sub <- submarg(mod, subgrp_var = 'yc', treat_var = 'treatment')

  testthat::expect_equal(raw_lvl_yc0$Margin,
                         sub %>% filter(yc == 0) %>% pull(level))
  testthat::expect_equal(raw_lvl_yc0$Margin,
                         sub %>% filter(yc == 0) %>% pull(level))
  testthat::expect_equal(raw_lvl_yc0$Standard.Error,
                         sub %>% filter(yc == 0) %>% pull(lvl_se))
  testthat::expect_equal(raw_lvl_yc0$P.Value,
                         sub %>% filter(yc == 0) %>% pull(lvl_pval))
  testthat::expect_equal(raw_lvl_yc0$`Lower CI (95%)`,
                         sub %>% filter(yc == 0) %>% pull(lvl_ci_low))
  testthat::expect_equal(raw_lvl_yc0$`Upper CI (95%)`,
                         sub %>% filter(yc == 0) %>% pull(lvl_ci_high))

  testthat::expect_equal(raw_eff_yc0$Margin,
                         sub %>% filter(yc == 0) %>% pull(margin))
  testthat::expect_equal(raw_eff_yc0$Standard.Error,
                         sub %>% filter(yc == 0) %>% pull(eff_se))
  testthat::expect_equal(raw_eff_yc0$P.Value,
                         sub %>% filter(yc == 0) %>% pull(eff_pval))
  testthat::expect_equal(raw_eff_yc0$`Lower CI (95%)`,
                         sub %>% filter(yc == 0) %>% pull(eff_ci_low))
  testthat::expect_equal(raw_eff_yc0$`Upper CI (95%)`,
                         sub %>% filter(yc == 0) %>% pull(eff_ci_high))

  testthat::expect_equal(raw_lvl_yc1$Margin,
                         sub %>% filter(yc == 1) %>% pull(level))
  testthat::expect_equal(raw_lvl_yc1$Margin,
                         sub %>% filter(yc == 1) %>% pull(level))
  testthat::expect_equal(raw_lvl_yc1$Standard.Error,
                         sub %>% filter(yc == 1) %>% pull(lvl_se))
  testthat::expect_equal(raw_lvl_yc1$P.Value,
                         sub %>% filter(yc == 1) %>% pull(lvl_pval))
  testthat::expect_equal(raw_lvl_yc1$`Lower CI (95%)`,
                         sub %>% filter(yc == 1) %>% pull(lvl_ci_low))
  testthat::expect_equal(raw_lvl_yc1$`Upper CI (95%)`,
                         sub %>% filter(yc == 1) %>% pull(lvl_ci_high))

  testthat::expect_equal(raw_eff_yc1$Margin,
                         sub %>% filter(yc == 1) %>% pull(margin))
  testthat::expect_equal(raw_eff_yc1$Standard.Error,
                         sub %>% filter(yc == 1) %>% pull(eff_se))
  testthat::expect_equal(raw_eff_yc1$P.Value,
                         sub %>% filter(yc == 1) %>% pull(eff_pval))
  testthat::expect_equal(raw_eff_yc1$`Lower CI (95%)`,
                         sub %>% filter(yc == 1) %>% pull(eff_ci_low))
  testthat::expect_equal(raw_eff_yc1$`Upper CI (95%)`,
                         sub %>% filter(yc == 1) %>% pull(eff_ci_high))

})


test_that("Subgroup Margins with Weights", {

  # Want to test that none of the data-munging changes the underlying value

  # Binomial, Interaction & Controls ----
  mod <- glm(outcome ~ treatment * yc + distance,
             data = margex, family = 'binomial', weight = margex$ycn)

  # Raw Margins Estimation
  raw_lvl_yc0 <- modmarg::marg(
    mod, var_interest = 'treatment', data = subset(margex, yc == 0),
    weight = margex$ycn[margex$yc == 0])[[1]]
  raw_lvl_yc1 <- modmarg::marg(
    mod, var_interest = 'treatment', data = subset(margex, yc == 1),
    weight = margex$ycn[margex$yc == 1])[[1]]
  raw_eff_yc0 <- modmarg::marg(
    mod, var_interest = 'treatment', type = 'effects',
    data = subset(margex, yc == 0),
    weight = margex$ycn[margex$yc == 0])[[1]]
  raw_eff_yc1 <- modmarg::marg(
    mod, var_interest = 'treatment', type = 'effects',
    data = subset(margex, yc == 1),
    weight = margex$ycn[margex$yc == 1])[[1]]

  # Toplines function
  sub <- submarg(mod, subgrp_var = 'yc', treat_var = 'treatment',
                 weight = mod$prior.weights)

  testthat::expect_equal(raw_lvl_yc0$Margin,
                         sub %>% filter(yc == 0) %>% pull(level))
  testthat::expect_equal(raw_lvl_yc0$Margin,
                         sub %>% filter(yc == 0) %>% pull(level))
  testthat::expect_equal(raw_lvl_yc0$Standard.Error,
                         sub %>% filter(yc == 0) %>% pull(lvl_se))
  testthat::expect_equal(raw_lvl_yc0$P.Value,
                         sub %>% filter(yc == 0) %>% pull(lvl_pval))
  testthat::expect_equal(raw_lvl_yc0$`Lower CI (95%)`,
                         sub %>% filter(yc == 0) %>% pull(lvl_ci_low))
  testthat::expect_equal(raw_lvl_yc0$`Upper CI (95%)`,
                         sub %>% filter(yc == 0) %>% pull(lvl_ci_high))

  testthat::expect_equal(raw_eff_yc0$Margin,
                         sub %>% filter(yc == 0) %>% pull(margin))
  testthat::expect_equal(raw_eff_yc0$Standard.Error,
                         sub %>% filter(yc == 0) %>% pull(eff_se))
  testthat::expect_equal(raw_eff_yc0$P.Value,
                         sub %>% filter(yc == 0) %>% pull(eff_pval))
  testthat::expect_equal(raw_eff_yc0$`Lower CI (95%)`,
                         sub %>% filter(yc == 0) %>% pull(eff_ci_low))
  testthat::expect_equal(raw_eff_yc0$`Upper CI (95%)`,
                         sub %>% filter(yc == 0) %>% pull(eff_ci_high))

  testthat::expect_equal(raw_lvl_yc1$Margin,
                         sub %>% filter(yc == 1) %>% pull(level))
  testthat::expect_equal(raw_lvl_yc1$Margin,
                         sub %>% filter(yc == 1) %>% pull(level))
  testthat::expect_equal(raw_lvl_yc1$Standard.Error,
                         sub %>% filter(yc == 1) %>% pull(lvl_se))
  testthat::expect_equal(raw_lvl_yc1$P.Value,
                         sub %>% filter(yc == 1) %>% pull(lvl_pval))
  testthat::expect_equal(raw_lvl_yc1$`Lower CI (95%)`,
                         sub %>% filter(yc == 1) %>% pull(lvl_ci_low))
  testthat::expect_equal(raw_lvl_yc1$`Upper CI (95%)`,
                         sub %>% filter(yc == 1) %>% pull(lvl_ci_high))

  testthat::expect_equal(raw_eff_yc1$Margin,
                         sub %>% filter(yc == 1) %>% pull(margin))
  testthat::expect_equal(raw_eff_yc1$Standard.Error,
                         sub %>% filter(yc == 1) %>% pull(eff_se))
  testthat::expect_equal(raw_eff_yc1$P.Value,
                         sub %>% filter(yc == 1) %>% pull(eff_pval))
  testthat::expect_equal(raw_eff_yc1$`Lower CI (95%)`,
                         sub %>% filter(yc == 1) %>% pull(eff_ci_low))
  testthat::expect_equal(raw_eff_yc1$`Upper CI (95%)`,
                         sub %>% filter(yc == 1) %>% pull(eff_ci_high))

})
