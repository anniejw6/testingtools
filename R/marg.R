# Helper function to clean the output of modmarg
.tidy_modmarg <- function(marg, type = c("eff", "lvl")){

  checkmate::assert_choice(type, choices = c('eff', 'lvl'))

  # Bind list together, if applicable
  marg_df <- data.table::rbindlist(marg, idcol = TRUE)

  # Clean up column names
  if(type == "eff"){

    dplyr::select(
      marg_df, -`Test.Stat`, label = Label, margin = Margin,
      eff_se = `Standard.Error`, eff_ci_low = dplyr::matches("Lower CI"),
      eff_ci_high = dplyr::matches("Upper CI"), eff_pval = `P.Value`)

  } else if(type == "lvl"){

    dplyr::select(
      marg_df, -`Test.Stat`, label = Label, level = Margin,
      lvl_se = `Standard.Error`, lvl_ci_low = dplyr::matches("Lower CI"),
      lvl_ci_high = dplyr::matches("Upper CI"), lvl_pval = `P.Value`)

  }

}

#' Return Topline Margins
#'
#' Basically returns marginal effects on the treatment variable
#'
#' @param mod glm model
#' @param treat_var Treatment variable, character
#' @param mod_df dataframe used in model, defaults to mod$data
#' @param ... Other parameters passed to modmarg::marg

#'
#' @return
#' @export
#'
#' @examples
#' library(modmarg)
#' data(margex)
#' mod <- glm(outcome ~ treatment, data = margex, family = 'binomial')
#' topmarg(mod, 'treatment')
topmarg <- function(mod, treat_var = 'treatment',
                    mod_df = mod$data,
                    ...){

  if(! checkmate::test_class(mod, c("glm"))){
    warning("This function has not been tested with non-glm models, although
            it should support everything supported by `modmarg::marg`")
  }
  checkmate::assert_choice(treat_var, all.vars(mod$formula))

  # Calculate Marginal Levels
  marg_eff <- modmarg::marg(mod, var_interest = treat_var,
                            data = mod_df,
                            type = "effects", ...)
  marg_eff <- .tidy_modmarg(marg_eff, 'eff')

  marg_lvl <- modmarg::marg(mod, var_interest = treat_var,
                            data = mod_df,
                            type = "levels",...)
  marg_lvl <- .tidy_modmarg(marg_lvl, 'lvl')

  merge(marg_lvl, marg_eff, by = c("label", ".id"), all = TRUE)

}

#' Return subgroup margins
#'
#' Calculates subgroup margins by running on subset of data. Doesn't matter for
#' OLS but does matter, e.g., for logit.
#'
#' @param mod glm
#' @param subgrp_var Subgroup variable, character. The subgroup variable must be
#' a categorical variable
#' @param treat_var Treatment variable, character
#' @param mod_df Dataframe used to estimate model, defaults to mod$data
#' @param weight Weight parameter to pass to `modmarg::marg`, defaults to mod$prior.weights
#' @param ... Other parameters passed to modmarg::marg

#'
#' @return
#' @export
#'
#' @examples
#' library(modmarg)
#' data(margex)
#' mod <- glm(outcome ~ treatment * yc + distance, data = margex, family = 'binomial')
#' submarg(mod, 'yc', 'treatment')
submarg <- function(mod, subgrp_var,
                    treat_var = 'treatment',
                    mod_df = mod$data,
                    weight = mod$prior.weights,
                    ...){

  if(! checkmate::test_class(mod, c("glm"))){
    warning("This function has not been tested with non-glm models, although
            it should support everything supported by `modmarg::marg`")
  }
  checkmate::assert_choice(treat_var, all.vars(mod$formula))
  checkmate::assert_data_frame(mod_df)
  checkmate::assert_choice(treat_var, names(mod_df))
  checkmate::assert_choice(subgrp_var, names(mod_df))
  checkmate::assert_numeric(weight, len = nrow(mod_df))

  unique_values <- mod_df[[subgrp_var]][! duplicated( mod_df[[subgrp_var]] )]
  unique_values <- unique_values[order(unique_values)]

  if(length(unique_values) >= 10){
    warning("There are more than 10 unique values in your subgroup - are you sure it is a categorical variable?")
  }

  marg_results <- lapply(unique_values, function(x){
    # Subset to relevant category
    tmp_df <- dplyr::filter(mod_df, get(subgrp_var) == x)
    tmp_wgt <- weight[mod_df[[subgrp_var]] == x]

    # Estimate toplines within that subset of the data
    topmarg <- topmarg(mod, treat_var = treat_var, data = tmp_df,
                       weight = tmp_wgt, ...)
    topmarg[[subgrp_var]] <- x
    topmarg
  })

  data.table::rbindlist(marg_results, idcol = FALSE)

}
