#' Compute linear regression on data using formula and make it work with magrittr pipe operator.
#'
#' @param data A dataframe, e.g. mtcars.
#' @param formula A formula, e.g. mpg ~ wt.
#' @return A modified dataframe containing the variables in the equation
#' plus the fitted value and the residuals.  The formula, model coefficients,
#' variable correlation matrix and overall R^2 and AdjR^2 are attached to
#' the dataframe as attributes.
#' @examples
#' library(datasets)
#' library(dplyr)
#' library(regressR)
#' result <- linear_regression(mtcars, mpg ~ wt)
#' result2 <- mtcars %>% linear_regression(mpg ~ wt)

linear_regression <- function(data, formula) {

  fitted_model <- lm(formula, data = data)
  model_summary <- summary(fitted_model)
  modeled_data <- cbind(as.data.frame(fitted_model$model), fitted = fitted_model$fitted.values, residuals = fitted_model$residuals )
  attr(modeled_data, 'formula') <- formula
  attr(modeled_data, 'coefficients') <- model_summary$coefficients
  attr(modeled_data, 'correlations') <- cor(fitted_model$model)
  attr(modeled_data, 'r_squared') <- model_summary$r.squared
  attr(modeled_data, 'adj_r_squared') <- model_summary$adj.r.squared

  class(modeled_data) = append('linear_regression', class(modeled_data))

  modeled_data
}
