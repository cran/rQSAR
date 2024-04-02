#' Perform variable selection using regression subsets
#'
#' This function performs variable selection using regression subsets method.
#'
#' @importFrom leaps regsubsets
#' @importFrom stats coef
#' @importFrom utils read.csv
#' @param file_path The file path of the dataset.
#' @param outcome_col The name of the outcome column.
#' @param des_sel_meth The method for variable selection (default is "exhaustive").
#'
#' @return A data frame containing the selected variables and the outcome.
#'
#' @export
perform_variable_selection <- function(file_path, outcome_col, des_sel_meth = "exhaustive") {
  # Read the data
  data <- read.csv(file_path)

  # Extract predictors and outcome
  predictors <- data[, -which(names(data) == outcome_col)]
  outcome <- data[[outcome_col]]

  # Check if the lengths of outcome and predictors match
  if (length(outcome) != nrow(predictors)) {
    stop("Lengths of outcome and predictor variables are different.")
  }

  # Perform variable selection
  Selected_variables <- leaps::regsubsets(y = outcome, x = predictors, nbest = 1,
                                          nvmax = ncol(predictors) - 1, method = des_sel_meth,
                                          all.best = FALSE, really.big = TRUE)

  # Get summary
  summary_info <- summary(Selected_variables)

  # Get the index of the model with the highest adjusted R2
  best_model_index <- which.max(summary_info$adjr2)

  # Get the coefficients of the selected variables
  coef_values <- stats::coef(Selected_variables, best_model_index)

  # Extract the selected variable names
  selected_var_names <- names(coef_values)[-1]  # Exclude the intercept

  # Create a dataframe with selected variables and outcome
  selected_data <- data.frame(data[, selected_var_names], Outcome = outcome)

  return(selected_data)
}
