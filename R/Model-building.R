#' Build QSAR models with k-fold cross-validation
#'
#' This function builds QSAR (Quantitative Structure-Activity Relationship) models
#' using multiple algorithms such as Multiple Linear Regression (MLR), Partial Least Squares (PLS),
#' and Random Forest with k-fold cross-validation.
#'
#' @import tibble
#' @importFrom dplyr filter mutate select
#' @importFrom randomForest randomForest
#' @importFrom pls plsr
#' @importFrom ggplot2 aes geom_point geom_smooth labs theme_minimal geom_hline
#' @importFrom caret createFolds
#' @importFrom stats formula predict lm
#' @importFrom utils read.csv
#' @param data_file The file path of the dataset.
#' @param k The number of folds for cross-validation (default is 5).
#'
#' @return A list containing MLR, PLS, and Random Forest models with their predictions, actuals, and formulas.
#'
#' @export
build_qsar_models <- function(data_file, k = 5) {
  # Read data from file
  data <- read.csv(data_file, header = TRUE)

  # Extract predictors (descriptors) and response (activity)
  X <- data[, -ncol(data), drop = FALSE]
  y <- data[, ncol(data)]

  # Combine predictors and response into a single data frame
  data <- cbind(X, y)

  # Set up k-fold cross-validation
  folds <- createFolds(y, k = k, list = TRUE, returnTrain = FALSE)

  # Initialize lists to store model performance metrics
  mlr_predictions <- list()
  pls_predictions <- list()
  rf_predictions <- list()
  actuals <- list()
  formulas <- list()  # To store formulas

  # Perform k-fold cross-validation for each model
  for (i in 1:k) {
    # Split data into training and testing sets for the current fold
    test_indices <- unlist(folds[i])
    train_indices <- setdiff(1:nrow(data), test_indices)
    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]

    # Split predictors and response for training and testing sets
    train_X <- train_data[, -ncol(train_data), drop = FALSE]
    train_y <- train_data[, ncol(train_data)]
    test_X <- test_data[, -ncol(test_data), drop = FALSE]
    test_y <- test_data[, ncol(test_data)]

    # Build QSAR models
    mlr_model <- lm(train_y ~ ., data = train_X)
    pls_model <- plsr(train_y ~ ., data = train_X, ncomp = 2)  # adjust 'ncomp' as needed
    rf_model <- randomForest(train_X, y = train_y)

    # Make predictions using MLR model
    mlr_predictions[[i]] <- predict(mlr_model, newdata = test_X)

    # Make predictions using PLS model
    pls_predictions[[i]] <- predict(pls_model, newdata = test_X, comps = 1)

    # Make predictions using Random Forest model
    rf_predictions[[i]] <- predict(rf_model, newdata = test_X)

    # Store actuals
    actuals[[i]] <- test_y
    # Store formulas
    formulas[[i]] <- list(MLR = formula(mlr_model), PLS = formula(pls_model), Random_Forest = deparse(rf_model$call))
  }

  # Combine predictions and actuals into lists
  mlr_results <- list(predictions = unlist(mlr_predictions), actuals = unlist(actuals))
  pls_results <- list(predictions = unlist(pls_predictions), actuals = unlist(actuals))
  rf_results <- list(predictions = unlist(rf_predictions), actuals = unlist(actuals))

  # Return results along with formulas
  return(list(MLR = list(results = mlr_results, formula = unlist(formulas)[c(1)]),
              PLS = list(results = pls_results, formula = unlist(formulas)[c(2)]),
              Random_Forest = list(results = rf_results, formula = unlist(formulas)[c(3)])))
}
