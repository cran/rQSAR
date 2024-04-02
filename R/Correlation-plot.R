#' Create correlation plots for QSAR models
#'
#' This function creates correlation plots for QSAR models, showing the relationship
#' between predicted and actual values with a correlation coefficient.
#'
#' @importFrom corrplot corrplot
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 geom_point geom_hline ggplot
#' @importFrom stats cor
#' @param model_results A list containing QSAR model results.
#'
#' @return A list of correlation plots for each QSAR model.
#'
#' @export
correlation_plots <- function(model_results) {
  plots <- lapply(names(model_results), function(model_name) {
    # Extract predictions and actuals from the model
    predictions <- model_results[[model_name]]$results$predictions
    actuals <- model_results[[model_name]]$results$actuals

    # Calculate correlation coefficient
    correlation <- cor(predictions, actuals)
    # Define Predicted and Actual variables
    Predicted <- NULL
    Actual <- NULL
    # Create data frame with Predicted and Actual columns
    plot_data <- data.frame(Predicted = predictions, Actual = actuals)

    # Create plot
    ggplot(plot_data, aes(x = Predicted, y = Actual)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = paste("Correlation Plot -", model_name),
           x = "Predicted Values",
           y = "Actual Values",
           caption = paste("Correlation coefficient:", round(correlation, 2))) +
      theme_minimal()
  })

  return(plots)
}
