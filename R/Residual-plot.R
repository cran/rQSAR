#' Function to create residual plots with model type labels
#'
#' @importFrom ggplot2 geom_point geom_hline ggplot theme_minimal
#' @param model_results A list containing model results
#' @return A list of ggplot objects representing residual plots
#'
#' @export
residual_plots <- function(model_results) {
  plots <- lapply(names(model_results), function(model_name) {
    # Extract predictions and actuals from the model
    predictions <- model_results[[model_name]]$results$predictions
    actuals <- model_results[[model_name]]$results$actuals

    # Calculate residuals
    residuals <- actuals - predictions

    # Create a data frame for plotting
    df <- data.frame(Predicted = predictions, Residuals = residuals, Model = model_name)
    Predicted = predictions
    Residuals = residuals
    # Create residual plot
    plot <- ggplot(df, aes(x = Predicted, y = Residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = paste("Residual Plot -", model_name),
           x = "Predicted Values",
           y = "Residuals") +
      theme_minimal()

    return(plot)
  })

  return(plots)
}
