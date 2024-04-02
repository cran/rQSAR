## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rQSAR)

## -----------------------------------------------------------------------------
library(rQSAR)

# Path to the SDF file
sdf_file<-sdf_file <- system.file("sample.sdf", package = "rQSAR")

# Generate descriptors
descriptors <- generate_descriptors_from_sdf(sdf_file)

## -----------------------------------------------------------------------------
descriptors<-system.file("descriptor1.csv", package = "rQSAR")
selected_data <- perform_variable_selection(descriptors, "Outcome_column_name")
print(head(selected_data))
write.csv(selected_data, "descriptor2.csv")

## -----------------------------------------------------------------------------
# Load the selected data
selected_data<-system.file("descriptor2.csv", package = "rQSAR")
selected_data <- read.csv(selected_data, header = TRUE)

# Compute the correlation matrix
correlation_matrix <- cor(selected_data)

# Plot the heatmap
corrplot(correlation_matrix, method = "color", type = "lower", tl.col = "black", tl.srt = 45)

## -----------------------------------------------------------------------------
# Example usage:
data_file <- system.file("descriptor2.csv", package = "rQSAR")
model_results <- build_qsar_models(data_file)
print(model_results)

## -----------------------------------------------------------------------------
# Correlation plots
plots <- correlation_plots(model_results)
for (i in seq_along(plots)) {
  print(plots[[i]])
}

# Residual plots
plots <- residual_plots(model_results)
grid.arrange(grobs = plots, ncol = 2)

