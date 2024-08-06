# Function to create a spaghetti plot for scaling[k] parameters
create_spaghetti_plot <- function(mcmc_out) {
  # Check if mcmc_out is a matrix
  if (!is.matrix(mcmc_out)) {
    stop("mcmc_out should be a matrix where each column represents a parameter.")
  }
  
  # Extract columns with names matching 'scaling[k]' pattern
  scaling_cols <- grep("^scaling\\[\\d+\\]$", colnames(mcmc_out), value = TRUE)
  
  # Check if we found any matching columns
  if (length(scaling_cols) == 0) {
    stop("No columns with names matching 'scaling[k]' pattern found.")
  }
  
  # Create a data frame in long format for ggplot2
  scaling_data <- do.call(rbind, lapply(seq_along(scaling_cols), function(i) {
    data.frame(
      Iteration = 1:nrow(mcmc_out),
      Value = mcmc_out[, scaling_cols[i]],
      Parameter = scaling_cols[i]
    )
  }))
  
  # Create the spaghetti plot
  ggplot(scaling_data, aes(x = Iteration, y = Value, color = Parameter, group = Parameter)) +
    geom_line(alpha = 0.7) +
    labs(title = "Spaghetti Plot of Scaling Parameters",
         x = "Iteration",
         y = "Value") +
    theme_minimal() +
    theme(legend.position = "right") +
    scale_color_discrete(name = "Parameter")
}

# Example usage:
# Assuming `mcmc_out` is your matrix containing MCMC samples
# Example setup
# mcmc_out <- matrix(data, nrow = 1000, ncol = K)  # Replace with actual MCMC output matrix

# Create the plot
create_spaghetti_plot(mcmc_out[,])


# Extract all columns with names matching 'scaling[k]'
scaling_cols <- grep("^scaling\\[\\d+\\]$", colnames(mcmc_out), value = TRUE)
scaling_data <- mcmc_out[-c(1:1000), scaling_cols]

# Compute summary statistics for each scaling parameter
summary_stats <- apply(scaling_data, 2, function(x) {
  c(mean = mean(x), median = median(x), sd = sd(x))
})

# Convert summary statistics to a data frame
summary_df <- as.data.frame(t(summary_stats))
summary_df$Parameter <- rownames(summary_df)

# Sort parameters by mean or median to identify outliers
sorted_summary <- summary_df[order(summary_df$mean), ]

# Print parameters with the lowest means
sorted_summary