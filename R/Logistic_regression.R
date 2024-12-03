# Load required library
library(stats)

# 1. Prepare Design Matrix for Numerical, Date, and Factor Data Types
prepare_data <- function(data, response) {
  # Create the design matrix using model.matrix
  response_var <- as.numeric(data[[response]])
  predictors <- model.matrix(as.formula(paste(response, "~ .")), data = data)[, -1]
  list(X = predictors, y = response_var)
}

# 2. Logistic Regression Estimation Using Numerical Optimization
estimate_beta <- function(data, response) {
  # Prepare design matrix and response variable
  prepared_data <- prepare_data(data, response)
  X <- prepared_data$X
  y <- prepared_data$y

  # Initial values using least squares
  beta_initial <- solve(t(X) %*% X) %*% t(X) %*% y

  # Log-likelihood function
  log_likelihood <- function(beta) {
    p <- 1 / (1 + exp(-X %*% beta))
    # Add a small value to p to prevent log(0)
    p <- pmin(pmax(p, .Machine$double.eps), 1 - .Machine$double.eps)
    -sum(y * log(p) + (1 - y) * log(1 - p))
  }

  # Optimization using optim
  opt <- optim(
    par = beta_initial,
    fn = log_likelihood,
    method = "BFGS"
  )
  list(beta = opt$par, value = opt$value, convergence = opt$convergence)
}

# 3. Bootstrap Confidence Intervals
bootstrap_ci <- function(data, response, beta_hat, n_bootstrap = 20, alpha = 0.05) {
  prepared_data <- prepare_data(data, response)
  X <- prepared_data$X
  y <- prepared_data$y
  n <- nrow(X)
  p <- length(beta_hat)
  bootstrap_estimates <- matrix(NA, nrow = n_bootstrap, ncol = p)

  for (i in 1:n_bootstrap) {
    # Resample the data
    indices <- sample(1:n, size = n, replace = TRUE)
    X_boot <- X[indices, , drop = FALSE]
    y_boot <- y[indices]

    # Re-estimate beta
    log_likelihood <- function(beta) {
      p <- 1 / (1 + exp(-X_boot %*% beta))
      # Add a small value to p to prevent log(0)
      p <- pmin(pmax(p, .Machine$double.eps), 1 - .Machine$double.eps)
      -sum(y_boot * log(p) + (1 - y_boot) * log(1 - p))
    }

    beta_boot <- optim(
      par = beta_hat,
      fn = log_likelihood,
      method = "BFGS"
    )$par
    bootstrap_estimates[i, ] <- beta_boot
  }

  # Compute confidence intervals
  ci <- apply(bootstrap_estimates, 2, function(x) {
    quantile(x, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
  })
  t(ci)  # Transpose for clarity
}

# 4. Predict Classes Based on Cutoff 0.5
predict_class <- function(beta, X) {
  prob <- 1 / (1 + exp(-X %*% beta))
  as.integer(prob > 0.5)
}

# 5. Confusion Matrix and Metrics
confusion_matrix <- function(data, response, beta) {
  prepared_data <- prepare_data(data, response)
  X <- prepared_data$X
  y <- prepared_data$y
  predictions <- predict_class(beta, X)

  # Confusion matrix components
  tp <- sum(predictions == 1 & y == 1)  # True Positives
  tn <- sum(predictions == 0 & y == 0)  # True Negatives
  fp <- sum(predictions == 1 & y == 0)  # False Positives
  fn <- sum(predictions == 0 & y == 1)  # False Negatives

  # Total actual positives and negatives
  actual_positives <- tp + fn
  actual_negatives <- tn + fp
  predicted_positives <- tp + fp
  predicted_negatives <- tn + fn

  # Metrics with checks to avoid division by zero
  prevalence <- mean(y)
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  sensitivity <- ifelse(actual_positives > 0, tp / actual_positives, NA)
  specificity <- ifelse(actual_negatives > 0, tn / actual_negatives, NA)
  false_discovery_rate <- ifelse(predicted_positives > 0, fp / predicted_positives, NA)
  dor_denominator <- fp * fn
  diagnostic_odds_ratio <- ifelse(dor_denominator > 0, (tp * tn) / dor_denominator, NA)

  # Confusion matrix as a table
  conf_matrix <- matrix(c(tp, fp, fn, tn), nrow = 2, byrow = TRUE,
                        dimnames = list("Predicted" = c("1", "0"), "Actual" = c("1", "0")))

  list(
    confusion_matrix = conf_matrix,
    prevalence = prevalence,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    false_discovery_rate = false_discovery_rate,
    diagnostic_odds_ratio = diagnostic_odds_ratio
  )
}

# 6. Example Usage with Simulated Data
set.seed(123)
data <- data.frame(
  x1 = rnorm(100),
  x2 = as.factor(sample(c("A", "B"), 100, replace = TRUE)),
  x3 = as.Date('2023-01-01') + sample(1:100, 100),
  y = rbinom(100, 1, 0.5)
)

# Estimate beta
result <- estimate_beta(data, "y")
cat("Estimated Beta Coefficients:\n")
print(result$beta)

# Bootstrap Confidence Intervals
ci <- bootstrap_ci(data, "y", result$beta, n_bootstrap = 100, alpha = 0.05)
cat("\nBootstrap Confidence Intervals:\n")
print(ci)

# Confusion Matrix and Metrics
metrics <- confusion_matrix(data, "y", result$beta)
cat("\nConfusion Matrix:\n")
print(metrics$confusion_matrix)
cat("\nMetrics:\n")
cat("Prevalence:", metrics$prevalence, "\n")
cat("Accuracy:", metrics$accuracy, "\n")
cat("Sensitivity:", metrics$sensitivity, "\n")
cat("Specificity:", metrics$specificity, "\n")
cat("False Discovery Rate:", metrics$false_discovery_rate, "\n")
cat("Diagnostic Odds Ratio:", metrics$diagnostic_odds_ratio, "\n")
