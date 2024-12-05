# Load required library
library(stats)

# Prepare the Forestfires Dataset
prepare_forestfires_data <- function(file_path) {
  file_path <- system.file("extdata", "forestfires.csv", package = "Group4")
  data <- read.csv(file_path)

  # Create binary response variable
  data$binary_area <- as.integer(data$area > 0)

  # Drop unnecessary columns
  data <- data[, !(names(data) %in% c("X", "Y", "area"))]

  # Return the processed dataset
  return(data)
}

# Use the prepare_data function as is
prepare_data <- function(data, response) {
  # Convert response variable to numeric
  response_var <- as.numeric(data[[response]])

  # Handle categorical variables by creating dummy variables
  factor_cols <- sapply(data, is.factor)
  data[factor_cols] <- lapply(data[factor_cols], function(x) as.numeric(as.factor(x)))

  # Handle date variables by converting to numeric or extracting features
  date_cols <- sapply(data, inherits, "Date")
  if (any(date_cols)) {
    data[date_cols] <- lapply(data[date_cols], function(x) as.numeric(as.POSIXct(x)))
  }

  # Create design matrix using model.matrix
  predictors <- model.matrix(as.formula(paste(response, "~ .")), data = data)[, -1]
  list(X = predictors, y = response_var)
}

# Estimate beta coefficients using logistic regression
estimate_beta <- function(data, response) {
  prepared_data <- prepare_data(data, response)
  X <- prepared_data$X
  y <- prepared_data$y

  # Add regularization to stabilize computation
  beta_initial <- solve(t(X) %*% X) %*% t(X) %*% y

  log_likelihood <- function(beta) {
    p <- 1 / (1 + exp(-X %*% beta))
    p <- pmin(pmax(p, .Machine$double.eps), 1 - .Machine$double.eps)
    -sum(y * log(p) + (1 - y) * log(1 - p))
  }

  opt <- optim(
    par = beta_initial,
    fn = log_likelihood,
    method = "BFGS"
  )
  list(beta = opt$par, value = opt$value, convergence = opt$convergence)
}


bootstrap_ci <- function(data, response, beta_estimate, n_bootstrap = 20, alpha = 0.05) {
  # Extract X and y from the prepared data
  prepared <- prepare_data(data, response)
  X <- prepared$X
  y <- prepared$y

  n <- length(y)
  p <- length(beta_estimate)
  bootstrap_coefs <- matrix(NA, nrow = n_bootstrap, ncol = p)

  # Define the log-likelihood function again for optimization
  log_likelihood <- function(beta, X, y) {
    p <- 1 / (1 + exp(-X %*% beta))
    p <- pmin(pmax(p, .Machine$double.eps), 1 - .Machine$double.eps)
    -sum(y * log(p) + (1 - y) * log(1 - p))
  }

  # Perform bootstrapping
  for (i in 1:n_bootstrap) {
    # Resample indices with replacement
    idx <- sample(1:n, size = n, replace = TRUE)
    Xb <- X[idx, , drop = FALSE]
    yb <- y[idx]

    # Initial estimate using least squares on the bootstrap sample
    beta_initial <- tryCatch({
      solve(t(Xb) %*% Xb) %*% t(Xb) %*% yb
    }, error = function(e) {
      # If solve is singular, add a small ridge regularization
      solve(t(Xb) %*% Xb + diag(1e-6, ncol(Xb))) %*% t(Xb) %*% yb
    })

    # Optimize the log-likelihood for the bootstrap sample
    opt <- optim(
      par = beta_initial,
      fn = function(b) log_likelihood(b, Xb, yb),
      method = "BFGS"
    )

    bootstrap_coefs[i, ] <- opt$par
  }

  # Compute percentile-based bootstrap confidence intervals
  lower_bound <- apply(bootstrap_coefs, 2, quantile, probs = alpha/2)
  upper_bound <- apply(bootstrap_coefs, 2, quantile, probs = 1 - alpha/2)

  ci <- cbind(lower_bound, upper_bound)
  rownames(ci) <- names(beta_estimate)
  colnames(ci) <- c(paste0((100*alpha/2), "%"), paste0((100*(1 - alpha/2)), "%"))
  return(ci)
}


confusion_matrix <- function(data, response, beta) {
  # Prepare data
  prepared <- prepare_data(data, response)
  X <- prepared$X
  y <- prepared$y

  # Predicted probabilities
  p <- 1 / (1 + exp(-X %*% beta))

  # Predicted classes
  y_pred <- ifelse(p > 0.5, 1, 0)

  # Calculate confusion matrix elements
  TP <- sum(y == 1 & y_pred == 1)
  TN <- sum(y == 0 & y_pred == 0)
  FP <- sum(y == 0 & y_pred == 1)
  FN <- sum(y == 1 & y_pred == 0)

  # Construct confusion matrix
  conf_mat <- matrix(c(TN, FP, FN, TP), nrow = 2, byrow = TRUE,
                     dimnames = list("Actual" = c("0","1"),
                                     "Predicted" = c("0","1")))

  # Prevalence = proportion of positives in the dataset
  prevalence <- sum(y == 1) / length(y)

  # Accuracy = (TP + TN) / (TP + TN + FP + FN)
  accuracy <- (TP + TN) / (TP + TN + FP + FN)

  # Sensitivity (Recall) = TP / (TP + FN)
  sensitivity <- if((TP + FN) > 0) TP / (TP + FN) else NA

  # Specificity = TN / (TN + FP)
  specificity <- if((TN + FP) > 0) TN / (TN + FP) else NA

  # False Discovery Rate = FP / (TP + FP)
  false_discovery_rate <- if((TP + FP) > 0) FP / (TP + FP) else NA

  # Diagnostic Odds Ratio = (TP/FN) / (FP/TN) = (TP * TN) / (FP * FN)
  diagnostic_odds_ratio <- if((FN > 0) & (FP > 0)) (TP * TN) / (FP * FN) else NA

  list(
    confusion_matrix = conf_mat,
    prevalence = prevalence,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    false_discovery_rate = false_discovery_rate,
    diagnostic_odds_ratio = diagnostic_odds_ratio
  )
}

# Example usage after your code
file_path <- "forestfires.csv"  # your dataset path
forestfires_data <- prepare_forestfires_data(file_path)

# Estimate beta coefficients
result <- estimate_beta(forestfires_data, "binary_area")
print(result)

# Bootstrap Confidence Intervals
ci <- bootstrap_ci(forestfires_data, "binary_area", result$beta, n_bootstrap = 20, alpha = 0.05)

# Confusion Matrix and Metrics
metrics <- confusion_matrix(forestfires_data, "binary_area", result$beta)
cat("\nConfusion Matrix:\n")
print(metrics$confusion_matrix)
cat("\nMetrics:\n")
cat("Prevalence:", metrics$prevalence, "\n")
cat("Accuracy:", metrics$accuracy, "\n")
cat("Sensitivity:", metrics$sensitivity, "\n")
cat("Specificity:", metrics$specificity, "\n")
cat("False Discovery Rate:", metrics$false_discovery_rate, "\n")
cat("Diagnostic Odds Ratio:", metrics$diagnostic_odds_ratio, "\n")
