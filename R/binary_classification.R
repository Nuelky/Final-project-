# Function to compute initial coefficients using the least squares formula
initialize_coefficients <- function(X, y) {
  # Ensure X includes a column for the intercept
  X <- cbind(Intercept = 1, X)

  # Compute the least-squares estimate of beta
  beta_init <- solve(t(X) %*% X) %*% t(X) %*% y

  return(as.vector(beta_init))  # Return as a vector for easier manipulation
}

# Example usage:
# Define predictors (X) and response (y)
X <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)  # Two predictors
y <- c(0, 1, 0)  # Binary response variable

# Compute initial coefficients
beta_initial <- initialize_coefficients(X, y)
print(beta_initial)
