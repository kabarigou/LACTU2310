# Load required libraries
set.seed(123) # For reproducibility

# Number of simulations
n <- 500000

# Simulating the features
X1 <- sample(c("female", "male"), size = n, replace = TRUE, prob = c(0.5, 0.5))
X2 <- sample(18:65, size = n, replace = TRUE, prob = rep(1/48, 48))
X3 <- sample(c("yes", "no"), size = n, replace = TRUE, prob = c(0.5, 0.5))
X4 <- sample(c("yes", "no"), size = n, replace = TRUE, prob = c(0.5, 0.5))

# Compute expected claim frequency \mu(x)
mu <- 0.1 * 
  (1 + 0.1 * (X1 == "male")) * 
  (1 + 0.4 * (X2 >= 18 & X2 < 30) + 0.2 * (X2 >= 30 & X2 < 45)) * 
  (1 + 0.15 * (X4 == "yes"))

# Simulate response variable Y (number of claims)
Y <- rpois(n, lambda = mu)

# Combine into a dataframe
data <- data.frame(Y, Gender = X1, Age = X2, Split = X3, Sport = X4)

# Display the first 10 rows
head(data, 10)


# Load the necessary library for tree-based methods
library(rpart)
library(rpart.plot)

# Fit the tree model with maximum depth=3
tree <- rpart(
  Y ~ Gender + Age + Split + Sport,
  data = data, # Replace 'data' with your dataset name if different
  method = "poisson",
  control = rpart.control(minsplit = 50, cp = 0.0001,maxdepth = 3) # Adjust parameters as needed
)

#rpart.control: Various parameters that control aspects of the rpart fit.

# minsplit the minimum number of observations that must exist in a node in order for a
#split to be attempted.

#cp complexity parameter. Any split that does not decrease the overall lack of fit by
#a factor of cp is not attempted.

#maxdepth Set the maximum depth of any node of the final tree, with the root node counted
#as depth 0.


# Display the tree structure
print(tree)

# Plot the tree
rpart.plot(tree)

# Fit the tree model with maximum depth=3
tree2 <- rpart(
  Y ~ Gender + Age + Split + Sport,
  data = data, # Replace 'data' with your dataset name if different
  method = "poisson",
  control = rpart.control(cp = 0.000001,maxdepth = 4) # Adjust parameters as needed
)

# Display the tree structure
print(tree2)

# Plot the tree
rpart.plot(tree2)

