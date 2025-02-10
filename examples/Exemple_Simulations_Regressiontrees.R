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
  control = rpart.control(minsplit = 50, cp = 0,maxdepth = 3) # Adjust parameters as needed
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

# Fit the tree model with maximum depth=4
fulltree <- rpart(
  Y ~ Gender + Age + Split + Sport,
  data = data, # Replace 'data' with your dataset name if different
  method = "poisson",
  control = rpart.control(cp = 0,maxdepth = 4) # Adjust parameters as needed
)

# Display the tree structure
print(fulltree)

# Plot the tree
rpart.plot(fulltree)

#Print complexity parameters
printcp(fulltree)

# Extract the complexity parameter (cp) table
cp_table <- fulltree$cptable

# Get the "before last" cp value
before_last_cp <- cp_table[nrow(cp_table) - 1, "CP"]

# Print the "before last" cp value
print(before_last_cp)

# Prune the tree to the first CP value
tree_pruned_1 <- prune(fulltree, cp = cp_table[nrow(cp_table) - 1, "CP"]) # First CP value
rpart.plot(tree_pruned_1, main = "Pruned Tree - Step 1")

# Prune the tree further
tree_pruned_2 <- prune(tree_pruned_1, cp = cp_table[nrow(cp_table) - 2, "CP"]) # Use the next CP value
rpart.plot(tree_pruned_2, main = "Pruned Tree - Step 2")

# Continue pruning iteratively (repeat for more steps)
tree_pruned_3 <- prune(tree_pruned_2, cp = cp_table[nrow(cp_table) - 3, "CP"]) # Next CP value
rpart.plot(tree_pruned_3, main = "Pruned Tree - Step 3")

tree_pruned_4 <- prune(tree_pruned_3, cp = cp_table[nrow(cp_table) - 4, "CP"]) # Final pruning
rpart.plot(tree_pruned_4, main = "Pruned Tree - Step 4")

#Print now the CP table of the pruned tree => Optimal tree in terms of cross-validation
printcp(tree_pruned_4)
