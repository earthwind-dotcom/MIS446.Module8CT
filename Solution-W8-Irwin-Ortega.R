# Name: Irwin Ortega
# Course: MIS446
# Module: 8

# Step 2: Build a multiple linear regression model using the mtcars data set.

# Load the built-in mtcars data set
data(mtcars)
cat("Original mtcars data set dimensions:", dim(mtcars)[1], "rows,", dim(mtcars)[2], "columns\n\n")

# Create a data frame that includes only the specified variables:
# qsec, hp, wt, cyl, carb, and disp
selected_mtcars <- mtcars[, c("qsec", "hp", "wt", "cyl", "carb", "disp")]

# List the variable names in this new data frame.
cat("Variable names in the selected_mtcars data frame:\n")
print(names(selected_mtcars))
cat("\n")

# Using qsec as the dependent variable and the other variables as possible predictors,
# confirm or disconfirm, statistically and visually, that each predictor is linearly related to the dependent variable.

# Statistical confirmation: Calculate correlation between qsec and each predictor
cat("Correlation between qsec and potential predictors:\n")
# We need to calculate correlation for each predictor with qsec
correlation_qsec_hp <- cor(selected_mtcars$qsec, selected_mtcars$hp)
correlation_qsec_wt <- cor(selected_mtcars$qsec, selected_mtcars$wt)
correlation_qsec_cyl <- cor(selected_mtcars$qsec, selected_mtcars$cyl)
correlation_qsec_carb <- cor(selected_mtcars$qsec, selected_mtcars$carb)
correlation_qsec_disp <- cor(selected_mtcars$qsec, selected_mtcars$disp)

cat("qsec vs hp correlation:", round(correlation_qsec_hp, 3), "\n")
cat("qsec vs wt correlation:", round(correlation_qsec_wt, 3), "\n")
cat("qsec vs cyl correlation:", round(correlation_qsec_cyl, 3), "\n")
cat("qsec vs carb correlation:", round(correlation_qsec_carb, 3), "\n")
cat("qsec vs disp correlation:", round(correlation_qsec_disp, 3), "\n\n")


# Visual confirmation: Use pairs() function to display scatter plots for all pairs of variables
# This will show the relationships between qsec and each predictor, and also between predictors.
# We'll plot qsec against hp, wt, cyl, carb, and disp.
# Set a margin to ensure plot labels are visible
par(mar = c(5, 4, 4, 2) + 0.1) # Default margins are c(5, 4, 4, 2) + 0.1
pairs(selected_mtcars,
      main = "Scatter Plot Matrix of qsec and Predictors",
      pch = 16, # Solid circles for points
      col = "blue") # Blue color for points

# Build a multiple linear regression model using qsec as the dependent variable
# and hp, wt, cyl, carb, and disp variables as its predictors.
# The '.' in the formula means "use all other variables in the data frame as predictors"
# since we've already subsetted 'selected_mtcars' to only include our desired variables.
full_regression_model <- lm(qsec ~ ., data = selected_mtcars)

# Explore the resulting multiple linear regression model using summary()
cat("Summary of the first (full) multiple linear regression model:\n")
summary(full_regression_model)


# Step 3: Print current date and time and username
cat("\n--- Starting Second Model and Predictions ---\n")
print(Sys.time())
print(Sys.getenv("earth wind"))
cat("\n")


# Step 4: Refine the regression modeling and predict qsec values

# Build a second multiple linear regression model using qsec as the dependent variable
# and only the three most significant predictors as identified in the first model.
# Based on the p-values from the summary of 'full_regression_model',
# the three most significant predictors are typically those with the lowest p-values.
# From the problem hint, these are wt, carb, and disp.
second_regression_model <- lm(qsec ~ wt + carb + disp, data = selected_mtcars)

# Explore the resulting second multiple linear regression model.
cat("Summary of the second (refined) multiple linear regression model:\n")
summary(second_regression_model)

# Using your second multiple regression model, predict values of the qsec variable
# for the given new observations.
new_observations <- data.frame(
  wt = c(2, 3, 4, 5),
  carb = c(2, 4, 4, 6),
  disp = c(100, 200, 300, 400)
)

# Predict qsec values for the new observations
predicted_qsec_values <- predict(second_regression_model, newdata = new_observations)

# Display the new observations along with their predicted qsec values
cat("\nPredicted qsec values for new observations:\n")
predicted_df <- data.frame(new_observations, Predicted_qsec = predicted_qsec_values)
print(predicted_df)


# Step 5: Print current date and time and username after calculations
cat("\n--- All Calculations and Plots Completed ---\n")
print(Sys.time())
print(Sys.getenv("earth wind"))
