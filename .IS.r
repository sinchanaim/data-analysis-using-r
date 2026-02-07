# Extract the numeric portion of the registration number (assuming your reg no is "22BBTCD020")
reg_no <- "22BBTCD020"
unique_identifier <- as.integer(substr(reg_no, nchar(reg_no) - 2, nchar(reg_no)))

# Define a function to generate a dataset based on a seed
generate_dataset <- function(seed) {
  set.seed(seed)  # Set the seed for reproducibility
  # Generate a dataset with random numbers
  data <- data.frame(
    random_int = sample(1:100, 100, replace = TRUE),
    random_float = runif(100, min = 1, max = 10)
  )
  return(data)
}

# Generate datasets using the unique identifier and its double as seeds
set1 <- generate_dataset(unique_identifier)
set2 <- generate_dataset(unique_identifier * 2)

# Print the first few rows of each dataset
head(set1)
head(set2)
# Calculate mean, variance, and standard deviation for each dataset
set1_mean <- mean(set1$random_int)
set1_var <- var(set1$random_int)
set1_sd <- sd(set1$random_int)

set2_mean <- mean(set2$random_int)
set2_var <- var(set2$random_int)
set2_sd <- sd(set2$random_int)

# Display the results
cat("Set 1 Mean:", set1_mean, "\n")
cat("Set 1 Variance:", set1_var, "\n")
cat("Set 1 Standard Deviation:", set1_sd, "\n")

cat("\nSet 2 Mean:", set2_mean, "\n")
cat("Set 2 Variance:", set2_var, "\n")
cat("Set 2 Standard Deviation:", set2_sd, "\n")

# Load necessary libraries
library(ggplot2)  # For plotting

# Set seed for reproducibility
set.seed(123)

# Generate a sample dataset (assuming unique identifier is 42)
unique_identifier <- 42
set <- data.frame(
  binomial = rbinom(1000, size = 10, prob = 0.5),  # Binomial distribution
  poisson = rpois(1000, lambda = 3),  # Poisson distribution
  exponential = rexp(1000, rate = 0.1),  # Exponential distribution
  normal = rnorm(1000, mean = 5, sd = 2),  # Normal distribution
  chi_square = rchisq(1000, df = 5)  # Chi-Square distribution
)

# Plot histograms for each distribution
ggplot(data = set, aes(x = binomial)) + geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Binomial Distribution") + theme_minimal()

ggplot(data = set, aes(x = poisson)) + geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Poisson Distribution") + theme_minimal()

ggplot(data = set, aes(x = exponential)) + geom_histogram(binwidth = 0.5, fill = "salmon", color = "black") +
  labs(title = "Exponential Distribution") + theme_minimal()

ggplot(data = set, aes(x = normal)) + geom_histogram(binwidth = 0.5, fill = "yellow", color = "black") +
  labs(title = "Normal Distribution") + theme_minimal()

ggplot(data = set, aes(x = chi_square)) + geom_histogram(binwidth = 1, fill = "red", color = "black") +
  labs(title = "Chi-Square Distribution") + theme_minimal()

# Generate a sample dataset (assuming unique identifier is 42)
unique_identifier <- 42
set <- rnorm(100, mean = 50, sd = 10)  # Generating a sample dataset (normal distribution)

# Calculate mean and standard deviation of the dataset
mean_set <- mean(set)
sd_set <- sd(set)

# Calculate sample size and degrees of freedom
n <- length(set)
df <- n - 1

# Calculate t-value for 95% confidence interval (two-tailed)
t_value <- qt(0.975, df)

# Calculate standard error of the mean
se <- sd_set / sqrt(n)

# Calculate margin of error
margin_of_error <- t_value * se

# Calculate lower and upper bounds of the confidence interval
lower_bound <- mean_set - margin_of_error
upper_bound <- mean_set + margin_of_error

# Print the results
cat("Mean:", mean_set, "\n")
cat("Standard Deviation:", sd_set, "\n")
cat("Sample Size:", n, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("t-value (95% CI):", t_value, "\n")
cat("Standard Error of Mean:", se, "\n")
cat("Margin of Error:", margin_of_error, "\n")
cat("95% Confidence Interval:", lower_bound, "to", upper_bound, "\n")


# Generate datasets using the unique identifier and its double as seeds
set1 <- generate_dataset(unique_identifier)
set2 <- generate_dataset(unique_identifier * 2)

# Conduct independent t-test
t_test_result <- t.test(set1$random_int, set2$random_int)

# Print the t-test result
print(t_test_result)

# Interpret the p-value
if (t_test_result$p.value < 0.05) {
  cat("The p-value is less than 0.05, indicating strong evidence against the null hypothesis.")
  cat("We reject the null hypothesis and conclude that there is a significant difference between the means of the two datasets.")
} else {
  cat("The p-value is greater than or equal to 0.05, indicating weak evidence against the null hypothesis.")
  cat("We fail to reject the null hypothesis and conclude that there is no significant difference between the means of the two datasets.")
}




