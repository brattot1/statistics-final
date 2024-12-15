GENDER AND POLAFFILIATION

CONTINGENCY TABLE _______________________________

library(dplyr)

# Load the CSV file
data <- read.csv("reponses.csv")

# Create a contingency table for gender and political affiliation
contingency_table <- table(data$`What.best.characterizes.your.political.affiliation.`,
                           data$`What.best.characterizes.your.gender.identity.`)

# Print the table
print(contingency_table)

CHI SQUARED TEST  _________________________

# Load the CSV file
data <- read.csv("reponses.csv")

# Create a contingency table for gender and political affiliation
contingency_table <- table(data$`What.best.characterizes.your.political.affiliation.`,
                           data$`What.best.characterizes.your.gender.identity.`)

# Perform a chi-squared test
chi_squared_result <- chisq.test(contingency_table)

# Print the results
print(chi_squared_result)


PATRIOTISM 1-10 AND DEMOCRATIC PARTY 1-10

MEAN ________________

# Load the CSV file
data <- read.csv("reponses.csv")

# Calculate the mean for feelings towards the Democratic Party
mean_democratic_party <- mean(data$`On.a.scale.of.1.10..how.do.you.feel.towards.the.Democratic.Party.`,
                              na.rm = TRUE)

# Calculate the mean for patriotism
mean_patriotism <- mean(data$`on.a.scale.from.1.10.how.patriotic.do.you.believe.you.are.`, 
                        na.rm = TRUE)

# Print the results
cat("Mean for feelings towards the Democratic Party:", mean_democratic_party, "\n")
cat("Mean for patriotism:", mean_patriotism, "\n")


STANDARD DEVIATION _______

# Load the CSV file
data <- read.csv("reponses.csv")

# Calculate the mean and standard deviation for feelings towards the Democratic Party
mean_democratic_party <- mean(data$`On.a.scale.of.1.10..how.do.you.feel.towards.the.Democratic.Party.`,
                              na.rm = TRUE)
sd_democratic_party <- sd(data$`On.a.scale.of.1.10..how.do.you.feel.towards.the.Democratic.Party.`,
                          na.rm = TRUE)

# Calculate the mean and standard deviation for patriotism
mean_patriotism <- mean(data$`on.a.scale.from.1.10.how.patriotic.do.you.believe.you.are.`,
                        na.rm = TRUE)
sd_patriotism <- sd(data$`on.a.scale.from.1.10.how.patriotic.do.you.believe.you.are.`,
                    na.rm = TRUE)

# Print the results
cat("Feelings towards the Democratic Party:\n")
cat("  Mean:", mean_democratic_party, "\n")
cat("  Standard Deviation:", sd_democratic_party, "\n\n")

cat("Patriotism:\n")
cat("  Mean:", mean_patriotism, "\n")
cat("  Standard Deviation:", sd_patriotism, "\n")


TABLE __________

# Load the CSV file
data <- read.csv("reponses.csv")

# Create a contingency table for gender and political affiliation
contingency_table <- table(data$`What.best.characterizes.your.gender.identity.`,
                           data$`What.best.characterizes.your.political.affiliation.`)

# Print the table
print(contingency_table)

# Optional: Add row and column names for clarity
rownames(contingency_table) <- make.names(unique(data$`What.best.characterizes.your.gender.identity.`))
colnames(contingency_table) <- make.names(unique(data$`What.best.characterizes.your.political.affiliation.`))

# Display the table again with custom row/column names
print(contingency_table)

SUMMARY _____________

# Load the CSV file
data <- read.csv("reponses.csv")

# Generate descriptive statistics for the entire dataset
summary(data)

# Optional: Generate descriptive statistics for specific variables
summary(data$`On.a.scale.of.1.10..how.do.you.feel.towards.the.Democratic.Party.`)
summary(data$`on.a.scale.from.1.10.how.patriotic.do.you.believe.you.are.`)


BOXPLOT ______

# Load the CSV file
data <- read.csv("reponses.csv")

# Boxplot for feelings towards the Democratic Party by gender
boxplot(data$`On.a.scale.of.1.10..how.do.you.feel.towards.the.Democratic.Party.` ~ data$`What.best.characterizes.your.gender.identity.`,
        main = "Feelings Towards the Democratic Party by Gender",
        xlab = "Gender",
        ylab = "Feelings Towards the Democratic Party",
        col = c("lightblue", "lightpink"),  # Optional color for each gender
        border = "black")

# Boxplot for feelings towards the Democratic Party by political affiliation
boxplot(data$`On.a.scale.of.1.10..how.do.you.feel.towards.the.Democratic.Party.` ~ data$`What.best.characterizes.your.political.affiliation.`,
        main = "Feelings Towards the Democratic Party by Political Affiliation",
        xlab = "Political Affiliation",
        ylab = "Feelings Towards the Democratic Party",
        col = c("lightgreen", "lightcoral"),  # Optional color for each affiliation
        border = "black")

# Boxplot for patriotism by gender
boxplot(data$`on.a.scale.from.1.10.how.patriotic.do.you.believe.you.are.` ~ data$`What.best.characterizes.your.gender.identity.`,
        main = "Patriotism by Gender",
        xlab = "Gender",
        ylab = "Patriotism",
        col = c("lightblue", "lightpink"),
        border = "black")

# Boxplot for patriotism by political affiliation
boxplot(data$`on.a.scale.from.1.10.how.patriotic.do.you.believe.you.are.` ~ data$`What.best.characterizes.your.political.affiliation.`,
        main = "Patriotism by Political Affiliation",
        xlab = "Political Affiliation",
        ylab = "Patriotism",
        col = c("lightgreen", "lightcoral"),
        border = "black")

Scatterplot __________
install.packages("viridis")

# Load necessary libraries
library(ggplot2)
library(viridis)

# Load the CSV file
data <- read.csv("reponses.csv")

# Convert gender and political affiliation columns to factors (categorical)
data$Gender <- factor(data$`What.best.characterizes.your.gender.identity.`)
data$Political_Affiliation <- factor(data$`What.best.characterizes.your.political.affiliation.`)

# Use the 'viridis' color scale for dynamic and continuous color generation
gender_colors <- scale_color_viridis(discrete = TRUE, option = "D")
political_colors <- scale_color_viridis(discrete = TRUE, option = "D")

# Plot for feelings towards the Democratic Party by gender
ggplot(data, aes(x = Gender, 
                 y = `On.a.scale.of.1.10..how.do.you.feel.towards.the.Democratic.Party..`)) +
  geom_jitter(aes(color = Gender), width = 0.1, height = 0, size = 3) +
  labs(title = "Feelings Towards the Democratic Party by Gender",
       x = "Gender", 
       y = "Feelings Towards the Democratic Party") +
  gender_colors +
  theme_minimal()

# Plot for feelings towards the Republican Party by gender
ggplot(data, aes(x = Gender, 
                 y = `On.a.scale.from.1.10.how.do.you.feel.towards.the.GOP.or.the.Republican.party.`)) +
  geom_jitter(aes(color = Gender), width = 0.1, height = 0, size = 3) +
  labs(title = "Feelings Towards the Republican Party by Gender",
       x = "Gender", 
       y = "Feelings Towards the Republican Party") +
  gender_colors +
  theme_minimal()

# Plot for patriotism by gender
ggplot(data, aes(x = Gender, 
                 y = `on.a.scale.from.1.10.how.patriotic.do.you.believe.you.are.`)) +
  geom_jitter(aes(color = Gender), width = 0.1, height = 0, size = 3) +
  labs(title = "Patriotism by Gender",
       x = "Gender", 
       y = "Patriotism") +
  gender_colors +
  theme_minimal()

# Plot for feelings towards the Democratic Party by political affiliation
ggplot(data, aes(x = Political_Affiliation, 
                 y = `On.a.scale.of.1.10..how.do.you.feel.towards.the.Democratic.Party..`)) +
  geom_jitter(aes(color = Political_Affiliation), width = 0.1, height = 0, size = 3) +
  labs(title = "Feelings Towards the Democratic Party by Political Affiliation",
       x = "Political Affiliation", 
       y = "Feelings Towards the Democratic Party") +
  political_colors +
  theme_minimal()

# Plot for feelings towards the Republican Party by political affiliation
ggplot(data, aes(x = Political_Affiliation, 
                 y = `On.a.scale.from.1.10.how.do.you.feel.towards.the.GOP.or.the.Republican.party.`)) +
  geom_jitter(aes(color = Political_Affiliation), width = 0.1, height = 0, size = 3) +
  labs(title = "Feelings Towards the Republican Party by Political Affiliation",
       x = "Political Affiliation", 
       y = "Feelings Towards the Republican Party") +
  political_colors +
  theme_minimal()

# Plot for patriotism by political affiliation
ggplot(data, aes(x = Political_Affiliation, 
                 y = `on.a.scale.from.1.10.how.patriotic.do.you.believe.you.are.`)) +
  geom_jitter(aes(color = Political_Affiliation), width = 0.1, height = 0, size = 3) +
  labs(title = "Patriotism by Political Affiliation",
       x = "Political Affiliation", 
       y = "Patriotism") +
  political_colors +
  theme_minimal()

RESIDUAL PLOT _____
# Load necessary library
library(ggplot2)

# Load the CSV file
data <- read.csv("reponses.csv")

# Remove rows with missing values in relevant columns
data_clean <- na.omit(data[, c("What.best.characterizes.your.gender.identity.", 
                               "On.a.scale.of.1.10..how.do.you.feel.towards.the.Democratic.Party..")])

# Convert gender column to factor (categorical)
data_clean$Gender <- factor(data_clean$`What.best.characterizes.your.gender.identity.`)

# Fit a linear regression model (Feelings Towards the Democratic Party ~ Gender)
model <- lm(`On.a.scale.of.1.10..how.do.you.feel.towards.the.Democratic.Party..` ~ Gender, data = data_clean)

# Add the residuals to the cleaned data frame
data_clean$residuals <- resid(model)

# Create a residual plot
ggplot(data_clean, aes(x = fitted(model), y = residuals)) +
  geom_point(aes(color = Gender), size = 3) +  # Scatter plot of residuals
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add horizontal line at 0
  labs(title = "Residual Plot: Feelings Towards the Democratic Party by Gender",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()
