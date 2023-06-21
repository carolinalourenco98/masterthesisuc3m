######################
### Dummy Analysis ###
######################

library(ggplot2)
library(car)


# install the necessary packages if not already installed
if (!require("ggridges")) install.packages("ggridges")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(ggplot2)
library(ggridges)
library(dplyr)


###############################
### Put everything together ###
###############################

Combined_overall <- bind_rows(combined_results_PT, combined_results_DE)


myanova <- aov(Sum_Value ~ nationality * stimulus, data = Combined_overall)
summary(myanova)



# Check assumptions for ANOVA model

# 1. Independence: No specific code required as independence assumption is typically ensured during data collection or study design.

# 2. Normality of Residuals
# Obtain residuals from the ANOVA model
residuals <- residuals(myanova)

# Visual inspection: Plot histogram and QQ plot of residuals
par(mfrow = c(1, 2))
hist(residuals, main = "Histogram of Residuals")
qqnorm(residuals, main = "Normal Q-Q Plot")
qqline(residuals) 

# Quantitative test: Shapiro-Wilk test for normality
shapiro.test(residuals) 

# 3. Homogeneity of Variance (Homoscedasticity)
# Visual inspection: Plot residuals against fitted values
plot(fitted(myanova), residuals, main = "Residuals vs. Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# Quantitative test: Levene's test for homogeneity of variance
leveneTest(residuals ~ interaction(nationality, stimulus), data = Combined_overall)

# 4. Independence of Errors: No specific test available in this context. It is typically ensured during data collection or study design.


# Apply logarithmic transformation to the Sum_Value variable
# Combined_overall$log_Sum_Value <- log(Combined_overall$Sum_Value)

# Perform the Shapiro-Wilk test on the transformed data
# shapiro.test(Combined_overall$log_Sum_Value)

myanova <- aov(Combined_overall$Sum_Value ~ nationality * stimulus, data = Combined_overall)
summary(myanova)

interaction.plot(x.factor = Combined_overall$nationality, trace.factor = Combined_overall$stimulus, response = Combined_overall$Sum_Value, type = "b", legend = TRUE)

# Adjust the margins of the plot (bottom, left, top, right)
par(mar = c(5.1, 4.1, 4.1, 8.1))

interaction.plot(x.factor = Combined_overall$nationality,
                 trace.factor = Combined_overall$stimulus,
                 response = Combined_overall$Sum_Value,
                 type = "b",
                   # We will manually add the legend
                 col = c("red", "blue", "green"),
                 ylab = "Attitude Score",
                 xlab = "Nationality")


# Create the plot using ggplot2
plot <- ggplot(Combined_overall, aes(x = nationality, y = Sum_Value, fill = stimulus)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Nationality", y = "Sum Value", fill = "Stimulus") +
  ggtitle("Interaction between Nationality and Stimulus") +
  theme_minimal()

print(plot)


plot <- ggplot(Combined_overall, aes(x = stimulus, y = Sum_Value, fill = stimulus)) +
  geom_point(
    aes(color = stimulus),
    position = position_jitter(height = 0),
    size = 1,
    alpha = 0.9
  ) +
  geom_boxplot(
    aes(group = stimulus),
    width = 0.5,
    outlier.shape = NA,
    coef = 0,
    color = "black"
  ) +
  labs(x = "Stimulus", y = "Sum Value", fill = "Stimulus") +
  ggtitle("Interaction between Nationality and Stimulus") +
  theme_minimal() +
  facet_wrap(~ nationality) +
  ylim(1, 100)  # Set the y-axis limits

print(plot)





