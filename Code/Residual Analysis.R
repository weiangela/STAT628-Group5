library(car)
library(ggplot2)
library(ggResidpanel)
library(lmtest)

bodyFat <- read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/Data/BodyFat_cleaned.csv")
best_bf_lm <- lm(BODYFAT ~ ABDOMEN + WEIGHT, data = bodyFat)

# Create data frame from residuals
residuals_data <- data.frame(residuals = residuals(best_bf_lm))

# QQ Plot of Residuals
ggplot(residuals_data, aes(sample = residuals)) +
  stat_qq(aes(color = residuals)) +                   
  stat_qq_line(color = "blue") +                      
  scale_color_gradient(low = "blue", high = "red") + 
  ggtitle("QQ Plot of Residuals") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal() +                                   
  theme(legend.position = "none")   

#Shapiro-Wilk normality test
shapiro.test(residuals(best_bf_lm))

#collinearity test
vif_values <- vif(best_bf_lm,)
print(vif_values)

#White test
white_test <- bptest(best_bf_lm)
print(white_test)
# Creating a data frame for plotting
plot_data <- data.frame(
  Fitted = fitted(best_bf_lm),
  Residuals = residuals(best_bf_lm)
)

# Using ggplot2 to make the plot
ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point(aes(color = Residuals), alpha = 0.7) +   # Points with color based on residuals
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # Horizontal line at zero
  ggtitle("Residuals vs. Fitted") +
  xlab("Fitted values") +
  ylab("Residuals") +
  theme_minimal() +                                   # Clean theme
  scale_color_gradient2(midpoint = 0, low = "blue", mid = "grey", high = "red") + # Gradient color
  theme(legend.position = "none")                     # Hide the legend
