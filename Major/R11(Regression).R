# Set the working directory
work2_dir <- "E:\\Pr\\R_Codes\\Major"
setwd(work2_dir)

# Read the CSV file into a dataframe
data.brake <- read.csv("Data11(Compression).CSV", header = TRUE, stringsAsFactors = TRUE)

# Display the dataframe
print(data.brake)

# Check the structure of the dataframe
str(data.brake)

# Fit a linear regression model
mod <- lm(BrakeHorsepower ~ rpm + RoadOctaneNumber + Compression, data = data.brake)

# Summary of the model
summary(mod)

# ANOVA table
anova(mod)

# Point estimate of the mean response for a given x = x0
x0 <- c(1, 1900, 89, 99)
co1 <- coef(mod)
y0 <- sum(x0 * co1)
y0 # Point estimate y0 = x0 * beta_hat

# Residual analysis
# Standarized residual
rstandard(mod)
# Studentized residual
rstudent(mod)

# Plotting residuals
par(mfrow = c(1, 1))
plot(rstandard(mod), ylab = "Standarized residual")
plot(rstudent(mod), ylab = "Studentized residual")

# PRESS (Predicted Residual Sum of Squares) calculation
x <- model.matrix(mod)
PRESS_res <- summary(mod)$res / (1 - hat(x))
print(PRESS_res)

# Plotting PRESS residuals
plot(PRESS_res, ylab = "PRESS residual")
PRESS <- sum(PRESS_res^2)
PRESS
# R-squared Prediction
SS_T <- sum(anova(mod)$"Sum Sq")
pred.r.squared <- 1 - PRESS / SS_T
pred.r.squared

# Q-Q plot
qqnorm(summary(mod)$res, ylab = "Ordinary Residuals")
qqline(summary(mod)$res)

# Checking non-constant variance
plot(mod$fit, mod$res, xlab = "Fitted", ylab = "Residual")
abline(h = 0)
