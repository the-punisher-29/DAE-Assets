# Set the working directory
work_dir <- "E:\\Pr\\R_Codes"
setwd(work_dir)

# Load the data
data <- read.csv("QNo4_Data.csv", header = TRUE, stringsAsFactors = TRUE)
str(data)

# (a) ANOVA
atest<- aov(Density ~ Temperature, data = data)
anova_table <- summary(atest)
print(anova_table)

# (b) Fisher LSD
# Fisher's LSD Test
library(agricolae)
lsd.test <- LSD.test(atest, "Temperature", alpha = 0.01, group = FALSE)
lsd.test

#Model_Adequacy
# Q-Q plot for normality check
qqnorm(resid(atest))
qqline(resid(atest), col = 'brown')

# Residual Analysis
plot(resid(atest), main = 'Residual vs. Run Order')
plot(x = predict(atest), y = resid(atest), main = 'Predicted vs. Residual')

# Equality of Variance Test
bartlett.test(Density ~ Temperature, data = data)