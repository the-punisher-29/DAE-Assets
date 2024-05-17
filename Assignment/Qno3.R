# Set the working directory
work_dir <- "E:\\Pr\\R_Codes"
setwd(work_dir)

# Load the data
data <- read.csv("QNo_3_Data.csv", header = TRUE, stringsAsFactors = TRUE)
str(data)

# (a) ANOVA
atest<- aov(Response_Time ~ Circuit_Type, data = data)
anova_table <- summary(atest)
print(anova_table)

# (c)Tukey's HSD Test
library(agricolae)
hsd.test <- HSD.test(atest, "Circuit_Type",alpha=0.1, group = FALSE)
hsd.test

#Model_Adequacy for transformed data
# Q-Q plot for normality check
qqnorm(resid(at2))
qqline(resid(at2), col = 'green')

# Residual Analysis
plot(resid(atest), main = 'Residual vs. Run Order')
plot(x = predict(atest), y = resid(atest), main = 'Predicted vs. Residual')

# Equality of Variance Test
bartlett.test(Response_Time ~ Circuit_Type, data = data)