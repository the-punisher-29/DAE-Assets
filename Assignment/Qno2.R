# Set the working directory
work_dir <- "E:\\Pr\\R_Codes"
setwd(work_dir)

# Load the data
data2 <- read.csv("QNo_2_Data.csv", header = TRUE, stringsAsFactors = TRUE)
str(data2)

# (a) ANOVA
atest<- aov(Observations ~ Treatments, data = data2)
anova_table <- summary(atest)
print(anova_table)

# Q-Q plot for normality check
qqnorm(resid(atest))
qqline(resid(atest), col = 'violet')

#Transformed data analysis
# Load the data
data3 <- read.csv("Qno2_Data(log).csv", header = TRUE, stringsAsFactors = TRUE)
str(data3)

# (a) ANOVA
at2<- aov(Observations ~ Treatments, data = data3)
anova_table1 <- summary(at2)
print(anova_table1)

#Model_Adequacy for transformed data
# Q-Q plot for normality check
qqnorm(resid(at2))
qqline(resid(at2), col = 'yellow')

# Residual Analysis
plot(resid(at2), main = 'Residual vs. Run Order')
plot(x = predict(at2), y = resid(at2), main = 'Predicted vs. Residual')

# Equality of Variance Test
bartlett.test(Observations ~ Treatments, data = data3)