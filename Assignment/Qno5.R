# Set the working directory
work_dir <- "E:\\Pr\\R_Codes"
setwd(work_dir)

# Load the data
data <- read.csv("Qno_5_Data.csv", header = TRUE, stringsAsFactors = TRUE)
str(data)

# (a) ANOVA
atest<- aov(Observations ~ CWP, data = data)
anova_table <- summary(atest)
print(anova_table)

#Fisher LSD
# Fisher's LSD Test
library(agricolae)
lsd.test <- LSD.test(atest, "CWP", alpha = 0.01, group = FALSE)
lsd.test

#Model_Adequacy
# Q-Q plot for normality check
qqnorm(resid(atest))
qqline(resid(atest), col = 'red')
# Residual Analysis
plot(resid(atest), main = 'Residual vs. Run Order')
plot(x = predict(atest), y = resid(atest), main = 'Predicted vs. Residual')
# Equality of Variance Test
bartlett.test(Observations ~ CWP, data = data)

# Dunnett Test
library(DescTools)
g <- factor(rep(1:5, c(5, 5, 5, 5, 5)), labels = c("15(C1)", "20(C2)", "25(C3)", "30(C4)","35(C5)"))
dunnett <- DunnettTest(data$Observations, g, control = "30(C4)",conf.level=0.95)
dunnett
