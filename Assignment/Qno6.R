# Set the working directory
work_dir <- "E:\\Pr\\R_Codes"
setwd(work_dir)

# Load the data
data <- read.csv("Qno_6_Data.csv", header = TRUE, stringsAsFactors = TRUE)
str(data)

# (a) ANOVA
atest<- aov(Tensile_Strength ~ Mixing_Technique, data = data)
anova_table <- summary(atest)
print(anova_table)

# (b) Fisher LSD
# Fisher's LSD Test
library(agricolae)
lsd.test <- LSD.test(atest, "Mixing_Technique", alpha = 0.05, group = FALSE)
lsd.test

#(c) Q-Q plot for normality check
qqnorm(resid(atest))
qqline(resid(atest), col = 'blue')

#(d)
plot(x = predict(atest), y = resid(atest), main = 'Predicted vs. Residual')

#(e)
# Load necessary library
library(ggplot2)
# Create scatter plot
ggplot(data, aes(x = Mixing_Technique, y = Tensile_Strength)) +
  geom_point() +
  labs(x = "Mixing_Technique", y = "Tensile_Strength (lb/in^2)") +
  theme_minimal()


#(f)
library(agricolae)
hsd.test <- HSD.test(atest, "Mixing_Technique",alpha=0.05, group = FALSE)
hsd.test

#(g)
