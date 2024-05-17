# Set working directory
setwd("E:\\Pr\\R_Codes\\Minor2")

# Load required libraries
library(agricolae)
library(DescTools)

##############################
#### Latin Square Design #####
##############################

# Read data for Latin Square Design
data.TV1 <- read.csv("Data4(TV).CSV", header = TRUE, stringsAsFactors = TRUE)

# Convert variables "Order" and "Operator" from numeric to factor
data.TV1$Order <- factor(data.TV1$Order)
data.TV1$Operator <- factor(data.TV1$Operator)

# ANOVA analysis
TV1.aov <- aov(Time ~ Order + Operator + Method, data = data.TV1)
summary(TV1.aov)

# Residual Analysis
par(mfrow = c(1, 2))
plot(x = predict(TV1.aov), y = resid(TV1.aov), main = 'Predicted vs. Residual')

# Tukey's Test
tukey <- TukeyHSD(TV1.aov, "Method", ordered = TRUE, conf.level = 0.99)
print(tukey)

# Fisher's LSD Test
df <- df.residual(TV1.aov)
MSerror <- deviance(TV1.aov) / df
lsd.test <- LSD.test(data.TV1$Time, data.TV1$Method, df, MSerror, alpha = 0.01,group = FALSE)
print(lsd.test)

# Dunnett's Test
dunnett <- DunnettTest(data.TV1$Time, data.TV1$Method, control = "B", conf.level = 0.99)
print(dunnett)

# Scheffe's Test
c.matrix <- cbind(c(1/3, 2/3, -1, 0), c(1/5, 2/5, 2/5, -1))
c.matrix
ScheffeTest(TV1.aov, which = "Method", contrasts = c.matrix, conf.level = 0.99)

# Multiple Comparisons using Orthogonal Contrast
c.matrix <- cbind(c(0.2887, 0.2887, 0.2887, -0.866),
                  c(-0.7071, 0.7071, 0, 0),
                  c(-0.4082, -0.4082, 0.8165, 0))
contrasts(data.TV1$Method) <- c.matrix
summary.aov(TV1.aov, split = list(Method = list("Contrast 1" = 1, "Contrast 2" = 2, "Contrast" = 3)))

##############################
#### Graeco-Latin-Square Design #####
##############################

# Read data for Graeco-Latin-Square Design
data.TV2 <- read.csv("Data5(TV).CSV", header = TRUE, stringsAsFactors = TRUE)

# Convert variables "Order" and "Operator" from numeric to factor
data.TV2$Order <- factor(data.TV2$Order)
data.TV2$Operator <- factor(data.TV2$Operator)

# ANOVA analysis
TV2.aov <- aov(Time ~ Order + Operator + Method + workplace, data = data.TV2)
summary(TV2.aov)

# Scheffe's Test for Method
c.matrix <- cbind(c(1/3, 2/3, -1, 0), c(1/5, 2/5, 2/5, -1))
ScheffeTest(TV2.aov, which = "Method", contrasts = c.matrix, conf.level = 0.99)

# Scheffe's Test for workplace
ScheffeTest(TV2.aov, which = "workplace", contrasts = c.matrix, conf.level = 0.99)
