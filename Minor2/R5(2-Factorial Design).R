# Set working directory
setwd("E:\\Pr\\R_Codes\\Minor2")

# Read the data from CSV files
Data.Fiber <- read.csv("Data6(Fiber).CSV", header = TRUE, stringsAsFactors = TRUE)
Data.Cloth <- read.csv("Data7(Cloth).CSV", header = TRUE, stringsAsFactors = TRUE)
Data.Chemical <- read.csv("Data8(ChemicalProcess).CSV", header = TRUE, stringsAsFactors = TRUE)

# Display data and internal structure
# Fiber data
Data.Fiber
str(Data.Fiber)

# Convert variables "Operator" and "Machine" to factors
Data.Fiber$Operator <- factor(Data.Fiber$Operator)
Data.Fiber$Machine <- factor(Data.Fiber$Machine)
str(Data.Fiber)

# ANOVA analysis of 2-factor Factorial Design with interaction
Data.aov <- aov(Strength ~ Operator * Machine, data = Data.Fiber)
summary(Data.aov)

# ANOVA analysis of 2-factor Factorial Design without interaction
Data2.aov <- aov(Strength ~ Operator + Machine, data = Data.Fiber)
summary(Data2.aov)

# Three Factor Factorial Design
# Cloth data
Data.Cloth
str(Data.Cloth)

# Convert variables to factors
Data.Cloth$CycleTime <- factor(Data.Cloth$CycleTime)
Data.Cloth$Operator <- factor(Data.Cloth$Operator)
Data.Cloth$Temparature <- factor(Data.Cloth$Temparature)
str(Data.Cloth)

# ANOVA analysis with interaction
Data.av <- aov(Score ~ CycleTime * Operator * Temparature, data = Data.Cloth)
summary(Data.av)

# Blocking in a factorial design
# Chemical process data
Data.Chemical
str(Data.Chemical)

# Convert variable to factor
Data.Chemical$Pressure <- factor(Data.Chemical$Pressure)
str(Data.Chemical)

# Residual analysis
Chemical.aov <- aov(Yield ~ Pressure * Temparature, data = Data.Chemical)
summary(Chemical.aov)
qqnorm(resid(Chemical.aov))
qqline(resid(Chemical.aov), col = 'red')
par(mfrow = c(1, 2))
plot(resid(Chemical.aov), main = 'Residual vs. Run Order')
plot(x = predict(Chemical.aov), y = resid(Chemical.aov), main = 'Predicted vs. Residual')

# Post-hoc tests
library(agricolae)
# Tukey's test
tukey <- TukeyHSD(Chemical.aov, "Temparature", ordered = TRUE, conf.level = 0.99)
tukey
# Fisher's LSD test
df <- df.residual(Chemical.aov)
MSerror <- deviance(Chemical.aov) / df
lsd.test <- LSD.test(Data.Chemical$Yield, Data.Chemical$Pressure, df, MSerror, alpha = 0.01,group=FALSE)
lsd.test
# Dunnett's test
library(DescTools)
g <- factor(Data.Chemical$Temparature)
dunnett <- DunnettTest(Data.Chemical$Yield, g, control = "Low", conf.level = 0.99)
dunnett
# Scheffe's test
con.matrix <- cbind(c(1/3, 1/3, -2/3), c(1, -1, 0))  # Contrast matrix
ScheffeTest(Chemical.aov, which = "Temparature", contrasts = NULL, conf.level = 0.99)  # For all pairwise comparisons
ScheffeTest(Chemical.aov, which = "Temparature", contrasts = con.matrix, conf.level = 0.99)
