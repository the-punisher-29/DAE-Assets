# Set working directory
setwd("E:\\Pr\\R_Codes\\Minor2")

# Load required library
library(agricolae)

# Read data
data.Jet <- read.csv("Data3(Jet).CSV", header = TRUE, stringsAsFactors = TRUE)

# Display data and structure
print(data.Jet)
str(data.Jet)

# Convert variable "Jet_Velocity" from numeric to factor
data.Jet$Jet_Velocity <- factor(data.Jet$Jet_Velocity)

# Check the structure after conversion
str(data.Jet)

# ANOVA analysis of RCBD
Jet.aov <- aov(Shape ~ Nozzle_Design + Jet_Velocity, data = data.Jet)
summary(Jet.aov)

# ANOVA analysis of CRD (One-way classification)
Jet1.aov <- aov(Shape ~ Nozzle_Design, data = data.Jet)
summary(Jet1.aov)

# Fisher's LSD Test
lsd.test <- LSD.test(Jet.aov, "Nozzle_Design", alpha = 0.01, group = FALSE)
print(lsd.test)

# Tukey's HSD Test
tukey <- TukeyHSD(Jet.aov, ordered = TRUE, conf.level = 0.99)
print(tukey)
