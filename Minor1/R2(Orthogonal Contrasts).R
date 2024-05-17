# Set working directory
work_dir <- "E:/Pr/R_Codes/Minor1"
setwd(work_dir)

# Read CSV file
data.d <- read.csv("Data1(Drug Consumption).CSV", header = TRUE, stringsAsFactors = TRUE)

# Define contrast matrix
c.matrix <- cbind(c(0.2887, 0.2887, 0.2887, -0.866),
                  c(-0.7071, 0.7071, 0, 0),
                  c(-0.4082, -0.4082, 0.8165, 0))
c.matrix # Output contrast matrix

# Assign contrast matrix to levels of 'Drug' variable
contrasts(data.d$Drug) <- c.matrix

# Perform Ordinary ANOVA
drug.aov <- aov(Improvement ~ Drug, data = data.d)
summary(drug.aov) # Summary of Ordinary ANOVA

# Verify contrast matrix assignment
drug.aov$contrasts

# Summary of Augmented ANOVA
summary.aov(drug.aov, split = list(Drug = list("P vs. P'" = 1, "A1 vs. A2" = 2, "A vs. Non-A" = 3)))
