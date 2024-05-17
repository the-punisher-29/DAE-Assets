# Load necessary library
library(ggplot2)

# Create data frame
data <- data.frame(
  MixingTechnique = rep(1:4, each = 4),
  TensileStrength = c(3129, 3000, 2865, 2890, 3200, 3300, 2975, 3150, 2800, 2900, 2985, 3050, 2600, 2700, 2600, 2765)
)

# Create scatter plot
ggplot(data, aes(x = MixingTechnique, y = TensileStrength)) + geom_point() +labs(x = "Mixing Technique", y = "Tensile Strength (lb/in^2)")
