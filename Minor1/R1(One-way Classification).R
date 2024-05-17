# Set working directory and read data
setwd("E:/Pr/R_Codes/Minor1")
data.br <- read.csv("Data2(Broker).CSV", header = TRUE, stringsAsFactors = TRUE)

# Display data structure
str(data.br)

# Box plot of the data
boxplot(Share_Price ~ Broker, data = data.br)

# ANOVA
br.aov <- aov(Share_Price ~ Broker, data = data.br)
summary(br.aov)

# Fisher's LSD Test
library(agricolae)
lsd.test <- LSD.test(br.aov, "Broker", alpha = 0.01, group = TRUE)
lsd.test

# Tukey's HSD Test
hsd <- HSD.test(br.aov, "Broker", group = TRUE)
hsd

# Alternative Tukey's test
tukey <- TukeyHSD(br.aov, ordered = TRUE, conf.level = 0.99)
tukey

# Dunnett's test
library(DescTools)
g <- factor(rep(1:5, c(6, 6, 6, 6, 6)), labels = c("B1", "B2", "B3", "B4", "B5"))
dunnett <- DunnettTest(data.br$Share_Price, g, control = "B2")
dunnett

# Scheffe's Test
c.matrix <- cbind(c(1/3, 2/3, -1, 0, 0), c(1/5, 2/5, 2/5, -1, 0))
c1.matrix <- cbind(c(1/6, 2/6, -1, 2/6, 1/6), c(1/5, 2/5, 2/5, -1/2, -1/2))
c.matrix
scheffe_all <- ScheffeTest(br.aov, which = "Broker", contrasts = NULL, conf.level = 0.99)
scheffe_c <- ScheffeTest(br.aov, which = "Broker", contrasts = c.matrix, conf.level = 0.95)
scheffe_c1 <- ScheffeTest(br.aov, which = "Broker", contrasts = c1.matrix, conf.level = 0.99)
scheffe_all
scheffe_c
scheffe_c1


# Model Adequacy Checking
#Normal Q-Q Plot
qqnorm(resid(br.aov))
qqline(resid(br.aov), col = 'red')

# Residual Analysis
plot(resid(br.aov), main = 'Residual vs. Run Order')
plot(x = predict(br.aov), y = resid(br.aov), main = 'Predicted vs. Residual')

# Equality of Variance Test
bartlett.test(Share_Price ~ Broker, data = data.br)
