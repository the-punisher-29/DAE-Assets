# Set the working directory
work_dir <- "E:\\Pr\\R_Codes"
setwd(work_dir)

# Load the data
data <- read.csv("QNO1_DATA.csv", header = TRUE, stringsAsFactors = TRUE)
str(data)

# (a) ANOVA
atest<- aov(Bolt ~ Chemical, data = data)
anova_table <- summary(atest)
print(anova_table)

# (b) Fisher LSD
# Fisher's LSD Test
library(agricolae)
lsd.test <- LSD.test(atest, "Chemical", alpha = 0.2, group = FALSE)
lsd.test

# (c)Tukey's HSD Test
hsd.test <- HSD.test(atest, "Chemical",alpha=0.055, group = FALSE)
hsd.test

# (e) Dunnett Test
library(DescTools)
g <- factor(rep(1:4, c(5, 5, 5, 5)), labels = c("C1", "C2", "C3", "C4"))
dunnett <- DunnettTest(data$Bolt, g, control = "C2",conf.level = 0.95)
dunnett

# (f) Scheffe test
c.matrix <- matrix(c(1, -1/3, -1/3, -1/3))
c.matrix
#contrasts(data$Chemical)
scheffe <- ScheffeTest(atest, which = "Chemical", contrasts = c.matrix, conf.level = 0.95)
scheffe 
