# Set working directory
setwd("E:\\Pr\\R_Codes\\Minor2")

# Define the lifetime data matrix
Lifetime <- matrix(c(7.037, 6.376, 14.707, 15.219, 11.635, 12.089, 17.273,
                     17.815, 10.403, 10.151, 4.368, 4.089, 9.360, 9.253,
                     13.440, 12.923, 8.561, 8.951, 16.867, 17.052, 13.876,
                     13.658, 19.824, 19.639, 11.846, 12.337, 6.125, 5.904,
                     11.190, 10.935, 15.653, 15.053),
                   byrow = TRUE, ncol = 2)
dimnames(Lifetime) <- list(c("(1)","a","b","ab","c","ac","bc","abc",
                             "d","ad", "bd","abd","cd","acd","bcd","abcd"),
                           c("Rep1","Rep2"))
Lifetime
# Define the factors
A <- rep(c(-1, 1), 8)
B <- rep(c(-1, -1, 1, 1), 4)
C <- c(rep(-1, 4), rep(1, 4), rep(-1, 4), rep(1, 4))
D <- c(rep(-1, 8), rep(1, 8))
Total <- apply(Lifetime, 1, sum)

# Display given data
cbind(A, B, C, D, Lifetime, Total)

# Define interaction matrix
I <- c(rep(1, 16))
AB <- A * B
AC <- A * C
BC <- B * C
ABC <- A * B * C
AD <- A * D
BD <- B * D
ABD <- A * B * D
CD <- C * D
ACD <- A * C * D
BCD <- B * C * D
ABCD <- A * B * C * D
Design.matrix <- cbind(I, A, B, AB, C, AC, BC, ABC, D, AD, BD, ABD, CD, ACD, BCD, ABCD, Total)
Design.matrix
# Calculate interaction and main effects
n <- 2  # Replication
Feff <- t(Total) %*% cbind(A, B, AB, C, AC, BC, ABC, D, AD, BD, ABD, CD, ACD, BCD, ABCD) / (8 * n)
Ieff <- t(Total) %*% cbind(I) / (16 * n)
eff <- cbind(Ieff, Feff)
Summary <- rbind(cbind(I, A, B, AB, C, AC, BC, ABC, D, AD, BD, ABD, CD, ACD, BCD, ABCD), eff)
dimnames(Summary)[[1]] <- c(dimnames(Lifetime)[[1]], "Effect")

# Display summary of effect estimates
Summary

# ANOVA Model
Life <- c(t(Lifetime))
Af <- rep(as.factor(A), rep(2, 16))
Bf <- rep(as.factor(B), rep(2, 16))
Cf <- rep(as.factor(C), rep(2, 16))
Df <- rep(as.factor(D), rep(2, 16))
data.Life <- data.frame(Af, Bf, Cf, Df, Life)
data.Life
Life.av <- aov(Life ~ Af * Bf * Cf * Df, data = data.Life)
summary(Life.av)

# Load the 'unrepx' library
library(unrepx)

# Calculate pilot effects
G <- Design.matrix[, 17]
pilotEff <- yates(G, labels = c("A", "B", "C", "D"))
pilotEff/2

# Plotting
hnplot(pilotEff, ID = 0)
qqnorm(pilotEff)  # Uncomment if needed
