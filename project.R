#############################Stage1(Data_Collection and Storage)########################################################
options(max.print = 9999)  # Adjust the value as needed
#Comprehension Score (RCS) data
RCS <- matrix(c(62, 38, 82, 23, 71, 47, 93, 13, 58, 18, 77, 27, 52, 42, 88, 13,
         47, 18, 63, 27, 52, 42, 88, 13, 48, 23, 67, 33, 52, 42, 88, 13),byrow=TRUE,ncol=1)

#dimension_names for a five-factor design
dimnames(RCS) <- list(c("(1)", "a", "b", "c", "d", "e", "ab", "ac", "ad", "ae", "bc", "bd", "be", "cd", "ce", "de", 
                             "abc", "abd", "abe", "acd", "ace", "ade", "bcd", "bce", "bde", "cde", 
                             "abcd", "abce", "abde", "acde", "bcde", "abcde"),
                           c("rcs"))
A <- rep(c(-1, 1), 16)
B <- rep(c(-1, -1, 1, 1), 8)
C <- rep(c(-1, -1, -1, -1, 1, 1, 1, 1), 4)
D <- rep(c(-1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1), 2)
E <- c(rep(-1, 16), rep(1, 16))
data.rcs <- data.frame(A, B, C, D, E, RCS)
data.rcs

###########design matrix #########################
I <- c(rep(1, 32))
AB <- A * B
AC <- A * C
AD <- A * D
AE <- A * E
BC <- B * C
BD <- B * D
BE <- B * E
CD <- C * D
CE <- C * E
DE <- D * E
ABC <- A * B * C
ABD <- A * B * D
ABE <- A * B * E
ACD <- A * C * D
ACE <- A * C * E
ADE <- A * D * E
BCD <- B * C * D
BCE <- B * C * E
BDE <- B * D * E
CDE <- C * D * E
ABCD <- A * B * C * D
ABCE <- A * B * C * E
ABDE <- A * B * D * E
ACDE <- A * C * D * E
BCDE <- B * C * D * E
ABCDE <- A * B * C * D * E
Design.matrix <- cbind(I, A, B, C, D, E, AB, AC, AD, AE, BC, BD, BE, CD, CE, DE,
                       ABC, ABD, ABE, ACD, ACE, ADE, BCD, BCE, BDE, CDE,
                       ABCD, ABCE, ABDE, ACDE, BCDE, ABCDE, RCS)
Design.matrix

#################################Stage2(Pre-Analysis)######################################################
#half normal plot
#doing priori analysis to figure out the important factors out of five chosen factors 
library(unrepx)
G=Design.matrix[,33]
pilotEff = yates(G, labels = c("A","B","C", "D", "E")) 
pilotEff
hnplot(pilotEff,ID=0)

################## Interaction and Main effects  ########################
n = 1 ##Replication
Feff = t(RCS) %*% cbind(A, B, AB, C, AC, BC, ABC, D, AD, BD, ABD, CD, ACD, BCD, ABCD, E, AE, BE, ABE, CE, ACE, BCE, ABCE, DE, ADE, BDE, ABDE, CDE, ACDE, BCDE, ABCDE)/(16*n)
Ieff=t(RCS) %*% cbind(I)/(32*n)
eff=cbind(Ieff,Feff)
Summary = rbind( cbind(I,A, B, AB, C, AC, BC, ABC, D, AD, BD, ABD, CD, ACD, BCD, ABCD, E, AE, BE, ABE, CE, ACE, BCE, ABCE, DE, ADE, BDE, ABDE, CDE, ACDE, BCDE, ABCDE),eff)
dimnames(Summary)[[1]] = c(dimnames(RCS)[[1]],"Effect")
Summary

lr=lm(RCS ~ A*B*C, data=data.rcs)
summary(lr)
library(DoE.base)
library(FrF2)
MEPlot(lr)
IAPlot(lr)

##########################  Regression ###########################
mod=lm(RCS ~ A+B+C+A:B+A:B:C, data=data.rcs)
summary(mod)
#normal and residual plot
residuals=mod$res
residuals
qqnorm(mod$res,ylab="Ordinary Residuals")
qqline(mod$res)

##  PRESS ##
x=model.matrix(mod)
PRESS_res=summary(mod)$res/(1-hat(x))
print(PRESS_res)

##############  Plotting of PRESS Residuals #########################
par(mfrow=c(1,1))
plot(PRESS_res,ylab="PRESS residual")
PRESS=sum(PRESS_res^2)

################  R^2 Prediction #######################
PRESS  
SS_T= sum(anova(mod)$"Sum Sq")
pred.r.squared = 1 - PRESS/(SS_T)
pred.r.squared

###############Main Statistical Analysis################
#Design Projection
Fil= matrix(c(62, 58, 47, 48 , 38, 18, 18, 23, 82, 77, 63, 67, 23, 27, 27, 33,
              71, 52, 52, 52, 47, 42, 42, 42, 93, 88, 88, 88, 13, 13, 13, 13),byrow=T,ncol=4)
dimnames(Fil) = list(c("(1)","a","b","ab","c","ac","bc","abc"),
                     c("Rep1","Rep2","Rep3","Rep4"))
A= rep(c(-1,1),4)
B =rep(c(-1,-1,1,1),2)
C= c(rep(-1,4),rep(1,4))
Total = apply(Fil,1,sum)
data2=data.frame(A,B,C,Fil,Total)
data2
data3=data.frame(A,B,C,Fil)
data3
#design matrix for updated df
I=c(rep(1,8))
AB=A*B
AC=A*C
BC=B*C
ABC=A*B*C
dm2=cbind(I,A,B,AB,C,AC,BC,ABC,Total)
dm2

#anova for updated df
Ft= c(t(Fil))
Ft
Af= rep(as.factor(A),rep(2,8))
Bf= rep(as.factor(B),rep(2,8))
Cf= rep(as.factor(C),rep(2,8))
dm=data.frame(Af,Bf,Cf,Ft)
dm
an=aov(Ft ~ Af*Bf*Cf, data=dm)
summary(an)

#tukey test(not feasible)
#library(agricolae)
#tk<-TukeyHSD(an,"Bf",group = TRUE)
#tk'''

#pilotEff
G=dm2[,9]
pilotEff = yates(G, labels = c("A","B","C")) 
pilotEff/4

