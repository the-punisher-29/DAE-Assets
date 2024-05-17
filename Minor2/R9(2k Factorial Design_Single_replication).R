setwd("E:\\Pr\\R_Codes\\Minor2")
Filtration= matrix(c(45, 71, 48,65,68,60,80,65,43,100,
                     45,104,75,86,70,96),byrow=T,ncol=1)
dimnames(Filtration) = list(c("(1)","a","b","ab","c","ac","bc","abc",
                            "d","ad", "bd","abd","cd","acd","bcd","abcd"),
                          c("Rate"))
A = rep(c(-1,1),8)
B =rep(c(-1,-1,1,1),4)
C= c(rep(-1,4),rep(1,4),rep(-1,4),rep(1,4))
D=c(rep(-1,8),rep(1,8))
data.rate=data.frame(A,B,C,D,Filtration)##The given data
data.rate
######################################
I=c(rep(1,16))
AB = A*B
AC = A*C
BC = B*C
ABC = A*B*C
AD=A*D
BD=B*D
ABD=A*B*D
CD=C*D
ACD=A*C*D
BCD=B*C*D
ABCD=A*B*C*D
Design.matrix=cbind(I, A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,
                    CD,ACD,BCD,ABCD,Filtration)
Design.matrix
################## Interaction and Main effects  ########################
n = 1 ##Replication
Feff = t(Filtration) %*% cbind(A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,CD,ACD,BCD,ABCD)/(8*n)
Ieff=t(Filtration) %*% cbind(I)/(16*n)
eff=cbind(Ieff,Feff)
Summary = rbind( cbind(I,A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,CD,ACD,BCD,ABCD),eff)
dimnames(Summary)[[1]] = c(dimnames(Filtration)[[1]],"Effect")
Summary
#################
library(unrepx)
G=Design.matrix[,17]
pilotEff = yates(G, labels = c("A","B","C", "D")) 
pilotEff
hnplot(pilotEff,ID=0)
################################
lm.rate=lm(Filtration ~ A*C*D, data=data.rate)
summary(lm.rate)
library(DoE.base)
library(FrF2)
MEPlot(lm.rate)
IAPlot(lm.rate)
#################################  Response Surface #######################
##########################  REgression ###########################
###########################################################################
mod=lm(Filtration ~ A+C+D+A:C+A:D, data=data.rate)
summary(mod)
residuals=mod$res
residuals
qqnorm(mod$res,ylab="Ordinary Residuals")
qqline(mod$res,c)
##  PRESS ############################################
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
###########################################################
##########################  Design Projection #############################
#################################################################################
### Reorganize the data as give replication wise ##############################
##################################################################################
Fil= matrix(c(45,48,71,65,68,80,60,65,43,45,100,104,75,70,86,96),byrow=T,ncol=2)
dimnames(Fil) = list(c("(1)","a","c","ac","d","ad","cd","acd"),
                          c("Rep1","Rep2"))
A= rep(c(-1,1),4)
C =rep(c(-1,-1,1,1),2)
D= c(rep(-1,4),rep(1,4))
Total = apply(Fil,1,sum)
data.rate2=data.frame(A,C,D,Total)##The given data
data.rate2
######################################
I=c(rep(1,8))
AC = A*C
AD=A*D
CD=C*D
ACD=A*C*D
Design.matrix2=cbind(I, A,C, AC,D,AD,CD,ACD,Total)
Design.matrix2
########################  ANOVA Model ##############
Frate= c(t(Fil))
Frate
Af= rep(as.factor(A),rep(2,8))
Cf= rep(as.factor(C),rep(2,8))
Df= rep(as.factor(D),rep(2,8))
data.mat=data.frame(Af,Cf,Df, Frate)
data.mat
Fil.av=aov(Frate ~ Af*Cf*Df, data=data.mat)
summary(Fil.av)
########################################################################################
#######################################################################################
###############################  Exercise 6.38 #######################################
####################################################################################
yield= matrix(c(6.08,6.04,6.53,6.43,6.31,6.09, 6.12,6.36,6.79,
                  6.68,6.73,6.08,6.77,6.38,6.49,6.23),byrow=T,ncol=1)
dimnames(yield) = list(c("(1)","a","b","ab","c","ac","bc","abc",
                              "d","ad", "bd","abd","cd","acd","bcd","abcd"),
                            c("yield"))
A = rep(c(-1,1),8)
B =rep(c(-1,-1,1,1),4)
C= c(rep(-1,4),rep(1,4),rep(-1,4),rep(1,4))
D=c(rep(-1,8),rep(1,8))
cbind(A,B,C,D,yield)##The given data
data.y=data.frame(A,B,C,D,yield)
data.y
######################################
I=c(rep(1,16))
AB = A*B
AC = A*C
BC = B*C
ABC = A*B*C
AD=A*D
BD=B*D
ABD=A*B*D
CD=C*D
ACD=A*C*D
BCD=B*C*D
ABCD=A*B*C*D
Design.matrix=cbind(I, A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,CD,ACD,BCD,ABCD,yield)
Design.matrix
################## Interaction and Main effects  ########################
n = 1 ##Replication
Feff = t(yield) %*% cbind(A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,CD,ACD,BCD,ABCD)/(8*n)
Ieff=t(yield) %*% cbind(I)/(16*n)
eff=cbind(Ieff,Feff)
Summary = rbind( cbind(I,A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,CD,ACD,BCD,ABCD),eff)
dimnames(Summary)[[1]] = c(dimnames(yield)[[1]],"Effect")
Summary
#################
library(unrepx)
Response=Design.matrix[,17]
Eff.mat = yates(Response, labels = c("A","B","C", "D")) 
Eff.mat
hnplot(Eff.mat,ID=0)
################################
lm.rate2=lm(yield ~ A+B+C+D, data=data.y)
summary(lm.rate2)
library(DoE.base)
library(FrF2)
MEPlot(lm.rate2)
IAPlot(lm.rate2)
######################################################################
##########################  The regression Model  #########################
##########################################################################
