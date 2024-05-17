setwd("E:\\Pr\\R_Codes\\Minor2")
Filtration= matrix(c(25, 71, 48,45,68,40,60,65,43,80,
                     25,104,55,86,70,76),byrow=T,ncol=1)
dimnames(Filtration) = list(c("(1)","a","b","ab","c","ac","bc","abc",
                              "d","ad", "bd","abd","cd","acd","bcd","abcd"),
                            c("Rate"))
A = rep(c(-1,1),8)
B =rep(c(-1,-1,1,1),4)
C= c(rep(-1,4),rep(1,4),rep(-1,4),rep(1,4))
D=c(rep(-1,8),rep(1,8))
Block=c(1,2,2,1,2,1,1,2,2,1,1,2,1,2,2,1)
Filtration
data.rate=data.frame(Block, A,B,C,D,Filtration)##The given data
data.rate
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
###################################
library(unrepx)
G=Design.matrix[,17]
pilotEff = yates(G, labels = c("A","B","C", "D")) 
pilotEff
hnplot(pilotEff,ID=0)
####################################
##########################  Design Projection #############################
#################################################################################
### Reorganize the data as give replication wise###########################
Fil= matrix(c(25, 48,71,45,68,60,40,65,43,
              25,80, 104,55,70,86,76),byrow=T,ncol=2) ##Please arrange the data carefully
dimnames(Fil) = list(c("(1)","a","c","ac","d","ad","cd","acd"),
                     c("Rep1","Rep2"))
Fil
A= rep(c(-1,1),4)
C =rep(c(-1,-1,1,1),2)
D= c(rep(-1,4),rep(1,4))
Total = apply(Fil,1,sum)
Block= c(1,2,2,1,2,1,1,2)
data.rate2=data.frame(Block,A,C,D,Fil,Total)##The given data
data.rate2
######################################
I=c(rep(1,8))
AC = A*C
AD=A*D
CD=C*D
ACD=A*C*D
Design.matrix2=cbind(Block,I, A,C, AC,D,AD,CD,ACD,Total)
Design.matrix2
########################  ANOVA Model ##############
Frate= c(t(Fil))
Af= rep(as.factor(A),rep(2,8))
Cf= rep(as.factor(C),rep(2,8))
Df= rep(as.factor(D),rep(2,8))
Block_ABCD=c(1,2,2,1,2,1,1,2,2,1,1,2,1,2,2,1)
Block_ABCD=as.factor(Block_ABCD)
data.mat=data.frame(Af,Cf,Df, Block_ABCD,Frate)
data.mat
str(data.mat)
Fil.av=aov(Frate ~ Block_ABCD+Af+Cf+Df+Af*Cf+Af*Df, data=data.mat)
summary(Fil.av)
###############################



