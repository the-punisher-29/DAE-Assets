setwd("E:\\Pr\\R_Codes\\Major")
#############################  Full Model ############################
Filtration= matrix(c(45, 71, 48,65,68,60,80,65,43,100,
                     45,104,75,86,70,96),byrow=T,ncol=1)
A = rep(c(-1,1),8)
B =rep(c(-1,-1,1,1),4)
C= c(rep(-1,4),rep(1,4),rep(-1,4),rep(1,4))
D=c(rep(-1,8),rep(1,8))
data.rate=data.frame(A,B,C,D,Filtration)##The given data
data.rate
M_data= rbind(data.rate,
  c(0, 0, 0, 0, 73),
  c(0, 0, 0, 0, 75),
  c(0, 0, 0, 0, 66),
  c(0, 0, 0, 0, 69)
)
M_data
Mod1=lm(Filtration~A*B*C*D, data=M_data)
anova(Mod1)
Mod2=lm(Filtration~A*B*C*D+I(A^2)+I(B^2)+I(C^2)+I(D^2), data=M_data)
anova(Mod2)
anova(Mod1,Mod2)
######################  Reduced Model #####################
Mod3=lm(Filtration~A+C+D+A:C+A:D, data=M_data)
anova(Mod3)
Mod4=lm(Filtration~A+C+D+A:C+A:D+I(A^2)+I(C^2)+I(D^2), data=M_data)
anova(Mod4)
anova(Mod3,Mod4)
###################
#library(rsm)
#Rate_rsm=rsm(Filtration~SO(A,C,D), data=M_data)
#summary(Rate_rsm)






