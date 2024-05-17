work_dir <- "E:\\Pr\\R_Codes\\Major"
setwd(work_dir)
data.plasma=read.csv("Data12(Plasma).CSV", header=T,stringsAsFactors=T)
data.plasma
mod1=lm(y~x_1+x_2, data=data.plasma)
anova(mod1)
mod2=lm(y~x_1*x_2+I(x_1^2)+I(x_2^2), data=data.plasma)
anova(mod1,mod2)
######################  2nd data SET ################
data.yield=read.csv("Data13(yield).CSV", header=T,stringsAsFactors=T)
data.yield
md1=lm(y~x_1+x_2, data=data.yield)
summary(md1)
anova(md1)
md2=lm(y~x_1*x_2+I(x_1^2)+I(x_2^2), data=data.yield)
anova(md2)
anova(md1,md2)
##########################  3rd data set  ############################
data.yield2=read.csv("Data14(yield).CSV", header=T,stringsAsFactors=T)
data.yield2
md11=lm(y~x_1+x_2, data=data.yield2)
summary(md11)
anova(md11)
md22=lm(y~x_1*x_2+I(x_1^2)+I(x_2^2), data=data.yield2)
anova(md22)
anova(md11,md22)
######################  4th data set  #########################
data.Spherical=read.csv("Data16(Spherical).CSV",
                    header=T,stringsAsFactors=T)
data.Spherical
Model=lm(y~x_1+x_2+x_3+x_1:x_2+x_1:x_3+x_2:x_3+I(x_1^2)+I(x_2^2)+I(x_3^2), data=data.Spherical)
summary(Model)
######################  5 th data set  #########################
data.bread=read.csv("Data15(Breadwrapper).CSV",
                    header=T,stringsAsFactors=T)
data.bread
model=lm(y~x_1+x_2+x_3+x_1:x_2+x_1:x_3+x_2:x_3+I(x_1^2)+I(x_2^2)+I(x_3^2), data=data.bread)
summary(model)
######################  6 th data set  #########################
data.cube=read.csv("Data17(Cuboidal).CSV",
                    header=T,stringsAsFactors=T)
data.cube
MOD=lm(y~x_1+x_2+x_3+x_1:x_2+x_1:x_3+x_2:x_3+I(x_1^2)+I(x_2^2)+I(x_3^2), data=data.cube)
summary(MOD)
######################  6 th data set  #########################
data.BBD=read.csv("Data18(BBD).CSV",
                   header=T,stringsAsFactors=T)
data.BBD
MOd=lm(y~x_1+x_2+x_3+x_1:x_2+x_1:x_3+x_2:x_3+I(x_1^2)+I(x_2^2)+I(x_3^2), data=data.BBD)
summary(MOd)




