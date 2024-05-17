#SETTING DIRECTORY
dir<-"E:\\Pr\\R_Codes\\Minor2"
setwd(dir)

#extracting df
data<-read.csv("ClassQues(LSD)_Data.csv",header = TRUE,stringsAsFactors = TRUE)
data
str(data)

#anova
at<-aov(Yield~Batch+AcidC+ST+CC,data=data)
summary(at)