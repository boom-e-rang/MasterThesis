options(contrasts=c("contr.sum","contr.poly"))
library(pwr)
library(bootES)
DIR<-"/home/ola/MIG-data-analysis/"
DIR_source<-"/home/ola/MIG-MDA-data/"
sink(paste(DIR,"stats_corr.txt",sep=""))
data = read.csv(paste(DIR_source,"multisubject-trials.csv",sep=""))
flag = TRUE
flag60 = !flag
se=function(x) sqrt(var(x)/length(x))
colorvec <- c(25,28,26,27)

MDA<- subset(data, data$Group==1 & data$Set==4,select=Subject:PAA)
set1stime<-subset(data,data$Group==1&data$Set==1,select=successtime)$successtime
set1skill<-subset(data,data$Group==1&data$Set==1,select=set1_skill)$set1_skill
set2paa<-subset(data,data$Group==1&data$Set==2,select=PAA)$PAA
set3skill<-subset(data,data$Group==1&data$Set==3,select=set3_skill)$set3_skill
set3stime<-subset(data,data$Group==1&data$Set==3,select=successtime)$successtime
set1tts<-subset(data,data$Group==1&data$Set==1,select=tts)$tts
set3tts<-subset(data,data$Group==1&data$Set==3,select=tts)$tts
set2tts<-subset(data,data$Group==1&data$Set==2,select=tts)$tts
set2stime<-subset(data,data$Group==1&data$Set==2,select=successtime)$successtime
set2erg<-subset(data,data$Group==1&data$Set==2,select=ergodicity)$ergodicity

set1erg<-subset(data,data$Group==1&data$Set==1,select=ergodicity)$ergodicity
set3erg<-subset(data,data$Group==1&data$Set==3,select=ergodicity)$ergodicity

ave_skill=(set3skill+set1skill)/2
improve=(set3skill-set1skill)

cat("\n \n")
print("time to success")
print(cor(set2tts,set2paa,method="pearson"))
print(cor.test(set2tts,set2paa,alternative = "two.sided",method="pearson"))

cat("\n \n")
print("success time")
print(cor(set2stime,set2paa,method="pearson"))
print(cor.test(set2stime,set2paa,alternative = "two.sided",method="pearson"))

cat("\n \n")
print("error")
print(cor(set2erg,set2paa,method="pearson"))
print(cor.test(set2erg,set2paa,alternative = "two.sided",method="pearson"))

# print(cor(set1skill,set2paa,method="pearson"))
# print(cor.test(set1skill,set2paa,alternative = "two.sided",method="pearson"))
#
# print(cor(set3skill,set2paa,method="pearson"))
# print(cor.test(set3skill,set2paa,alternative = "two.sided",method="pearson"))
#
# print(cor(ave_skill,set2paa,method="pearson"))
# print(cor.test(ave_skill,set2paa,alternative = "two.sided",method="pearson"))
#
# print(cor(improve,set2paa,method="pearson"))
# print(cor.test(improve,set2paa,alternative = "two.sided",method="pearson"))
