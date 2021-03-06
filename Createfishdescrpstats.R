#generating basic analytics for fish data

#already have a dataset generated by Createfishdatabase.R 


head(fishdata)
tail(fishdata)
names(fishdata)
table(fishdata$primary)
sum(fishdata$primary)
summary(fishdata$primary)

#more tables
library(psych)
library(doBy)
plot(fishdata$primary)
table(coursedata$textbook)
table(coursedata$Course)
by(coursedata$textbook, coursedata$Course, summary)
by(coursedata$textbook, coursedata$Course)
by(coursedata$textbook, coursedata$Course, table)
rm(list=ls())


library(ggplot2)

# Basic scatter plot
ggplot(fishdata, aes(x=cost, y=visitperyr)) + geom_point()

#add labels
ggplot(fishdata, aes(x=cost, y=vizmarina)) +
  geom_point() + 
  geom_text(label=fish$CODE, size=3, hjust=0, vjust=0)

ggplot(fishdata, aes(x=cost, y=vizmarina)) +
  geom_point() + 
  geom_text(label=fish$CODE, size=3, hjust=0, vjust=0, check_overlap=TRUE)

#removing one outlier
subfish<-subset(fishdata, fish$CODE!="CA11")
ggplot(subfish, aes(x=cost, y=vizmarina)) +
  geom_point() + 
  geom_text(label=subfish$CODE, size=3, hjust=0, vjust=0)


#check nudge_x and nudge_y commands

#data summary
library(dplyr)

f2<-summarise(fishdata, Mean=mean(distance, SD=sd(distance))

f3<-summarise(group_by(fishdata, race), Mean=mean(distance, SD=sd(distance)

library(magrittr)
f4<-fishdata %>%
  group_by(race) %>%
  summarise(Mean = mean(distance),
            SD=sd(distance))

fishdata %>%
  group_by(race) %>%
  summarise(Mean = mean(distance)) %>%
  ggplot(aes(x = race, y = Mean, fill = race)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Race",
    y = "Average Distance ",
    title = paste(
      "Summary Based on Groups"
    )
  )



summarise(fishdata,count = n(distance))



#### old - anoshua file


#read in excel file on students
library(readxl)
studata<-read_excel("econ_101_102_data.xlsx")
names(studata)
summary(studata$SEX)
table(studata$SEX)
table(studata$SATMATH)
summary(studata$SATMATH[studata$SATMATH>0])
describe(studata$SATMATH[studata$SATMATH>0])
describe(studata$SATVERB)
describe(studata$SATVERB[studata$SATVERB>0])
describe(studata$HSGPA)
describe(studata$HSGPA[studata$HSGPA>0])
table(studata$YEARTERM)
table(studata$GRADE)

#create points from letter grade

studata$points<-studata$GRADE
table(studata$points)
studata$points[studata$points=="A"]<-4
studata$points[studata$points=="A-"]<-3.7
studata$points[studata$points=="B+"]<-3.3
studata$points[studata$points=="B"]<-3
studata$points[studata$points=="B-"]<-2.7
studata$points[studata$points=="C+"]<-2.3
studata$points[studata$points=="C"]<-2.0
studata$points[studata$points=="C-"]<-1.7
studata$points[studata$points=="D+"]<-1.3
studata$points[studata$points=="D"]<-1
studata$points[studata$points=="D-"]<-0.7
studata$points[studata$points=="F" | studata$points=="NC" | studata$points=="WU"]<-0

#NAs introduced by coercion 
#applies to I's and W's and CRs

studata$points<-as.numeric(studata$points)
table(studata$points)


#create covid dummy

summary(studata$YEARTERM)
yearsem<-as.numeric(studata$YEARTERM)
summary(yearsem)
postcovid<-ifelse(yearsem>20200,1,0)
table(postcovid)

#create HSGPA
#HSGPA is 0 for transfer students and then just 0 for others
#sub in  Transfer gpa where hspga = 0 and transgpa exists, others 0's left as NA

hsgpa<-studata$HSGPA
hsgpa<-ifelse(hsgpa>0,hsgpa,studata$TRANSGPA)
table(hsgpa, useNA = "always")
summary(hsgpa)

#compare hsgpa class composition pre and post covid
describeBy(hsgpa,postcovid)
t.test(hsgpa[postcovid==0],hsgpa[postcovid==1])

#cor tests http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
cor(hsgpa, postcovid,  method = "pearson", use = "complete.obs")
cor.test(hsgpa, postcovid,  method = "pearson", use = "complete.obs")

#plot
library("ggpubr")
studata$Rhsgpa<-hsgpa
studata$postcovid<-postcovid

ggscatter(studata, x = "Rhsgpa", y = "postcovid", 
+           add = "reg.line", conf.int = TRUE, 
+           cor.coef = TRUE, cor.method = "pearson",
+           xlab = "hsgpa", ylab = "postcovid=1")



