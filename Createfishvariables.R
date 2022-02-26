# clear the environment
rm(list=ls())

#if need to start here:
setwd("~/Sail/BMASP/Fish study")

library(readxl)    
library(zipcodeR)
fish2<-read_excel("Fish-edited.xlsx",
                  col_types = c("text","date","text","text","numeric","text","text"))


################################fish vars set off in here
#create travel cost variable
#create column with berkeley marina zipcode 94710

fish2$marzip <- '94710'
length(fish2$marzip)
length(fish2$zipcode)
zip_distance(fish2$marzip, fish2$zipcode)



vec_blank <- fishdata$zipcode                                        # Duplicate vector
vec_blank[is.na(vec_blank)] <- ""                       # Replace NA with blank
vec_blank                                               # Print updated vector
# [1] "A" "B" ""  "A" ""  "C"

zipnum<-as.numeric(vec_blank)
zipnum
marnum <- as.numeric(fishdata$marzip)
zip_distance(marnum,zipnum)

length(marnum)
[1] 51
> length(zipnum)
[1] 51
> zip_distance(94710, 95301)
zipcode_a zipcode_b distance
1     94710     95301    99.69

zip_distance(NA, 95301)
Error in p[, 3:4, drop = FALSE] : subscript out of bounds

zip_distance("NA", "95301")
Error in p[, 3:4, drop = FALSE] : subscript out of bounds
View(dist)
zip_distance("94703", "95301")

dist1<-dist$distance
dist1






#get roundtrip distance

distrt <- distance * 2

#multiply by $0.58 as cost per mile according to IRS. 

tc <- distrt*0.58


######################################
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



#datafile location: C:\Users\camil\Documents\Class Material\SFSU-ECON101\Student learning project\R data

#set working directory
setwd("~/Class Material/SFSU-ECON101/Student learning project/R data")

# import basline assessment results - "Basline_results.xlsx"
library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets("Basline_results.xlsx")
  Econ101_Q1_PRC <- mysheets$`101Price`[complete.cases(mysheets$`101Price`), ]
  Econ101_Q2_MNP <- mysheets$`101Monopoly`[complete.cases(mysheets$`101Monopoly`), ]
  Econ102_Q1_CPI <- mysheets$`102CPI`[complete.cases(mysheets$`102CPI`), ]
  Econ102_Q2_EMP <- mysheets$`102Unemployment`[complete.cases(mysheets$`102Unemployment`), ]
rm(mysheets)

# summary statistics 
# https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
library(ggplot2)
library(tidyr)
library(viridis)
library(ggpubr)

# figue 1 - econ 101 quiz 1 - 273 obs (kirill 99 obs; max 114 obs; sudip 60 obs)
Econ101_Q1_PRC <- gather(Econ101_Q1_PRC, Questions, Status, Q1:Q12, factor_key=TRUE)
Econ101_Q1_PRC$Answers <- ifelse(Econ101_Q1_PRC$Status == 1, "Right", ifelse(Econ101_Q1_PRC$Status == 0, "Wrong", "Half-Right"))
Econ101_Q1_PRC$Percentage <- 1
Econ101_Q1_PRC$Questions <- gsub("Q", "", Econ101_Q1_PRC$Questions)
Econ101_Q1_PRC$Questions <- factor(Econ101_Q1_PRC$Questions, levels=unique(as.character(Econ101_Q1_PRC$Questions)) )
kirill <- subset(Econ101_Q1_PRC, as.character(Econ101_Q1_PRC$Instructor)=='Kirill')
max <- subset(Econ101_Q1_PRC, as.character(Econ101_Q1_PRC$Instructor)=='Max')
sudip <- subset(Econ101_Q1_PRC, as.character(Econ101_Q1_PRC$Instructor)=='Sudip')

# full sample
graph1 <- ggplot(Econ101_Q1_PRC, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_viridis(discrete = T) +
  scale_x_discrete()+
  ggtitle("Full Sample (273 Obs)")
# sub-sample
graph2 <- ggplot(kirill, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Kirill (99 Obs)")
graph3 <-ggplot(max, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Max (114 Obs)")
graph4 <-ggplot(sudip, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Sudip (60 Obs)")


# plot all graphs together
jpeg(file = "Fig1_Econ101_Quiz1_Price_Summary.jpg", units="in", width=5, height=5, res=800) 
figure <- ggarrange(graph1, graph2, graph3, graph4, 
          common.legend = TRUE, legend = "bottom", ncol = 2, nrow = 2)
annotate_figure(figure,
          top = text_grob("Econ 101- Quiz 1 Baseline Results", color = "black", face = "bold", size = 12),
          fig.lab = "Figure 1", fig.lab.face = "bold")
dev.off() 


# figue 2 - econ 101 quiz 2 - 189 obs (kirill 81 obs; max 108 obs; sudip 0 obs)
Econ101_Q2_MNP <- gather(Econ101_Q2_MNP, Questions, Status, Q1:Q11, factor_key=TRUE)
Econ101_Q2_MNP$Answers <- ifelse(Econ101_Q2_MNP$Status == 1, "Right", ifelse(Econ101_Q2_MNP$Status == 0, "Wrong", "Half-Right"))
Econ101_Q2_MNP$Percentage <- 1
Econ101_Q2_MNP$Questions <- gsub("Q", "", Econ101_Q2_MNP$Questions)
Econ101_Q2_MNP$Questions <- factor(Econ101_Q2_MNP$Questions, levels=unique(as.character(Econ101_Q2_MNP$Questions)) )
kirill <- subset(Econ101_Q2_MNP, as.character(Econ101_Q2_MNP$Instructor)=='Kirill')
max <- subset(Econ101_Q2_MNP, as.character(Econ101_Q2_MNP$Instructor)=='Max')
sudip <- subset(Econ101_Q2_MNP, as.character(Econ101_Q2_MNP$Instructor)=='Sudip')

# full sample
graph1 <- ggplot(Econ101_Q2_MNP, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_viridis(discrete = T) +
  scale_x_discrete()+
  ggtitle("Full Sample (189 Obs)")

# sub-sample
graph2 <- ggplot(kirill, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Kirill (81 Obs)")
graph3 <-ggplot(max, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Max (108 Obs)")
graph4 <-ggplot(sudip, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Sudip (0 Obs)")

# plot all graphs together
jpeg(file = "Fig2_Econ101_Quiz2_Monopoly_Summary.jpg", units="in", width=5, height=5, res=800) 
figure <- ggarrange(graph1, graph2, graph3, 
                    common.legend = TRUE, legend = "bottom", ncol = 2, nrow = 2)
annotate_figure(figure,
                top = text_grob("Econ 101- Quiz 2 Baseline Results", color = "black", face = "bold", size = 12),
                fig.lab = "Figure 2", fig.lab.face = "bold")
dev.off()


# figue 3 - econ 102 quiz 1 - 104 obs (tombari 104 obs)
Econ102_Q1_CPI <- gather(Econ102_Q1_CPI, Questions, Status, Q1:Q5, factor_key=TRUE)
Econ102_Q1_CPI$Answers <- ifelse(Econ102_Q1_CPI$Status == 1, "Right", ifelse(Econ102_Q1_CPI$Status == 0, "Wrong", "Half-Right"))
Econ102_Q1_CPI$Percentage <- 1
Econ102_Q1_CPI$Questions <- gsub("Q", "", Econ102_Q1_CPI$Questions)
Econ102_Q1_CPI$Questions <- factor(Econ102_Q1_CPI$Questions, levels=unique(as.character(Econ102_Q1_CPI$Questions)) )
tombari <- subset(Econ102_Q1_CPI, as.character(Econ102_Q1_CPI$Instructor)=='Tombari')
norman <- subset(Econ102_Q1_CPI, as.character(Econ102_Q1_CPI$Instructor)=='Norman')
wenzel <- subset(Econ102_Q1_CPI, as.character(Econ102_Q1_CPI$Instructor)=='Wenzel')

# full sample
graph1 <- ggplot(Econ102_Q1_CPI, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_viridis(discrete = T) +
  scale_x_discrete()+
  ggtitle("Full Sample (104 Obs)")

# sub-sample
graph2 <- ggplot(tombari, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Tombari (104 Obs)")
graph3 <-ggplot(norman, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Norman (0 Obs)")
graph4 <-ggplot(wenzel, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Wenzel (0 Obs)")

# plot all graphs together
jpeg(file = "Fig3_Econ102_Quiz1_CPI_Summary.jpg", units="in", width=5, height=5, res=800) 
figure <- ggarrange(graph1, graph2, 
                    common.legend = TRUE, legend = "bottom", ncol = 2, nrow = 2)
annotate_figure(figure,
                top = text_grob("Econ 102 - Quiz 1 Baseline Results", color = "black", face = "bold", size = 12),
                fig.lab = "Figure 3", fig.lab.face = "bold")
dev.off()


# figue 4 - econ 102 quiz 2 - 217 obs (tombari 110 obs; norman 30 obs; wenzel 77 obs)
Econ102_Q2_EMP <- gather(Econ102_Q2_EMP, Questions, Status, Q1:Q8, factor_key=TRUE)
Econ102_Q2_EMP$Answers <- ifelse(Econ102_Q2_EMP$Status == 1, "Right", ifelse(Econ102_Q2_EMP$Status == 0, "Wrong", "Half-Right"))
Econ102_Q2_EMP$Percentage <- 1
Econ102_Q2_EMP$Questions <- gsub("Q", "", Econ102_Q2_EMP$Questions)
Econ102_Q2_EMP$Questions <- factor(Econ102_Q2_EMP$Questions, levels=unique(as.character(Econ102_Q2_EMP$Questions)) )
tombari <- subset(Econ102_Q2_EMP, as.character(Econ102_Q2_EMP$Instructor)=='Tombari')
norman <- subset(Econ102_Q2_EMP, as.character(Econ102_Q2_EMP$Instructor)=='Norman')
wenzel <- subset(Econ102_Q2_EMP, as.character(Econ102_Q2_EMP$Instructor)=='Wenzel')

# full sample
graph1 <- ggplot(Econ102_Q2_EMP, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_viridis(discrete = T) +
  scale_x_discrete()+
  ggtitle("Full Sample (217 Obs)")

# sub-sample
graph2 <- ggplot(tombari, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Tombari (110 Obs)")
graph3 <-ggplot(norman, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Norman (30 Obs)")
graph4 <-ggplot(wenzel, aes(fill=Answers, y=Percentage, x=Questions)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Wenzel (77 Obs)")

# plot all graphs together
jpeg(file = "Fig4_Econ102_Quiz2_Employment_Summary.jpg", units="in", width=5, height=5, res=800) 
figure <- ggarrange(graph1, graph2, graph3, graph4,
                    common.legend = TRUE, legend = "bottom", ncol = 2, nrow = 2)
annotate_figure(figure,
                top = text_grob("Econ 102 - Quiz 2 Baseline Results", color = "black", face = "bold", size = 12),
                fig.lab = "Figure 4", fig.lab.face = "bold")
dev.off()


