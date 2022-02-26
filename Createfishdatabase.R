# clear the environment
#datafile location: C:\Users\camil\Documents\Sail\BMASP\Fish study....
#set working directory
setwd("~/Sail/BMASP/Fish study")

# read excel file - "Fishdata-edited.xlsx"
library(readxl)    

fishdata<-read_excel("Fish-edited.xlsx")

#see vars
head(fishdata)
tail(fishdata)
names(fishdata)

#remove first timers to marina
#CA10, CA12, maybe CP1 are first timers? 


#Set up tidetables and visitation times

tideM<-read_excel("TidetablesNOAAMarch.xlsx")
head(tideM)


#above 3'water mark
grepl('["*"]', tideM$Pred)
above3<-grepl('["*"]', tideM$Pred)
above3
class(above3)

class(tideM$Date)

#weekend
wknd<-tideM$Day=="Sat" | tideM$Day == "Sun"
wknd
    
#Times     
time1<-tideM$Time
class(time1)

#f you only want to you base R, take advantage of as.Date(..., format = ("...")) 
#to transform your date into a standard format. Then, you can use substr to extract the time. 
#e.g. substr("2013-10-01 01:23:45 UTC", 12, 16) gives you 01:23.
time1
time2<-substr(time1, 12, 16) 
time2

#need to put cols together for tideM vars,  above3, wknd, wkday, time2

#weekday morn params
weekmorn <- above3==TRUE & wknd==FALSE & time2 >= 9:00 & time2 < 15:00

#weekday afternoon params
weekpm <- above3==TRUE  & wknd==FALSE & time2 >=15:00 & time2 < 18:00

#weekend params
weekend <- above3==TRUE  & wknd==TRUE & time2 >=9:00 & time2 <=18:00

#need to put cols together for tideM vars,  above3, wknd, wkday, time2, weekmorn, weekpm, weekend
#export into new excel file or csv


tideM$Pred

