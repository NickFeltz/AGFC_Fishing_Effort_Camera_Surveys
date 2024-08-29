#### Install Packages #### Do not run if already installed ####
install.packages("tidyverse")
install.packages("exiftoolr")

#### Initiate Packages ####
library(lubridate)
library(chron)
library(suncalc)
library(exiftoolr)
install_exiftool()



set.seed(397)       #Fixed randomization so results are repeatable. Change number for different outcome

#### Dates Setup ####
Start.Date=as.Date("2022/10/01")
End.Date=as.Date("2023/02/28")
Holidays=as.Date(c('2022/05/30',  #Memorial Day
                   '2022/07/04',    #Independence Day
                   '2022/09/05',    #Labor Day
                   '2022/11/11',    #Veterans Day
                   '2022/11/24',    #Thanksgiving
                   '2022/11/25',    #Thanksgiving
                   '2022/12/23',    #Christmas Eve
                   '2022/12/26',    #Christmas Day
                   '2023/01/02',    #New Year's Day
                   '2023/01/16',    #Martin Luther King Jr Day
                   '2023/02/20'))   #President's Day

HighUseStrata=as.Date(NA)          #List any high use days if high use strata are included

#### Location Setup ####
lat=34.0000
long=-92.3937     #Make sure longitude in negative

#### Set Up Calendar & Daylight Period ####
calendar=data.frame(Date=seq(Start.Date,End.Date,by='day'),Day=weekdays(seq(Start.Date,End.Date,by='day')),Month=month(seq(Start.Date,End.Date,by='day')))
calendar$DayType=ifelse(is.weekend(calendar$Date)|calendar$Date %in% Holidays,'End','Day')
calendar$DayType[calendar$Date %in% HighUseStrata]="HighUse"

calendar=cbind(calendar,format(getSunlightTimes(date=calendar$Date,lat=lat,lon=long,keep=c("sunrise","sunset"),tz="America/Chicago")[,4:5], format = "%H:%M"))
calendar$dl_hrs=signif(as.numeric(as.difftime(calendar$sunset, format = "%H:%M")-as.difftime(calendar$sunrise, format = "%H:%M")),digits=4)

#### Set Creel Design ####
DayType=c('Day','End')          #Designate DayType, could include high use strata
Period=c(10:12,1:2)                   #Designated months to be included
Selection=c(4,4)
Counts.per.Day=2

#Designated number of selections per period/month for weekdays and weekend days
#Shift=c('AM','PM')              #Designate shifts
#Shift.prob=c(0.45,0.55,0.45,0.55)   #Designate Shift probabilities AM,PM,AM,PM

Inst.time=c("7:00","8:00","9:00","10:00",
            "11:00","12:00","13:00","14:00",
            "15:00","16:00","17:00","18:00")                #Designate Instantaneous Count Times
Inst.prob=rep(1/6,6)            #Designate Time probabilities

Shift=c("07:00","08:00","09:00","10:00",
        "11:00","12:00","13:00","14:00",
        "15:00","16:00","17:00","18:00")              #Designate shifts
Shift.prob=rep(1/12,24)   #Designate Shift probabilities AM,PM,AM,PM

Access=c('Tucker Hollow',
         'Lead Hill',
         'Lakeview',
         'Dam Site',
         'BS Lake Boat Dock',
         'Oakland',
         'Hwy 125',
         'Buck Creek',
         'Fairview')    #Designate possible access point list
Access.prob=c(0.15,
              0.25,
              0.08,
              0.05,
              0.19,
              0.10,
              0.05,
              0.05,
              0.08)             #Designate Access point selection probabilities

f=1  #Counts AM/PM per Day
s=1  #Daytype Strata
n=5  #Sample size (n of days to collect counts)
m=1  #Month/Period
i=1  #Instantaneous Count
    Design.Summary=matrix(c(Inst.prob),nrow=length(Inst.time),ncol=length(DayType),dimnames=list(c(Inst.time),DayType))
    Design.Summary=rbind(Design.Summary,matrix(Selection,nrow=1,ncol=length(DayType),dimnames=list('Selection')))
    Access.Summary=as.data.frame(cbind(Access,Access.prob))
    schedule=matrix(c(0,0,0,0),nrow=1,ncol=4)
    for (m in 1:length(Period)){
      for (d in 1:length(DayType)){
        Day=sample(calendar[which(calendar$DayType==DayType[d] & calendar$Month==Period[m]),]$Date, size=Selection[d])
        Location=sample(Access,size=Selection[d],prob=Access.prob,replace=TRUE)
        TimeAM=matrix(NA,nrow=length(Day),ncol=Counts.per.Day/2)
        TimePM=matrix(NA,nrow=length(Day),ncol=Counts.per.Day/2)
      for (i in 1:Selection[d]){
        TimeAM[i,]=sample(Inst.time[1:(length(Inst.time)/2)], size=Counts.per.Day/2,prob=Inst.prob,replace = FALSE)
        TimePM[i,]=sample(Inst.time[((length(Inst.time)/2)+1):length(Inst.time)], size=Counts.per.Day/2,prob=Inst.prob,replace = FALSE)
      }
      temp=data.frame(Date=rep(Day,Counts.per.Day),DayType=rep(DayType[d],Selection[d]*Counts.per.Day),Time=c(TimeAM,TimePM),Access=Location)
      schedule=rbind(schedule,as.matrix(temp))
      }
    }

    schedule=as.data.frame(schedule[-1,])
FullSchedule=merge(calendar,schedule,all=TRUE)
ScheduleOnly=na.omit(FullSchedule)
FullSchedule
ScheduleOnly
table(ScheduleOnly$Access,ScheduleOnly$Month)
sort(marginSums(table(ScheduleOnly$Access,ScheduleOnly$Month),margin=1),decreasing=TRUE)
table(ScheduleOnly$Time,ScheduleOnly$Month)
#write.csv(FullSchedule, file="BSRovingSchedule.csv")
#################################################################################
setwd("E:\\Conway Creel\\Summer\\")



ScheduleOnly$DateTime=format(strptime(paste(ScheduleOnly$Date, ScheduleOnly$Time), format="%Y-%m-%d %H:%M"), format="%Y-%m-%d %H:%M")

pull.folders = list.files(path = getwd(),full.name = TRUE)[1:2] 
site.folders = list.files(path = pull.folders, full.name = TRUE)
image.files = list.files(path = site.folders, full.name = TRUE)
multi.camera.image.files = list.files(path = image.files, full.name = TRUE)
image.files = list.files(path = site.folders, pattern = ".JPG", full.name = TRUE)
end = list.files(path = multi.camera.image.files, full.name = TRUE)
image.paths = c(image.files,multi.camera.image.files, end)

image.info=exif_read(image.paths,tags=c("SourceFile", "FileName", "FileCreateDate"))



image.info$DateTime=format(round(strptime(gsub("\\-.*","",image.info$FileCreateDate),format="%Y:%m:%d %H:%M"),"hour"), format="%Y-%m-%d %H:%M")

d=1
for (d in 1:length(image.info$SourceFile)){
image.info$Access[d]=unlist(strsplit(image.info$SourceFile[d], split='/', fixed=TRUE))[5]
}

test=merge(image.info,ScheduleOnly[,c('DateTime','DayType')],by=c("DateTime"))

test=test[order(test$SourceFile),]

dir.create("Copied Files")
i=1
for(i in seq_along(test$SourceFile)) {
 file.copy(test$SourceFile[i],paste0(getwd(),"/","Copied Files/"))
  file.rename(paste0(getwd(),"/","Copied Files/",test$FileName[i]),paste0(getwd(),"/","Copied Files/",i,test$FileName[i]))
}


unlink("Copied Files", recursive = TRUE)

