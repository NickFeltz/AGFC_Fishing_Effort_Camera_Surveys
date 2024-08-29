#### Install Packages #### Do not run if already installed ####
install.packages("tidyverse")
install.packages("patchwork")

#### Initiate Packages ####

library(lubridate)
library(chron)
library(suncalc)
library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(ggplot2)
library(patchwork)




rm(list=ls(all=TRUE))

set.seed(500)       #Fixed randomization so results are repeatable. Change number for different outcome

#### Dates Setup ####
start.date=as.Date("2023/06/01")
end.date=as.Date("2024/2/29")
holidays=as.Date(c('2023/05/29',  #Memorial Day
                   '2023/07/04',    #Independence Day
                   '2023/09/04',    #Labor Day
                   '2023/11/10',    #Veterans Day
                   '2023/11/23',    #Thanksgiving
                   '2023/11/24',    #Thanksgiving
                   '2023/12/25',    #Christmas Eve
                   '2023/12/26',    #Christmas Day
                   '2024/01/01',    #New Year's Day
                   '2024/01/15',    #Martin Luther King Jr Day
                   '2024/02/19'))   #President's Day

#### Location Setup #### Assign general coordinates for where the survey is taking place for determining sunrise, sunset, and duration of daylight times.
lat=34.0000
long=-92.3937     #Make sure longitude in negative

#### Set Up Calendar & Daylight Period ####
Spring=c(3:5) #You can manipulated these seasons however you like. AGFC typical considers spring, March-May, summer, June-August, etc. 
Summer=c(6:8)
Fall=c(9:11)
Winter=c(12,1,2)
calendar=data.frame(date=seq(start.date,end.date,by='day'),day=weekdays(seq(start.date,end.date,by='day')),month=month(seq(start.date,end.date,by='day')))
calendar$daytype=ifelse(is.weekend(calendar$date)|calendar$date %in% holidays,'weekend','weekday')
calendar$period[calendar$month %in% Spring]= 1
calendar$period[calendar$month %in% Summer]= 2
calendar$period[calendar$month %in% Fall]= 3
calendar$period[calendar$month %in% Winter]= 4
calendar=cbind(calendar,format(getSunlightTimes(date=calendar$date,lat=lat,lon=long,keep=c("sunrise","sunset"),tz="America/Chicago")[,4:5], format = "%H:%M"))
calendar$dl_hrs=signif(as.numeric(as.difftime(calendar$sunset, format = "%H:%M")-as.difftime(calendar$sunrise, format = "%H:%M")),digits=4)

#################################################
######          Import Trailer data        ######
#################################################

trailer=read.csv(file.choose(),na.strings=".",header=T)
trailer$date=as.Date(trailer$DateTime, format="%Y-%m-%d")
trailer$time=format(as.POSIXct(trailer$DateTime), format = "%H:%M")
trailer$hour=format(round_date(as.POSIXct(trailer$DateTime),"hour"), format = "%H:%M")
trailer$TTC=as.numeric(trailer$TTC)
trailer$TVC=as.numeric(trailer$TVC)
trailer$hour=as.factor(trailer$hour)

counts=trailer %>% group_by(hour,access=RelativePath,date) %>% summarize(TTC=sum(TTC),TVC=sum(TVC),Total=sum(TTC,TVC))

counts=merge(counts,calendar,by="date")

N.data=as.data.frame(table(calendar$period,droplevels(as.factor(calendar$daytype))))
colnames(N.data)=c('period','daytype','N')


#################################################
######                Analysis             ######
#################################################

sim=subset(counts,counts$hour %in% c("06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00"))


#sim$boat_hours = sim$TTC*sim$dl_hrs
#sim$boat_hours = sim$TVC*sim$dl_hrs
sim$boat_hours = sim$Total*sim$dl_hrs

Y_ = sim %>% group_by(access,date) %>% summarize(Y_=mean(boat_hours))
sim = merge(sim,Y_,by=c("date","access")) #Add column for number of days sampled
sim$deltaY_ = (sim$boat_hours-sim$Y_)^2 # Top of variance within day (yij-yi_)^2


#daily.sum =  sim %>% group_by(date,access) %>% summarize(Y_=mean(boat_hours),deltaY_.sum=sum(deltaY_),counts.per.day=length(TTC)) #Sum of deltaY across date and access
#daily.sum =  sim %>% group_by(date,access) %>% summarize(Y_=mean(boat_hours),deltaY_.sum=sum(deltaY_),counts.per.day=length(TVC)) #Sum of deltaY across date and access
daily.sum =  sim %>% group_by(date,access) %>% summarize(Y_=mean(boat_hours),deltaY_.sum=sum(deltaY_),counts.per.day=length(Total)) #Sum of deltaY across date and access


daily.sum = merge(unique(sim[,c("date","day","daytype","period")]),daily.sum,by=c("date"))
Y__ =   daily.sum %>% group_by(period,daytype,access) %>% summarize(Y__=mean(Y_)) #mean count each day
daily.sum = merge(daily.sum,Y__,by=c("period","daytype","access")) #Add column for number of days sampled
daily.sum$deltaY__ = (daily.sum$Y_-daily.sum$Y__)^2
strata.sum = daily.sum %>% group_by(period,daytype,access) %>% summarize(Y__=mean(Y_),deltaY_.sum=sum(deltaY_.sum),n=length(unique(date)),m=mean(counts.per.day),deltaY__.sum=sum(deltaY__))
strata.sum=merge(strata.sum,N.data,by=c("period","daytype"))
strata.sum$E = strata.sum$Y__*strata.sum$N
strata.sum$sw = strata.sum$deltaY_.sum/(strata.sum$n*(strata.sum$m-1))
strata.sum$sw[is.infinite(strata.sum$sw)] = 0
strata.sum$sb = strata.sum$deltaY__.sum/(strata.sum$n-1)
strata.sum$sb[is.nan(strata.sum$sb)] = 0
strata.sum$varY__ = (((1-(strata.sum$n/strata.sum$N))/strata.sum$n)*strata.sum$sb)+((1/(strata.sum$m*strata.sum$N))*strata.sum$sw)
strata.sum$varE = (strata.sum$N^2)*strata.sum$varY__


(total.summary = strata.sum %>% group_by(period,access,daytype) %>% summarize(E=sum(E),sw=sum(sw),sb=sum(sb),var=sum(varE),SE=sqrt(sum(varE)),RSE=((sqrt(sum(varE))/sum(E))*100)))
(strata.summary = strata.sum %>% group_by(period,daytype) %>% summarize(E=sum(E),sw=sum(sw),sb=sum(sb),var=sum(varE),SE=sqrt(sum(varE)),RSE=((sqrt(sum(varE))/sum(E))*100)))
(access.summary = strata.sum %>% group_by(period,access) %>% summarize(E=sum(E),var=sum(varE),SE=sqrt(sum(varE)),RSE=((sqrt(sum(varE))/sum(E))*100)))
(period.summary = strata.sum %>% group_by(period) %>% summarize(E=sum(E),sw=sum(sw),sb=sum(sb),var=sum(varE),SE=sqrt(sum(varE)),RSE=((sqrt(sum(varE))/sum(E))*100)))


(strata.summary = strata.sum %>% group_by(daytype) %>% summarize(E=sum(E),sw=sum(sw),sb=sum(sb),var=sum(varE),SE=sqrt(sum(varE)),RSE=((sqrt(sum(varE))/sum(E))*100)))
(access.summary = strata.sum %>% group_by(access) %>% summarize(E=sum(E),var=sum(varE),SE=sqrt(sum(varE)),RSE=((sqrt(sum(varE))/sum(E))*100)))
(summary = strata.sum %>% summarize(E=sum(E),sw=sum(sw),sb=sum(sb),var=sum(varE),SE=sqrt(sum(varE)),RSE=((sqrt(sum(varE))/sum(E))*100)))


#Mean Daily Effort
N.summary=N.data %>% group_by(period=period) %>% summarize(N=sum(N))
period.summary$E/N.summary$N
summary$E/sum(N.summary$N)
#Effort per Acre
period.summary$E/125
summary$E/125

#Access and Daytype Percents
(access.summary$E/summary$E)*100
(strata.summary$E/summary$E)*100

#Seasonal Effort
(period.summary$E/summary$E)*100
