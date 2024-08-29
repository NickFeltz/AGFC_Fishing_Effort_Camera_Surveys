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
start.date=as.Date("2022/03/01")
end.date=as.Date("2022/08/31")
holidays=as.Date(c('2022/05/30',  #Memorial Day
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
Spring=c(3:5)
Summer=c(6:8)
Fall=c(9:11)
Winter=c(12,1,2)
calendar=data.frame(date=seq(start.date,end.date,by='day'),day=weekdays(seq(start.date,end.date,by='day')),month=month(seq(start.date,end.date,by='day')))
calendar$daytype=ifelse(is.weekend(calendar$date)|calendar$date %in% holidays,'weekend','weekday')
calendar$daytype[calendar$date %in% HighUseStrata]="HighUse"
calendar$period[calendar$month %in% Spring]= 1
calendar$period[calendar$month %in% Summer]= 2
calendar$period[calendar$month %in% Fall]= 3
calendar$period[calendar$month %in% Winter]= 4
calendar=cbind(calendar,format(getSunlightTimes(date=calendar$date,lat=lat,lon=long,keep=c("sunrise","sunset"),tz="America/Chicago")[,4:5], format = "%H:%M"))
calendar$dl_hrs=signif(as.numeric(as.difftime(calendar$sunset, format = "%H:%M")-as.difftime(calendar$sunrise, format = "%H:%M")),digits=4)


#### Creel Selection Design Set Up ####
daytype=c('Day','End')          #Designate DayType, could include high use strata
month=c(3:8)                   #Designated months to be included
selection=list(c(1:20),c(1:8))                #Designated number of selections per period/month for weekdays and weekend days

inst.time=c("7:00","8:00","9:00","10:00",
        "11:00","12:00","13:00","14:00",
        "15:00","16:00","17:00","18:00")                #Designate Instantaneous Count Times
inst.prob=rep(1/6,6)            #Designate Time probabilities
truth=c(8393,8936)        #Truth for Conway spring
#truth=c(12620,9370)        #Truth for Nimrod spring
#truth=c(889,822)        #Truth for Jack Nolan spring
#truth=c(7863,2530)        #Truth for Blue Mountain spring
#truth=c(28752,20660)        #Truth for Dardanelle spring
#truth=c(1402,1217)      #Truth for Sugarloaf spring

#################################################
######          Import Trailer data        ######
#################################################

trailer=read.csv("creel_data.csv",na.strings=".")
#trailer=trailer[which(trailer$period==2),] #only include period 2
trailer$date=as.Date(trailer$date,format = "%m/%d/%Y")
#trailer$TTC=as.numeric(trailer$TTC)
trailer$TVC=as.numeric(trailer$TVC)
trailer$daytype=as.factor(trailer$daytype)
trailer$day=as.factor(trailer$day)
trailer$month=as.factor(trailer$period)
trailer$hour=as.factor(trailer$hour)

counts=aggregate(trailer["TVC"],list(date=trailer$date,access=trailer$access,daytype=trailer$daytype,period=trailer$period,hour=trailer$hour,day=trailer$day),sum,na.rm=T)#sum multicamera ramps
#counts=aggregate(trailer["TTC"],list(date=trailer$date,daytype=trailer$daytype,period=trailer$period,hour=trailer$hour,day=trailer$day),sum,na.rm=T)#sum multicamera ramps

counts=merge(counts,calendar[,c(1,8)],by="date")
N.data=as.data.frame(table(calendar$period,droplevels(as.factor(calendar$daytype))))
colnames(N.data)=c('period','daytype','N')


############## Simulation  ###############
##########################################
runs=1000
maxcount=c(0.5,1,2,6)
c=4  #Counts per day shift
s=1  #Daytype Strata
n=6  #Sample size (n of days to collect counts)
r=100  #Run of Simulation
m=1  #Month/Period
i=1  #Instantaneous Count

results=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
pb = txtProgressBar(min = 0, max = length(maxcount), initial = 0, style=3, width=length(maxcount),char= "=") 
init <- numeric(length(maxcount))
end <- numeric(length(maxcount))
for (c in 1:length(maxcount)){
    init[c] <- Sys.time()
for (s in 1:length(daytype)){
for (n in 1:length(selection[[s]])){
pb1 = txtProgressBar(min = 0, max = runs, initial = 0, style=3, width=10,char= "=") 
for (r in 1:runs){
    design.summary=matrix(c(inst.prob),nrow=length(inst.time),ncol=1,dimnames=list(c(inst.time),daytype[s]))
    design.summary=rbind(design.summary,matrix(selection[[s]][n],nrow=1,ncol=1,dimnames=list('selection')))
    schedule=c(NA,NA,NA)
for (m in 1:length(month)){
    day=sample(calendar[which(calendar$daytype==daytype[s] & calendar$month==month[m]),]$date, size=selection[[s]][n],replace = FALSE)
    
if ((maxcount[c]*2)==1){
    time=matrix(NA,nrow=length(day),ncol=maxcount[c]*2)
      
for (i in 1:selection[[s]][n]){
    time[i,]=sample(inst.time[1:length(inst.time)], size=(maxcount[c]*2),prob=c(inst.prob,inst.prob),replace = FALSE)
}
    temp=data.frame(date=rep(day,(2*maxcount[c])),daytype=rep(daytype[s],(selection[[s]][n]*(2*maxcount[c]))),hour=c(time))
    schedule=rbind(schedule,temp)      
} else {
    timeAM=matrix(NA,nrow=length(day),ncol=maxcount[c])
    timePM=matrix(NA,nrow=length(day),ncol=maxcount[c]) 
    
for (i in 1:selection[[s]][n]){
    timeAM[i,]=sample(inst.time[1:(length(inst.time)/2)], size=maxcount[c],prob=inst.prob,replace = FALSE)
    timePM[i,]=sample(inst.time[((length(inst.time)/2)+1):length(inst.time)], size=maxcount[c],prob=inst.prob,replace = FALSE)
}
    temp=data.frame(date=rep(day,(2*maxcount[c])),daytype=rep(daytype[s],(selection[[s]][n]*(2*maxcount[c]))),hour=c(timeAM,timePM))
    schedule=rbind(schedule,temp)
}
}
    schedule=as.data.frame(schedule[-1,])
    FullSchedule=merge(calendar,schedule,all=TRUE)
  
    #sim=merge(schedule[,c("date","hour")],counts,by=c("date","hour"))
    sim=subset(counts,counts$hour %in% c("6:00","7:00","8:00","9:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00"))
    #sim$boat_hours = sim$TTC*sim$dl_hrs
    sim$boat_hours = sim$TVC*sim$dl_hrs
    Y_ = sim %>% group_by(access,date) %>% summarize(Y_=mean(boat_hours))
    sim = merge(sim,Y_,by=c("date","access")) #Add column for number of days sampled
    sim$deltaY_ = (sim$boat_hours-sim$Y_)^2 # Top of variance within day (yij-yi_)^2
    #daily.sum =  sim %>% group_by(date,access) %>% summarize(Y_=mean(boat_hours),deltaY_.sum=sum(deltaY_),counts.per.day=length(TTC)) #Sum of deltaY across date and access
    daily.sum =  sim %>% group_by(date,access) %>% summarize(Y_=mean(boat_hours),deltaY_.sum=sum(deltaY_),counts.per.day=length(TVC)) #Sum of deltaY across date and access
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
    
if ((maxcount[c]*2) == 1){
    strata.sum$varY__ = strata.sum$sb/strata.sum$n
} else {
    strata.sum$varY__ = (((1-(strata.sum$n/strata.sum$N))/strata.sum$n)*strata.sum$sb)+((1/(strata.sum$m*strata.sum$N))*strata.sum$sw)
}
    strata.sum$varE = (strata.sum$N^2)*strata.sum$varY__
    
    strata.summary = strata.sum %>% group_by(period,daytype) %>% summarize(E=sum(E),sw=sum(sw),sb=sum(sb),var=sum(varE),SE=sqrt(sum(varE)))
    access.summary = strata.sum %>% group_by(period,access) %>% summarize(E=sum(E),var=sum(varE),SE=sqrt(sum(varE)))
    period.summary = strata.sum %>% group_by(period) %>% summarize(E=sum(E),sw=sum(sw),sb=sum(sb),var=sum(varE),SE=sqrt(sum(varE)))
    period.summary$DayType=daytype[s]
    period.summary$N=selection[[s]][n]
    period.summary$Simulation=r
    period.summary$Counts=(maxcount[c]*2)
    period.summary$PE = (abs(truth[s]-period.summary$E)/truth[s])*100
    period.summary$RSE = (period.summary$SE/period.summary$E)*100
    results = rbind(results,period.summary)
    setTxtProgressBar(pb1, r)
}
  close(pb1)
}
}
  end[c] <- Sys.time()
  setTxtProgressBar(pb, c)
  time <- round(seconds_to_period(sum(end - init)), 0)
  est <- length(maxcount) * (mean(end[end != 0] - init[init != 0])) - time
  remainining <- round(seconds_to_period(est), 0)
  
  cat(paste(" // Execution time:", time,
            " // Estimated time remaining:", remainining), "")
}
close(pb)
    
results = as.data.frame(results[-1,])
summary=results[,c(1,10,7,8,9,2,3,4,5,6,11,12)]
summary=summary[order(summary$period,summary$Counts,summary$DayType,summary$N,summary$Simulation),]

###########################################

write.csv(Pooled.Results, file="Pooled.Results.csv")
save(summary,file="sbsw")

summary=read.csv("Pooled.Results.csv",na.strings=".")
load("Pooled.Results")

################### PE BoxPlot Comparison #######################################

(wd=ggplot(Pooled.Results[which(Pooled.Results$DayType=="Day"),],aes(x=N,y=PE,fill=Counts)) + 
  geom_boxplot(outlier.shape=NA) +
   #geom_text(aes(x=filter(Intermediate.goal,DayType=="Day")$N[1], y=20, label=1), col='red', size=4) +
   #geom_text(aes(x=filter(Intermediate.goal,DayType=="Day")$N[2], y=20, label=2), col='red', size=4) +
   #eom_text(aes(x=filter(Intermediate.goal,DayType=="Day")$N[3], y=20, label=4), col='red', size=4) +
   #geom_text(aes(x=filter(Intermediate.goal,DayType=="Day")$N[4], y=20, label=12), col='red', size=4) +
   facet_wrap(Lake~.) +
  scale_x_discrete(breaks=seq(0,20,2)) +
  theme_classic() +
  coord_cartesian(ylim=c(0, 100)) +
  geom_hline(yintercept = 20,color='red') +
  labs(x='Weekdays',y='Percent Error'))

we=ggplot(Pooled.Results[which(Pooled.Results$DayType=="End"),],aes(x=N,y=PE,fill=Counts)) + 
  geom_boxplot(outlier.shape=NA) +
  facet_wrap(Lake~.) +
  theme_classic() +
  coord_cartesian(ylim=c(0, 100)) +
  geom_hline(yintercept = 20,color='red') +
  labs(x='Weekends',y='Percent Error')

(pooled.boxplot = wd/we)

ggsave('Lake.boxplot.PE.png',pooled.boxplot,width=11,height=8,units="in")

# Inflection point of Median
med=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(median = median(PE, na.rm = TRUE))

med$CountFreq=as.numeric(as.character(med$Counts))*as.numeric(med$N)
med$Time=as.numeric(((med$CountFreq*0.0036)+0.1662))
med.wd=med[which(med$DayType=="Day"),]
med.we=med[which(med$DayType=="End"),]

med[which(med$DayType=="End" & med$Counts=="4" & med$N==4),]
med[which(med$DayType=="Day" & med$Counts=="4" & med$N==7),]

day.wgt = 0.5    #Weighting factor of number of days
count.wgt = 0.5  #Weighting factor of percent error

med.wd$releff=((1-as.numeric(med.wd$N)/max(as.numeric(med.wd$N)))*day.wgt)+((1-med.wd$median/max(med.wd$median))*count.wgt)
med.we$releff=((1-as.numeric(med.we$N)/max(as.numeric(med.we$N)))*day.wgt)+((1-med.we$median/max(med.we$median))*count.wgt)

(infl.wd = arrange((med.wd %>% group_by(Counts,DayType) %>% slice_max(releff)),Counts))
(infl.we = arrange((med.we %>% group_by(Counts,DayType) %>% slice_max(releff)),Counts))

# 75th %tile falls below 20% error
(Intermediate.goal = arrange((filter((Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(quartile = quantile(PE, probs = c(0.80)))),quartile<20)) %>% group_by(Counts,DayType) %>% slice_max(quartile),Counts))

# 75th %tile falls below 10% error
(Intermediate.goal = arrange((filter((Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(quartile = quantile(PE, probs = c(0.80)))),quartile<10)) %>% group_by(Counts,DayType) %>% slice_max(quartile),Counts))

################### PE BoxPlot Comparison across counts #######################################


y5=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y5= quantile(PE, probs = c(0.05)))
y20=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y20 = quantile(PE, probs = c(0.20)))
y50=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y50 = quantile(PE, probs = c(0.50)))
y80=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y80 = quantile(PE, probs = c(0.80)))
y95=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y95 = quantile(PE, probs = c(0.95)))
df=cbind(y5,y20=y20$y20,y50=y50$y50,y80=y80$y80,y95=y95$y95)
df.wd=df[which(df$DayType=="Day"),]

ggplot(df.wd, aes(x=N,fill=Counts)) +
  geom_boxplot(aes(ymin = y5, lower = y20, middle = y50, upper = y80, ymax = y95),stat = "identity"
  ) +
  geom_hline(yintercept = 20,color='red')

(wd=ggplot(Pooled.Results[which(Pooled.Results$DayType=="Day"),],aes(x=N,y=PE)) + 
   geom_boxplot(outlier.shape=NA,aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
                stat = "identity") +
   #geom_text(aes(x=filter(Intermediate.goal,DayType=="Day")$N[1], y=20, label=1), col='red', size=4) +
   #geom_text(aes(x=filter(Intermediate.goal,DayType=="Day")$N[2], y=20, label=2), col='red', size=4) +
   #geom_text(aes(x=filter(Intermediate.goal,DayType=="Day")$N[3], y=20, label=4), col='red', size=4) +
   #geom_text(aes(x=filter(Intermediate.goal,DayType=="Day")$N[4], y=20, label=12), col='red', size=4) +
   facet_wrap(Counts~., ncol=4) +
   scale_x_discrete(breaks=seq(0,20,2)) +
   theme_classic() +
   coord_cartesian(ylim=c(0, 50)) +
   geom_hline(yintercept = 20,color='red') +
   labs(x='Weekdays',y='Percent Error'))

we=ggplot(Pooled.Results[which(Pooled.Results$DayType=="End"),],aes(x=N,y=PE)) + 
  geom_boxplot(outlier.shape=NA) +
  facet_wrap(Counts~., ncol=4) +
  theme_classic() +
  coord_cartesian(ylim=c(0, 50)) +
  geom_hline(yintercept = 20,color='red') +
  labs(x='Weekends',y='Percent Error')

(pooled.boxplot = wd/we)

ggsave('Count.boxplot.PE.png',pooled.boxplot,width=11,height=8,units="in")

# Inflection point of Median
med=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(median = median(PE, na.rm = TRUE))

med$CountFreq=as.numeric(as.character(med$Counts))*as.numeric(med$N)
med$Time=as.numeric(((med$CountFreq*0.0036)+0.1662))
med.wd=med[which(med$DayType=="Day"),]
med.we=med[which(med$DayType=="End"),]

med[which(med$DayType=="End" & med$Counts=="4" & med$N==4),]
med[which(med$DayType=="Day" & med$Counts=="4" & med$N==7),]

day.wgt = 0.5    #Weighting factor of number of days
count.wgt = 0.5  #Weighting factor of percent error

med.wd$releff=((1-as.numeric(med.wd$N)/max(as.numeric(med.wd$N)))*day.wgt)+((1-med.wd$median/max(med.wd$median))*count.wgt)
med.we$releff=((1-as.numeric(med.we$N)/max(as.numeric(med.we$N)))*day.wgt)+((1-med.we$median/max(med.we$median))*count.wgt)

(infl.wd = arrange((med.wd %>% group_by(Counts,DayType) %>% slice_max(releff)),Counts))
(infl.we = arrange((med.we %>% group_by(Counts,DayType) %>% slice_max(releff)),Counts))

# 75th %tile falls below 20% error
(Intermediate.goal = arrange((filter((Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(quartile = quantile(PE, probs = c(0.75)))),quartile<20)) %>% group_by(Counts,DayType) %>% slice_max(quartile),Counts))

# 75th %tile falls below 10% error
(Intermediate.goal = arrange((filter((Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(quartile = quantile(PE, probs = c(0.75)))),quartile<10)) %>% group_by(Counts,DayType) %>% slice_max(quartile),Counts))

################### RSE BoxPlot Comparison ######################################
(wd=ggplot(Pooled.Results[which(Pooled.Results$DayType=="Day"),],aes(x=N,y=RSE,fill=Counts)) + 
  geom_boxplot(outlier.shape=NA) +
  facet_wrap(Lake~.) +
  scale_x_discrete(breaks=seq(0,20,2)) +
  theme_classic() +
  coord_cartesian(ylim=c(0, 100)) +
  geom_hline(yintercept = 20,color='red') +
  labs(x='Weekdays',y='Relative Standard Error'))

we=ggplot(Pooled.Results[which(Pooled.Results$DayType=="End"),],aes(x=N,y=RSE,fill=Counts)) + 
  geom_boxplot(outlier.shape=NA) +
  facet_wrap(Lake~.) +
  theme_classic() +
  coord_cartesian(ylim=c(0, 100)) +
  geom_hline(yintercept = 20,color='red') +
  labs(x='Weekends',y='Relative Standard Error')

(pooled.boxplot = wd/we)

ggsave('Pooled.boxplot.RSE.png',pooled.boxplot,width=11,height=8,units="in")

################### RSE vs PE BoxPlot Comparison ################################
Lake = "Conway"
(pe=ggplot(Pooled.Results[which(Pooled.Results$Lake==Lake),],aes(x=N,y=PE,fill=Counts)) + 
   geom_boxplot(outlier.shape=NA) +
   facet_wrap(DayType~.,scales = "free_x") +
   theme_classic() +
   coord_cartesian(ylim=c(0, 100)) +
   geom_hline(yintercept = 20,color='red') +
   labs(x='Weekdays',y='Percent Error'))

(rse=ggplot(Pooled.Results[which(Pooled.Results$Lake==Lake),],aes(x=N,y=RSE,fill=Counts)) + 
    geom_boxplot(outlier.shape=NA) +
    facet_wrap(DayType~.,scales = "free_x") +
    theme_classic() +
    coord_cartesian(ylim=c(0, 100)) +
    geom_hline(yintercept = 20,color='red') +
    labs(x='Weekdays',y='Relative Standard Error'))

(rsevspe = rse/pe)

ggsave('RSE_PE_Comparison.png',rsevspe,width=11,height=8,units="in")

################### Median calculations #########################################
med=Pooled.Results %>% group_by(Counts,N,DayType,Lake) %>% summarise(median = median(PE, na.rm = TRUE))
med=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(median = median(PE, na.rm = TRUE))



med$CountFreq=as.numeric(as.character(med$Counts))*as.numeric(med$N)
med$Time=as.numeric(((med$CountFreq*0.0036)+0.1662))
med.wd=med[which(med$DayType=="Day"),]
med.we=med[which(med$DayType=="End"),]

# Inflection point of Median
day.wgt = 0.5    #Weighting factor of number of days
count.wgt = 0.5  #Weighting factor of percent error

med.wd$releff=((1-as.numeric(med.wd$N)/max(as.numeric(med.wd$N)))*day.wgt)+((1-med.wd$median/max(med.wd$median))*count.wgt)
med.we$releff=((1-as.numeric(med.we$N)/max(as.numeric(med.we$N)))*day.wgt)+((1-med.we$median/max(med.we$median))*count.wgt)

infl.wd = arrange((med.wd %>% group_by(Counts,DayType,Lake) %>% slice_max(releff)),Lake,Counts)
infl.we = arrange((med.we %>% group_by(Counts,DayType,Lake) %>% slice_max(releff)),Lake,Counts)

# 75th %tile falls below 20% error
(Intermediate.goal = arrange((filter((Pooled.Results %>% group_by(Counts,N,DayType,Lake) %>% summarise(quartile = quantile(PE, probs = c(0.75)))),quartile<20)) %>% group_by(Counts,DayType,Lake) %>% slice_max(quartile),Lake,Counts))

################### Facet Days per Month Comparison #############################
(med.wd.plot=ggplot(med.wd,aes(x=N,y=median, color = Counts)) + 
    geom_point() +
    ggtitle("Weekdays") +
    scale_x_discrete(breaks=seq(0,20,2)) +
    #facet_wrap(Lake~.) +
    theme_classic() +
    geom_hline(yintercept = 20,color='red') +
    labs(x='Days per Month',y='Percent Error'))

med.we.plot=ggplot(med.we,aes(x=N,y=median, color = Counts)) + 
  geom_point() +
  ggtitle("Weekend Days") +
  #facet_wrap(Lake~.) +
  theme_classic() +
  geom_hline(yintercept = 20,color='red') +
  labs(x='Days per Month',y='Percent Error')

(facet.median = med.wd.plot/med.we.plot)

ggsave('Lake.Comparison.MedianPE_Days_per_Month.png',facet.median,width=8,height=11,units="in")

################### Facet Counts per Month Comparison #############################
(med.wd.plot=ggplot(med.wd,aes(x=CountFreq,y=median, color = Counts)) + 
    geom_point() +
    ggtitle("Weekdays") +
 #   scale_x_discrete(breaks=seq(0,240,40)) +
 #   facet_wrap(Lake~.) +
    theme_classic() +
    geom_hline(yintercept = 10,color='red') +
    labs(x='Counts per Month',y='Percent Error'))

(med.we.plot=ggplot(med.we,aes(x=CountFreq,y=median, color = Counts)) + 
  geom_point() +
  ggtitle("Weekend Days") +
#  facet_wrap(Lake~.) +
  theme_classic() +
  geom_hline(yintercept = 10,color='red') +
  labs(x='Counts per Month',y='Percent Error'))

(facet.median.counts = med.wd.plot/med.we.plot)

ggsave('Lake.Comparison.MedianPE_Counts_per_Month.png',facet.median.counts,width=8,height=11,units="in")
ggsave('WildScience.png',facet.median.counts,width=8,height=11,units="in")

################### Attempt at Maximum Efficiency ###############################
#### Days per Month ####
(releff.wd.plot=ggplot(med.wd,aes(x=N,y=releff,color=Counts)) + 
  geom_point() +
  ggtitle("Weekdays") +
  scale_x_discrete(breaks=seq(0,20,2)) +
  facet_wrap(Lake~.) +
  theme_classic() +
  labs(x='Days per Month',y='Releff'))

(releff.we.plot=ggplot(med.we,aes(x=N,y=releff,color=Counts)) + 
    geom_point() +
    ggtitle("Weekend Days") +
    facet_wrap(Lake~.) +
    theme_classic() +
    labs(x='Days per Month',y='Releff'))

(releff.days = releff.wd.plot/releff.we.plot)

ggsave('Relative.Eff.Days_per_Month.png.png',releff.days ,width=8,height=11,units="in")

#### Counts per Month ####
med.wd$releff=((1-as.numeric(med.wd$CountFreq)/max(as.numeric(med.wd$CountFreq)))*day.wgt)+((1-med.wd$median/max(med.wd$median))*count.wgt)
releff.wd.plot=ggplot(med.wd,aes(x=CountFreq,y=releff,color=Counts)) + 
  geom_point() +
  ggtitle("Weekdays") +
  facet_wrap(Lake~.) +
  theme_classic() +
  labs(x='Counts per Month',y='Releff')

med.we$releff=((1-as.numeric(med.we$CountFreq)/max(as.numeric(med.we$CountFreq)))*day.wgt)+((1-med.we$median/max(med.we$median))*count.wgt)
releff.we.plot=ggplot(med.we,aes(x=CountFreq,y=releff,color=Counts)) + 
  geom_point() +
  ggtitle("Weekend Days") +
  facet_wrap(Lake~.) +
  theme_classic() +
  labs(x='Counts per Month',y='Releff')

(releff.counts = releff.wd.plot/releff.we.plot)

ggsave('Relative.Eff.Counts_per_Month.png',releff.counts ,width=8,height=11,units="in")

# Inflection point of Median

infl.wd = arrange((med.wd %>% group_by(Counts,DayType,Lake) %>% slice_max(releff)),Lake,Counts)
infl.we = arrange((med.we %>% group_by(Counts,DayType,Lake) %>% slice_max(releff)),Lake,Counts)

# 75th %tile falls below 20% error
(Intermediate.goal = arrange((filter((Pooled.Results %>% group_by(Counts,N,DayType,Lake) %>% summarise(quartile = quantile(PE, probs = c(0.75)))),quartile<20)) %>% group_by(Counts,DayType,Lake) %>% slice_max(quartile),Lake,Counts))




Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(mean = mean(PE, na.rm = TRUE))

Count1.Results=Pooled.Results[which(Pooled.Results$Counts==1),]
Count2.Results=Pooled.Results[which(Pooled.Results$Counts==2),]
Count4.Results=Pooled.Results[which(Pooled.Results$Counts==4),]
Count12.Results=Pooled.Results[which(Pooled.Results$Counts==12),]
Count.Comparison=cbind(Count1.Results[,c(1,3,4,5)],Count1=Count1.Results[,9],Count2=Count2.Results[,9],Count4=Count4.Results[,9],Count12=Count12.Results[,9])
Count.Comparison$Diff1.2=Count.Comparison$Count1-Count.Comparison$Count2
Count.Comparison$Diff2.4=Count.Comparison$Count2-Count.Comparison$Count4
Count.Comparison$Diff4.12=Count.Comparison$Count4-Count.Comparison$Count12
mean(Count.Comparison$Diff1.2)
mean(Count.Comparison$Diff2.4)
mean(Count.Comparison$Diff4.12)

Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(mean = mean(PE, na.rm = TRUE))

Count1.Results=Pooled.Results[which(Pooled.Results$DayType=="Day" & Pooled.Results$N==10),]
Count2.Results=Pooled.Results[which(Pooled.Results$DayType=="Day" & Pooled.Results$N==11),]
Count4.Results=Pooled.Results[which(Pooled.Results$DayType=="Day" & Pooled.Results$N==12),]
Count12.Results=Pooled.Results[which(Pooled.Results$DayType=="Day" & Pooled.Results$N==13),]
Count.Comparison=cbind(Count1.Results[,c(1,3,4,5)],Count1=Count1.Results[,9],Count2=Count2.Results[,9],Count4=Count4.Results[,9],Count12=Count12.Results[,9])
Count.Comparison$Diff1.2=Count.Comparison$Count1-Count.Comparison$Count2
Count.Comparison$Diff2.4=Count.Comparison$Count2-Count.Comparison$Count4
Count.Comparison$Diff4.12=Count.Comparison$Count4-Count.Comparison$Count12
mean(Count.Comparison$Diff1.2)
mean(Count.Comparison$Diff2.4)
mean(Count.Comparison$Diff4.12)
