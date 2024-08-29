load("Pooled.Results")

################### PE BoxPlot Comparison by Lake #######################################
y5=Pooled.Results %>% group_by(Counts,N,DayType,Lake) %>% summarise(y5= quantile(PE, probs = c(0.05)))
y20=Pooled.Results %>% group_by(Counts,N,DayType,Lake) %>% summarise(y20 = quantile(PE, probs = c(0.20)))
y50=Pooled.Results %>% group_by(Counts,N,DayType,Lake) %>% summarise(y50 = quantile(PE, probs = c(0.50)))
y80=Pooled.Results %>% group_by(Counts,N,DayType,Lake) %>% summarise(y80 = quantile(PE, probs = c(0.80)))
y95=Pooled.Results %>% group_by(Counts,N,DayType,Lake) %>% summarise(y95 = quantile(PE, probs = c(0.95)))
df=cbind(y5,y20=y20$y20,y50=y50$y50,y80=y80$y80,y95=y95$y95)

(wd=ggplot(df[which(df$DayType=="Day"),],aes(x=N,fill=Counts)) + 
  geom_boxplot(outlier.shape=NA,aes(ymin = y5, lower = y20, middle = y50, upper = y80, ymax = y95),stat = "identity") +
  facet_wrap(Lake~.) +
  scale_x_discrete(breaks=seq(0,20,2)) +
  theme_classic() +
  coord_cartesian(ylim=c(0, 100)) +
  #geom_hline(yintercept = 20,color='red') +
  #geom_hline(yintercept = 10,linetype=2,color='red') +
  scale_fill_discrete(name = "Photos per day") +
  labs(x='Weekdays',y='Percent Error'))

(we=ggplot(df[which(df$DayType=="End"),],aes(x=N,fill=Counts)) + 
  geom_boxplot(outlier.shape=NA,aes(ymin = y5, lower = y20, middle = y50, upper = y80, ymax = y95),stat = "identity") +
  facet_wrap(Lake~.) +
  theme_classic() +
  coord_cartesian(ylim=c(0, 100)) +
  #geom_hline(yintercept = 20,color='red') +
  #geom_hline(yintercept = 10,linetype=2,color='red') +
  scale_fill_discrete(name = "Photos per day") +
  labs(x='Weekends',y='Percent Error'))

(lake.boxplot = wd/we)

ggsave('Lake.boxplot.PE2.png',lake.boxplot,width=11,height=8,units="in")

################### PE BoxPlot Comparison Pooled #######################################
y5=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y5= quantile(PE, probs = c(0.05)))
y20=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y20 = quantile(PE, probs = c(0.20)))
y50=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y50 = quantile(PE, probs = c(0.50)))
y80=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y80 = quantile(PE, probs = c(0.80)))
y95=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y95 = quantile(PE, probs = c(0.95)))
df=cbind(y5,y20=y20$y20,y50=y50$y50,y80=y80$y80,y95=y95$y95)

(wd=ggplot(df[which(df$DayType=="Day"),],aes(x=N,fill=Counts)) + 
    geom_boxplot(outlier.shape=NA,aes(ymin = y5, lower = y20, middle = y50, upper = y80, ymax = y95),stat = "identity") +
    scale_x_discrete(breaks=seq(0,20,2)) +
    theme_classic() +
    coord_cartesian(ylim=c(0, 100)) +
    #geom_hline(yintercept = 20,linewidth=1.1, color='red') +
    #geom_hline(yintercept = 10,linewidth=1.1, linetype=2,color='red') +
    scale_fill_discrete(name = "Photos per day") +
    labs(x='Weekdays',y='Percent Error'))

(we=ggplot(df[which(df$DayType=="End"),],aes(x=N,fill=Counts)) + 
    geom_boxplot(outlier.shape=NA,aes(ymin = y5, lower = y20, middle = y50, upper = y80, ymax = y95),stat = "identity") +
    theme_classic() +
    coord_cartesian(ylim=c(0, 100)) +
    #geom_hline(yintercept = 20,linewidth=1.1,color='red') +
    #geom_hline(yintercept = 10,linewidth=1.1,linetype=2,color='red') +
    scale_fill_discrete(name = "Photos per day") +
    labs(x='Weekends',y='Percent Error'))

(pooled.boxplot = wd/we)

ggsave('pooled.boxplot.PE.png',pooled.boxplot,width=11,height=8,units="in")

################### PE BoxPlot Comparison across counts #######################################
y5=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y5= quantile(PE, probs = c(0.05)))
y20=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y20 = quantile(PE, probs = c(0.20)))
y50=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y50 = quantile(PE, probs = c(0.50)))
y80=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y80 = quantile(PE, probs = c(0.80)))
y95=Pooled.Results %>% group_by(Counts,N,DayType) %>% summarise(y95 = quantile(PE, probs = c(0.95)))
df=cbind(y5,y20=y20$y20,y50=y50$y50,y80=y80$y80,y95=y95$y95)

(wd=ggplot(df[which(df$DayType=="Day"),],aes(x=N)) + 
    geom_boxplot(outlier.shape=NA,aes(ymin = y5, lower = y20, middle = y50, upper = y80, ymax = y95),stat = "identity") +
    facet_wrap(Counts~., ncol=4) +
    scale_x_discrete(breaks=seq(0,20,2)) +
    theme_classic() +
    coord_cartesian(ylim=c(0, 80)) +
    geom_hline(yintercept = 20,color='red') +
    geom_hline(yintercept = 10,linetype=2,color='red') +
    labs(x='Weekdays',y='Percent Error'))

(we=ggplot(df[which(df$DayType=="End"),],aes(x=N)) + 
    geom_boxplot(outlier.shape=NA,aes(ymin = y5, lower = y20, middle = y50, upper = y80, ymax = y95),stat = "identity") +
    facet_wrap(Counts~., ncol=4) +
    theme_classic() +
    coord_cartesian(ylim=c(0, 80)) +
    geom_hline(yintercept = 20,color='red') +
    geom_hline(yintercept = 10,linetype=2,color='red') +
    labs(x='Weekends',y='Percent Error'))


(Count.boxplot = wd/we)

ggsave('Count.boxplot.PE.png',Count.boxplot,width=11,height=8,units="in")


