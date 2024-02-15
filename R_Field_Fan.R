install.packages(c("svglite","effsize", "extrafont","ggpubr","Metrics"))

library(tidyverse);library(readxl);library(extrafont);library(ggpubr);library(effsize);library(scales);library(Metrics)


########
##spreadsheets  Q2
Q2 <- read_excel("C:/Users/mafo0648/OneDrive - The University of Sydney (Staff)/Documents/campo2022/medicao/Quest2-2022-total.xlsx") #responses questionnaire Q2
##Outdoor conditions - weather station INMET 806
Out <- read_delim("C:/Users/mafo0648/OneDrive - The University of Sydney (Staff)/Documents/campo2022/medicao/dados_A806_H_2022-01-01_2022-02-19_inmet.csv", 
                  skip = 10, delim = ";")%>%select(`Data Medicao`,`Hora Medicao`,`TEMPERATURA DO AR - BULBO SECO, HORARIA(°C)`,`UMIDADE RELATIVA DO AR, HORARIA(%)`)%>%
  rename(day=`Data Medicao`,time=`Hora Medicao`,Text=`TEMPERATURA DO AR - BULBO SECO, HORARIA(°C)`,RHext=`UMIDADE RELATIVA DO AR, HORARIA(%)`)%>%
  mutate(time=as.numeric(time)/100)%>%
  mutate(day_time=as.POSIXct(paste0(day,"",time,":00:00"),format="%Y-%m-%d %H:%M:%S"))
##measured indoor conditions
Indoor <- read_delim()


### Process Q2 
##Add indoor conditions
Q2<-Q2%>%
  mutate(day_time=round_date(day_time, unit = "minutes"))%>%
  left_join(Indoor,
              by=join_by(closest(day_time>= day_time),sensor))
              
## add classifiers
Q2<-Q2%>%
  mutate(Prepost=ifelse(num> 86, 1,0),
         Tar_bin=cut(Tar, breaks=c(22,23,24,25,26,27,28), labels = c("22-23","23-24", "24-25", "25-26", "26-27", "27-28")))%>%
  mutate(Comf01=ifelse(COMFORT>0,1,0),Pref01=ifelse(PREFERENCE==0,0,1))

## add outdoor conditions
Q2 <- Q2%>%
  left_join(
    Out%>%
      mutate(day_factor=format(day_time,"%m/%d"))%>%
      group_by(day_factor)%>%
      summarise(TextM=mean(Text),RHextM=mean(RHext))%>%
      mutate(Tmpa=0.34*lag(TextM,n=1)+
               0.23*lag(TextM,n=2)+
               0.16*lag(TextM,n=3)+
               0.11*lag(TextM,n=4)+
               0.08*lag(TextM,n=5)+
               0.05*lag(TextM,n=6)+
               0.03*lag(TextM,n=7)))

## Q2 spreadsheet by day - used for index analysis
  ## Q2 daily    
Q2_daily <-
  Q2%>%
  mutate(day_factor=format(day_time.x,"%m/%d"))%>%
  group_by(day_factor)%>%
  summarise(conf=sum(Comf01)/length(Comf01)*100,
            pref=(length(Pref01)-sum(Pref01))/length(Pref01)*100,
            d2=mean(d2),Tmpa,TextM,RHextM)
  
Q2_daily <-Q2_daily%>%
  left_join(
Indoor%>%
    mutate(hour=as.numeric(format(day_time,"%H")),day_factor=format(day_time,"%m/%d"))%>%
    filter(between(hour,8,18),day_factor %in% c(unique(format(Q2$day,"%m/%d"))))%>%
    group_by(day_factor,hour)%>%
    summarise(hour=mean(hour),Tar=mean(Tar),RH=mean(RH))%>%
  left_join(Q2%>%
              group_by(day_factor)%>%
              summarise(d2=mean(d2)))%>%
    mutate(
    Indoor%>%
    mutate(hour=as.numeric(format(day_time,"%H")),day_factor=format(day_time,"%m/%d"))%>%
    filter(between(hour,8,18),day_factor %in% c(unique(format(Q2$day,"%m/%d"))))%>%
    mutate(Prepost=ifelse(day_factor<"01/12",0,1))%>%
    filter(Prepost==0)%>%
    summarise(Q80=quantile(Tar,prob=0.80),Q90=quantile(Tar,prob=0.90)))%>%
  left_join(
    Indoor%>%
      mutate(hour=as.numeric(format(day_time,"%H")),day_factor=format(day_time,"%m/%d"))%>%
      filter(between(hour,8,18),day_factor %in% c(unique(format(Q2$day,"%m/%d"))))%>%
      group_by(day_factor,hour)%>%
      left_join(Q2%>%
                  group_by(day_factor)%>%
                  summarise(d2=mean(d2)))%>%
      group_by(d2)%>%
      summarise(Q80_2=quantile(Tar,probs = 0.8),Q90_2=quantile(Tar,probs = 0.9))%>%
      mutate(d2=d2+1))%>%
  left_join(
    Out%>%
      mutate(day_factor=format(day_time,"%m/%d"))%>%
      group_by(day_factor)%>%
      summarise(TextM=mean(Text))%>%
      mutate(Tmpa=0.34*lag(TextM,n=1)+
               0.23*lag(TextM,n=2)+
               0.16*lag(TextM,n=3)+
               0.11*lag(TextM,n=4)+
               0.08*lag(TextM,n=5)+
               0.05*lag(TextM,n=6)+
               0.03*lag(TextM,n=7)))%>%
  group_by(day_factor)%>%
    summarise(Tar_m=mean(Tar),Tar_max=max(Tar),Q80P=sum(ifelse(Tar>Q80,1,0))/n()*100,Q90P=sum(ifelse(Tar>Q90,1,0))/n()*100,
              Q80P2=sum(ifelse(Tar>Q80_2,1,0))/n()*100,Q90P2=sum(ifelse(Tar>Q90_2,1,0))/n()*100,
              Q80_day=quantile(Tar,probs = 0.8),Tmpa_ex=sum(ifelse(Tar>Tmpa,1,0))/n()*100)%>%
      mutate(D_m=abs(Tar_m-lag(Tar_m)),D_max=abs(Tar_max-lag(Tar_max)),
        D80=abs(Q80_day-lag(Q80_day))))


### graphs ###

  ###  Fig 2 - Temp indoor vs setpoint by period ### 
ggsave(file="C:/Users/mafo0648/OneDrive - The University of Sydney (Staff)/Documents/campo2022/final/Tar_setpoint.svg",
       Indoor%>%
         mutate(hour=as.numeric(format(day_time,"%H")),day_factor=format(day_time,"%m/%d"))%>%
         filter(between(hour,8,18), day_factor %in% c(unique(format(Q2$day,"%m/%d"))))%>%
         left_join(Q2%>%
                     mutate(day_factor=format(Q2$day_time.x,"%m/%d"))%>%
                     select(day_factor,setpoint,period))%>%
         ggplot( aes(Tar, fill=period,as.factor(setpoint)))+
         geom_boxplot(varwidth = TRUE,outlier.shape = NA)+labs(x="Air temperature (ºC)", y="Setpoint temperature (ºC)\n")+
         scale_fill_discrete(labels=c("Morning","Afternoon"), "")+
         scale_y_discrete(expand = c(0,0))+
         xlim(23.5,28)+
         geom_vline(xintercept = 25.09, lty="dashed", col="black")+
         annotate("text", x=24.95, y = "26", hjust = -0.2, label="Median", angle=90, size=4.3, family="sans")+
         theme_pubclean(base_family = "sans")+
         theme( legend.key = element_rect(colour = NA, fill = NA),text=element_text(size=14),legend.text = element_text(size=12),axis.title = element_text(size=12),
                legend.position = c(0.8, 0.15),legend.direction = "horizontal"), 
       width=8, height=3.5)

    ## median indoor air temp = 25.1°C
Indoor%>%
  mutate(hour=as.numeric(format(day_time,"%H")),day_factor=format(day_time,"%m/%d"))%>%
  filter(between(hour,8,18), day_factor %in% c(unique(format(Q2$day,"%m/%d"))))%>%
  left_join(Q2%>%
              mutate(day_factor=format(Q2$day_time.x,"%m/%d"))%>%
              select(day_factor,setpoint,period))%>%
  summarise(median(Tar))

### Fig 3 - Temp & RH indoor vs outdoor  ###
ggsave(file="C:/Users/mafo0648/OneDrive - The University of Sydney (Staff)/Documents/campo2022/final/Out_Indoor.svg",
       ggarrange(
             Indoor %>%
               mutate(hour=as.numeric(format(day_time,"%H")))%>%
               mutate(day_factor=format(day_time,"%m/%d"))%>%
               filter(between(hour,8,18), day_factor %in% c(unique(format(Q2$day,"%m/%d"))))%>%
               group_by(day_factor, hour)%>%
               summarise(TarM=mean(Tar),RHM=mean(RH),Tar_sd=sd(Tar))%>%
           left_join(
             Out%>%
               mutate(hour=as.numeric(format(day_time,"%H")))%>%
               mutate(day_factor=format(day_time,"%m/%d"))%>%
               group_by(day_factor, hour)%>%
               summarise(TextM=mean(Text),RHextM=mean(RHext)))%>%
             mutate(period=ifelse(hour<13,"M","T"))%>%
             left_join(Q2%>%
                         mutate(day_factor=format(day,"%m/%d"))%>%
                         group_by(day_factor,period)%>%
                         summarise(setpoint=mean(setpoint)))%>%
           mutate(line=ifelse(TextM<=24,"1","2"))%>%
           ggplot(aes(TextM,TarM,shape=line))+geom_point(aes(col=as.factor(setpoint)))+
           geom_smooth(col="black", method = "lm", show.legend = NA)+
           theme_pubclean(base_family = "sans")+
           theme( legend.key = element_rect(colour = NA, fill = NA),legend.position="bottom",text=element_text(size=14),legend.text = element_text(size=12), axis.title =element_text(size=12),plot.subtitle = element_text(face = "bold") )+
           scale_color_manual(values = c("#4477AA", "#66CCEE","#CCBB44","#EE6677", "#AA3377"))+
            #stat_cor(aes(y=TarM,x=TextM),col="black", method = "spearman")+ #p and rho lines
           labs(x="Outdoor air temperature (°C)",y="Indoor air temperature (°C)", col="Setpoint",subtitle = "a)")+
           scale_y_continuous(limits = c(23.5,27))+guides(shape="none")+scale_shape_manual(values = c(19,19,19,19))+
           scale_x_continuous(limits = c(19.5,34),breaks = seq(min(20), max(34), by = 4))+coord_fixed(ratio = 1), 

           Indoor %>%
             mutate(hour=as.numeric(format(day_time,"%H")))%>%
             mutate(day_factor=format(day_time,"%m/%d"))%>%
             filter(between(hour,8,18), day_factor %in% c(unique(format(Q2$day,"%m/%d"))))%>%
             group_by(day_factor, hour)%>%
             summarise(TarM=mean(Tar),RHM=mean(RH),Tar_sd=sd(Tar))%>%
             left_join(
               Out%>%
                 mutate(hour=as.numeric(format(day_time,"%H")))%>%
                 mutate(day_factor=format(day_time,"%m/%d"))%>%
                 group_by(day_factor, hour)%>%
                 summarise(TextM=mean(Text),RHextM=mean(RHext)))%>%
             mutate(period=ifelse(hour<13,"M","T"))%>%
             left_join(Q2%>%
                         mutate(day_factor=format(day,"%m/%d"))%>%
                         group_by(day_factor,period)%>%
                         summarise(setpoint=mean(setpoint)))%>%
           ggplot(aes(RHextM,RHM))+
           geom_smooth(col="black")+geom_point(aes(col=as.factor(setpoint)))+
           theme_pubclean(base_family = "sans")+
           theme( legend.key = element_rect(colour = NA, fill = NA),text=element_text(size=14),legend.text = element_text(size=11), axis.title =element_text(size=12),plot.subtitle = element_text(face = "bold"))+
           scale_color_manual(values = c("#4477AA", "#66CCEE","#CCBB44","#EE6677", "#AA3377"))+
           labs(x="Outdoor relative humidity (%)",y="Indoor relative humidity (%)", col="",subtitle = "b)")+
           scale_y_continuous(limits = c(45,80))+scale_x_continuous(breaks = seq(min(40), max(90), by = 20))+coord_fixed(ratio = 1),
         ncol=2, nrow=1,  widths = c(2.2222,1), heights = c(1,1),common.legend = TRUE, legend = "bottom"), width = 8, height=3)


###  Fig 4 - COMFORT & PREFERENCE      
ggsave(file="C:/Users/mafo0648/OneDrive - The University of Sydney (Staff)/Documents/campo2022/final/Comf-pref.svg",
       ggarrange (   
         Q2%>%
           filter(!Tar_bin %in% c("22-23","26-27","27-28"))%>%
           group_by(Prepost,Tar_bin)%>%
           count(PREFERENCE)%>%
           left_join(
             Q2%>%
               filter(!Tar_bin %in% c("22-23","26-27","27-28"))%>%
               group_by(Prepost,Tar_bin)%>%
               count(Tar_bin)%>%
               rename(sum_p=n))%>%
           mutate(lab=round(n/sum_p*100))%>%
           ggplot(aes(x=Tar_bin, y=lab, fill=as.factor(PREFERENCE)))+
           geom_bar(stat="identity")+
           labs(x="Air temperature bin (°C)", y="Votes (%)", subtitle = "a) Preference votes")+
           facet_wrap(~Prepost, labeller = as_labeller(c("0"="Pre","1"="Post")))+
           scale_y_continuous(expand = c(0,2))+
           geom_text(aes(label=ifelse(PREFERENCE==0,paste("N=",sum_p),NA), y=103))+
           theme_pubclean(base_family = "sans")+
           theme(strip.background = element_rect(fill=NA, color = NA), axis.ticks.x = element_blank(), text=element_text(size=14),strip.text.x = element_text(face="bold", size=12),
                 axis.title.y = element_text(size=12), axis.title.x = element_blank(),legend.text = element_text(size=12),axis.text.x = element_blank())+
           scale_fill_manual(values = c("#FFB2A5","#AAD37B","#3ADAE4"),"",labels = c("Cooler", "No change", "Warmer"))+
           geom_text(aes(label=ifelse(PREFERENCE==0,lab,NA)), position=position_stack(vjust = 0.5)),
         Q2%>%
           filter(PREFERENCE==0)%>%
           ggplot(aes(Tar, as.factor(Prepost),fill=as.factor(Prepost)))+geom_boxplot(outlier.shape = NA)+scale_y_discrete("",limits=c("1","0"),labels=c("Post","Pre"))+
           scale_x_continuous(breaks = seq(23,26,1), limits = c(23,26.5))+
           theme_pubclean(base_family = "sans")+labs(x="Air temperature (°C)", subtitle = "c) Preferred temperatures")+scale_fill_manual("",values = c("#cce5b0ff","#cce5b0ff"),labels=c("Pre","Post"))+
           theme(legend.key = element_rect(fill="transparent", color = "transparent"), axis.ticks.y = element_blank(), text=element_text(size=14),#axis.text.y = element_blank(),
                 axis.title = element_text(size=12), legend.text = element_text(size=12),axis.title.x = element_blank(),axis.ticks.x = element_blank(),
                 axis.text.x = element_blank(),panel.grid.major.x = element_line(color = "grey",linetype = 3),panel.grid.major.y = element_blank())+guides(fill=guide_legend(nrow=2,byrow=TRUE))+
           stat_summary(fun=mean, colour="black", geom="point", show.legend = FALSE)+
           stat_summary(vjust=-2.5, aes( label=round(..x.., digits=1)),fun=mean, colour="black", geom="text", show.legend = FALSE),
         Q2%>%
           filter(!Tar_bin %in% c("22-23","26-27","27-28"))%>%
           group_by(Prepost,Tar_bin)%>%
           count(COMFORT)%>%
           left_join(
             Q2%>%
               filter(!Tar_bin %in% c("22-23","26-27","27-28"))%>%
               group_by(Prepost,Tar_bin)%>%
               count(Tar_bin)%>%
               rename(sum_p=n))%>%
           mutate(lab=round(n/sum_p*100))%>%
           mutate(conf=ifelse(COMFORT==2,"4",ifelse(COMFORT==1,"3",ifelse(COMFORT==-1,"2", "1"))))%>%
           ggplot(aes(x=Tar_bin, y=lab, fill=conf))+
           geom_bar(stat="identity")+
           labs(x="Air temperature bin (°C)", y="Votes (%)", subtitle = "b) Comfort votes")+
           facet_wrap(~Prepost, labeller = as_labeller(c("0"="Pre","1"="Post")))+
           scale_y_continuous(expand = c(0,2))+#labels = percent_format(suffix = "") se não tiver calculado o percentual 
           #geom_text(aes(label=ifelse(COMFORT==1,paste("N=",sum_p),NA), y=103))+
           theme_pubclean(base_family = "sans")+
           theme(strip.background = element_rect(fill="transparent", color = "transparent"), axis.ticks.x = element_blank(), text=element_text(size=14),strip.text.x = element_blank(),
                 axis.title = element_text(size=12), legend.text = element_text(size=12), legend.position = "bottom")+guides(fill=guide_legend(nrow=2,byrow=TRUE))+
           scale_fill_manual(values = c("#7CBD47","#AAD37B","#FFA2B3","#FD5F5F"),"",breaks = c("4","3","2","1"),labels = c("Very comfortable", "Just comfortable", "Just uncomfortable", "Very uncomfortable"))+
           geom_text(aes(label=ifelse(COMFORT %in% c(2,1) ,lab,NA)), position=position_stack(vjust = 0.5)),
         Q2%>%
           filter(Comf01==1)%>%
           ggplot(aes(Tar, as.factor(Prepost),fill=as.factor(Prepost)))+geom_boxplot(outlier.shape = NA)+
           scale_x_continuous(breaks = seq(23,26,1), limits = c(23,26.5))+scale_y_discrete("",limits=c("1","0"),labels=c("Post","Pre"))+
           theme_pubclean(base_family = "sans")+labs(x="Air temperature (°C)", subtitle = "d) Comfortable temperatures")+scale_fill_manual("",values = c("#cce5b0ff","#cce5b0ff"), labels=c("Pre","Post"))+
           theme(legend.key = element_rect(fill="transparent", color = "transparent"), axis.ticks.y = element_blank(), text=element_text(size=14),#axis.text.y = element_blank(),
                 axis.title = element_text(size=12), legend.text = element_text(size=12), legend.position = "bottom", axis.ticks.x = element_blank(),
                 panel.grid.major.x = element_line(color = "grey",linetype = 3),panel.grid.major.y = element_blank())+guides(fill=guide_legend(nrow=2,byrow=TRUE))+
           stat_summary(fun=mean, colour="black", geom="point", show.legend = FALSE)+  
           stat_summary(vjust=-2.5, aes( label=round(..x.., digits=1)),fun=mean, colour="black", geom="text", show.legend = FALSE),
         ncol=2, nrow=2, align="hv", heights = c(0.88,1), widths = c(1,0.7)), width = 8, height = 6)
  
## preferred and comfable temperature intervals
Q2%>%
  filter(PREFERENCE==0)%>%
  group_by(Prepost)%>%
  summarise(mean(Tar),sd(Tar),quantile(Tar,.25),quantile(Tar,.75))
Q2%>%
  filter(Comf01==1)%>%
  group_by(Prepost)%>%
  summarise(mean(Tar),sd(Tar),quantile(Tar,.25),quantile(Tar,.75))

    ## Cliff delta
cliff.delta(d=Q2$Pref01, f=Q2$Prepost)# delta: -0.48 (large)
cliff.delta(d=Q2$Comf01, f=Q2$Prepost)# delta: -0.05 (negligible)
   ## t.test
t.test(Q2$Tar[Q2$Pref01==0 & Q2$Prepost==0],Q2$Tar[Q2$Pref01==0 & Q2$Prepost==1],paired=FALSE, method="spearman") # significantly diff
t.test(Q2$Tar[Q2$Comf01==0 & Q2$Prepost==0],Q2$Tar[Q2$Comf01==0 & Q2$Prepost==1],paired=FALSE, method="spearman") # significantly diff


###  Fig 5 - daily comfort & temperatures ###
ggsave(file="C:/Users/mafo0648/OneDrive - The University of Sydney (Staff)/Documents/campo2022/final/Comf_daily.svg",
       ggarrange (
         Q2 %>%
           mutate(day_factor=as.factor(format(day,"%m/%d"))) %>%
           mutate(comf=ifelse(COMFORT==2,"4",ifelse(COMFORT==1,"4",ifelse(COMFORT==-1,"2", "1"))))%>%
           ggplot(aes(day_factor,fill=as.factor(conf)))+
           scale_fill_manual(breaks = c("4","2","1"),values = c("#AAD37B","#FFB2A5","#FD5F5F"),labels = c("Comfortable", "Just uncomfortable", "Very uncomfortable"))+
           scale_y_continuous(labels = percent_format(suffix = ""),expand = c(0,0))+
           geom_bar(position="fill")+
           theme_pubclean(base_family = "sans")+
           theme(strip.background = element_rect(fill=NA, color = NA), text=element_text(size=14),strip.text.x = element_text(face="bold", size=12),
                 axis.title = element_text(size=12), legend.text = element_text(size=12),legend.direction="horizontal", legend.position = "top", axis.text.x = element_blank())+
           labs(x=NULL, fill=NULL,y="Percent of votes (%)"),
         Indoor%>%
           mutate(hour=as.numeric(format(day_time,"%H")),day_factor=format(day_time,"%m/%d"))%>%
           filter(between(hour,8,18), day_factor %in% c(unique(format(Q2$day,"%m/%d"))))%>%
           select(day_factor,Tar)%>%
           left_join(Q2%>%
                       mutate(day_factor=as.factor(format(day,"%m/%d"))) %>%
                       group_by(day_factor)%>%
                       summarise(SetpointM=mean(setpoint[period=="M"]), SetpointT=mean(setpoint[period=="T"]),Prepost=mean(Prepost))%>%
                       mutate(Prepost=ifelse(Prepost>0,1,0))%>%
           left_join(Out%>%
                       mutate(day_factor=format(day_time,"%m/%d"))%>%
                       group_by(day_factor)%>%
                       summarise(TextM=mean(Text))))%>%
           ggplot(aes(x=day_factor))+
           geom_boxplot(aes(y=Tar, fill=as.factor(Prepost),group=day_factor),outlier.shape = NA)+
           stat_summary(aes(y=Tar, group=day_factor),fun=mean, colour="black", geom="point", show.legend = FALSE)+
           stat_summary(aes(y=TextM, group=2),fun=mean, geom="line", lty="dashed")+
           #stat_summary(aes(y=X7, group=2),fun=mean, geom="line", lty="dashed")+ #RH
           #stat_summary(aes(y=TextM, group=2),fun=mean, geom="point")+ #Text
           #annotate("text", x="02/15", y = 28.5,hjust = 0.3, label=expression('T'['out']),  size=4.3, family="sans")+
           annotate("text", x="01/10", y = 28.5, hjust = 0.2, label="Pre-int.",  size=4.3, family="sans")+
           stat_summary(aes(y=as.numeric(SetpointM), group=1),fun=mean, geom="point", shape=6)+
           stat_summary(aes(y=as.numeric(SetpointT), group=1),fun=mean, geom="point", shape=6)+
           scale_y_continuous(expand = c(0,0),breaks = seq(21,29,1))+
           scale_fill_manual(values = c("#757575","#D8D8D8"))+
           theme_pubclean(base_family = "sans")+
           theme(strip.background = element_rect(fill=NA, color = NA), text=element_text(size=14),strip.text.x = element_text(face="bold", size=12),
                 axis.title = element_text(size=12), legend.text = element_text(size=12),legend.position="none", axis.text.x = element_text(angle = 90, vjust = 0.5) )+
           labs(x=NULL,y="Temperature (°C)\n"),
         ncol=1, nrow=2, align="v", heights = c(2.2,2.8)),
       width = 8, height = 5)

## calculate mean Tar pre-intervention vs last week
t.test(Indoor$Tar[Indoor$day_time < "2022-01-12 UTC"],Indoor$Tar[Indoor$day_time > "2022-02-14 UTC" & Indoor$day_time < "2022-02-18 UTC"], paired=FALSE, method="spearman")#p<0.05 signifficantly different 24.2 & 25.2

#compare comf votes pre-intervention vs last week
Q2 %>%
  mutate(day_factor=as.factor(format(day,"%m/%d"))) %>%
  mutate(comf=ifelse(COMFORT==2,"4",ifelse(COMFORT==1,"4",ifelse(COMFORT==-1,"2", "1"))))%>%
  filter(day_factor %in% c("01/10","01/11","02/15","02/16","02/17"))%>%
  group_by(week,day_factor)%>%
  count(comf)%>% rename(prop=n)%>%
  left_join(
    Q2%>%
      mutate(day_factor=as.factor(format(day,"%m/%d"))) %>%
      filter(day_factor %in% c("01/10","01/11","02/15","02/16","02/17"))%>%
      group_by(day_factor)%>%
      count(week,day_factor))%>%
  mutate(P=prop/n*100)%>%
  filter(comf==4)%>%
  group_by(week)%>%
  summarise(mean(P))
  

### Fig 6 - hourly analysis fan status vs pref e comf - conclusion: non-linear relation, on/off fan cannot be used to predict perception
ggsave(file="C:/Users/mafo0648/OneDrive - The University of Sydney (Staff)/Documents/campo2022/final/Percept_fan.svg",
       ggarrange(
  Q2%>%
    filter(Prepost==1)%>%
  mutate(day_factor=format(day,"%m/%d"))%>%
  mutate(hour=format(day_time.x,"%H"))%>%
  group_by(day_factor,hour)%>%
  summarise(percent_comf=sum(Comf01)/n(),percent_pref=sum(1-Pref01)/n(),P_status=sum(status_fan)/n())%>%
  ggplot(aes(P_status*100,percent_pref*100))+geom_count()+geom_smooth(col="black")+
  theme_pubclean(base_family = "sans")+scale_radius(range=c(1,5))+
  theme( legend.key = element_rect(fill=NA),axis.text = element_text(size=12),text=element_text(size=12),legend.text = element_text(size=12),
        axis.title = element_text(size=12))+labs(y="Hourly pref. or comf. votes (%)", x="Fans on (%)", title="a) Preference for no change"),
  Q2%>%
    filter(Prepost==1)%>%
    mutate(day_factor=format(day,"%m/%d"))%>%
    mutate(hour=format(day_time.x,"%H"))%>%
    group_by(day_factor,hour)%>%
    summarise(percent_comf=sum(Comf01)/n(),percent_pref=sum(1-Pref01)/n(),P_status=sum(status_fan)/n())%>%
    ggplot(aes(P_status*100,percent_comf*100))+geom_count()+geom_smooth(col="black")+
    theme_pubclean(base_family = "sans")+scale_radius(range=c(1,7.4))+
    theme( legend.key = element_rect(fill=NA),axis.text = element_text(size=12),text=element_text(size=12),legend.text = element_text(size=12),
           axis.title = element_text(size=12), axis.title.y = element_blank())+labs( x="Fans on (%)",title="b) Comfortable"),ncol=2, nrow=1,widths = c(2.6,2.4), common.legend = TRUE), width = 8, height=4)

## table 4 - analysis 
cor.test(Q2_daily$conf,Q2_daily$Tmpa_ex,method = "spearman") #p=0.054 rho=0.46
cor.test(Q2_daily$conf,Q2_daily$Q80P,method = "spearman") #p=0.13 rho=-0.37
cor.test(Q2_daily$conf,Q2_daily$Q90P,method = "spearman") #p<0.05 rho=-0.74
cor.test(Q2_daily$conf,Q2_daily$Q80P2,method = "spearman") #p=0.37 rho=-0.24
cor.test(Q2_daily$conf,Q2_daily$Q90P2,method = "spearman") #p=0.24 rho=-0.31
cor.test(Q2_daily$conf,Q2_daily$D_m,method = "spearman") #p=0.98 rho=0.006
cor.test(Q2_daily$conf,Q2_daily$D_max,method = "spearman") #p=0.61 rho=-0.13
cor.test(Q2_daily$conf,Q2_daily$D80,method = "spearman") #p=0.78 rho=-0.07

cor.test(Q2_daily$pref,Q2_daily$Tmpa_ex,method = "spearman") #p=0.41 rho=0.20
cor.test(Q2_daily$pref,Q2_daily$Q80P,method = "spearman") #p=0.68 rho=-0.1
cor.test(Q2_daily$pref,Q2_daily$Q90P,method = "spearman") #p<0.05 rho=-0.54
cor.test(Q2_daily$pref,Q2_daily$Q80P2,method = "spearman") #p=0.1 rho=-0.43
cor.test(Q2_daily$pref,Q2_daily$Q90P2,method = "spearman") #p<0.05 rho=-0.56
cor.test(Q2_daily$pref,Q2_daily$D_m,method = "spearman") #p=0.99 rho=0.002
cor.test(Q2_daily$pref,Q2_daily$D_max,method = "spearman") #p=0.53 rho=-0.16
cor.test(Q2_daily$pref,Q2_daily$D80,method = "spearman") #p=0.95 rho=-0.017

## max temperature difference between FC 1 and 2 during intervention
Indoor%>%
  left_join(
  Q2%>%
  count(FC,sensor)%>%
    select(!n))%>%
  mutate(quarter=round_date(day_time,unit="15 minutes"),day=format(day_time,"%m/%d"),
         h=as.numeric(format(day_time,"%H")))%>%
  filter(!day %in% c("01/10","01/11"),between(h,8,18))%>%
  group_by(quarter,FC)%>%
  summarise(tar=mean(Tar))%>%
  pivot_wider(names_from = FC,values_from = tar,names_glue ="{.value}_{FC}")%>%
  mutate(dif=abs(tar_1-tar_2))%>%
  filter(!is.na(dif))%>%
  ungroup()%>%
  summarise(max(dif))

## multiple regression
summary(glm(Pref01~Tar+TextM+status_fan, data=subset(Q2,Prepost==1), family = binomial(link="probit")))# significant Tar & status_fan
summary(glm(Pref01~Prepost, data=Q2, family = binomial(link="probit")))# significant Prepost
summary(glm(Pref01~Tar+FC, data=subset(Q2,Prepost==1), family = binomial(link="probit")))# not sign FC
summary(glm(Comf01~Tar+TextM, data=subset(Q2,Prepost==1), family = binomial(link="probit")))# sign Tar & TextM
summary(glm(Comf01~Tar+Prepost, data=Q2, family = binomial(link="probit")))# not sign Prepost
summary(glm(Comf01~Tar+status_fan, data=subset(Q2,Prepost==1), family = binomial(link="probit")))#  not sign status_fan
summary(glm(Comf01~FC, data=subset(Q2,Prepost==1), family = binomial(link="probit")))# not sign FC
mae(Q2$Comf01[Q2$Prepost==1], predict(glm(Comf01~Tar+status_fan, data=subset(Q2,Prepost==1), family = binomial(link="probit"))))# MAE 0.28
mae(Q2$Comf01[Q2$Prepost==1], predict(glm(Comf01~Tar, data=subset(Q2,Prepost==1), family = binomial(link="probit"))))# MAE 0.26
mae(Q2$Comf01[Q2$Prepost==1], predict(glm(Comf01~status_fan, data=subset(Q2,Prepost==1), family = binomial(link="probit"))))# MAE 0.28
mae(Q2$Pref01[Q2$Prepost==1], predict(glm(Pref01~Tar+status_fan, data=subset(Q2,Prepost==1), family = binomial(link="probit"))))# MAE 0.68
mae(Q2$Pref01[Q2$Prepost==1], predict(glm(Pref01~Tar, data=subset(Q2,Prepost==1), family = binomial(link="probit"))))# MAE 0.68
mae(Q2$Pref01[Q2$Prepost==1], predict(glm(Pref01~status_fan, data=subset(Q2,Prepost==1), family = binomial(link="probit"))))# MAE 0.67

#temperature pref and comfortable between FC
Q2%>%
  filter(Pref01==0 & Prepost==1)%>%
  group_by(FC)%>%
 summarise(mean(Tar))

Q2%>%
  filter(Comf01==1 & Prepost==1)%>%
  group_by(FC)%>%
  summarise(mean(Tar))