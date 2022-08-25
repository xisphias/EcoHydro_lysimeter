#results figures, tables
library(tidyverse)

wd<-"/Volumes/GoogleDrive/My Drive/PhD/lysimeter/results2022"
setwd(wd)

# depends on
# towerDat<-load("/Users/jac/Documents/projects/plantWater/data/modelprep/modelinput/imn2017Dat1ALLWholeSeason.rda")
# aR2<-readRDS("/Volumes/GoogleDrive/My Drive/PhD/lysimeter/awatR4.RDS")

#data
{
  #get met tower data for precip
  {
    towerDat<-load("/Users/jac/Documents/projects/plantWater/data/modelprep/modelinput/imn2017Dat1ALLWholeSeason.rda")
    towerDat<-get(towerDat)
    towerDat<-towerDat %>% filter(Sensor==1)
  }
  
  #awat data
  aR2<-readRDS("/Volumes/GoogleDrive/My Drive/PhD/lysimeter/awatR4.RDS")
  
  #remove lysimeters with poor accuracy
  aR2<- aR2 %>% unite('Nodech',Node,ch,remove=FALSE) %>% filter(!Nodech%in%c('1_5','3_1','3_5','3_6','7_1','8_6','9_3','10_2','10_4','10_6'))
  
  #categorize lysimeters
  lType<-read.csv("/Volumes/GoogleDrive/My Drive/PhD/lysimeter/lysType.csv")
  lType$Node<-factor(lType$node)
  lType$ch<-factor(lType$ch)
  lType<-lType %>% select(-node)
  aR2<-merge(aR2,lType,all.x=TRUE)
  
  #gather
  aR2g<-gather(aR2,key=f,value=mm_f,mm_f1,mm_f2,mm_f3,mm_f4)
  
  #add date, hour, half hour, 10min
  {
    aR2g$DateTime_Hour<-as.POSIXct(format(aR2g$DateTime,format = '%Y-%m-%d %H:00:00'))
    aR2g$Date<-as.POSIXct(format(aR2g$DateTime,format = '%Y-%m-%d 00:00:00'))
    aR2g$Hour<-as.numeric(as.character(factor(format(aR2g$DateTime,format = '%H'))))
    
    #calculate half hours
    aR2g$halfhourD<-as.numeric((aR2g$DateTime-aR2g$DateTime_Hour)/60)
    aR2g$DateTime_HalfHour<-ifelse((aR2g$halfhourD)<29.5,
                                   as.numeric(aR2g$DateTime_Hour),
                                   as.numeric(aR2g$DateTime_Hour)+(60*30))
    aR2g$DateTime_HalfHour<-as.POSIXct(aR2g$DateTime_HalfHour,origin="1970-01-01",timezone="AKDT")
    aR2g<-aR2g %>% select(-halfhourD)
    
    #calculate 10mins
    aR2g$minD<-as.numeric((aR2g$DateTime-aR2g$DateTime_HalfHour)/60)
    aR2g$DateTime_10min<-0
    aR2g$DateTime_10min<-ifelse((aR2g$minD)<30&(aR2g$minD)>=20,as.numeric(aR2g$DateTime_HalfHour)+(60*20),aR2g$DateTime_10min)
    aR2g$DateTime_10min<-ifelse((aR2g$minD)<20&(aR2g$minD)>=10,as.numeric(aR2g$DateTime_HalfHour)+(60*10),aR2g$DateTime_10min)
    aR2g$DateTime_10min<-ifelse((aR2g$minD)<10&(aR2g$minD)>=(-1),as.numeric(aR2g$DateTime_HalfHour)+(60*0),aR2g$DateTime_10min)
    aR2g$DateTime_10min<-as.POSIXct(aR2g$DateTime_10min,origin="1970-01-01",timezone="AKDT")
    aR2g<-aR2g %>% select(-minD)
  } 
  
  #mean by 10min before diff
  aR2g2 <- aR2g %>% filter(f=='mm_f1', moss!='control',!is.na(mm_f)) %>% ungroup() %>% group_by(Node,ch,DateTime_10min) %>% mutate(mm_f10=mean(mm_f,na.rm=TRUE)) %>% select(-DateTime,-mm_f,-mm_cum,-mm_thres,-mm_ma,-sigma,-win,-sres_t,-f,-mm_filter,-rowid)
  aR2g2<-unique(aR2g2)
  
  #data for awat plot
  #add p to awat
  awatDat <- aR2g %>% filter(f=='mm_f1', moss!='control',!is.na(mm_f)) %>% ungroup() 
  awatDat<-merge(awatDat,(towerDat %>% filter(DateTimeHr>='2017-06-01'&DateTimeHr<'2017-09-01') %>% select(DateTimeHr,P) %>% rename(DateTime_Hour=DateTimeHr)),
                 by=c('DateTime_Hour'),
                 all.x = TRUE)
  
  #remove rain for non-hours
  awatDat$P[awatDat$DateTime_Hour!=awatDat$DateTime_10min]<-NA
  
  #calc diffs
  aR2_ET2<- aR2g2 %>% ungroup() %>% group_by(Node,ch) %>% arrange(DateTime_10min,.by_group=TRUE) %>% filter(!is.na(mm_f10)) %>% mutate(mm_filter_diff=c(0,diff(mm_f10)))
  aR2_ET2<- aR2_ET2 %>% group_by(Node,ch) %>% arrange(DateTime_10min,.by_group=TRUE) %>% mutate(timediff_h=difftime(DateTime_10min,lag(DateTime_10min),units = 'hours'),timediff_m=difftime(DateTime_10min,lag(DateTime_10min),units = 'mins')  )
  #if timediff>1H then mm_filter_diff is 0, resets diffs between recPer, # NA timediff is first data points in recPer
  aR2_ET2$mm_filter_diff[!is.na(aR2_ET2$timediff_h)&aR2_ET2$timediff_h>1]<-0
  
  #filter some noise
  #calc mean diff by node by 10min,if ch diff is > 1 & > 3Xmean diff ==>0
  aR2_ET2 <- aR2_ET2 %>% ungroup() %>% group_by(Node,DateTime_10min) %>% mutate(NmeanDiff=mean(mm_filter_diff))
  aR2_ET2$mm_filter_diff[abs(aR2_ET2$mm_filter_diff)>1 & abs(aR2_ET2$mm_filter_diff*3) > abs(aR2_ET2$NmeanDiff)]<-0
  
  #if sign is different and diff is > 0.05
  aR2_ET2$mm_filter_diff[sign(aR2_ET2$mm_filter_diff)!=sign(aR2_ET2$NmeanDiff) & abs(aR2_ET2$mm_filter_diff)>0.05]<-0
  
  #second mean
  aR2_ET2 <- aR2_ET2 %>% ungroup() %>% group_by(Node,DateTime_10min) %>% mutate(NmeanDiff2=mean(mm_filter_diff),NsdDiff2=sd(mm_filter_diff))
  aR2_ET2$NsdDiff2<-ifelse(is.na(aR2_ET2$NsdDiff2),0,aR2_ET2$NsdDiff2)                                                                                
  aR2_ET2$mm_filter_diff[abs((aR2_ET2$mm_filter_diff)-(aR2_ET2$NmeanDiff2))> (2*aR2_ET2$NsdDiff2) & abs(aR2_ET2$NmeanDiff2)<0.05]<-0
  aR2_ET2$mm_filter_diff[abs((aR2_ET2$mm_filter_diff))> (abs(aR2_ET2$NmeanDiff2)+2*aR2_ET2$NsdDiff2)]<-0
  
  #create hourly rate
  aR2_ET2$mm_h_mult<-(60/floor(as.numeric(aR2_ET2$timediff_m)))
  aR2_ET2$mm_h_mult[is.na(aR2_ET2$mm_h_mult)]<-0
  aR2_ET2$mm_filter_diff_h<-aR2_ET2$mm_filter_diff*aR2_ET2$mm_h_mult
  
  #calc means
  #by node,ch, hour
  {
    aR2_ET_Nodech_h<-aR2_ET2 %>% ungroup() %>% 
      group_by(Node,ch,moss,veg,DateTime_Hour) %>% 
      summarise(mm_diff_m=mean(mm_filter_diff_h,na.rm=TRUE),
                mm_diff_se=sqrt(stats::var(mm_filter_diff_h,na.rm = TRUE)/length(na.omit(mm_filter_diff_h))),
                mm_diff_sd=sd(mm_filter_diff_h,na.rm = TRUE),
                mm_diff_n=length(na.omit(mm_filter_diff_h)),
                evap=ifelse(mm_diff_m>=0,0,mm_diff_m),
                ppt=ifelse(mm_diff_m<=0,0,mm_diff_m),
                xi=mean(mm_f10,na.rm = TRUE)) %>% 
      ungroup()
    aR2_ET_Nodech_h<-aR2_ET_Nodech_h %>% ungroup() %>% 
      group_by(Node,ch,moss,veg) %>% 
      arrange(DateTime_Hour,.by_group=TRUE) %>%
      mutate(mm_filterT=diffinv(mm_diff_m,xi=0)[-1],
             mm_filterTse=diffinv(mm_diff_se,xi=0)[-1],
             evapcum=cumsum(evap),
             pptcum=cumsum(ppt))
    #add gauge P and extra hours for time gaps
    aR2_ET_Nodech_h<-aR2_ET_Nodech_h %>% ungroup() %>% arrange(DateTime_Hour) %>% unite('Nodech',Node,ch,remove = FALSE)
    datSub<-aR2_ET_Nodech_h %>% ungroup() %>% select(Nodech,DateTime_Hour,mm_diff_m) 
    Nodespread<-datSub %>% spread(Nodech,mm_diff_m,drop = FALSE)
    Nodemerge<-merge(Nodespread,(towerDat %>% filter(DateTimeHr>='2017-06-01'&DateTimeHr<'2017-09-01') %>% select(DateTimeHr,P) %>% rename(DateTime_Hour=DateTimeHr)),all = TRUE)
    Nodegather<-Nodemerge %>% gather(Nodech,mm_diff_m,-DateTime_Hour,-P) %>% select(-mm_diff_m)
    aR2_ET_Nodech_h_full<-merge(aR2_ET_Nodech_h,Nodegather,all = TRUE)
    aR2_ET_Nodech_h_P<-merge(aR2_ET_Nodech_h,Nodegather,all.x = TRUE)
    
  }
  
  #calc means
  #by veg, hour
  {
    #mean diff, se by type
    aR2_ET_dtVeg_Sum<-aR2_ET2 %>% ungroup() %>% 
      group_by(veg,DateTime_Hour) %>% 
      summarise(mm_diff_m=mean(mm_filter_diff_h,na.rm=TRUE),
                mm_diff_se=sqrt(stats::var(mm_filter_diff_h,na.rm = TRUE)/length(na.omit(mm_filter_diff_h))),
                mm_diff_sd=sd(mm_filter_diff_h,na.rm = TRUE),
                mm_diff_n=length(na.omit(mm_filter_diff_h)),
                evap=ifelse(mm_diff_m>=0,0,mm_diff_m),
                ppt=ifelse(mm_diff_m<=0,0,mm_diff_m),
                xi=mean(mm_f10,na.rm = TRUE)) %>% 
      ungroup()
    
    aR2_ET_dtVeg_Sum<-aR2_ET_dtVeg_Sum %>% ungroup() %>% 
      group_by(veg) %>% 
      arrange(DateTime_Hour,.by_group=TRUE) %>%
      mutate(mm_filterT=diffinv(mm_diff_m,xi=0)[-1],
             mm_filterTse=diffinv(mm_diff_se,xi=0)[-1],
             evapcum=cumsum(evap),
             pptcum=cumsum(ppt))

    #add gauge P and extra hours for time gaps
    datSub<-aR2_ET_dtVeg_Sum %>% ungroup() %>% select(veg,DateTime_Hour,mm_diff_m)
    datSub$veg<-droplevels(factor(datSub$veg))
    Nodespread<-datSub %>% spread(veg,mm_diff_m,drop = FALSE)
    Nodemerge<-merge(Nodespread,(towerDat %>% filter(DateTimeHr>='2017-06-01'&DateTimeHr<'2017-09-01') %>% select(DateTimeHr,P) %>% rename(DateTime_Hour=DateTimeHr)),all = TRUE)
    Nodegather<-Nodemerge %>% gather(veg,mm_diff_m,-DateTime_Hour,-P) %>% select(-mm_diff_m)
    aR2_ET_dtVeg_Sum<-merge(aR2_ET_dtVeg_Sum,Nodegather,all.x = TRUE)
    aR2_ET_dtVeg_Sum_full<-merge(aR2_ET_dtVeg_Sum,Nodegather,all = TRUE)
  }
  
  #calculate 10min data
  #add hourly/10min rate by day to get daily cum mm
  {
    aR2_ET2<- aR2_ET2 %>% ungroup() %>% 
      group_by(Node,ch,Date) %>% 
      arrange(DateTime_10min,.by_group=TRUE) %>% 
      mutate(mm_cum_10md=cumsum(ifelse(is.na(mm_filter_diff),0,mm_filter_diff)))
    
    aR2_ET2_Nodech_h10md<-aR2_ET2 %>% ungroup() %>% 
      group_by(Node,ch,moss,veg,Date,DateTime_Hour) %>% 
      summarise(mm_diff_m=mean(mm_filter_diff_h,na.rm=TRUE),
                mm_diff_se=sqrt(stats::var(mm_filter_diff_h,na.rm = TRUE)/length(na.omit(mm_filter_diff_h))),
                mm_diff_sd=sd(mm_filter_diff_h,na.rm = TRUE),
                mm_diff_n=length(na.omit(mm_filter_diff_h)),
                evap=ifelse(mm_diff_m>=0,0,mm_diff_m),
                ppt=ifelse(mm_diff_m<=0,0,mm_diff_m),
                xi=mean(mm_f10,na.rm = TRUE)) %>% 
      ungroup()
    aR2_ET2_Nodech_h10md<-aR2_ET2_Nodech_h10md %>% ungroup() %>% 
      group_by(Node,ch,moss,veg) %>% 
      arrange(DateTime_Hour,.by_group=TRUE) %>%
      mutate(mm_filterT=diffinv(mm_diff_m,xi=0)[-1],
             mm_filterTse=diffinv(mm_diff_se,xi=0)[-1],
             evapcum=cumsum(evap),
             pptcum=cumsum(ppt))
    head(aR2_ET2_Nodech_h10md)
    
    aR2_ET2_Nodech_h10md %>% ungroup() %>% 
      group_by(Node,ch,moss,veg,Date) %>% 
      arrange(DateTime_Hour,.by_group=TRUE) %>%
      summarise(evapcumD=sum(evap),
                pptcumD=sum(ppt))%>%
      filter(evapcumD==0&pptcumD==0)%>%
      ggplot(.,aes(x=Date,y=evapcumD,color=ch))+geom_point()+facet_wrap(~Node)
    
    aR2_ET2_Nodech_h10md<-aR2_ET2_Nodech_h10md %>% ungroup() %>% 
      group_by(Node,ch,moss,veg,Date) %>% 
      arrange(DateTime_Hour,.by_group=TRUE) %>%
      mutate(mm_filterD=diffinv(mm_diff_m,xi=0)[-1],
             mm_filterDse=diffinv(mm_diff_se,xi=0)[-1],
             evapcumD=cumsum(evap),
             pptcumD=cumsum(ppt))
  }
  
  #calculate mean rate for each lysimeter
  #fill missing data
  #can pull missing date/hours from aR2_ET_Nodech_h_full based on nodech when node/evap is NA. 
  missing<-aR2_ET_Nodech_h_full %>% filter(is.na(Node)) %>% select(Nodech,DateTime_Hour) %>% mutate(Hour=format(DateTime_Hour,format = '%H'))
  #need to pull in node, ch, veg from new list
  nodechveg<-aR2_ET_Nodech_h_full %>% group_by(Nodech,Node,ch,veg) %>% summarise(Nodech=Nodech[1],Node=Node[1],ch=ch[1],veg=veg[1]) %>% drop_na()
  missing<-merge(missing,nodechveg,all.x=TRUE)
  missingE<-merge(missing,hourlyE %>% select(veg,Hour,evapDmean,evapDsd),by=c('veg','Hour'),all.x = TRUE) %>% as_tibble()
  
  aR2_ET_Nodech_h_full_filled<-rbind(aR2_ET_Nodech_h_P %>% select(Nodech,Node,ch,veg,DateTime_Hour,evap) %>% mutate(source='lys'),
                                      missingE %>% rename(evap=evapDmean) %>% select(-Hour,-evapDsd) %>% mutate(source='mean')
                                      ) %>% as_tibble()
  
  #sum by lysimeter, filled data
  lysTot<-aR2_ET_Nodech_h_full_filled %>% 
              group_by(Nodech,Node,ch,veg) %>%
              dplyr::summarise(evapH_hrs=length(evap),
              evapLmean=mean(evap),
              evapLsd=sd(evap),
              evapLTot=sum(evap))
  
  #season total
  lysTotseason <- lysTot %>% drop_na() %>%
              group_by(veg) %>%
              dplyr::summarise(evapLmeanmean=mean(evapLmean),
              evapLmeansd=sd(evapLmean),
              evapTotmean=mean(evapLTot),
              evapTotsd=sd(evapLTot),
              evapTotn=length(unique(Nodech)))

  #sum by lysimeter, not filled data
  # aR2_ET_Nodech_h_P
  lysTot_unfilled<-aR2_ET_Nodech_h_P %>% 
    group_by(Nodech,Node,ch,veg) %>%
    dplyr::summarise(evapH_hrs=length(evap),
                     evapLmean=mean(evap),
                     evapLsd=sd(evap),
                     evapLTot=sum(evap))
  
  #season total, not filled data
  lysTot_unfilled_season <- lysTot_unfilled %>% drop_na() %>%
    group_by(veg) %>%
    dplyr::summarise(evapTotn=length(unique(Nodech)),
                     evalTot_hrsmean=mean(evapH_hrs),
                     evalTot_dmean=mean(evapH_hrs)/24,
                     evapLmeanmean=-mean(evapLmean),
                     evapLmeansd=sd(evapLmean),
                     evapTotmean=-mean(evapLTot),
                     evapTotsd=sd(evapLTot)
                     )
}#end of data

    
# dewfall estimate
  #sum by lysimeter, not filled data
  # aR2_ET_Nodech_h_P
  lysTot_unfilled_dew<-aR2_ET_Nodech_h_P %>% 
    group_by(Nodech,Node,ch,veg) %>%
    mutate(Hour=as.numeric(format(DateTime_Hour,format='%H'))) %>%
    filter(P==0,evap==0) %>%
    filter(Hour>=18 | Hour<=6) %>%
    dplyr::summarise(pptL_hrs=length(evap),
                     pptLmean=mean(ppt),
                     pptLsd=sd(ppt),
                     pptLTot=sum(ppt))
  #season total dewfall
  lysTot_unfilled_dew_season <- lysTot_unfilled_dew %>% drop_na() %>%
    group_by(veg) %>%
    dplyr::summarise(evapTotn=length(unique(Nodech)),
                     evalTot_hrsmean=mean(pptL_hrs),
                     evalTot_dmean=mean(pptL_hrs)/24,
                     evapLmeanmean=mean(pptLmean),
                     evapLmeansd=sd(pptLmean),
                     evapTotmean=mean(pptLTot),
                     evapTotsd=sd(pptLTot)
    )
  # A tibble: 3 x 8
  # veg     evapTotn evalTot_hrsmean evalTot_dmean evapLmeanmean evapLmeansd evapTotmean evapTotsd
  # 1 mixed         21            82.7          3.44       -0.0604      0.0255       -4.88      2.70
  # 2 moss          17            92.2          3.84       -0.0682      0.0248       -6.85      3.79
  # 3 tussock        6            77.2          3.22       -0.0881      0.0193       -6.98      3.78
  # mean dew fall 6.23
  
  #hrs with dewfall compared to hours, if all dew hours would be 50%
  lysTot_unfilled_dew_season$evalTot_hrsmean/lysTot_unfilled_season$evalTot_hrsmean
  # 0.1580049 0.1690019 0.1425932
  # mean 15.6% of lysimeter hours
  92*24*0.16*0.0722
  92 * 24 *
    mean(lysTot_unfilled_dew_season$evalTot_hrsmean/lysTot_unfilled_season$evalTot_hrsmean) *
    mean(lysTot_unfilled_dew_season$evapLmeanmean)
  # 24.9669 mm
  92 * 24 *
    mean(lysTot_unfilled_dew_season$evalTot_hrsmean/lysTot_unfilled_season$evalTot_hrsmean) *
    mean(lysTot_unfilled_dew_season$evapLmeansd)
  # 8.015524 sd 
  
  #nights with dewfall
  lysTot_unfilled_dew_nights<-aR2_ET_Nodech_h_P %>% 
    mutate(Hour=as.numeric(format(DateTime_Hour,format='%H'))) %>%
    mutate(Date=format(DateTime_Hour,format='%y-%m-%d')) %>%
    group_by(Nodech,Node,ch,veg,Date) %>%
    filter(P==0,evap==0) %>%
    filter(Hour>=18 | Hour<=6) %>%
    dplyr::summarise(pptL_hrs=length(evap),
                     pptLmean=mean(ppt),
                     pptLsd=sd(ppt),
                     pptLTot=sum(ppt)) %>%
    group_by(Nodech,Node,ch,veg) %>%
    summarise(nNights=length(unique(Date)),
              meanHrs=mean(pptL_hrs)) 
  #nights with data
  nightsData<-aR2_ET_Nodech_h_P %>% 
    mutate(Hour=as.numeric(format(DateTime_Hour,format='%H'))) %>%
    mutate(Date=format(DateTime_Hour,format='%y-%m-%d')) %>%
    group_by(Nodech,Node,ch,veg,Date) %>%
    filter(Hour>=18 | Hour<=6) %>%
    dplyr::summarise(pptL_hrs=length(evap),
                     pptLmean=mean(ppt),
                     pptLsd=sd(ppt),
                     pptLTot=sum(ppt)) %>%
    group_by(Nodech,Node,ch,veg) %>%
    summarise(nNights_data=length(unique(Date))) 
  lysTot_unfilled_dew_nights <- merge(lysTot_unfilled_dew_nights,nightsData,all = TRUE) %>% mutate(nNights_pct=nNights/nNights_data)
  lysTot_unfilled_dew_nights %>%
    group_by(veg) %>%
    summarise(meanNights=mean(nNights),sdNights=sd(nNights),
              meanNights_pct=mean(nNights_pct),
              meanHrsT=mean(meanHrs))
  # veg     meanNights sdNights meanNights_pct meanHrsT
  # 1 mixed         18.5     5.74          0.659     4.36
  # 2 moss          18.6     5.24          0.651     4.89
  # 3 tussock       18.8     6.11          0.672     3.93
  #~66% of nights
  
  #gather, spread, reshape
  {
    #timeseries
    #means
    #cum mm
    #veg v non veg
    EP_veg<-aR2_ET_dtVeg_Sum %>% select(veg,DateTime_Hour,evapcum,mm_diff_se,mm_diff_sd,pptcum,P) %>% 
      gather('ETP',value='measure',-veg,-DateTime_Hour,-P,-mm_diff_se,-mm_diff_sd,factor_key = TRUE) %>% arrange(DateTime_Hour)
    EP_veg$measure[EP_veg$ETP=='evapcum']<-EP_veg$measure[EP_veg$ETP=='evapcum']*(-1)
    EP_veg$ETP2<-factor(EP_veg$ETP,levels=c("evapcum","pptcum"),labels=c('Evap.','Precip.'))
    #veg v non veg, full
    #need full to show gaps with missing data
    EP_full_veg<-aR2_ET_dtVeg_Sum_full %>% select(veg,DateTime_Hour,evapcum,mm_diff_se,mm_diff_sd,pptcum,P) %>% 
      gather('ETP',value='measure',-veg,-DateTime_Hour,-P,-mm_diff_se,-mm_diff_sd,factor_key = TRUE) %>% arrange(DateTime_Hour)
    EP_full_veg$measure[EP_full_veg$ETP=='evapcum']<-EP_full_veg$measure[EP_full_veg$ETP=='evapcum']*(-1)
    levels(EP_full_veg$ETP)
    EP_full_veg$ETP2<-factor(EP_full_veg$ETP,levels=c("evapcum","pptcum"),labels=c('Evap.','Precip.'))
  }
  
  #tables
  {
    # Table 2
    outTab<-'/Volumes/GoogleDrive/My Drive/PhD/lysimeter/results2022/tables'
    totETtab_v<-merge(lysTot_unfilled_season,lysTotseason %>% select(veg,SeasonTot=evapTotmean,SeasonSD=evapTotsd),all=TRUE,by='veg')
    write.csv(totETtab_v,file=paste0(outTab,'/table2.csv'),row.names = FALSE)
    #add anova to table 2
    #see stats_lysimTotal_veg_final.r
  }
