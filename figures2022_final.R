#figures for ecohydro manuscript
library(tidyverse)
library(gtable)
library(grid)
library(gridExtra)
library(ggh4x)
library(patchwork)
library(RColorBrewer)

# depends on
# results_veg3_final.r

#figure 3
# awatDat from results_veg3_final.r
{
  #awat plots, points, ma, spline, awat. 
  #evap
  datSub<-awatDat%>%
    dplyr::filter(DateTime_Hour>='2017-07-27',DateTime_Hour<'2017-07-28 06:00:00',Node==9)%>%
    group_by(ch)%>%
    mutate(mm_cum=mm_input-mm_input[1],mm_ma=mm_ma-mm_input[1],mm_thres=mm_thres-mm_input[1],mm_filter=mm_filter-mm_input[1])
  
  legDat<-data.frame(x=rep(min(datSub$DateTime),3),y=as.numeric(rep(0,3)),text=factor(c('Moving Avg.','Threshold','AWATsnap'),levels = c('Moving Avg.','Threshold','AWATsnap')))
  
  Fig3a<-ggplot((datSub%>%dplyr::filter(ch==1)), aes(x=DateTime,y=mm_filter) )+
    geom_line(data=legDat,aes(x=x,y=y,color=text,lty=text,size=text))+
    scale_color_manual(values=c(1:2,4),guide=guide_legend(title=''))+
    scale_linetype_manual(values=c(1,3,1),guide=guide_legend(title=''))+
    scale_size_manual(values=c(0.5,1.2,1.2),guide=guide_legend(title=''))+
    geom_point(aes(group=Nodech,y=mm_cum),size=0.5,color=grey(0.6))+
    geom_line(aes(group=Nodech,y=mm_ma),size=0.5)+
    geom_line(aes(group=Nodech,y=mm_thres),size=0.8,color=2,lty='1111')+
    geom_line(aes(group=Nodech,y=mm_filter),size=0.8,color=4,alpha=0.8)+
    ylab(bquote('Cumulative Water, mm'))+
    guides(color=guide_legend(''),fill=guide_legend(''))+xlab('Time')+
    theme_bw()+theme(panel.grid.minor = element_blank())+
    scale_x_datetime(date_breaks = '6 hours',date_labels = '%H:%M',expand = expansion())+
    theme(legend.text = element_text(family='sans',size=10,color=1),
          axis.text = element_text(family='sans',size=10,color=1),
          axis.title = element_text(family='sans',size=10,color=1),
          axis.text.x = element_text(angle = 30,hjust=1,vjust=1))+
    theme(legend.position = c(1,1),legend.justification = c(1,1),legend.background = element_blank(),legend.spacing.y = unit(0,units='pt'),legend.spacing.x = unit(1,units='pt'),legend.title = element_blank() ,legend.margin = margin(1,1,1,1))
  
  # ggsave(paste0(wd,'/tsawat_evap_june.png'),width = 7,height = 7)
  ggsave(Fig3a,filename=paste0(wd,'/Fig3a.png'),width = 7,height = 7)
  
  #rain
  datSub<-awatDat%>%
    dplyr::filter(DateTime_Hour>='2017-08-03 06:00:00',DateTime_Hour<'2017-08-04 06:00:00',Node==3)%>%
    group_by(ch)%>%
    mutate(mm_cum=mm_input-mm_input[1],mm_ma=mm_ma-mm_input[1],mm_thres=mm_thres-mm_input[1],mm_filter=mm_filter-mm_input[1])
  legDat<-data.frame(x=rep(min(datSub$DateTime),3),y=as.numeric(rep(0,3)),text=factor(c('Moving Avg.','Threshold','AWATsnap'),levels = c('Moving Avg.','Threshold','AWATsnap')))
  #16.25mm rain
  Fig3b<-ggplot((datSub%>%dplyr::filter(ch==3)), aes(x=DateTime,y=mm_filter) )+
    geom_line(data=legDat,aes(x=x,y=y,color=text,lty=text,size=text))+
    scale_color_manual(values=c(1:2,4),guide=guide_legend(title=''))+
    scale_linetype_manual(values=c(1,3,1),guide=guide_legend(title=''))+
    scale_size_manual(values=c(0.5,1.2,1.2),guide=guide_legend(title=''))+
    geom_point(aes(group=Nodech,y=mm_cum),size=0.5,color=grey(0.6))+
    geom_line(aes(group=Nodech,y=mm_ma),size=0.5)+
    geom_line(aes(group=Nodech,y=mm_thres),size=0.8,color=2,lty='1111')+
    geom_line(aes(group=Nodech,y=mm_filter),size=0.8,color=4,alpha=0.8)+
    ylab(bquote('Cumulative Water, mm'))+
    guides(color=guide_legend(''),fill=guide_legend(''))+xlab('Time')+
    theme_bw()+theme(panel.grid.minor = element_blank())+
    scale_x_datetime(date_breaks = '6 hours',date_labels = '%H:%M',expand = expansion())+
    theme(legend.text = element_text(family='sans',size=10,color=1),
          axis.text = element_text(family='sans',size=10,color=1),
          axis.title = element_text(family='sans',size=10,color=1),
          axis.text.x = element_text(angle = 30,hjust=1,vjust=1))+
    theme(legend.position = c(0,1),legend.justification = c(0,1),legend.background = element_blank(),legend.spacing.y = unit(0,units='pt'),legend.spacing.x = unit(1,units='pt'),legend.title = element_blank() ,legend.margin = margin(1,1,1,1))
  
  ggsave(Fig3b,filename=paste0(wd,'/fig3b.png'),width = 7,height = 7)
  
  #strongwind
  datSub<-awatDat%>%
    dplyr::filter(DateTime_Hour>='2017-08-01 00:00:00',DateTime_Hour<'2017-08-02 12:00:00',Node==2)%>%
    group_by(ch)%>%
    mutate(mm_cum=mm_input-mm_input[1],mm_ma=mm_ma-mm_input[1],mm_thres=mm_thres-mm_input[1],mm_filter=mm_filter-mm_input[1])
  
  legDat<-data.frame(x=rep(min(datSub$DateTime),3),y=as.numeric(rep(0,3)),text=factor(c('Moving Avg.','Threshold','AWATsnap'),levels = c('Moving Avg.','Threshold','AWATsnap')))
  
  Fig3c<-ggplot((datSub%>%dplyr::filter(ch==6)), aes(x=DateTime,y=mm_filter) )+
    geom_line(data=legDat,aes(x=x,y=y,color=text,lty=text,size=text))+
    scale_color_manual(values=c(1:2,4),guide=guide_legend(title=''))+
    scale_linetype_manual(values=c(1,3,1),guide=guide_legend(title=''))+
    scale_size_manual(values=c(0.5,1.2,1.2),guide=guide_legend(title=''))+
    geom_point(aes(group=Nodech,y=mm_cum),size=0.5,color=grey(0.6))+
    geom_line(aes(group=Nodech,y=mm_ma),size=0.5)+
    geom_line(aes(group=Nodech,y=mm_thres),size=0.8,color=2,lty='1111')+
    geom_line(aes(group=Nodech,y=mm_filter),size=0.8,color=4,alpha=0.8)+
    ylab(bquote('Cumulative Water, mm'))+
    guides(color=guide_legend(''),fill=guide_legend(''))+xlab('Time')+
    theme_bw()+theme(panel.grid.minor = element_blank())+
    scale_x_datetime(date_breaks = '6 hours',date_labels = '%H:%M',expand = expansion())+
    theme(legend.text = element_text(family='sans',size=10,color=1),
          axis.text = element_text(family='sans',size=10,color=1),
          axis.title = element_text(family='sans',size=10,color=1),
          axis.text.x = element_text(angle = 30,hjust=1,vjust=1))+
    theme(legend.position = c(1,1),legend.justification = c(1,1),legend.background = element_blank(),legend.spacing.y = unit(0,units='pt'),legend.spacing.x = unit(1,units='pt'),legend.title = element_blank() ,legend.margin = margin(1,1,1,1))
  
  ggsave(Fig3c,filename=paste0(wd,'/Fig3c.png'),width = 7,height = 7)
  # summary(towerDat %>% filter(DateTimeHr>='2017-08-01 00:00:00',DateTimeHr<'2017-08-02 12:00:00') %>% pull(Ws))
  # boxplot(towerDat %>% filter(DateTimeHr>='2017-08-01 00:00:00',DateTimeHr<'2017-08-02 12:00:00') %>% pull(Ws))
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 3.746   5.742   7.059   6.765   7.996   8.865 
  
  #combine plots
  {
    g1 <- ggplotGrob(
      Fig3a+theme(axis.title.x=element_blank())+
        labs(tag='a')+
        theme(plot.tag.position = c(0,1),
              plot.tag = element_text(family = 'sans',size=14,
                                      margin=margin(0,5,0,2)))
    )
    g2 <-ggplotGrob(
      Fig3b+theme(legend.position = 'none')+theme(axis.title.x=element_blank())+
        labs(tag='b')+
        theme(plot.tag.position = c(0,1),
              plot.tag = element_text(family = 'sans',size=14,
                                      margin=margin(0,5,0,2)))
    )
    g3 <-ggplotGrob(
      Fig3c+theme(legend.position = 'none')+
        labs(tag='c')+
        theme(plot.tag.position = c(0,1),
              plot.tag = element_text(family = 'sans',size=14,
                                      margin=margin(0,5,0,2)))
    )
    
    g <- rbind(g1, g2, g3, size = "first")
    g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths)
    # grid.newpage()
    # grid.draw(g)
    
    gg<-arrangeGrob(g)
    ggsave(plot=gg,filename=paste0(wd,'/fig3_ver.png'),width=6.5/2,height = (6.5/2)*3,units = 'in',dpi=450)
    
    p123<- Fig3a + Fig3b + Fig3c + plot_layout(ncol = 3)
    p123[[1]]<-p123[[1]]+labs(tag='a')+
      theme(plot.tag.position = c(0,1),
            plot.tag = element_text(family = 'sans',size=14,
                                    margin=margin(0,5,0,2)))
    p123[[2]]<-p123[[2]]+theme(legend.position = 'none')+theme(axis.title.y=element_blank())+
      labs(tag='b')+
      theme(plot.tag.position = c(0,1),
            plot.tag = element_text(family = 'sans',size=14,
                                    margin=margin(0,5,0,2)))
    p123[[3]]<-p123[[3]]+theme(legend.position = 'none')+theme(axis.title.y=element_blank())+
      labs(tag='c')+
      theme(plot.tag.position = c(0,1),
            plot.tag = element_text(family = 'sans',size=14,
                                    margin=margin(0,5,0,2)))
    # p123 + plot_annotation(tag_levels = "a")
    ggsave(plot=p123,filename=paste0(wd,'/fig3_hor.png'),width=6.5,height = (6.5/3),units = 'in',dpi=450)
  }
  print(paste0(' plots done: ',outDir))
}

#figure 4
# 10min data
# aR2_ET2_Nodech_h10md from results_veg3_final.r
{
  #strong evap
  
  fig4a<-ggplot( (aR2_ET2_Nodech_h10md%>%
                    dplyr::filter(DateTime_Hour>='2017-06-18',DateTime_Hour<'2017-06-19')%>%
                    mutate(veg2=factor(veg, levels = c('moss','mixed','tussock'), labels = c('moss E','mixed ET','tussock ET') ) ) ), 
                 aes(x=DateTime_Hour,y=mm_filterD))+
    geom_line(aes(group=factor(paste(Node,ch)),color=veg2),size=0.4,alpha=0.6)+
    stat_summary(geom = 'ribbon',fun.data = 'mean_se',alpha=0.33,aes(fill=veg2))+
    stat_summary(geom = 'line',fun.data = 'mean_se',aes(color=veg2),size=1.5)+
    ylab(bquote('Cumulative Water, mm'))+
    guides(color=guide_legend(''),fill=guide_legend(''))+xlab('Time')+
    theme_bw()+theme(panel.grid.major.x=element_blank(),panel.grid.minor = element_blank())+
    scale_fill_brewer(palette = 'Set2')+
    scale_color_brewer(palette = 'Set2')+
    scale_x_datetime(date_breaks = '4 hours',date_labels = '%H:%M',expand = expansion())+
    theme(legend.text = element_text(family='sans',size=10,color=1),
          axis.text = element_text(family='sans',size=10,color=1),
          axis.title = element_text(family='sans',size=10,color=1))+
    theme(legend.position = c(1,1),legend.justification = c(1,1),legend.background = element_blank(),legend.spacing.y = unit(0,units='pt'),legend.spacing.x = unit(1,units='pt'),legend.title = element_blank() ,legend.margin = margin(1,1,1,1))
  
  ggsave(paste0(wd,'/fig4a.png'),width = 7,height = 7)
  
  datSub<-aR2_ET_Nodech_h%>%
    dplyr::filter(DateTime_Hour>='2017-08-03 06:00:00',DateTime_Hour<'2017-08-04 05:00:00')%>%
    group_by(Node,ch)%>%
    mutate(mm_cum=diffinv(mm_diff_m,xi=0)[-1],
           mm_cum2=cumsum(mm_diff_m))%>% 
    unite(col=Nodech,Node,ch,remove = FALSE) %>%
    dplyr::filter(!Nodech%in%c('1_6','7_2','10_4','10_3','1_1','1_2','9_6'))
  datSub$Nodech[datSub$mm_cum>20]
  max(cumsum(towerDat %>% dplyr::filter(DateTimeHr>='2017-08-03 06:00:00',DateTimeHr<'2017-08-04 05:00:00') %>% pull(P)))
  #16.26mm rain
  fig4b<-ggplot(datSub, aes(x=DateTime_Hour,y=mm_cum) )+
    geom_line(aes(color=veg,group=Nodech),size=0.4,alpha=0.6)+
    stat_summary(geom = 'ribbon',fun.data = 'mean_se',alpha=0.33,aes(fill=veg,group=veg))+
    stat_summary(geom = 'line',fun.data = 'mean_se',aes(color=veg,group=veg),size=1.5)+
    geom_bar(data=(towerDat %>% dplyr::filter(DateTimeHr>='2017-08-03 05:59:00',DateTimeHr<'2017-08-04 05:00:00')),aes(x=DateTimeHr,y=P,group=NULL),stat='identity',position = 'dodge')+
    geom_line(data=(towerDat %>% dplyr::filter(DateTimeHr>='2017-08-03 05:59:00',DateTimeHr<'2017-08-04 05:00:00')),aes(x=DateTimeHr,y=cumsum(P),group=NULL),color=1)+
    ylab(bquote('Cumulative Water, mm'))+
    guides(color=guide_legend(''),fill=guide_legend(''))+xlab('Time')+
    theme_bw()+theme(panel.grid.major.x=element_blank(),panel.grid.minor = element_blank())+
    scale_fill_brewer(palette = 'Set2')+
    scale_color_brewer(palette = 'Set2')+
    scale_x_datetime(date_breaks = '4 hours',date_labels = '%H:%M',expand = expansion())+
    theme(legend.text = element_text(family='sans',size=10,color=1),
          axis.text = element_text(family='sans',size=10,color=1),
          axis.title = element_text(family='sans',size=10,color=1))+
    theme(legend.position = c(0,1),legend.justification = c(0,1),legend.background = element_blank(),legend.spacing.y = unit(0,units='pt'),legend.spacing.x = unit(1,units='pt'),legend.title = element_blank() ,legend.margin = margin(1,1,1,1))
  ggsave(paste0(wd,'/fig4b.png'),width = 7,height = 7)
  
  #combine plots
  {
    g1 <- ggplotGrob(
      fig4a+theme(axis.title.x=element_blank())+
        labs(tag='a')+
        theme(plot.tag.position = c(0,1),
              plot.tag = element_text(family = 'sans',size=14,
                                      margin=margin(0,5,0,2)))
    )
    g2 <-ggplotGrob(
      fig4b+theme(legend.position = 'none')+
        labs(tag='b')+
        theme(plot.tag.position = c(0,1),
              plot.tag = element_text(family = 'sans',size=14,
                                      margin=margin(0,5,0,2)))
    )
    
    g <- rbind(g1, g2, size = "first")
    g$widths <- unit.pmax(g1$widths, g2$widths)
    # grid.newpage()
    # grid.draw(g)
    
    gg<-arrangeGrob(g)
    ggsave(plot=gg,filename=paste0(wd,'/fig4.png'),width=6.5/2,height = 6.5,units = 'in',dpi=450)
  }
  print(paste0(' plots done: ',outDir))
}

#figure 5, monthly boxplot
{
  #figure 5 friedman
  #see stats_Month_final.R
  # ggplot(friedDat,aes(x=Month,y=median,color=veg2))+geom_boxplot(notch = FALSE) + 
  #   facet_wrap(~veg2)+
  #   stat_pvalue_manual(pwc, hide.ns = TRUE)+
  #   # stat_summary(fun.data = 'mean_sdl',fun.args = list(mult=0),aes(y=median,color=veg2),position=position_dodge(0.75),shape=18)+
  #   ylab(bquote('Evapotranspiration, mm h'^-1))+
  #   guides(color=guide_legend(''))+xlab('')+#ylim(0,1)+
  #   theme_bw()+theme(panel.grid.major.x=element_blank(),panel.grid.minor = element_blank())+
  #   scale_color_brewer(palette = 'Set2')+
  #   theme(legend.text = element_text(family='sans',size=10,color=1),
  #         axis.text = element_text(family='sans',size=10,color=1),
  #         axis.title = element_text(family='sans',size=10,color=1))+
  #   theme(legend.position = 'none',legend.background = element_blank(),legend.spacing.y = unit(0,units='pt'),legend.spacing.x = unit(6,units='pt'),legend.title = element_blank() ,legend.margin = margin(1,1,1,1))+
  #   theme(strip.text = element_text(family='sans',size=10,color=1),
  #         strip.background = element_rect(fill='white',color = NULL,linetype = 0))
  # 
  # ggsave(paste0(wd,'/Fig5fried.png'),width = 3.75,height = 3.75,units = 'in',dpi=450)  
}

#figure 6
# EP_full_veg from results_veg3_final.r
{
    EP_full_veg$veg2<-factor(EP_full_veg$veg,levels=c("moss","mixed","tussock"),labels = c("moss E","mixed ET","tussock ET"))
    ggplot((EP_full_veg %>% dplyr::filter(ETP2=='Evap.',DateTime_Hour>'2017-06-12',DateTime_Hour<'2017-08-16') ),aes(x=DateTime_Hour))+
      geom_ribbon(aes(ymin=measure-(mm_diff_sd*1),ymax=measure+(mm_diff_sd*1),fill=veg2,group=veg2),alpha=0.5)+
      geom_line(aes(y=measure,color=veg2),size=0.8)+
      #cum P is ~400mm 
      # geom_line(data=(EP_full_vegMT %>% dplyr::filter(veg2=='Moss E',DateTime_Hour>'2017-06-12') %>% mutate(P2=ifelse(is.na(P),0,P))),aes(y=cumsum(P2)),size=0.8)+
      ylab('cumulative ET, mm')+xlab('')+
      # geom_bar(aes(y=P*5),stat='identity',position = 'dodge')+
      guides(color=guide_legend(''),fill=guide_legend(''))+
      scale_x_datetime(date_breaks = '14 days',date_labels = '%b-%d')+
      # scale_y_continuous(sec.axis = sec_axis(~./5,name='Hourly Precipitation, mm'))+
      scale_color_brewer(palette = 'Set2')+
      scale_fill_brewer(palette = 'Set2')+
      theme_bw()+theme(panel.grid.major.x=element_blank(),panel.grid.minor = element_blank())+
      theme(legend.text = element_text(family='sans',size=10,color=1),
            axis.text = element_text(family='sans',size=10,color=1),
            axis.title = element_text(family='sans',size=10,color=1))+
      theme(axis.text.x = element_text(angle = 45,hjust=1,vjust=1,family='sans',size=10,color=1))+
      theme(legend.position = c(0,1),legend.justification = c(0,1),legend.background = element_blank(),legend.spacing.y = unit(0,units='pt'),legend.spacing.x = unit(1,units='pt'),legend.title = element_blank() ,legend.margin = margin(1,1,1,1))
    
    ggsave(filename=paste0(wd,'/Fig6.png'),width = 3.75,height = 3.75,units = 'in',dpi=450)
    
    #compare to table 2 values
    (EP_full_veg %>% dplyr::filter(ETP2=='Evap.',DateTime_Hour>'2017-06-12',DateTime_Hour<'2017-08-16') ) %>% as_tibble() %>% group_by(veg2) %>% summarise(ETmax=max(measure,na.rm=TRUE))
}

#figure 7
# lysTotseason from results_veg3_final.r
{
  # > lysTotseason
  # # A tibble: 3 x 6
  # veg     evapLmeanmean evapLmeansd evapTotmean evapTotsd evapTotn
  # mixed          -0.106     0.00468       -234.     10.3        21
  # moss           -0.118     0.00661       -260.     14.6        17
  # tussock        -0.112     0.00311       -247.      6.87        6
  bdat<-data.frame(study=c('Kane et al. 2004 ET','Euskirchen et al. 2012 ET', 'S. pulchra, B. nana T','S. alaxensis T', 'mixed T*','mixed ET*','moss E*','tussock ET*'),
                   site=c(rep('Tundra shrub',8)),
                   type=c(rep('ET',2),rep('T',3),'ET','E','ET'),
                   col=factor(c('Kane et al. 2004','Euskirchen et al. 2012','Ch. 2', 'Ch. 2', 'This study','This study','This study','This study')),
                   col2=factor(c('Landscape ET','Landscape ET','Ch. 2', 'Ch. 2', 'This study','This study','This study','This study')),
                   mean=c(179,mean(c(144,177,186,164,171,142,170,166,168)),11.9,107.3,NA,-(lysTotseason%>%dplyr::filter(veg=='mixed')%>%pull(evapTotmean)),-(lysTotseason%>%dplyr::filter(veg=='moss')%>%pull(evapTotmean)),-(lysTotseason%>%dplyr::filter(veg=='tussock')%>%pull(evapTotmean))),
                   sd=c(40,sd(c(144,177,186,164,171,142,170,166,168)),(3.73/1),(59.57/sqrt(1)),NA,lysTotseason%>%dplyr::filter(veg=='mixed')%>%pull(evapTotsd),lysTotseason%>%dplyr::filter(veg=='moss')%>%pull(evapTotsd),lysTotseason%>%dplyr::filter(veg=='tussock')%>%pull(evapTotsd) ) )
  bdat$study2<-factor(bdat$study,levels =c('Kane et al. 2004 ET','Euskirchen et al. 2012 ET', 'S. pulchra, B. nana T','S. alaxensis T', 'mixed T*','mixed ET*','moss E*','tussock ET*')
  )
  bdat$type2<-factor(bdat$type,levels=c('ET',"T","E"))
  bdat$studyType<-factor(bdat$study,levels =c('Euskirchen et al. 2012 ET','Kane et al. 2004 ET','mixed ET*','tussock ET*','mixed T*','S. alaxensis T','S. pulchra, B. nana T','moss E*'))
  bdat<-bdat %>% arrange(studyType)
  stLab=c('Euskirchen et al. 2012 ET','Kane et al. 2004 ET','mixed ET*','tussock ET*',expression(italic('S. alaxensis')*' T'),expression(italic('S. pulchra, B. nana')*' T'),'moss E*')
  barw<-0.4
  pd<-position_dodge(width = 0.5)
  pd<-position_dodge2(preserve = "single")
  
  #custom colors to match previous figures
  pal<-brewer.pal(n=8,name='Set2')
  pal<-pal[c(5,6,2,3,7,8,1)]
  bdat<-bdat %>% merge(.,data.frame(study=c('Euskirchen et al. 2012 ET','Kane et al. 2004 ET','mixed ET*','tussock ET*','S. alaxensis T','S. pulchra, B. nana T','moss E*'),pal),by='study',all.x=TRUE)
  
  ggplot(bdat %>% dplyr::filter(study!='Mixed T*'),aes(x=type2,y=mean,group=studyType))+
    geom_col(aes(fill=pal),position = pd)+
    geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),position = pd)+
    scale_fill_identity(guide=guide_legend(title = ''),labels=stLab,breaks=pal)+
    scale_y_continuous(expand = expansion(mult=0.01))+
    scale_x_discrete(expand=expansion(add=0.5))+
    theme_bw()+
    theme(legend.text = element_text(family='sans',size=12,color=1),axis.text = element_text(family='sans',size=12,color=1),axis.title = element_text(family='sans',size=12,color=1))+
    theme(panel.grid.major.x = element_blank())+
    theme(legend.text.align = 0)+
    labs(x='Water Flux Component',y='Cumulative Water, mm')+
    theme_bw()+theme(panel.grid.major.x=element_blank(),panel.grid.minor = element_blank())+
    theme(legend.text = element_text(family='sans',size=10,color=1),
          axis.text = element_text(family='sans',size=10,color=1),
          axis.title = element_text(family='sans',size=10,color=1))+
    theme(legend.position = 'top',legend.justification = c(0.5,1),legend.background = element_blank(),legend.spacing.y = unit(0,units='pt'),legend.spacing.x = unit(1,units='pt'),legend.title = element_blank() ,legend.margin = margin(1,25,1,1))
  
  ggsave(filename=paste0(wd,'/Fig7.png'),width=6.5,height = 3.25,units='in',dpi=600)
  ggsave(filename=paste0(wd,'/Fig7_sq.png'),width=6.5,height = 6.5,units='in',dpi=600)
}
