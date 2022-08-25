#friedman, evap over time (months), separate tests for each veg
library(tidyverse)
library(ggpubr)
library(rstatix)

# depends on 
# results_veg3_final.r

# aR2_ET_Nodech_h_P from results_veg3_final.r

#calc monthly stats by lysimeter (nodech)
friedDat<-aR2_ET_Nodech_h_P %>% dplyr::filter(ppt==0) %>% group_by(veg2,Month,Nodech) %>% mutate(evap=evap*-1) %>%
  get_summary_stats(evap, type = "common")

#Friedman test
res.fried <- friedDat %>% group_by(veg2) %>%friedman_test(median ~ Month |Nodech)
res.fried
# veg2       .y.        n statistic    df         p method       
# * <fct>      <chr>  <int>     <dbl> <dbl>     <dbl> <chr>        
# 1 moss E     median    17     19.2      2 0.0000685 Friedman test
# 2 mixed ET   median    21      6.67     2 0.0355    Friedman test
# 3 tussock ET median     6      4.33     2 0.115     Friedman test
# Effect size
friedDat %>% group_by(veg2) %>% friedman_effsize(median ~ Month |Nodech)
# 
# veg2       .y.        n effsize method    magnitude
# * <fct>      <chr>  <int>   <dbl> <chr>     <ord>    
# 1 moss E     median    17   0.564 Kendall W large    
# 2 mixed ET   median    21   0.159 Kendall W small    
# 3 tussock ET median     6   0.361 Kendall W moderate 
#post-hoc wilcox pairwise comparison
pwc <- friedDat %>% group_by(veg2) %>% 
  wilcox_test(median ~ Month, paired = TRUE, p.adjust.method = "bonferroni")
pwc
# veg2       .y.    group1 group2    n1    n2 statistic        p    p.adj p.adj.signif
# 1 moss E     median June   July      17    17        10 0.000656 0.002    **          
# 2 moss E     median June   August    17    17         4 0.000107 0.000321 ***         
# 3 moss E     median July   August    17    17        41 0.098    0.295    ns          
# 4 mixed ET   median June   July      21    21        88 0.355    1        ns          
# 5 mixed ET   median June   August    21    21        37 0.005    0.015    *           
# 6 mixed ET   median July   August    21    21        35 0.009    0.028    *           
# 7 tussock ET median June   July       6     6         7 0.563    1        ns          
# 8 tussock ET median June   August     6     6         2 0.094    0.281    ns          
# 9 tussock ET median July   August     6     6         4 0.219    0.657    ns  

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Month")
ggplot(friedDat,aes(x=Month,y=median,color=veg2))+geom_boxplot(notch = FALSE) + 
  facet_wrap(~veg2)+
  stat_pvalue_manual(pwc, hide.ns = TRUE)+
  ylab(bquote('Evapotranspiration, mm h'^-1))+
  guides(color=guide_legend(''))+xlab('')+#ylim(0,1)+
  theme_bw()+theme(panel.grid.major.x=element_blank(),panel.grid.minor = element_blank())+
  scale_color_brewer(palette = 'Set2')+
  theme(legend.text = element_text(family='sans',size=10,color=1),
        axis.text = element_text(family='sans',size=10,color=1),
        axis.title = element_text(family='sans',size=10,color=1))+
  theme(legend.position = 'none',legend.background = element_blank(),legend.spacing.y = unit(0,units='pt'),legend.spacing.x = unit(6,units='pt'),legend.title = element_blank() ,legend.margin = margin(1,1,1,1))+
  theme(strip.text = element_text(family='sans',size=10,color=1),
        strip.background = element_rect(fill='white',color = NULL,linetype = 0))

ggsave(paste0(wd,'/Fig5fried.png'),width = 3.75,height = 3.75,units = 'in',dpi=450)  

#median rates by veg, month
friedDat %>% group_by(Month,veg2) %>% summarise(medET=median(median))
# Month  veg2        medET
# 1 June   moss E     0.061 
# 2 June   mixed ET   0.056 
# 3 June   tussock ET 0.071 
# 4 July   moss E     0.099 
# 5 July   mixed ET   0.077 
# 6 July   tussock ET 0.0795
# 7 August moss E     0.124 
# 8 August mixed ET   0.095 
# 9 August tussock ET 0.13  