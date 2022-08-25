# Anova for lysimeter total evap between veg
library(tidyverse)
library(performance)

#lysTot df from results_veg_final.R

# total ET
# check assumptions for anova
  #outliers
    #4, 1_4
    # lysTot[4,]
  
  #outliers
    par(mfrow=c(2,2))
    plot(lysTot[-4,] %>% aov(evapLTot~veg,data=.))
    par(mfrow=c(1,1))
    #no pattern/trend in residuals
  #normality
    is_norm <- check_normality(lysTot[-4,] %>% aov(evapLTot~veg,data=.))
    # OK: residuals appear as normally distributed (p = 0.221).
    plot(is_norm, type = "qq", detrend = TRUE)
    #no outliers on qq
    #fits normality
  # homogeneity between group variance
    check_homogeneity(lysTot[-4,] %>% aov(evapLTot~veg,data=.))
    # OK: There is not clear evidence for different variances across groups (Bartlett Test, p = 0.461).
  #visually compare data  
  ggplot(lysTot[-4,],aes(y=evapLTot))+geom_boxplot(aes(x=veg,color=veg),notch=TRUE)
#test anova
  Lys_aov<-lysTot[-4,] %>% aov(evapLTot~veg,data=.)
  summary(Lys_aov)
  # Df Sum Sq Mean Sq F value   Pr(>F)    
  # veg          2   7431    3716   34.32 2.09e-09 ***
  #   Residuals   40   4330     108     
  tukey.Lys_aov<-TukeyHSD(Lys_aov)
  tukey.Lys_aov
  # $veg
  # diff        lwr        upr     p adj
  # moss-mixed    -28.59805 -37.001597 -20.194505 0.0000000
  # tussock-mixed -13.29757 -25.020282  -1.574864 0.0230495
  # tussock-moss   15.30048   3.177562  27.423393 0.0104347

  #removing the outlier changes the total for moss ~1%
  lysTot[-c(4),] %>% group_by(veg) %>% get_summary_stats(evapLTot,type='common')

# mean hourly ET
  # check assumptions for anova
  #outliers
  #1_4, 10_3, 10_5
  # lysTot_unfilled[c(4,7,8),]
  
  #outliers
  par(mfrow=c(2,2))
  plot(lysTot_unfilled[-c(4,7,8),] %>% aov(evapLmean~veg,data=.))
  par(mfrow=c(1,1))
  #no pattern/trend in residuals
  #normality
  is_norm <- check_normality(lysTot_unfilled[-c(4,7,8),] %>% aov(evapLmean~veg,data=.))
  # OK: residuals appear as normally distributed (p = 0.337).
  plot(is_norm, type = "qq", detrend = TRUE)
  #no outliers on qq
  #fits normality
  # homogeneity between group variance
  check_homogeneity(lysTot[-c(4,7,8),] %>% aov(evapLmean~veg,data=.))
  # OK: There is not clear evidence for different variances across groups (Bartlett Test, p = 0.590).
  #visually compare data  
  ggplot(lysTot[-c(4,7,8),],aes(y=evapLmean))+geom_boxplot(aes(x=veg,color=veg),notch=TRUE)
  #test anova
  Lys_aov<-lysTot[-c(4,7,8),] %>% aov(evapLmean~veg,data=.)
  summary(Lys_aov)
  # Df    Sum Sq   Mean Sq F value  Pr(>F)    
  # veg          2 0.0014554 0.0007277   43.71 1.4e-10 ***
  #   Residuals   38 0.0006326 0.0000166        
  tukey.Lys_aov<-TukeyHSD(Lys_aov)
  tukey.Lys_aov
  # $veg
  # diff          lwr           upr     p adj
  # moss-mixed    -0.013029870 -0.016428721 -0.0096310186 0.0000000
  # tussock-mixed -0.005429592 -0.010061446 -0.0007977388 0.0184103
  # tussock-moss   0.007600278  0.002793576  0.0124069790 0.0012287
  
  #removing the outliers changes the average rates ~1%
  lysTot_unfilled[-c(4,7,8),] %>% group_by(veg) %>% get_summary_stats(evapLmean,type='common')
  