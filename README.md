# DAS-Group-15
library(tidyverse)
dt=read_csv('dataset15.csv')

dt=dt %>%
drop_na() %>% 
  dplyr::filter(country_of_origin!='Taiwan'& !harvested %in% c(2010,2011,2012)) %>% 
  mutate(Qualityclass=as_factor(Qualityclass))


pairs(dt[-1])



dt[c(-1,-8)]=scale(dt[c(-1,-8)])


glm.fit1=glm(Qualityclass~aroma+flavor+acidity+category_two_defects+altitude_mean_meters
            , data=dt, family =binomial)

summary(glm.fit1) 


glm.fit2=glm(Qualityclass~aroma+flavor+acidity+altitude_mean_meters
            , data=dt, family =binomial)

summary(glm.fit2) 
glm.fit3=glm(Qualityclass~aroma+acidity+altitude_mean_meters
             , data=dt, family =binomial)
summary(glm.fit3) 
pred=glm.fit2$fitted.values
pred[pred>=0.5]='Poor'
pred[pred<=0.5]='Good'