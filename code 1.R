coffee_new<-coffee15_1
library(ggplot2)

#定性变量转化为定量变量
coffee_new$Qualityclass_numeric<-ifelse(coffee_new$Qualityclass == "Good",1,0)

#对数据集中的定量变量进行标准化
library(dplyr)
coffee_new_std<-coffee_new %>%
  select(aroma,flavor,acidity,category_two_defects,Qualityclass) %>%
  mutate_if(is.numeric,scale)

#变量转换
coffee_new_std$Qualityclass_numeric<-ifelse(coffee_new_std$Qualityclass == "Good",1,0)

#建立二项式回归模型
model<-glm(Qualityclass_numeric ~ aroma+flavor+acidity+category_two_defects,data=coffee_new_std,family = binomial)
summary(model)
anova(model)
plot(model)

model1<-model<-glm(Qualityclass_numeric ~ aroma+flavor+acidity,data=coffee_new_std,family = binomial)
summary(model1)

model2<-glm(Qualityclass_numeric ~ aroma+flavor,data=coffee_new_std,family = binomial)
summary(model2)

model3<-glm(Qualityclass_numeric ~ aroma+acidity,data=coffee_new_std,family = binomial)
summary(model3)

#绘制箱线图以国家为例
library(ggplot2)
ggplot(coffee_new, aes(x =harvested, y = flavor, fill = country_of_origin)) +
  geom_bar(stat = "identity", position = "dodge")

