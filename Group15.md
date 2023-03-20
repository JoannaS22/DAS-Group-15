DAS-Group-15
================
2023-03-15

``` r
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggplot2)
```

# Data Introduction

This data come from the Coffee Quality Database (CQD). The database
contains information from the CoffeevQuality Institute which is a
non-profit organisation working internationally to improve the quality
of coffee and thelives of the people who produce it. Each of the 5
datasets contain information on features of coffee and its production
includinan overall score of quality. • *country_of_origin* – Country
where the coffee bean originates from. • *aroma* – Aroma grade (ranging
from 0-10) • *flavor* – Flavour grade (ranging from 0-10) • *acidity* –
Acidity grade (ranging from 0-10) • *category_two_defects* – Count of
category 2 type defects in the batch of coffee beans tested. •
*altitiude_mean_meters* – Mean altitude of the growers farm (in metres)
• *harvested* – Year the batch was harvested • *Qualityclass* – Quality
score for the batch (Good - ≥ 82.5, Poor - \<82.5).

# Data Cleaning

First clear the null lines in the data, and then delete all lines with
the line name *Taiwan* since *Taiwan* is not a country.

``` r
coffee.data <- read.csv("/Users/sunyazhu/Desktop/University of Glasgow/R program/AllDatasetsR/coffee.csv")
coffee <- na.omit(coffee.data)
coffee<- subset(coffee, country_of_origin != 'Taiwan')
coffee
```

Using Qualityclass as a classification variable, assign Qualityclass 0
and 1 in terms of it’s original value poor and good.

``` r
coffee$Qualityclass <- as.integer(coffee$Qualityclass == "Good")
coffee
```

A subset with reasonable data distribution is selected from
*coffee.data’s* data set and stored in a new data set, *coffee*.

``` r
quantiles_aroma <- quantile(coffee.data$aroma,probs=c(0.25,0.75),na.rm=FALSE)
```

## Clearing outliers

``` r
quantiles_aroma <- quantile(coffee$aroma,probs=c(0.25,0.75),na.rm=FALSE)
IQR_aroma <- IQR(coffee$aroma)
Lower_aroma <- quantiles_aroma[1]-1.5*IQR_aroma
Upper_aroma <- quantiles_aroma[2]+1.5*IQR_aroma
coffee<- subset(coffee,coffee$aroma>Lower_aroma & coffee$aroma< Upper_aroma)

quantiles_flavor <- quantile(coffee$flavor,probs=c(0.25,0.75),na.rm=FALSE)
IQR_flavor <- IQR(coffee$flavor)
Lower_flavor <- quantiles_flavor[1]-1.5*IQR_flavor
Upper_flavor <- quantiles_flavor[2]+1.5*IQR_flavor
coffee<- subset(coffee,coffee$flavor>Lower_flavor & coffee$flavor< Upper_flavor)

quantiles_acidity <- quantile(coffee$acidity,probs=c(0.25,0.75),na.rm=FALSE)
IQR_acidity <- IQR(coffee$acidity)
Lower_acidity <- quantiles_acidity[1]-1.5*IQR_acidity
Upper_acidity <- quantiles_acidity[2]+1.5*IQR_acidity
coffee<- subset(coffee,coffee$acidity>Lower_acidity & coffee$acidity< Upper_acidity)
coffee
```

## Store the cleared data

``` r
write.csv(coffee,file="/Users/sunyazhu/Desktop/University of Glasgow/R program/AllDatasetsR/coffee_clean.csv",row.names=FALSE)
```

# Correlation test between variables

    ##                            aroma      flavor      acidity category_two_defects
    ## aroma                 1.00000000  0.67160282  0.538079249          -0.19698110
    ## flavor                0.67160282  1.00000000  0.705622108          -0.23495703
    ## acidity               0.53807925  0.70562211  1.000000000          -0.16888150
    ## category_two_defects -0.19698110 -0.23495703 -0.168881503           1.00000000
    ## altitude_mean_meters -0.03490461 -0.02777236 -0.006467399          -0.02396095
    ## harvested            -0.03412132  0.04100676  0.047885262          -0.13277330
    ## Qualityclass          0.59321981  0.66766788  0.604611195          -0.14226533
    ##                      altitude_mean_meters   harvested Qualityclass
    ## aroma                        -0.034904613 -0.03412132   0.59321981
    ## flavor                       -0.027772358  0.04100676   0.66766788
    ## acidity                      -0.006467399  0.04788526   0.60461119
    ## category_two_defects         -0.023960951 -0.13277330  -0.14226533
    ## altitude_mean_meters          1.000000000  0.07402986  -0.05075134
    ## harvested                     0.074029856  1.00000000   0.03792407
    ## Qualityclass                 -0.050751344  0.03792407   1.00000000

# Data visualization

    ## List of 1
    ##  $ legend.position: chr "none"
    ##  - attr(*, "class")= chr [1:2] "theme" "gg"
    ##  - attr(*, "complete")= logi FALSE
    ##  - attr(*, "validate")= logi TRUE

    ## Warning: Removed 24 rows containing non-finite values (`stat_boxplot()`).

    ## Warning: Removed 37 rows containing non-finite values (`stat_boxplot()`).

    ## Warning: Removed 28 rows containing non-finite values (`stat_boxplot()`).

    ## Warning: Removed 67 rows containing non-finite values (`stat_boxplot()`).

    ## Warning: Removed 24 rows containing non-finite values (`stat_boxplot()`).

![](Group15_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

# Standardized data

``` r
## Creates a logical vector that determines which columns in the data box coffee contain numeric data. It uses the function sapply to determine whether the data type of each column is numeric.
numeric_cols <- sapply(coffee,is.numeric)
## The columns containing only numerical data in data box coffee are extracted and assigned to new data box numeric_coffee
numeric_coffee <- coffee[,numeric_cols]
## A new data box scaled_coffee is obtained by standardizing numerical data in numeric_coffee. The standardization process is to transform the data range of different variables into the same scale for easy comparison.
scaled_coffee <- as.data.frame(scale(numeric_coffee))
## Add the category_two_defects column from the original data box coffee to scaled_coffee.
scaled_coffee$category_two_defects <- coffee$category_two_defects
## Assign the numerical data in the standardized data frame scaled_coffee back to the corresponding column in the original data frame coffee, and overwrite the original data.
coffee[,numeric_cols] <- scaled_coffee
coffee
```

# Modeling

``` r
model1 <- glm(Qualityclass~aroma+flavor+acidity,family=binomial(link="logit"),
              data=coffee)
model2 <- glm(Qualityclass~aroma*flavor+acidity,family=binomial(link="logit"),
              data=coffee)
model3 <- glm(Qualityclass~aroma+flavor*acidity,family=binomial(link="logit"),
              data=coffee)
model4 <- glm(Qualityclass~aroma*acidity+flavor,family=binomial(link="logit"),
              data=coffee)
model5 <- glm(Qualityclass~aroma*flavor+aroma*acidity,family=binomial(link="logit"),
              data=coffee)
model6 <- glm(Qualityclass~aroma*flavor+acidity*flavor,family=binomial(link="logit"),
              data=coffee)
model7 <- glm(Qualityclass~aroma*acidity+flavor*acidity,family=binomial(link="logit"),
              data=coffee)
model8 <- glm(Qualityclass~aroma*flavor+acidity*flavor+acidity*aroma,family=binomial(link="logit"),
              data=coffee)
model9 <- glm(Qualityclass~aroma*flavor*acidity,family=binomial(link="logit"),
              data=coffee)
model10 <- glm(Qualityclass~aroma+flavor,family=binomial(link="logit"),
              data=coffee)
model11 <- glm(Qualityclass~flavor+acidity,family=binomial(link="logit"),
              data=coffee)
model12 <- glm(Qualityclass~aroma+acidity,family=binomial(link="logit"),
              data=coffee)
anova(model1,model2,model3,model4,model5,model6,model7,
      model8,model9,model10,model11,model12)
```

    ## Analysis of Deviance Table
    ## 
    ## Model  1: Qualityclass ~ aroma + flavor + acidity
    ## Model  2: Qualityclass ~ aroma * flavor + acidity
    ## Model  3: Qualityclass ~ aroma + flavor * acidity
    ## Model  4: Qualityclass ~ aroma * acidity + flavor
    ## Model  5: Qualityclass ~ aroma * flavor + aroma * acidity
    ## Model  6: Qualityclass ~ aroma * flavor + acidity * flavor
    ## Model  7: Qualityclass ~ aroma * acidity + flavor * acidity
    ## Model  8: Qualityclass ~ aroma * flavor + acidity * flavor + acidity * 
    ##     aroma
    ## Model  9: Qualityclass ~ aroma * flavor * acidity
    ## Model 10: Qualityclass ~ aroma + flavor
    ## Model 11: Qualityclass ~ flavor + acidity
    ## Model 12: Qualityclass ~ aroma + acidity
    ##    Resid. Df Resid. Dev Df Deviance
    ## 1        895     560.73            
    ## 2        894     556.92  1    3.807
    ## 3        894     559.31  0   -2.387
    ## 4        894     559.35  0   -0.039
    ## 5        893     556.16  1    3.189
    ## 6        893     555.58  0    0.577
    ## 7        893     558.13  0   -2.554
    ## 8        892     555.07  1    3.063
    ## 9        891     554.98  1    0.089
    ## 10       896     612.86 -5  -57.882
    ## 11       896     623.63  0  -10.769
    ## 12       896     668.01  0  -44.377

``` r
summary(model9)
```

    ## 
    ## Call:
    ## glm(formula = Qualityclass ~ aroma * flavor * acidity, family = binomial(link = "logit"), 
    ##     data = coffee)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.3244  -0.2890   0.0973   0.4774   2.4013  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.02205    0.11010   0.200   0.8413    
    ## aroma                 1.27037    0.17537   7.244 4.36e-13 ***
    ## flavor                2.04169    0.23186   8.806  < 2e-16 ***
    ## acidity               1.20831    0.18136   6.662 2.69e-11 ***
    ## aroma:flavor         -0.51524    0.28630  -1.800   0.0719 .  
    ## aroma:acidity        -0.20096    0.25986  -0.773   0.4393    
    ## flavor:acidity       -0.34362    0.32587  -1.054   0.2917    
    ## aroma:flavor:acidity  0.10076    0.34201   0.295   0.7683    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1245.07  on 898  degrees of freedom
    ## Residual deviance:  554.98  on 891  degrees of freedom
    ## AIC: 570.98
    ## 
    ## Number of Fisher Scoring iterations: 8

``` r
AIC(model1)
```

    ## [1] 568.7271

``` r
AIC(model2)
```

    ## [1] 566.9203

``` r
AIC(model3)
```

    ## [1] 569.3077

``` r
AIC(model4)
```

    ## [1] 569.3465

``` r
AIC(model5)
```

    ## [1] 568.1577

``` r
AIC(model6)
```

    ## [1] 567.5803

``` r
AIC(model7)
```

    ## [1] 570.1348

``` r
AIC(model8)
```

    ## [1] 569.0716

``` r
AIC(model9)
```

    ## [1] 570.9826

``` r
AIC(model10)
```

    ## [1] 618.8648

``` r
AIC(model11)
```

    ## [1] 629.634

``` r
AIC(model12)
```

    ## [1] 674.0113

``` r
library(ROCR)
coffee.pr <- predict(model1, type="response")
score <- prediction(coffee.pr,coffee$Qualityclass)
perf <- performance(score,"tpr","fpr")
auc <- performance(score,"auc")
perfd <- data.frame(x= perf@x.values[1][[1]], y=perf@y.values[1][[1]])
p1 <- ggplot(perfd, aes(x= x, y=y)) + geom_line() +
  xlab("False positive rate") + ylab("True positive rate") +
  ggtitle(paste("Area under the curve:", round(auc@y.values[[1]], 3)))
p1
```

![](Group15_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
