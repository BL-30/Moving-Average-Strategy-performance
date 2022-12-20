---
title: "Assignment 1 - Grp 17"
output:
  html_document: default
  pdf_document: default
date: "2022-11-10"
---


```{r include=FALSE}
library(lmtest)
library(sandwich)
library(AER)
library(xts)
library(PerformanceAnalytics)

```

```{r 1}
data1 <- read.csv("C:/Users/natha/Documents/Imperial/R4finance/compustat_sec_2019_2022/compustat_sec_2019_2022.csv")
data <- data1[1:35,];data
```

Question 1

```{r}
mean(data$growth)
mean(data$tradeshr)
sd(data$growth)
sd(data$tradeshr)
```
The sample mean of growth and tradeshr is 1.942715 and 0.564703. The sample standard deviation of growth and tradeshr is 1.89712 and 0.2892703.

```{r Daily returns}
# Create a new column in the data frame with lagged prccd

shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

data$lagged_p <- shift(data$prccd, 1)

# Calculate daily returns and store in a new column named daily_return

data$daily_return <- stock_data$prccd/stock_data$lagged_p -1

```

```{r}

daily_return <- sum(data$prccd) / nrown(na.omit(data$datadate)) 


data$daily_return <- data$prccd/data$lagged_p -1

# Extract the month from each date using floor_date(), and store in a new column
#data$month <- floor_data(  )
data$month <- format(data$datadate,"%m")

# Create a new column named max_return with the maximum daily returns each month
max_daily_return <-  data  %>%
  group_by(conm, month ) %>%
  filter(is.finite(daily_return)) %>%
  mutate(max_return = max(daily_return)  ) %>%
  ungroup()
  
# Keep only the rows where the maximum daily return matches the daily return
max_daily_return.uni <- 
  max_daily_return[max_daily_return$daily_return == max_daily_return$max_return,]

# Remove any duplicate rows
max_daily_return.uni1 <- 
  max_daily_return.uni[!duplicated(max_daily_return.uni[,c("datadate", "conm", "daily_return","month", "max_return")]), ]
  
  
```

```{r}

fast_ma <- rollapply(stock_ts, width = 50, FUN = mean)
slow_ma <- rollapply(stock_ts, width = 200, FUN = mean)
signals <- sign(fast_ma - slow_ma)


```

```{r}

moving_a <-function( data, n ) {
  
  for(i in 0:n-1){
    ma.append(None)
  }
  
  for(i in n:len(data$prccd)+1){
    somme <- sum( data$prccd[i-n:i] )
    ma <- somme/n
  }
  return(ma)
}


```

```{r}
ma1 <- moving_a(data,7)
ma2 <- moving_a(data,30)

cross_o_strat <- function(ma1,ma2){
  
    for(i in 1:len(ma2)){
      if(ma1[i]==None || ma2[i]==None){
        ind.append(None)
      }
            
        else{
          if(ma1[i]>ma2[i]){
             ind.append(1)
          }
               
            if(ma1[i]<ma2[i]){
               ind.append(0)
            } 
        }
    }
    return(ind)
}

```

Question 2 

```{r}
reg <- lm(growth ~ tradeshr, data = data)
vcov <- vcovHC(reg, type = "HC1")
coeftest(reg, vcov. = vcov) 
```
a) The coefficient on tradeshr is 2.30643. It means that one unit increase of tradeshr is, on average, predicted to increase growth by 2.30643. Based on some researched, it shows that the numerical value of our estimate is large in an economic real world sense. 


b) 
```{r}
plot(data$tradeshr, data$growth,col = "blue", main = "Regression of growth on tradeshr",xlab = "tradeshr", ylab = "growth")
abline(reg)
```
 
c) The p value of the slope coefficient is 0.0009235, which is less than 5%. Therefore, we reject the null hypothesis and there is enough evidence to show that the slope coefficient is statistically significantly different from zero at the 5% significance level.

d)
```{r}
2.30643 + 1.96 * 0.66329
2.30643 - 1.96 * 0.66329
```
The 95% confidence interval for $\beta_{1}$ is (1.006382, 3.606478) with heteroskedasticity-robust standard errors.

e)
```{r}
summary(reg)
```
The R-squared is 12.37%, which means that 12.37% variance of the dependent variable is explained  by the estimated regression line.

f)
```{r}
cor(data$growth,data$tradeshr)
cor(data$growth,data$tradeshr)^2
```

The square of correlation coefficient is equal to the R-squared.

g)
```{r}

RSS <- (1.79^2)*63
RMSE <- sqrt(RSS/65)
RMSE
```
RMSE measures the average distance between the predicted values from the model and the actual values and it tells us how concentrated around the linear regression model the residuals are. In this case, the average error between the predicted values from the model and the actual values is 1.762246.

h)The regression error appears to be heteroskedastic since the data point are not equally spread around the regression line.


i)
```{r}
summary(reg)
coeftest(reg, vcov. = vcov) 
```
In this case, heteroskedasticity-robust standard errors are lower than homoskedasticity-only standard errors.

j)
```{r}
data_drop_outlier <- data[data$tradeshr < max(data$tradeshr),]
reg <- lm(growth ~ tradeshr, data = data_drop_outlier)
vcov <- vcovHC(reg, type = "HC1")
coeftest(reg, vcov. = vcov) 
```

Dropping the outlier does make a qualitative difference, which makes the slop coefficient much less statistically significant. The p values of the slope coefficient without the outlier(0.05670) is much larger than p values with the outlier(0.0009235). 

k) 
```{r}
data[data$tradeshr == max(data$tradeshr),]
```
The outlying observation is the greatest value of tradeshr, the average share of trade in the Maltese economy from 1960 to 1995.

The outlier should be omitted because, by the end of the fifties, some changes were observable in the Malta economy. These were mostly related to the expansion of the manufacturing sector and the phasing out of the British forces bases in Malta. The Maltese Gross Domestic Product and other important macroeconomic variables have also followed a cyclical pattern of change.

Thus, it should be omitted from the regression, due to its unique economy system of Malta.


Question 3
```{r}
data$quartile <- as.character(cut(data$rgdp60,quantile(data$rgdp60,probs = seq(0,1,length = 5),type = 5),include.lowest = T, labels = c("q1","q2","q3","q4")))
data$lorgdp60 <- as.numeric((data$quartile == "q1"))
```

```{r}
reg <- lm(growth ~ lorgdp60, data = data)
vcov <- vcovHC(reg, type = "HC1")
coeftest(reg, vcov. = vcov) 
```

a) The coefficient on lorgdp60 is -1.38212. It means that the growth will be 1.38212 lower if the economy’s GDP is in the bottom quartile of GDP for 1960. The numerical value of the estimate is large in an economic (real-world) sense from 1960 to 1995.

b) The p value for lorgdp60 is 0.04095, which is less than 5%. Therefore, there is enough evidence to reject the null hypothesis at 5% significance level and show that the mean growth rate from 1960-1995 of  economies with lorgdp60 = 1 is different from economies with lorgdp60 = 0.

c)
```{r}
low_rgdp60 <- data[data$lorgdp60 == 1, "growth"]
high_rgdp60 <- data[data$lorgdp60 == 0, "growth"]
mean(low_rgdp60) - mean(high_rgdp60)
t.test(low_rgdp60, high_rgdp60, var.equal = T)
t.test(low_rgdp60, high_rgdp60, var.equal = F)
```

There are two cases as to conduct the differences-of means t test. 

Case 1: Assuming equal variance between these two groups, the p value is 0.01027. There is enough evidence to reject the null hypothesis at the 5% significance level and show that the mean growth rates are different.

Case 2: Assuming unequal variance between these two groups, the p value is 0.05397. There is no enough evidence to reject the null hypothesis at the 5% significance level.

d)
```{r}
summary(reg)
coeftest(reg, vcov. = vcov) 
t.test(low_rgdp60, high_rgdp60, var.equal = T)
t.test(low_rgdp60, high_rgdp60, var.equal = F)
```
t-statistics computed in (c): -2.6462(equal variance); -2.0572(unequal variance)
t-statistics with homoskedasticity-only standard errors:-2.646
t-statistics with heteroskedasticity-robust standard errors: -2.0869

The t-statistics with heteroskedasticity-robust standard errors is much less than the one with homoskedasticity-only standard errors.

If we assume equal variance for the two groups, the t-statistics of the differences-in-mean is almost equal to the t-statistics with homoskedasticity-only standard errors.

Question 4
```{r}
#(1)
data_s <- data[data$X != "Malta",]
reg <- lm(growth ~ tradeshr+school60, data = data_s)
summary(reg)
vcov <- vcovHC(reg, type = "HC1")
coeftest(reg, vcov. = vcov) 
linearHypothesis(reg, c("tradeshr=0", "school60=0"),white.adjust =
"hc1")
```
```{r}
RMSE = sqrt(((1.691^2)*61)/64)
RMSE
```

```{r}
#(2)
reg <- lm(growth ~ tradeshr+school60+capstock60, data = data_s)
summary(reg)
vcov <- vcovHC(reg, type = "HC1")
coeftest(reg, vcov. = vcov) 
linearHypothesis(reg, c("tradeshr=0", "school60=0"),white.adjust =
"hc1")
linearHypothesis(reg, c("tradeshr=0", "school60=0","capstock60=0"),white.adjust =
"hc1")
```
```{r}
RMSE = sqrt(((1.641^2)*60)/64)
RMSE
```

```{r}
#(3)
reg <- lm(growth ~tradeshr+school60+capstock60+rev_coups+civil, data = data_s)
summary(reg)
vcov <- vcovHC(reg, type = "HC1")
coeftest(reg, vcov. = vcov) 
linearHypothesis(reg, c("tradeshr=0", "school60=0"),white.adjust =
"hc1")
linearHypothesis(reg, c("tradeshr=0", "school60=0","capstock60=0"),white.adjust =
"hc1")
linearHypothesis(reg, c("tradeshr=0", "school60=0","capstock60=0","rev_coups=0","civil=0"),white.adjust =
"hc1")
```
```{r}
RMSE = sqrt((1.42^2*57)/63)
RMSE
```


Question 5
```{r}
reg <- lm(growth ~  tradeshr + school60 + oil, data = data)
summary(reg)
```
The coefficient on oil is None. Since all the values of this variable is 0, without any variation, this oil variable has no predictability for growth.

 
Question 6

```{r}
reg <- lm(growth ~ tradeshr+school60, data = data_s)
summary(reg)
vcov <- vcovHC(reg, type = "HC1")
coeftest(reg, vcov. = vcov) 
linearHypothesis(reg, c("tradeshr=0", "school60=0"),type = "HC1")
```

a) $growth = -0.122236(0.691165) + 1.897823(0.865541)*tradeshr + 0.242975(0.075892)*school60$
    
b) Holding other variables constant, One more average year of schooling in total population in 1960 is predicted on average to increase growth by 0.242975.

c) The p-value is 0.032157 which is less than 5%. Therefore, there is enough evidence against the null hypothesis  and show that the coefficient on tradeshr is statistically different from zero at the 5% significance level. 

In everyday words, tradeshr is positively related to growth, which means average share of trade in the economy would result in average annual percentage growth of real per capita.

d) tradeshr

regression (1):1.897823

regression (2):1.81851

regression (3):1.24298 

Indeed, the coefficients on tradeshr differ in regressions (1), (2), and (3) in a substantively important way.


e) The p value for the F test is 0.001968, which  is much less than 5%. There is enough evidence against null hypothesis at the 5% significance level and show that at least one coefficient is nonzero. 

f) The coefficient on rev_coups is -1.50708.

We think the magnitude makes sense, but there's no enough evidence to prove the correctness of the sign.

From previous study, many economists conclude the significant relationship between revolutions and economic growth[1-4]. However, they hold different views about whether they are negatively or positively correlated. For example, Carl[1] conducts statistical analysis and shows that revolutions occur more frequently after economic crises, and Leonid[2] believes that revolutions in general tend to impede rather than to promote the economic growth. While Robert[3] and Richard[4] states the huge economic growth has positive relationship with Industrial Revolution, the Demographic Revolution and so on. 
   
So we guess the sign of relationship also depends on the country(region), the type of revolutions, and some other historical reasons.

Source:

[1] Carl Henrik Knutsen, 2014. "Income Growth and Revolutions," Social Science Quarterly, Southwestern Social Science Association, vol. 95(4), pages 920-937, December.

[2] Leonid Grinin, Ilya Ilyin, Peter Herrmann & Andrey V. Korotayev (eds.), Globalistics and Globalization Studies: Big History & Global History. Volgograd,Russia: Uchitel Publishing House. pp. 87-109 (2015)

[3] Is U.S. Economic Growth Over? Faltering Innovation Confronts the Six Headwinds[J]. Social Science Electronic Publishing.

[4] Easterlin, R.A. Three Revolutions of the Modern Era. Comp Econ Stud 61, 521–530 (2019). https://doi.org/10.1057/s41294-019-00098-9

g) The coefficient of the factor that represents the level of civil liberties of a country is -0.33581. This indicates
that one unit (in scale) of increase in civil liberty index will cause about 0.336 less growth in per capita
GDP of a country from 1960 to 1965, which means more civil liberty will obviously slow down the economic
growth of a country during that time.

This negative sign makes sense due to the reason that, in most cases, the government will make the correct
decisions and legislation that is most helpful to economic development in the long term. At the same time,
civics sometimes are not always rational. Therefore, one may consider that there’s a trade off between civil
liberty and GDP growth, and we could deduce that the more freedom the citizens have politically, the less
likely the economy will grow rapidly.

The magnitude of the coefficient, however, is arguable. The extend of influence to economic growth to a
country from this factor may vary in different national conditions.


h) Both rev_coups and civil are not statistically significant at the 5% significance level since their p value 0.0903 and 0.0572 are larger than 0.05.

i) The p-value for the F test is 3.114e-05, which is less than 1%. Therefore, there is enough evidence against the null hypothesis at the 1% significance level.

Based on the result of F test, it shows that at least one of the factors is significant. However, according to part(h), the individual test for each coefficient is not significant. 


j) p value of school60

regression (1):0.075892

regression (2):0.142630

regression (3):0.129493

The p value of school60 in those three regressions are very large. It indicates that there is no enough evidence to show that a better educated work force will have a higher rate of productivity and
therefore have a higher growth rate. So this prediction is not borne out in the regression results

k)

regression (1):0.242975 (0.075892)

regression (2):0.50130 (0.142630)

The difference in the standard error and the coefficient on school60 in regression(1) and (2) may be attributed to the addition of the new factor capstock60. The new variable may be correlated with school60, which will influence the fitting of the coefficient on school60. 

l)
```{r}
4*0.391570
4*(0.391570-1.96*0.129493)
4*(0.391570+1.96*0.129493)
```
Holding constant the other variables, a country with 8 years of school will has on average 1.56628 more average annual percentage growth of real per capita GDP from 1960 to 1995 than a country with 4 years of school. The 95% confidence interval for this difference is (0.5510549, 2.581505).
 






