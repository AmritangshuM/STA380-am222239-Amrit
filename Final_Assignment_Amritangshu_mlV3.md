## Question 1 - Probability practice

## Answer Part A -

p(users_yes)=0.65

p(users_no)=0.35

p(random_clicker)=0.3

p(fraction of random clickers yes)=0.5

p(truthfull_clicker)=0.7

p(fraction of truth clickers yes)=0.5

##### Fraction of people who are truthful clickers answered yes=\>

p(users_yes)=p(truthfull_clicker) \* p(fraction of people who are
truthful clickers answered yes) + p(random_clicker) \* p(fraction of
random clickers yes)

0.65=0.7\*x+0.3\*0.5

x=0.714

**Answer :** So the fraction of fraction of people who are truthful
clickers answered yes are 0.714

## Answer Part B -

p(positive_test/disease)=0.993

p(test negative/no disease) = 0.9999

p(disease)=0.000025

p(disease/test_positive)=p(disease)\*p(test_positive/disease)/p(test_positive)

p(disease/test_positive)=p(disease)*p(test_positive/disease)/(*p(disease)*p(test_positive/disease)*+*p(no_disease)*\*p(test_positive/no
disease))

p(disease/test_positive)=0.000025\*0.993/(0.000025\*0.993+(1-0.000025)(1-0.9999))

p(disease/test_positive)=0.1989

**Answer :** Suppose someone tests positive probability that they have
the disease 0.1989

## Question 2 - **Wrangling the Billboard Top 100**

## Answer Part A -

``` r
library(dplyr)
library(tidyverse)
data1 <- read.csv("billboard.csv")
data2<-data1[,c("performer","song","year","week","week_position")]

data3<-data2%>%group_by(performer,song)%>%summarize(count = n())%>%arrange(desc(count)) 
print('THe sorted dataframe is below')
```

    ## [1] "THe sorted dataframe is below"

``` r
head(data3,n=10)
```

    ## # A tibble: 10 × 3
    ## # Groups:   performer [10]
    ##    performer                                 song                          count
    ##    <chr>                                     <chr>                         <int>
    ##  1 Imagine Dragons                           Radioactive                      87
    ##  2 AWOLNATION                                Sail                             79
    ##  3 Jason Mraz                                I'm Yours                        76
    ##  4 The Weeknd                                Blinding Lights                  76
    ##  5 LeAnn Rimes                               How Do I Live                    69
    ##  6 LMFAO Featuring Lauren Bennett & GoonRock Party Rock Anthem                68
    ##  7 OneRepublic                               Counting Stars                   68
    ##  8 Adele                                     Rolling In The Deep              65
    ##  9 Jewel                                     Foolish Games/You Were Meant…    65
    ## 10 Carrie Underwood                          Before He Cheats                 64

## Answer Part B -

``` r
data4<-unique(data2[,c("year","song")])
data5<-data4%>%group_by(year)%>%summarize(count = n())

data5<-data5 %>% filter(year !=1958 & year !=2021)
print('Plotting the data required')
```

    ## [1] "Plotting the data required"

``` r
plot(data5)
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%202(b)-1.png)

## Answer Part C -

``` r
data6<-unique(data2[,c("performer","song","week")])
data7<-data6%>%group_by(performer,song)%>%summarize(count = n())

data8<-data7%>% filter(count >=10)
data9<-unique(data8[,c("performer","song")])
data10<-data9%>%group_by(performer)%>%summarize(count = n())
data11<-data10%>% filter(count >30)


ggplot(data11) + 
  geom_col(aes(x=performer, y=count)) +coord_flip()
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%202(c)-1.png)

``` r
print('Elton John seems to have the most 10 week hits in the billboard top 100.')
```

    ## [1] "Elton John seems to have the most 10 week hits in the billboard top 100."

## Question 3 - **Visual story telling part 1: green buildings**

## Answer 3-

``` r
library(corrplot)
library(mosaic)
library(tidyverse)

data_load <- read.csv('greenbuildings.csv')

print('Lets look at the distribution of these features across various metrics')
```

    ## [1] "Lets look at the distribution of these features across various metrics"

``` r
ggplot(data=data_load) + 
  geom_point(mapping=aes(x=cluster_rent, y=Rent))
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%203(a)-1.png)

``` r
ggplot(data=data_load) + 
  geom_point(mapping=aes(x=age, y=Rent))
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%203(a)-2.png)

``` r
ggplot(data=data_load) + 
  geom_point(mapping=aes(x=size, y=Rent))
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%203(a)-3.png)

``` r
ggplot(data=data_load) + 
  geom_point(mapping=aes(x=age, y=Rent))
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%203(a)-4.png)

``` r
print('we observed that Rent is correlated with the cluster rent, size, class A. Additionally, Class a buildings get higher rent')
```

    ## [1] "we observed that Rent is correlated with the cluster rent, size, class A. Additionally, Class a buildings get higher rent"

``` r
ggplot(data_load, aes(class_a, ..count..)) + geom_bar(aes(fill = green_rating))
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%203(a)-5.png)

``` r
g = ggplot(data_load, aes(x=age))
g + geom_density(aes(fill=factor(green_rating)))
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%203(a)-6.png)

``` r
g = ggplot(data_load, aes(x=size))
g + geom_density(aes(fill=factor(green_rating)))
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%203(a)-7.png)

``` r
g = ggplot(data_load, aes(x=cluster_rent))
g + geom_density(aes(fill=factor(green_rating)))
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%203(a)-8.png)

``` r
g = ggplot(data_load, aes(x=Rent))
g + geom_density(aes(fill=factor(green_rating)))
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%203(a)-9.png)

we observed most of the green buildings are younger than non-green
buildings and the proportion of class a buildings is higher in green
buildings

Since Stats Guru fails to account for all factors that affect rent, his
analysis is wrong In order to calculate the returns, he began by using
the median rent for all buildings.Because of this, he fails to factor in
other factors, such as the size and class of the buildings,into his
analysis. For instance, we have A class a building will yield a higher
rent than a non-green building.

## Question 4 - **Visual story telling part 2: Capital Metro data**

## Answer 4 -

## Question 5 - **Portfolio modeling**

## Answer 5 -

### Portfolio 1

``` r
library(mosaic)
library(quantmod)
library(foreach)
#----------------------Portfolio 1----------------------------------------------
mystocks = c("WMT", "TGT", "XOM", "MRK", "JNJ","MRK", "IDV", "SPY", "VTI", "DIA")
myprices = getSymbols(mystocks, from = "2015-01-01")

for(ticker in mystocks) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}

# Combine all the returns in a matrix
all_returns = cbind(    ClCl(WMTa),
                     ClCl(TGTa),
                     ClCl(XOMa),
                     ClCl(MRKa),
                     ClCl(JNJa),ClCl(MRKa),ClCl(IDVa),ClCl(SPYa),
                     ClCl(VTIa),ClCl(DIAa))
#head(all_returns)
all_returns = as.matrix(na.omit(all_returns))

# Compute the returns from the closing prices
#pairs(all_returns)

# Now loop over 4 trading weeks

## begin block
total_wealth = 100000
weights = c(0.1,0.1,0.1, 0.1, 0.1,0.1,0.1,0.1, 0.1, 0.1)
holdings = weights * total_wealth
n_days = 20  # capital T in the notes
wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
for(today in 1:n_days) {
  return.today = resample(all_returns, 1, orig.ids=FALSE)  # sampling from R matrix in notes
  holdings = holdings + holdings*return.today
  total_wealth = sum(holdings)
  wealthtracker[today] = total_wealth
}
total_wealth
```

    ## [1] 101841.4

``` r
plot(wealthtracker, type='l')
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%205(a)-1.png)

``` r
## end block


# Now simulate many different possible futures
# just repeating the above block thousands of times
initial_wealth = 100000
sim1 = foreach(i=1:20, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.1,0.1,0.1, 0.1, 0.1,0.1,0.1,0.1, 0.1, 0.1)
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

# each row is a simulated trajectory
# each column is a data
#head(sim1)
hist(sim1[,n_days], 25)
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%205(a)-2.png)

``` r
# Profit/loss
mean(sim1[,n_days])
```

    ## [1] 100927.5

``` r
mean(sim1[,n_days] - initial_wealth)
```

    ## [1] 927.4711

``` r
hist(sim1[,n_days]- initial_wealth, breaks=30)
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%205(a)-3.png)

``` r
# 5% value at risk:
quantile(sim1[,n_days]- initial_wealth, prob=0.05)
```

    ##        5% 
    ## -6343.782

This first portfolio performs well and this time with a -ve VAR value
but a lower profit margin overall compared to our next portfolio. This
is potentially due to the high number of stocks which can cause our
profits to average out over the stocks that we have.

Let’s test out this theory by using a lower total number of stocks in
our portfolio and try to measure the profit and VAR value.

### Portfolio 2

``` r
#----------------------Portfolio 2----------------------------------------------
mystocks = c("MRK", "IDV", "SPY", "VTI", "DIA")
myprices = getSymbols(mystocks, from = "2015-01-01")

for(ticker in mystocks) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}

# Combine all the returns in a matrix
all_returns = cbind(    ClCl(MRKa),
                     ClCl(JNJa),ClCl(MRKa),ClCl(IDVa),ClCl(SPYa),
                     ClCl(VTIa),ClCl(DIAa))
#head(all_returns)
all_returns = as.matrix(na.omit(all_returns))

# Compute the returns from the closing prices
#pairs(all_returns)

# Now loop over 4 trading weeks

## begin block
total_wealth = 100000
weights = c(0.2,0.2,0.2, 0.2, 0.2)
holdings = weights * total_wealth
n_days = 20  # capital T in the notes
wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
for(today in 1:n_days) {
  return.today = resample(all_returns, 1, orig.ids=FALSE)  # sampling from R matrix in notes
  holdings = holdings + holdings*return.today
  total_wealth = sum(holdings)
  wealthtracker[today] = total_wealth
}
total_wealth
```

    ## [1] 136926.6

``` r
plot(wealthtracker, type='l')
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%205(b)-1.png)

``` r
## end block


# Now simulate many different possible futures
# just repeating the above block thousands of times
initial_wealth = 100000
sim1 = foreach(i=1:20, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.2,0.2,0.2, 0.2, 0.2)
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

# each row is a simulated trajectory
# each column is a data
#head(sim1)
hist(sim1[,n_days], 25)
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%205(b)-2.png)

``` r
# Profit/loss
mean(sim1[,n_days])
```

    ## [1] 139392.1

``` r
mean(sim1[,n_days] - initial_wealth)
```

    ## [1] 39392.1

``` r
hist(sim1[,n_days]- initial_wealth, breaks=30)
```

![](Final_Assignment_Amritangshu_mlV3_files/figure-markdown_github/Answer%205(b)-3.png)

``` r
# 5% value at risk:
quantile(sim1[,n_days]- initial_wealth, prob=0.05)
```

    ##       5% 
    ## 31225.07

This second portfolio performs better than portfolio 1 but this time
with a +ve VAR value but a higher profit margin overall. This means that
it was helpful to have a lower number of total stocks in the portfolio
and as a result assign higher weights to individual stocks in the
portfolio.
