getwd()
setwd('F:/PGP Data Science - Purdue University/PG DS - Data Science with R - Course 3/Project/Assessment Project -1/')
library(csv)

my_walmart = read.csv('Walmart_Store_sales.csv')

View(my_walmart)
class(my_walmart)
str(my_walmart)

### changed date format in data frame since it was in character format before ###

my_walmart$Date = as.Date(my_walmart$Date,format = "%d-%m-%Y") 

rm(my_walmart)

#### TASK 1 - Which store has maximum sales #### 

library(dplyr)
library(plyr)

max_sales = aggregate(Weekly_Sales~Store,my_walmart,sum)
class(max_sales)
View(max_sales)
max_sales <- max_sales[order(max_sales$Weekly_Sales,decreasing = T),] ## ordering in decreasing order 
View(max_sales)

#Answer is Store number 20, Sales:  301397792


## END OF ANSWER 1 ##

## -------------------------------------------------------------------------------##

#### TASK 2 - Which store has maximum standard deviation i.e., the sales vary a lot. 
####     Also, find out the coefficient of mean to standard deviation

Stores_with_mean = aggregate(Weekly_Sales~Store,my_walmart,mean) #mean
Stores_with_mean

Stores_with_std= aggregate(Weekly_Sales~Store,my_walmart,sd) #standard deviation
Stores_with_std

Stores_with_std <- Stores_with_std[order(Stores_with_std$Weekly_Sales,decreasing = T),]
View(Stores_with_std ) ## ANSWER  - STORE 14 has maximum maximum standard deviation 317569.95

Stores_with_std_mean = cbind(Stores_with_mean,Stores_with_std)

str(Stores_with_std_mean)

View(Stores_with_std_mean)

colnames(Stores_with_std_mean)[2] = "Mean_value" # renaming columns 
colnames(Stores_with_std_mean)[4] = "Std_value"  # renaming columns 
View(Stores_with_std_mean)

Stores_with_std_mean = Stores_with_std_mean[,-3] 
View(Stores_with_std_mean)

Stores_with_std_mean['Coeff']= Stores_with_std_mean$Std_value/Stores_with_std_mean$Mean_value ## ANSWER coefficient of mean to standard deviation

View(Stores_with_std_mean)

## END OF ANSWER 2##

#--------------------------------------------------------------------------#

####  TASK 3- Which store/s has good quarterly growth rate in Q3'2012 ##### 

## taken FINANACIAL YEAR FROM JAN - DEC

Stores_Quarter_Flag = transform(my_walmart,Q_Flag= ifelse((Date>='2012-04-01' & Date<= '2012-06-30'),"Q2_2012",
                                                            ifelse((Date>='2012-07-01' & Date<= '2012-09-30'),"Q3_2012","-")))
View(Stores_Quarter_Flag)

# summarizing 
Stores_with_Q_Flag_sum = aggregate(Weekly_Sales~Store+Q_Flag,Stores_Quarter_Flag,sum)
View(Stores_with_Q_Flag_sum)
str(Stores_with_Q_Flag_sum)

##then transposing using reshaping

Stores_with_Q_Flag_sum_t = reshape(Stores_with_Q_Flag_sum,idvar="Store",timevar ='Q_Flag',direction="wide")

View(Stores_with_Q_Flag_sum_t)
class(Stores_with_Q_Flag_sum_t)


Stores_with_Q_Flag_sum_t_GR = transform(Stores_with_Q_Flag_sum_t,
                                        GR=((Weekly_Sales.Q3_2012-Weekly_Sales.Q2_2012)/Weekly_Sales.Q2_2012*100))
View(Stores_with_Q_Flag_sum_t_GR)

Stores_with_Q_Flag_sum_t_GR %>% arrange(desc(Stores_with_Q_Flag_sum_t_GR$GR)) ## STORE 7 has god growth rate of 13.33%

##END OF ANSWER 3 ##

#--------------------------------------------------------------#

## TASK 4 -Some holidays have a negative impact on sales. 
##Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together

View(my_walmart)

Non_Holiday_sales = my_walmart%>%group_by(Weekly_Sales)%>%filter(Holiday_Flag==0)# filtering non holidays weekly_sales

View(Non_Holiday_sales)

avg_non_holiday_sales = mean(Non_Holiday_sales$Weekly_Sales) #mean of weekly sales of non holidays sales
avg_non_holiday_sales


## holiday name added and holidays which have higher sales than the mean sales in non-holiday season

high_sales = filter(my_walmart,Weekly_Sales>avg_non_holiday_sales & Holiday_Flag==1) 

high_sales = transform(high_sales,Holiday_name = ifelse((Date=='2010-02-12' | Date== '2011-02-11' | Date== '2012-02-10' | Date=='2013-02-8'),"Super Bowl"
                                                        ,ifelse((Date=='2010-09-10' | Date== '2011-09-09'| Date== '2012-09-7'| Date=='2013-09-6'),"Labour Day"
                                                                ,ifelse((Date=='2010-11-26' | Date== '2011-11-25'| Date== '2012-11-23'| Date=='2013-11-29'),"Thanksgiving"
                                                                        ,ifelse((Date=='2010-12-31' | Date== '2011-12-30'| Date== '2012-12-28'| Date=='2013-12-27'),"Christmas","-" 
                                                                        )))))
View(high_sales) ## ANSWER

## END OF ASNSWER 4

#--------------------------------------------------------------------------#

## TASK 5 - Provide a monthly and semester view of sales in units and give insights
my_walmart = read.csv('Walmart_Store_sales.csv')
my_walmart$Date = as.Date(my_walmart$Date,format = "%d-%m-%Y") 
View(my_walmart)
install.packages('lubridate')
library(lubridate)

my_walmart = transform(my_walmart, Months = months(as.Date(Date,format="%B"))) ## Months column extracted and added 
View(my_walmart)

my_walmart = transform(my_walmart, Year = year(as.Date(Date,format="%Y"))) ## Year column extracted and added 
View(my_walmart)
                                               
                                                    
#mean of monthly Sales
install.packages('dplyr')
library(dplyr)
library(plyr)

Monthly_Sales = my_walmart%>%group_by(Months)%>%summarise(sum_Monthly_Sales=sum(Weekly_Sales))
str(Monthly_Sales)
View(Monthly_Sales) ## Overall July month has most sales and January has lowest sales

Monthly_Sales %>% arrange(desc(Monthly_Sales$Months))

Monthly_Sales = Monthly_Sales[order(Monthly_Sales$Months),]
View(Monthly_Sales)
Monthly_mean_Sales = my_walmart%>%group_by(Months)%>%summarise(Mean_Monthly_Sales=mean(Weekly_Sales))
View(Monthly_mean_Sales) ## DEC month as highest average sales, January Month has lowest average sales

Yearly_Sales = my_walmart%>%group_by(Year)%>%summarise(sum_year_Sales=sum(Weekly_Sales))
View(Yearly_Sales) ## Overall year 2011 has highest sales and year 2012 has lowest sales

#Yearly_mean_Sales = my_walmart%>%group_by(Year)%>%summarize(Mean_Year_Sales=mean(Weekly_Sales))
#View(Yearly_mean_Sales) ## year 2010 has highest average sales, year 2012 has lowest average sales


## END OF ANSWER 5 ##

#---------------------------------------------------------------------------------------------#

## TASK 6 
#For Store 1 - Build  prediction models to forecast demand
#Linear Regression - Utilize variables like date and restructure dates as 1 for 5 Feb 2010 
#(starting from the earliest date in order). Hypothesize if CPI, unemployment, and fuel price have any impact on sales.

library(lubridate)
my_walmart = read.csv('Walmart_Store_sales.csv')

View(my_walmart)
# Extracting Store 1 data from my_walmart
Store_one_data = my_walmart %>% filter(Store == 1)
View(Store_one_data)

days = c(1:143) ## creating vector for dates

Store_one_data = cbind(Store_one_data,day_id = days) ##  restructure dates as 1 for 5 Feb 2010

View(Store_one_data)
str(Store_one_data)
summary(Store_one_data)

## Linear model

LM_Store_one = lm(Weekly_Sales~Fuel_Price+CPI+Unemployment ,Store_one_data)
summary(LM_Store_one)

# Fuel price is insignificant variable here and can be dropped
LM_Store_one = lm(Weekly_Sales~ CPI+Unemployment ,Store_one_data)
summary(LM_Store_one)


# Unemployment is also insignificant variable here and can be dropped
LM_Store_one = lm(Weekly_Sales~ CPI,Store_one_data)
summary(LM_Store_one)   
## intercept : -190192 , R square : 0.05, Adjusted R square : 0.044, p value : 0.006 < 0.05

#Consumer Price Index has impact on Weekly Sales

## Prediction Sales

predicted_Store_sales = predict(LM_Store_one,Store_one_data)
View(LM_Store_sales)
str(LM_Store_sales)

LM_Store_one_final = cbind(Store_one_data,as.data.frame(predicted_Store_sales))
View(LM_Store_one_final)
str(LM_Store_one_final)

### Actual vs predicted value and Error rate
LM_Store_one_final$actvsprd = LM_Store_one_final$Weekly_Sales - LM_Store_one_final$predicted_Store_sales
LM_Store_one_final$Err = abs(LM_Store_one_final$Weekly_Sales - LM_Store_one_final$predicted_Store_sales)/LM_Store_one_final$Weekly_Sales
View(LM_Store_one_final)


### Accuracy 93.73%

mean(LM_Store_one_final$Err)
Accuracy = 1-mean(LM_Store_one_final$Err)
Accuracy

#Consumer Price Index has impact on Weekly Sales

## END OF TASK 6 
