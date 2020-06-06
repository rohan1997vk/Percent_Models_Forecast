start.time <- Sys.time()
library(DataCombine)
library(dplyr)
library(lubridate)
library(tidyverse)
library(forecast)
library(zoo)
library(xgboost)
library(Matrix)

setwd("D:/7_Neenopal/02_Singer/new_data")
train_data <- read.csv("python_data.csv")  #Data from 2016 to 2020-04
dataset <- train_data
Percent_table_All <- read.csv("Percentage_All.csv") # All_Percentages
Percent_table <- read.csv("Percentage_Singer.csv") # Singer_Percentages
dataset$SALE_TRANS_DATE <- as.Date(dataset$SALE_TRANS_DATE)
dataset <- dataset %>% filter(SALE_NET_VAL != 0)
dataset <- dataset %>% select("SALE_TRANS_DATE","SALE_TOT_QTY","CHANNEL","CATEGORY")
dataset <- dataset %>% filter (CATEGORY != "GRINDERS & BLENDERS")
dataset <- dataset %>% filter (CATEGORY != "KITCHEN APPLIANCES")
dataset <- dataset %>% select("SALE_TRANS_DATE","SALE_TOT_QTY","CATEGORY")
names(dataset) <- c("SALE_TRANS_DATE","SALE_TOT_QTY","Category_old")
r_cat <- sort(unique(dataset$Category_old))   # All categories sorted order
ncat <- as.numeric(length(r_cat))  # Number of categories 
#Forecasting_dates
date_new <- data.frame(dates = seq(as.Date('2020-05-01'),as.Date('2020-10-01'),by = "1 month"),Sales = 0)
predict_dates <- tidyr::expand_grid(date_new,sort(r_cat))
names(predict_dates) <- c("SALE_TRANS_DATE","SALE_TOT_QTY","Category_old")
dataset1 <- rbind(dataset,predict_dates)


#Percent_Models
#3-months
current_monthno = 53
data_3_month <<- data.frame()
model_3_month <- function(data, category,Percent,current_monthno)
{
  category = as.character(category)
  data_preprocessed <- filter(dataset1, Category_old == category) %>% 
    group_by(date = floor_date(SALE_TRANS_DATE,"month")) %>%
    summarize(SALES = sum(SALE_TOT_QTY)) %>%
    separate(date, sep="-", into = c("year", "month")) %>%
    mutate(lag_12 = shift(SALES,-12),
           lag_24 = shift(SALES,-24)) %>% 
    rowwise() %>% 
    mutate(min_lag = min(lag_12,lag_24),
           max_lag = max(lag_12,lag_24)) %>%
    mutate (normalize = ifelse(SALES>max_lag*1.25,max_lag*1.25,
                               ifelse(SALES<min_lag*0.75,min_lag*0.75 ,SALES))) %>%
    mutate (month_no = (as.numeric(month) + (as.numeric(year) -2016)*12)) %>%
    mutate (normalize = ifelse(is.na(normalize),SALES,normalize))
  AC_p <<- data.frame(month = 1:12,Percent = Percent) 
  AC_p$month <<- as.numeric(AC_p$month)
  last_n_months <<- subset(data_preprocessed,month_no >= (current_monthno - 3))
  last_n_months <<- subset(last_n_months,month_no < current_monthno )
  last_n_months$month <<- as.numeric(last_n_months$month)
  last_n_season  <<- left_join(last_n_months,AC_p, by = "month")
  last_n_season[,11] <<- last_n_season[,8]/last_n_season[,10]
  avg_n_season <<- last_n_season %>%
    mutate(Category = "Category") %>%
    group_by(Category) %>% summarize(avg_pred_year_total= mean(last_n_season$normalize.1))
  predict_df <<- subset(data_preprocessed,month_no>=current_monthno)
  predict_df <<- predict_df[1:6,]
  predict_df$month <<- as.numeric(predict_df$month)
  predict_df <<- left_join(predict_df,AC_p,by = "month")
  predict_df$Forecasted <<- predict_df[,10] * avg_n_season$avg_pred_year_total
  print(predict_df)
  final1 <- data.frame(model = 3,Category = r_cat[i],Year = predict_df$year,Month = predict_df$month, Sales = predict_df$SALES,Lag_12 = predict_df$lag_12,Lag_24 = predict_df$lag_24,Min_lag = predict_df$min_lag,Max_lag = predict_df$max_lag,Normalize = predict_df$normalize,month_no = predict_df$month_no,Percent = predict_df[,10],Forecasated = predict_df$Forecasted)
  colnames(final1)  <- c("model","cat","year","month","sales","lag_12","lag_24","min_lag","max_lag","norm","mon_no","percent","forecasted")
  data_3_month <<- rbind(data_3_month,final1)
}
for( i in 1:ncat )
{
  model_3_month(dataset1, r_cat[i],Percent_table[i], current_monthno)
  print(i)
}



#4-months
data_4_month <<- data.frame()
model_4_month <- function(data, category,Percent,current_monthno)
{
  category = as.character(category)
  data_preprocessed <<- filter(dataset1, Category_old == category) %>% 
    group_by(date = floor_date(SALE_TRANS_DATE,"month")) %>%
    summarize(SALES = sum(SALE_TOT_QTY)) %>%
    separate(date, sep="-", into = c("year", "month")) %>%
    mutate(lag_12 = shift(SALES,-12),
           lag_24 = shift(SALES,-24)) %>% 
    rowwise() %>% 
    mutate(min_lag = min(lag_12,lag_24),
           max_lag = max(lag_12,lag_24)) %>%
    mutate (normalize = ifelse(SALES>max_lag*1.25,max_lag*1.25,
                               ifelse(SALES<min_lag*0.75,min_lag*0.75 ,SALES))) %>%
    mutate (month_no = (as.numeric(month) + (as.numeric(year) -2016)*12)) %>%
    mutate (normalize = ifelse(is.na(normalize),SALES,normalize))
  AC_p <<- data.frame(month = 1:12,Percent = Percent) 
  AC_p$month <<- as.numeric(AC_p$month)
  last_n_months <<- subset(data_preprocessed,month_no >= (current_monthno - 4))
  last_n_months <<- subset(last_n_months,month_no < current_monthno )
  last_n_months$month <<- as.numeric(last_n_months$month)
  last_n_season  <<- left_join(last_n_months,AC_p, by = "month")
  last_n_season[,11] <<- last_n_season[,8]/last_n_season[,10]
  avg_n_season <<- last_n_season %>%
    mutate(Category = "Category") %>%
    group_by(Category) %>% summarize(avg_pred_year_total= mean(last_n_season$normalize.1))
  predict_df <<- subset(data_preprocessed,month_no>=current_monthno)
  predict_df <<- predict_df[1:6,]
  predict_df$month <<- as.numeric(predict_df$month)
  predict_df <<- left_join(predict_df,AC_p,by = "month")
  predict_df$Forecasted <<- predict_df[,10] * avg_n_season$avg_pred_year_total
  print(predict_df)
  final1 <- data.frame(model = 4,Category = r_cat[i],Year = predict_df$year,Month = predict_df$month, Sales = predict_df$SALES,Lag_12 = predict_df$lag_12,Lag_24 = predict_df$lag_24,Min_lag = predict_df$min_lag,Max_lag = predict_df$max_lag,Normalize = predict_df$normalize,month_no = predict_df$month_no,Percent = predict_df[,10],Forecasated = predict_df$Forecasted)
  colnames(final1)  <- c("model","cat","year","month","sales","lag_12","lag_24","min_lag","max_lag","norm","mon_no","percent","forecasted")
  data_4_month <<- rbind(data_4_month,final1)
  
  
}
for( i in 1:ncat )
{
  model_4_month(dataset, r_cat[i],Percent_table[i], current_monthno)
  print(i)
}
#5-months
data_5_month <<- data.frame()
model_5_month <- function(data, category,Percent,current_monthno)
{
  category = as.character(category)
  data_preprocessed <<- filter(dataset1, Category_old == category) %>% 
    group_by(date = floor_date(SALE_TRANS_DATE,"month")) %>%
    summarize(SALES = sum(SALE_TOT_QTY)) %>%
    separate(date, sep="-", into = c("year", "month")) %>%
    mutate(lag_12 = shift(SALES,-12),
           lag_24 = shift(SALES,-24)) %>% 
    rowwise() %>% 
    mutate(min_lag = min(lag_12,lag_24),
           max_lag = max(lag_12,lag_24)) %>%
    mutate (normalize = ifelse(SALES>max_lag*1.25,max_lag*1.25,
                               ifelse(SALES<min_lag*0.75,min_lag*0.75 ,SALES))) %>%
    mutate (month_no = (as.numeric(month) + (as.numeric(year) -2016)*12)) %>%
    mutate (normalize = ifelse(is.na(normalize),SALES,normalize))
  AC_p <<- data.frame(month = 1:12,Percent = Percent) 
  AC_p$month <<- as.numeric(AC_p$month)
  last_n_months <<- subset(data_preprocessed,month_no >= (current_monthno - 5))
  last_n_months <<- subset(last_n_months,month_no < current_monthno )
  last_n_months$month <<- as.numeric(last_n_months$month)
  last_n_season  <<- left_join(last_n_months,AC_p, by = "month")
  last_n_season[,11] <<- last_n_season[,8]/last_n_season[,10]
  avg_n_season <<- last_n_season %>%
    mutate(Category = "Category") %>%
    group_by(Category) %>% summarize(avg_pred_year_total= mean(last_n_season$normalize.1))
  predict_df <<- subset(data_preprocessed,month_no>=current_monthno)
  predict_df <<- predict_df[1:6,]
  predict_df$month <<- as.numeric(predict_df$month)
  predict_df <<- left_join(predict_df,AC_p,by = "month")
  predict_df$Forecasted <<- predict_df[,10] * avg_n_season$avg_pred_year_total
  print(predict_df)
  final1 <- data.frame(model = 5,Category = r_cat[i],Year = predict_df$year,Month = predict_df$month, Sales = predict_df$SALES,Lag_12 = predict_df$lag_12,Lag_24 = predict_df$lag_24,Min_lag = predict_df$min_lag,Max_lag = predict_df$max_lag,Normalize = predict_df$normalize,month_no = predict_df$month_no,Percent = predict_df[,10],Forecasated = predict_df$Forecasted)
  colnames(final1)  <- c("model","cat","year","month","sales","lag_12","lag_24","min_lag","max_lag","norm","mon_no","percent","forecasted")
  data_5_month <<- rbind(data_5_month,final1)
}
for( i in 1:ncat )
{
  model_5_month(dataset1, r_cat[i],Percent_table[i], current_monthno)
  print(i)
}

#6-months
data_6_month <<- data.frame()
model_6_month <- function(data, category,Percent,current_monthno)
{
  category = as.character(category)
  data_preprocessed <<- filter(dataset1, Category_old == category) %>% 
    group_by(date = floor_date(SALE_TRANS_DATE,"month")) %>%
    summarize(SALES = sum(SALE_TOT_QTY)) %>%
    separate(date, sep="-", into = c("year", "month")) %>%
    mutate(lag_12 = shift(SALES,-12),
           lag_24 = shift(SALES,-24)) %>% 
    rowwise() %>% 
    mutate(min_lag = min(lag_12,lag_24),
           max_lag = max(lag_12,lag_24)) %>%
    mutate (normalize = ifelse(SALES>max_lag*1.25,max_lag*1.25,
                               ifelse(SALES<min_lag*0.75,min_lag*0.75 ,SALES))) %>%
    mutate (month_no = (as.numeric(month) + (as.numeric(year) -2016)*12)) %>%
    mutate (normalize = ifelse(is.na(normalize),SALES,normalize))
  AC_p <<- data.frame(month = 1:12,Percent = Percent) 
  AC_p$month <<- as.numeric(AC_p$month)
  last_n_months <<- subset(data_preprocessed,month_no >= (current_monthno - 6))
  last_n_months <<- subset(last_n_months,month_no < current_monthno )
  last_n_months$month <<- as.numeric(last_n_months$month)
  last_n_season  <<- left_join(last_n_months,AC_p, by = "month")
  last_n_season[,11] <<- last_n_season[,8]/last_n_season[,10]
  avg_n_season <<- last_n_season %>%
    mutate(Category = "Category") %>%
    group_by(Category) %>% summarize(avg_pred_year_total= mean(last_n_season$normalize.1))
  predict_df <<- subset(data_preprocessed,month_no>=current_monthno)
  predict_df <<- predict_df[1:6,]
  predict_df$month <<- as.numeric(predict_df$month)
  predict_df <<- left_join(predict_df,AC_p,by = "month")
  predict_df$Forecasted <<- predict_df[,10] * avg_n_season$avg_pred_year_total
  print(predict_df)
  final1 <- data.frame(model = 6,Category = r_cat[i],Year = predict_df$year,Month = predict_df$month, Sales = predict_df$SALES,Lag_12 = predict_df$lag_12,Lag_24 = predict_df$lag_24,Min_lag = predict_df$min_lag,Max_lag = predict_df$max_lag,Normalize = predict_df$normalize,month_no = predict_df$month_no,Percent = predict_df[,10],Forecasated = predict_df$Forecasted)
  colnames(final1)  <- c("model","cat","year","month","sales","lag_12","lag_24","min_lag","max_lag","norm","mon_no","percent","forecasted")
  data_6_month <<- rbind(data_6_month,final1)
}
for( i in 1:ncat )
{
  model_6_month(dataset1, r_cat[i],Percent_table[i], current_monthno)
  print(i)
}

Percent_Forecasting <- data.frame(category = data_3_month$cat,
                                  Year = data_3_month$year,
                                  Month = data_3_month$month,
                                  Day = 1,
                                  Sales=data_3_month$sales,
                                  Month_no = data_3_month$mon_no,
                                  Month_3 = data_3_month$forecasted,
                                  Month_4 = data_4_month$forecasted,
                                  Month_5 = data_5_month$forecasted,
                                  Month_6 = data_6_month$forecasted)

Percent_Forecasting$Date <- as.yearmon(paste(Percent_Forecasting$Year, Percent_Forecasting$Month,Percent_Forecasting$Day), "%Y %m %d")
Percent_Forecasting$Date <- as.Date(Percent_Forecasting$Date,format='%Y %m %d')

Final_Forecasted_Perent_Models <- Percent_Forecasting %>% select("category","Date","Sales","Month_3","Month_4","Month_5","Month_6")
View(Final_Forecasted_Perent_Models)

#Arima_Forecasted
dates <<- date_new$dates
arima_forecasted <<- data.frame()
arima_function <- function(dataset,category)
{  
  category = as.character(category)
  
  
  dataset_preprocessed <- filter(dataset, Category_old == category) %>% 
    group_by(Date = floor_date(SALE_TRANS_DATE,"month")) %>%
    summarize(Sales = sum(SALE_TOT_QTY))
  dataset1 <- dataset_preprocessed
  dataset1 <- ts(dataset1$Sales, frequency = 12, start = c(2016,7))
  x <- auto.arima(dataset1,p=0, d=1, max.d =2, max.p=2, max.q= 2, max.P=2, max.Q=2, max.D=2, start.p =0, start.q=0, start.P=0, start.Q=0, stepwise=TRUE, trace=TRUE)
  fit_m_final <- arima(dataset1,order=x$arma[c(1, 7, 2)],
                       seasonal=list(order=x$arma[c(3, 7, 4)],
                                     period=x$arma[5]))
  fit_m_final_forcast <<- predict(fit_m_final, n.ahead = 6, prediction.interval = TRUE)
  arima_forecast <- data.frame(category,dates,ARIMA = as.numeric(abs(fit_m_final_forcast$pred)))
  arima_forecasted <<- rbind(arima_forecasted,arima_forecast)
}
for(i in 1:ncat)
{
  arima_function(dataset,r_cat[i])
  print(i)
}

Holt_Winter_Forecasted <<- data.frame()
Holt_Winter_Function <- function(dataset,category) {
  category = as.character(category)
  dataset_preprocessed <- filter(dataset, Category_old == category) %>% 
    group_by(Date = floor_date(SALE_TRANS_DATE,"month")) %>%
    summarize(Sales = sum(SALE_TOT_QTY))
  sales <- dataset_preprocessed
  salesTS <- ts(sales$Sales, frequency = 12, start = c(2016,7))
  class(salesTS)
  salesLog <- log(salesTS)
  salesLogHW <- HoltWinters(salesLog)
  nextYearSales <- as.data.frame(forecast(salesLogHW, h=6))
  new_data <- data.frame(category,dates,as.numeric(exp(nextYearSales$`Point Forecast` )))
  colnames(new_data) <- c("cat","date","Holt_Winter")
  Holt_Winter_Forecasted <<- rbind(Holt_Winter_Forecasted,new_data)
}
for(i in 1:ncat)
{
  Holt_Winter_Function(dataset,r_cat[i])
  print(i)
}

arima_Holt <- data.frame(cat = arima_forecasted$category, Date = arima_forecasted$dates,ARIMA = arima_forecasted$ARIMA,Holt_Winter = Holt_Winter_Forecasted$Holt_Winter)
Final_AR_HW_Percent <- data.frame(Category =  arima_Holt$cat,
                                  Date = arima_Holt$Date,
                                  Sales = Final_Forecasted_Perent_Models$Sales,
                                  ARIMA = arima_Holt$ARIMA,
                                  Holt_Winter = arima_Holt$Holt_Winter,
                                  Month_3 = Final_Forecasted_Perent_Models$Month_3,
                                  Month_4 = Final_Forecasted_Perent_Models$Month_4,
                                  Month_5 = Final_Forecasted_Perent_Models$Month_5,
                                  Month_6 = Final_Forecasted_Perent_Models$Month_6)


History_2016 <- read.csv("D:/7_Neenopal/02_Singer/new_data/Aug/All1.csv")
History_2016$Date <- as.Date(History_2016$Date)

#XGBoost
train1 <- History_2016
names(train1) <- c("Category","Date","Sales","Holt.Winter","ARIMA","X3.month","X4.month","X5.month","X6.month")
test <- Final_AR_HW_Percent
names(test) <- c("Category","Date","Sales","Holt.Winter","ARIMA","X3.month","X4.month","X5.month","X6.month")
r_cat <- sort(unique(train1$Category))
n_cat <- as.numeric(length(r_cat))
xgboost_data <<- data.frame()
xgboost_function <- function(train1,test,category)  {
  category = as.character(category)
  data_fil1 <- filter(train1,Category == category)
  data_fil2 <- filter(test,Category == category)
  data_fil_train <<- data_fil1
  data_fil_test <<- data_fil2
  trainMatrix <<- sparse.model.matrix(~ Holt.Winter + ARIMA + X3.month + X4.month + X5.month + X6.month
                                      , data = data_fil_train
                                      , sparse = FALSE, sci = FALSE)
  
  testMatrix <- sparse.model.matrix(~ Holt.Winter + ARIMA  + X3.month+ X4.month + X5.month + X6.month
                                    , data = data_fil_test
                                    , sparse = FALSE, sci = FALSE)
  
  params <- list(booster = "gbtree"
                 , objective = "reg:linear"
                 , eta=0.4
                 , gamma=0
  )
  label <- data_fil_train$Sales
  label1 <- data_fil_test$Sales
  
  trainDMatrix <- xgb.DMatrix(data = trainMatrix, label = label)
  testDMatrix <- xgb.DMatrix(data= testMatrix,label = label1)
  xgb.tab <- xgb.cv(data = trainDMatrix
                    , param = params
                    , maximize = FALSE, evaluation = "rmse", nrounds = 100
                    , nthreads = 10, nfold = 2, early_stopping_round = 10)
  
  num_iterations = xgb.tab$best_iteration
  
  model <- xgb.train(data = trainDMatrix
                     , param = params
                     , maximize = FALSE, evaluation = 'rmse', nrounds = num_iterations)
  
  importance <- xgb.importance(feature_names = colnames(trainMatrix), model = model)
  #xgb.ggplot.importance(importance_matrix = importance)
  
  pred <- predict(model, testDMatrix)
  final123 <- data.frame(category,dates,Actual_Sales=data_fil_test$Sales,pred)
  print(final123)
  xgboost_data <<- rbind(xgboost_data,final123)
  
}
for( i in 1:n_cat){
  xgboost_function(train1,test,r_cat[i])
  i = i+1
}


#Regression Model

train1 <<- History_2016
names(train1) <- c("Category","Date","Sales","Holt.Winter","arimna","X3.month","X4.month","X5.month","X6.month")
test <<- Final_AR_HW_Percent
names(test) <- c("Category","Date","Sales","Holt.Winter","arimna","X3.month","X4.month","X5.month","X6.month")
r_cat <- sort(unique(train1$Category))
n_cat <- as.numeric(length(r_cat))

regression_data <<- data.frame()


regression_function <- function(train1,test,category)  {
  category = as.character(category)
  data_fil1 <- filter(train1,Category == category)
  data_fil2 <- filter(test,Category == category)
  
  #data_fil <- data_fil[,-c(1,2,5)]
  
  data_fil_train <<- data_fil1
  data_fil_train <<- data_fil1[,-c(1,2)]
  #View(data_fil_train)
  data_fil_test <<- data_fil2
  data_fil_test <<- data_fil2[,-c(1,2)]
  reg <- lm(Sales ~. , data = data_fil_train)
  #summary(reg)
  predict_lm <- predict(reg,newdata = data_fil_test)
  final123 <- data.frame(category,dates,Actual_Sales=data_fil_test$Sales,predict_lm)
  print(final123)
  regression_data <<- rbind(regression_data,final123)
}
for( i in 1:n_cat){
  regression_function(train1,test,r_cat[i])
  i = i+1
}

All_Models_Done <- data.frame(Category = Final_AR_HW_Percent$Category,
                              Date = Final_AR_HW_Percent$Date,
                              Sales = Final_AR_HW_Percent$Sales,
                              ARIMA = Final_AR_HW_Percent$ARIMA,
                              Holt_Winter = Final_AR_HW_Percent$Holt_Winter,
                              Month_3 = Final_AR_HW_Percent$Month_3,
                              Month_4 = Final_AR_HW_Percent$Month_4,
                              Month_5 = Final_AR_HW_Percent$Month_5,
                              Month_6 = Final_AR_HW_Percent$Month_6,
                              XGB = xgboost_data$pred,
                              Regression = regression_data$predict_lm)
df1 <- read.csv("D:/7_Neenopal/02_Singer/new_data/Allpercentageratio.csv")
df2 <- All_Models_Done[,c(1,4,5,6,7,8,9,10,11)]
names(df1) <- names(df2)
test12345 <- cbind(Category = df2$Category, df1[match(df2$Category, df1$Category),][-1] * df2[-1])
test123456 <- cbind(test12345,rowSums(test12345[,2:9]))

Final_Forecasted_Result <- data.frame(Category = Final_AR_HW_Percent$Category,
                                      Date = Final_AR_HW_Percent$Date,
                                      Sales = Final_AR_HW_Percent$Sales,
                                      Forecasted = test123456$`rowSums(test12345[, 2:9])`)

View(Final_Forecasted_Result)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken














