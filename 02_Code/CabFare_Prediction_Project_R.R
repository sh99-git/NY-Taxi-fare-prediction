###--------------clearing environment and setting up directory-------------------###

rm(list = ls())
setwd("K:/Data_Science/Project_01/Trials")
getwd()

###------------------- importing libraries --------------------------------------###

libs = c("ggplot2", "geosphere", "corrgram", "DMwR", "usdm", "caret", "raster", "rpart", "randomForest", "xgboost","stats", "sp")

#load Packages
lapply(libs, require, character.only = TRUE)
rm(libs)


###-------------------- loading data --------------------------------------------###

dfTrain <- read.csv("train_cab.csv", header=TRUE, na.strings = c(" ", "", "NA"))
dfTest <- read.csv("test.csv", header=TRUE)

###----------------------overview of data----------------------------------------###

str(dfTrain)

#'data.frame':	16067 obs. of  7 variables:
#$ fare_amount      : chr  "4.5" "16.9" "5.7" "7.7" ...
#$ pickup_datetime  : chr  "2009-06-15 17:26:21 UTC" "2010-01-05 16:52:16 UTC" ...
#$ pickup_longitude : num  -73.8 -74 -74 -74 -74 ...
#$ pickup_latitude  : num  40.7 40.7 40.8 40.7 40.8 ...
#$ dropoff_longitude: num  -73.8 -74 -74 -74 -74 ...
#$ dropoff_latitude : num  40.7 40.8 40.8 40.8 40.8 ...
#$ passenger_count  : num  1 1 2 1 1 1 1 1 1 2 ...

str(dfTest)
#'data.frame':	9914 obs. of  6 variables:
#$ pickup_datetime  : chr  "2015-01-27 13:08:24 UTC" "2015-01-27 13:08:24 UTC" ...
#$ pickup_longitude : num  -74 -74 -74 -74 -74 ...
#$ pickup_latitude  : num  40.8 40.7 40.8 40.8 40.8 ...
#$ dropoff_longitude: num  -74 -74 -74 -74 -74 ...
#$ dropoff_latitude : num  40.7 40.7 40.7 40.8 40.7 ...
#$ passenger_count  : int  1 1 1 1 1 1 1 1 1 1 ...

summary(dfTrain)
summary(dfTest)

###------------------------adjusting data types----------------------------------###

dfTrain$pickup_datetime <- as.POSIXct(dfTrain$pickup_datetime, format = '%Y-%m-%d %H:%M:%S', tz ='UTC')
dfTrain$fare_amount <- as.numeric(dfTrain$fare_amount)
dfTrain$passenger_count <- as.integer(dfTrain$passenger_count)

dfTest$pickup_datetime <- as.POSIXct(dfTest$pickup_datetime, format = '%Y-%m-%d %H:%M:%S', tz ='UTC')

###-------------------- removing non-sensical values-----------------------------###

dfTrain <- subset(dfTrain, !(dfTrain$fare_amount < 1))
#observation with negative fare amount removed
dfTrain <- subset(dfTrain, !((dfTrain$passenger_count < 1)|(dfTrain$passenger_count > 6)))
#observation with illogical passenger count removed
dfTrain <- subset(dfTrain, !((dfTrain$pickup_longitude < -75.5)|(dfTrain$pickup_longitude > -71.8)))
#observation with out of bound pickup longitude removed
dfTrain <- subset(dfTrain, !((dfTrain$pickup_latitude < 39.5)|(dfTrain$pickup_latitude > 41.9)))
#observation with out of bound pickup latitude removed
dfTrain <- subset(dfTrain, !((dfTrain$dropoff_longitude < -75.5)|(dfTrain$dropoff_longitude > -71.8)))
#observation with out of bound dropoff longitude removed
dfTrain <- subset(dfTrain, !((dfTrain$dropoff_latitude < 39.5)|(dfTrain$dropoff_latitude > 41.9)))
#observation with out of bound dropoff latitude removed
dfTrain <- na.omit(dfTrain)
#missing values omited since number is too low compared to data size

###------------------------distribution of individual variables------------------###
ggplot(dfTrain, aes(x = dropoff_latitude)) + geom_area( stat = 'count')
ggplot(dfTrain, aes(x = dropoff_longitude)) + geom_area( stat = 'count')
ggplot(dfTrain, aes(x = pickup_latitude)) + geom_area( stat = 'count')
ggplot(dfTrain, aes(x = pickup_longitude)) + geom_area( stat = 'count')
ggplot(dfTrain, aes(x = fare_amount)) + geom_area( stat = 'count')
ggplot(dfTrain, aes(x = passenger_count)) + geom_area( stat = 'count')

###------------------------feature tranformation---------------------------------###

#function to create new categorical variable based on length of trip
trip_len <- function(distance){
  
  if (distance >=15){ return('long') }
  else if (distance <15){ return('short')}

}

#new varaible geodesic distance
dfTrain$distGeod <- distGeo(cbind(dfTrain$pickup_longitude, dfTrain$pickup_latitude), cbind(dfTrain$dropoff_longitude, dfTrain$dropoff_latitude))/1000
dfTest$distGeod <- distGeo(cbind(dfTest$pickup_longitude, dfTest$pickup_latitude), cbind(dfTest$dropoff_longitude, dfTest$dropoff_latitude))/1000

#new varaible trip length
dfTrain$tripLength <- as.factor(as.character(lapply(X = dfTrain$distGeod, FUN = trip_len)))
dfTest$tripLength <- as.factor(as.character(lapply(dfTest$distGeod, trip_len)))

#function to create new categorical variable based on month
season <- function(month){
  
  if (!(is.na(month))){
    if ((month > 2)&(month <= 5 )){ return('spring') }
    else if ((month > 5)&(month <= 8 )){ return('summer') }
    else if ((month > 8)&(month <= 11 )){ return('fall') }
    else if ((month > 11)|(month <= 2 )){ return('winter') }
  }
  
}

#function to create new categorical variable based on hour
shift <- function(hour){
  
  if (!(is.na(hour))){
    if ((hour > 2) & (hour <= 8 )){ return('dawn') }
    else if ((hour > 8) & (hour <= 14 )){ return('morning') }
    else if ((hour > 14) & (hour <= 20 )){ return('evening') }
    else if ((hour > 20) | (hour <= 2 )){ return('night') }
  }
  
}

#function to create new categorical variable based on weekday
weekend <- function(wkday){
  
  if (!(is.na(wkday))){
    if (wkday >= 6){ return('wkend') }
    else if (wkday < 6 ){ return('wkday')}
  }
  
}

#new varaible pickup year
dfTrain$p_year <- as.factor(format(dfTrain$pickup_datetime,"%Y"))
dfTest$p_year <- as.factor(format(dfTest$pickup_datetime,"%Y"))

#new varaible pickup month
dfTrain$p_mnth <- as.integer(format(dfTrain$pickup_datetime,"%m"))
dfTest$p_mnth <- as.integer(format(dfTest$pickup_datetime,"%m"))

#new varaible pickup day
dfTrain$p_wkdy <- as.integer(format(as.Date(dfTrain$pickup_datetime),"%u"))
dfTest$p_wkdy <- as.integer(format(as.Date(dfTest$pickup_datetime),"%u"))

#new varaible pickup hour
dfTrain$p_hour <- as.integer(format(dfTrain$pickup_datetime,"%H"))
dfTest$p_hour <- as.integer(format(dfTest$pickup_datetime,"%H"))

#new varaible pickup season
dfTrain$p_sson <- as.factor(as.character(lapply(dfTrain$p_mnth, season)))
dfTest$p_sson <- as.factor(as.character(lapply(dfTest$p_mnth, season)))

#new varaible pickup shift
dfTrain$p_shft <- as.factor(as.character(lapply(dfTrain$p_hour, shift)))
dfTest$p_shft <- as.factor(as.character(lapply(dfTest$p_hour, shift)))

#new varaible pickup weekend
dfTrain$p_wknd <- as.factor(as.character(lapply(dfTrain$p_wkdy, weekend)))
dfTest$p_wknd <- as.factor(as.character(lapply(dfTest$p_wkdy, weekend)))


###-----------------------------outlier analysis---------------------------------###

#function to canculate and replace outliers from data with NA
replace_outlier <- function(column){
  
  lw_lmt <- (quantile(column, 0.25) - 1.5 * IQR(column, na.rm = FALSE))
  up_lmt <- (quantile(column, 0.75) + 1.5 * IQR(column, na.rm = FALSE))
  column[(column > up_lmt)|(column < lw_lmt)] <- NA
  return(column)

}

#outlier replcaed with NA for continuous variables
dfTrain$fare_amount <- replace_outlier(dfTrain$fare_amount)
dfTrain$distGeod <- replace_outlier(dfTrain$distGeod)

#trial to impute NA values
##sample from train data to perform trial
dfImputTrial <- na.omit(dfTrain)
dfImputTrial <- dfImputTrial[sample(nrow(dfImputTrial), 500, replace = F), ]
dfImputTrial <- subset(dfImputTrial, select = c(fare_amount, distGeod, p_year, p_mnth, p_hour, p_wkdy, passenger_count))
rownames(dfImputTrial) <- 1:NROW(dfImputTrial)

##5 random values set to na
fare_sample <- c(7, 130, 375, 283, 461)
geod_sample <- c(38, 271, 459, 390, 135)

true_fare <- dfImputTrial$fare_amount[fare_sample]
true_geod <- dfImputTrial$distGeod[geod_sample]

dfImputTrial$fare_amount[fare_sample] <- NA
dfImputTrial$distGeod[geod_sample] <- NA


##imputing with mean
mean_fare <- mean(dfTrain$fare_amount, na.rm = TRUE)
mean_geod <- mean(dfTrain$distGeod, na.rm = TRUE)

absErr_mFare <- sum(abs(true_fare - mean_fare))
#Error in mean imputation: [1] 15.64046
absErr_mGeod <- sum(abs(true_geod - mean_geod))
#Error in mean imputation:[1] 4.055288

##imputing with KNN
dfImputTrial <- knnImputation(dfImputTrial, k = 3)

knn_fare <- dfImputTrial$fare_amount[fare_sample]
knn_geod <- dfImputTrial$distGeod[geod_sample]

absErr_kFare <- sum(abs(true_fare - knn_fare))
#Error in KNN imputation:[1] 7.74958
absErr_kGeod <- sum(abs(true_geod - knn_geod))
#Error in KNN imputation:[1] 3.017385

#Based on absolute error KNN method is selected for imputation
dfImputation <- subset(dfTrain, select = c(fare_amount, distGeod, p_year, p_mnth, p_hour, p_wkdy, passenger_count))
dfImputation <- knnImputation(dfImputation, k = 3)
sum(is.na(dfImputation))
#missing values in data: [1] 0

dfTrain$fare_amount <- dfImputation$fare_amount
dfTrain$distGeod <- dfImputation$distGeod

sum(is.na(dfTrain))
#missing values in data: [1] 0

###----------------------------------feature selection---------------------------###
#correlation analysis
corrgram(dfImputation, order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#scatter plot between geodesic distance and fare amount
ggplot(dfTrain, aes(distGeod, fare_amount)) + geom_point()

#adjusting data type for passenger count
dfTrain$passenger_count <- as.factor(dfTrain$passenger_count)
dfTest$passenger_count <- as.factor(dfTest$passenger_count)

#removing used variables
dfTrain <- subset(dfTrain, select = -c(pickup_datetime, pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude, p_mnth, p_wkdy, p_hour))
dfTest <- subset(dfTest, select = -c(pickup_datetime, pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude, p_mnth, p_wkdy, p_hour))

#one-hot coding categorical variables
dummy <- dummyVars(" ~ . ", data = dfTrain, fullRank = TRUE)
dfTrain <- data.frame(predict(dummy, newdata = dfTrain))

dummy <- dummyVars(" ~ . ", data = dfTest, fullRank = TRUE)
dfTest <- data.frame(predict(dummy, newdata = dfTest))

#vif analysis for multicolinearity
vif(dfTrain[,-1])
#           Variables      VIF
#1  passenger_count.2   1.035537
#2  passenger_count.3   1.019321
#3  passenger_count.4   1.012757
#4  passenger_count.5   1.023401
#5  passenger_count.6   1.017955
#6           distGeod   1.019970
#7   tripLength.short   1.004666
#8        p_year.2010   1.692045
#9        p_year.2011   1.686567
#10       p_year.2012   1.704758
#11       p_year.2013   1.705418
#12       p_year.2014   1.666356
#13       p_year.2015   1.373740
#14     p_sson.spring   1.627420
#15     p_sson.summer   1.549805
#16     p_sson.winter   1.588536
#17    p_shft.evening   2.184980
#18    p_shft.morning   2.139119
#19      p_shft.night   2.082691
#20      p_wknd.wkend   1.019661
# no vif more than 5

###----------------------------model building------------------------------------###
#train-test split
set.seed(1000)
tr.idx = createDataPartition(dfTrain$fare_amount,p=0.75,list = FALSE)
to_train <- dfTrain[tr.idx, ]
to_test <- dfTrain[-tr.idx, ]

#linear regression
linearModel <- lm(fare_amount ~.,data=to_train)
summary(linearModel)
linearPredTr = predict(linearModel,to_test[,2:21])
regr.eval(to_test[,1],linearPredTr)
#mae       mse      rmse      mape 
#1.5290301 4.6964678 2.1671335 0.1811715 

#decision tree regression
DTModel <- rpart(fare_amount ~ ., data = to_train, method = "anova")
summary(DTModel)
dtreePredTr <- predict(DTModel,to_test[,2:21])
regr.eval(to_test[,1], dtreePredTr)
#mae       mse      rmse      mape 
#1.7642704 5.8476199 2.4181853 0.2122266 

#random forest regression
RFModel <- randomForest(fare_amount ~., data=to_train)
summary(RFModel)
rforstPredTr <- predict(RFModel, to_test[,2:21])
regr.eval(to_test[,1], rforstPredTr)
#mae       mse      rmse      mape 
#1.5355820 4.6534580 2.1571875 0.1841384

#improving accuracy through XGBoost regression
to_train_matrix <- as.matrix(sapply(to_train[-1],as.numeric))
to_test_matrix <- as.matrix(sapply(to_test[-1],as.numeric))

XGBModel <- xgboost(data = to_train_matrix, label = to_train$fare_amount, nrounds = 15,verbose = FALSE)
summary(XGBModel)
xgbPredTr <- predict(XGBModel, to_test_matrix)
regr.eval(to_test[,1], xgbPredTr)
#mae       mse      rmse      mape 
#1.4774921 4.5175955 2.1254636 0.1734023 

###------------------------------final model-------------------------------------###
dfTrain_matrix <- as.matrix(sapply(dfTrain[-1],as.numeric))
dfTest_matrix <- as.matrix(sapply(dfTest,as.numeric))

Final_XGB_Model <- xgboost(data = dfTrain_matrix, label = dfTrain$fare_amount, nrounds = 15,verbose = FALSE)
XGB_Prediction <- predict(Final_XGB_Model, dfTest_matrix)

XGB_Pred = data.frame("fare_amount" = XGB_Prediction)

#saving predictions in csv file
write.csv(XGB_Pred,"cabfare_xgb_output_r.csv",row.names = TRUE)

#saving model in dump format
saveRDS(Final_XGB_Model, "./cabfare_xgbmodel_r")

####################################################################################
