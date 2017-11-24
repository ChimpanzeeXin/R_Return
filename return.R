library(rpart)
library(rpart.plot)

setwd("C:/Users/ZhuXin/Desktop/各个课程/Business analytics/Assignment_BADS_WS1718")
old_data <- read.csv("BADS_WS1718_known.csv",header=TRUE)
new_data <- read.csv("BADS_WS1718_class.csv",header=TRUE)
summary(old_data)
str(old_data)

#write a function to clean data
clean <- function(data){
 idx_factor <- sapply(data,is.factor)
 data[data == "?"] <- NA
 str(data)

 #deal with time interval between delivery day and order day, replace NA with mean value of the interval
 interval <- as.Date(data$delivery_date) - as.Date(data$order_date)
 interval[interval < 0] <- NA
 delivery_mean<- mean(interval[is.na(interval)==FALSE])
 interval[is.na(interval)] <- delivery_mean
 data$delivery_date <- interval


 #deal with age(user_dob), extract the birth year of customers as age
 dob_date <- as.Date(data$user_dob)
 age <- substr(dob_date,1,4)
 age_numeric <- as.numeric(age)
 age_mean <- round(mean(age_numeric[is.na(age_numeric)==FALSE]))
 age_numeric[is.na(age_numeric)] <- age_mean  
 quantile(age_numeric)
 age_numeric[age_numeric<quantile(age_numeric,0.25)-1.5*IQR(age_numeric)]<-age_mean
 data$user_dob <- factor(age_numeric)

 #deal with brand
 data$brand_id <- as.factor(data$brand_id) 
 return(data)
}

clean(data = old_data)
  


#make classfication tree: use delivery_date, item_size, item_color, brand_id, price,user_dob(noted as age),user_state
myFormula <- return ~ delivery_date + item_size + item_color + brand_id +item_price + user_dob + user_state
dt <- rpart(myFormula, data=old_data, method = "class" )

#before prediction, clean the newdata
clean(new_data)

#prediction
pred.dt <- predict(dt, newdata = new_data, type = "prob" )


