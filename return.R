
library(rpart)
library(rpart.plot)
library(caret)

setwd("C:/Users/ZhuXin/Desktop/各个课程/Business analytics/Assignment_BADS_WS1718")
old_data <- read.csv("BADS_WS1718_known.csv",header=TRUE)
new_data <- read.csv("BADS_WS1718_class.csv",header=TRUE)

#deal with unrecgnized level in new data
a <- as.character(old_data$item_size)
b <- as.character(new_data$item_size)
c <- as.factor(c(a,b))
a1 <- c[1:length(a)]
b1 <- c[(1+length(a)):length(c)]
old_data$item_size <- a1
new_data$item_size <- b1

#鉴于不会写函数，此处重复无数遍来整合变量的level。。。
a <- as.character(old_data$item_color)
b <- as.character(new_data$item_color)
c <- as.factor(c(a,b))
a1 <- c[1:length(a)]
b1 <- c[(1+length(a)):length(c)]
old_data$item_color <- a1
new_data$item_color <- b1

a <- as.character(old_data$brand_id)
b <- as.character(new_data$brand_id)
c <- as.factor(c(a,b))
a1 <- c[1:length(a)]
b1 <- c[(1+length(a)):length(c)]
old_data$brand_id <- a1
new_data$brand_id <- b1

clean.fac <- function(known,test,att = "item_color"){
  #known相当于你的old
  #test相当于你的new
  #att是你的变量名,输入任何一个都可以
  #默认值是"item_color"
  #函数返回一个list,result$Training里面存储你的old,result$Tesing里面存储你的new
  a <- as.character(known[,c(att)])
  b <- as.character(test[,c(att)])
  c <- as.factor(c(a,b))
  a1 <- c[1:length(a)]
  b1 <- c[(1+length(a)):length(c)]
  known$item_color <- a1
  test$item_color <- b1
  Result <- list(Trining = known,Testing = test)
  return(Result)
}
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
 data$delivery_interval <- interval
 
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

c.old_data <- clean(old_data)

#make classfication tree(training): use delivery_date, item_size, item_color, brand_id, price,user_dob(noted as age),user_state
#CV:devide data into training and testing data
folds<-createFolds(y=c.old_data$return,k=10)
correctrate<-rep(1,10)
for(i in 1:10){
  #每次先选好训练集和测试集
  train_cv<-c.old_data[-folds[[i]],]
  test_cv<-c.old_data[folds[[i]],]
  #然后训练模型并预测,假设train_cv最后一列是target，前面的列都是features
  myFormula <- return ~ delivery_interval + item_size + item_color + brand_id +item_price + user_dob + user_state
  dt <- rpart(myFormula, data=train_cv, method = "class" )
  pred <- predict(dt,newdata = test_cv,type = "class")
  prp(dt, extra = 104, border.col = 0, box.palette="auto")
  correctrate[i]<-mean(pred==test_cv$return)
}
correctrate

#before prediction, clean the newdata
c.new_data <- clean(new_data)

#prediction
pred.dt <- predict(dt, newdata = c.new_data, type = "prob" )


