## Chicago-Housing ##

#Add Functions and Load Data

source("http://bigblue.depaul.edu/jlee141/econdata/R/func_lib.R")

house<-read.csv("https://bigblue.depaul.edu/jlee141/econdata/housing/mls2021_sample.csv")

str(house)

### Descriptive Statistics ###

library('dplyr')
library('ggplot2')

house$ZIP <- as.factor(house$ZIP)
summary(house)

#Correlation

house %>% summarise(correlation = cor(HPRICE, SQFT))
house %>% summarise(correlation = cor(HPRICE, AGEBLD))
house %>% summarise(correlation = cor(HPRICE, BEDROOM))
house %>% summarise(correlation = cor(HPRICE, BATHROOM))
house %>% summarise(correlation = cor(HPRICE, GARAGE))
house %>% summarise(correlation = cor(HPRICE, FIREPLACE))

#ANOVA

res_aov <- aov(HPRICE~SQFT+BEDROOM+BATHROOM+GARAGE+FIREPLACE+AGEBLD,
               data = house)
summary(res_aov)

#The scatter plots between the log of house price vs. the log of square feet
#Add linear regression line of the scatter plot. 

par(mfrow=c(3,2))
plot(house$SQFT, house$HPRICE, main="The scatter plots between the house price vs. the square feet", col="skyblue")
reg_model=lm(HPRICE~SQFT, data = house)
abline(reg_model, col="red")

plot(house$AGEBLD, house$HPRICE, main="The scatter plots between the house price vs. the age of building", col="skyblue")
reg_model2=lm(HPRICE~AGEBLD, data = house)
abline(reg_model2, col="red")

plot(house$BEDROOM, house$HPRICE, main="The scatter plots between the ouse price vs. the number of bedroom", col="skyblue")
reg_model3=lm(HPRICE~BEDROOM, data = house)
abline(reg_model3, col="red")

plot(house$BATHROOM, house$HPRICE, main="The scatter plots between the ouse price vs. the number of bathroom", col="skyblue")
reg_model4=lm(HPRICE~BATHROOM, data = house)
abline(reg_model4, col="red")

plot(house$GARAGE, house$HPRICE, main="The scatter plots between the ouse price vs. the number of garage", col="skyblue")
reg_model5=lm(HPRICE~GARAGE, data = house)
abline(reg_model5, col="red")

plot(house$FIREPLACE, house$HPRICE, main="The scatter plots between the ouse price vs. the number of fireplace", col="skyblue")
reg_model6=lm(HPRICE~FIREPLACE, data = house)
abline(reg_model6, col="red")
par(mfrow=c(1,1))

#The histograms of the log of house price, the age of building, and the square feet

hist(house$HPRICE, col="skyblue",main="The histograms of the house price")
hist(house$SQFT, col="orange",main="The histograms of the square feet")
hist(house$AGEBLD, col="pink",main="The histograms of the age of building")

hist(house$BEDROOM, col="gray",main="The histograms of the number of bedroom")
hist(house$BATHROOM, col="gray",main="The histograms of the number of bathroom")
hist(house$GARAGE, col="gray",main="The histograms of the number of garage")
hist(house$FIREPLACE, col="gray",main="The histograms of the number of fireplace")

### Create New Data Sets ###

#Chicago HPRICE

house1<-subset(house,select=-c(ZIP,HOUSEID,LOG_PRICE,LOG_SQFT,SOLD_30DAY))
str(house1)

#Chicago PRICE_LEVEL (dummy variable)

house2<-house1
house2$PRICE_LEVEL<-ifelse(house2$HPRICE>324000,1,0)
house2<-subset(house2,select=-c(HPRICE))
str(house2)

## Descriptive Statistics ##

### K-Means Clustering ###

library(factoextra)

house1_labels<-house1$HPRICE
table(house1_labels)
house1_data<-house1[2:7]
house1_data_scale<-scale(house1_data)
house1_data<-dist(house1_data_scale)
house1_data

#Find the optimal number of clusters

fviz_nbclust(house1_data_scale,kmeans,method="wss")+labs(subtitle="Elbow Method")

km.out<-kmeans(house1_data_scale,centers=10,nstart=100)
print(km.out)

km.clusters<-km.out$cluster
rownames(house1_data_scale)<-paste(house1$HPRICE,1:dim(house1)[1],sep="_")
fviz_cluster(list(data=house1_data_scale,cluster=km.clusters))

### Linear Regression ###

library(tidyverse)

plot(house1$SQFT,house1$HPRICE)
house1_regression<-lm(HPRICE~SQFT,data=house1)
abline(house1_regression,col="blue",lwd=2)
summary(house1_regression)

house1_df<-as.data.frame(house1)
ggplot(data=house1_df,aes(x=SQFT,y=HPRICE))+geom_point(alpha=0.5)+geom_smooth(method='lm')

### Multiple Regression ###

house_mr<-house1[,c(1,2,4)]
str(house_mr)
plot(house_mr)
multiple_regression<-lm(HPRICE~SQFT+BATHROOM,data=house_mr)
summary(multiple_regression)

multiple_regression<-lm(HPRICE~.,data=house1)
summary(multiple_regression)

### Linear Probability Model ###

#set up train and test dataset 

indata<-house2
train_idx<-sample(nrow(indata),round(.8*nrow(indata)))
train<-indata[train_idx,]
test<-indata[-train_idx,]
testy<-test$PRICE_LEVEL

lm0<-lm(PRICE_LEVEL~SQFT,data=train)
summary(lm0)
yhat0<-predict(lm0,newdata=test)
conf_table(yhat0,testy,"LPM")
auc_plot(yhat0,testy,"LPM")

lm1<-lm(PRICE_LEVEL~SQFT+BATHROOM,data=train)
summary(lm1)
yhat1<-predict(lm1,newdata=test)
conf_table(yhat1,testy,"LPM")
auc_plot(yhat1,testy,"LPM")

lm2<-lm(PRICE_LEVEL~.,data=train)
summary(lm2)
yhat2<-predict(lm2,newdata=test)
conf_table(yhat2,testy,"LPM")
auc_plot(yhat2,testy,"LPM")

### Logistic Regression ###

logit0<-glm(PRICE_LEVEL~SQFT,data=train,family=binomial(link=logit))
summary(logit0)
loghat0<-predict(logit0,newdata=test,type="response")
conf_table(loghat0,testy,"LOGIT")
auc_plot(loghat0,testy,"LOGIT")

logit1<-glm(PRICE_LEVEL~SQFT+BATHROOM,data=train,family=binomial(link=logit))
summary(logit1)
loghat1<-predict(logit1,newdata=test,type="response")
conf_table(loghat1,testy,"LOGIT")
auc_plot(loghat1,testy,"LOGIT")

logit2<-glm(PRICE_LEVEL~.,data=train,family=binomial(link=logit))
summary(logit2)
loghat2<-predict(logit2,newdata=test,type="response")
conf_table(loghat2,testy,"LOGIT")
auc_plot(loghat2,testy,"LOGIT")

### Random Forest ###

library(randomForest)

train$PRICE_LEVEL <- as.factor(train$PRICE_LEVEL)
test$PRICE_LEVEL <-  as.factor(test$PRICE_LEVEL)

rf0 <- randomForest(formula=PRICE_LEVEL ~ ., data=train, mtry=5,ntree=500)
summary(rf0)
rfhat0 <- predict(rf0, newdata=test, type="prob")
rfhat0 <- rfhat0[,2]
conf_table(rfhat0, testy, "RANDOMFOREST")
auc_plot(rfhat0, testy, "RANDOMFOREST")

#Find the best mtry

oob.values <- vector(length = 12)
for (i in 1:12) {temp.model <- randomForest(formula=PRICE_LEVEL ~ ., data=train, mtry=i,ntree=500) 
oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]}
cbind(1:12, oob.values)

#the best mtry is 2.

#Find the best ntree

rf_tree <- randomForest(formula=PRICE_LEVEL ~ ., data=train, mtry=i,ntree=1000)
Trees <- rep(1:nrow(rf_tree$err.rate))
Error.rate <- rf_tree$err.rate[,"OOB"]
plot(Trees, Error.rate, col="skyblue")

#Tree might be 250, 400, 600.

rf1 <- randomForest(formula(PRICE_LEVEL~.),data=train,mtry=2,ntree=250)
summary(rf1)
rfhat1 <- predict(rf1,newdata=test,type="prob")
rfhat1=rfhat1[,2]
conf_table(rfhat1,testy,"RANDOMFOREST")
auc_plot(rfhat1,testy,"RANDOMFOREST")

rf2 <- randomForest(formula(PRICE_LEVEL~.),data=train,mtry=2,ntree=400)
summary(rf2)
rfhat2 <- predict(rf2,newdata=test,type="prob")
rfhat2=rfhat2[,2]
conf_table(rfhat2,testy,"RANDOMFOREST")
auc_plot(rfhat2,testy,"RANDOMFOREST")

rf3 <- randomForest(formula(PRICE_LEVEL~.),data=train,mtry=2,ntree=600)
summary(rf3)
rfhat3 <- predict(rf3,newdata=test,type="prob")
rfhat3=rfhat3[,2]
conf_table(rfhat3,testy,"RANDOMFOREST")
auc_plot(rfhat3,testy,"RANDOMFOREST")

par(mfrow=c(2,2))
auc_plot(rfhat0, testy, "RANDOMFOREST mtry=5 ntree=500")
auc_plot(rfhat1,testy,"RANDOMFOREST mtry=2 ntree=410")
auc_plot(rfhat2,testy,"RANDOMFOREST mtry=2 ntree=980")
auc_plot(rfhat3,testy,"RANDOMFOREST mtry=2 ntree=300")
par(mfrow=c(1,1))

### Group data by location (zip code) ###

library(sqldf)

sub_north<-sqldf("SELECT*FROM house WHERE ZIP in (60002,60004,60010,60013,60014,60025,60030,60031,60035,60046,60047,
                60048,60634,60050,60056,60060,60062,60067,60068,60073,60089,60102,60103,60107)")
sub_west<-sqldf("SELECT*FROM house WHERE ZIP in (60126,60134,60137,60142,60148,60174,60175,60187,60402,60506,60540)")
sub_southwest<-sqldf("SELECT*FROM house WHERE ZIP in (60435,60440,60453,60543,60560,60564,60565,60586,60638)")
sub_south<-sqldf("SELECT*FROM house WHERE ZIP in (60411,60619,60620,60628,60643)")
sub_far<-sqldf("SELECT*FROM house WHERE ZIP in (61701,61704,61761,61821)")

sub_north1<-subset(sub_north,select=-c(ZIP,HOUSEID,LOG_PRICE,LOG_SQFT,SOLD_30DAY))
sub_west1<-subset(sub_west,select=-c(ZIP,HOUSEID,LOG_PRICE,LOG_SQFT,SOLD_30DAY))
sub_southwest1<-subset(sub_southwest,select=-c(ZIP,HOUSEID,LOG_PRICE,LOG_SQFT,SOLD_30DAY))
sub_south1<-subset(sub_south,select=-c(ZIP,HOUSEID,LOG_PRICE,LOG_SQFT,SOLD_30DAY))
sub_far1<-subset(sub_far,select=-c(ZIP,HOUSEID,LOG_PRICE,LOG_SQFT,SOLD_30DAY))

summary(sub_north1) # Median=360000
summary(sub_west1) # Median=365500
summary(sub_southwest1) # Median=310000
summary(sub_south1) # Median=172500
summary(sub_far1) # Median=178000

sub_north2<-sub_north1
sub_north2$PRICE_LEVEL<-ifelse(sub_north2$HPRICE>360000,1,0)
sub_north2<-subset(sub_north2,select=-c(HPRICE))
str(sub_north2)

sub_west2<-sub_west1
sub_west2$PRICE_LEVEL<-ifelse(sub_west2$HPRICE>365500,1,0)
sub_west2<-subset(sub_west2,select=-c(HPRICE))
str(sub_west2)

sub_southwest2<-sub_southwest1
sub_southwest2$PRICE_LEVEL<-ifelse(sub_southwest2$HPRICE>310000,1,0)
sub_southwest2<-subset(sub_southwest2,select=-c(HPRICE))
str(sub_southwest2)

sub_south2<-sub_south1
sub_south2$PRICE_LEVEL<-ifelse(sub_south2$HPRICE>172500,1,0)
sub_south2<-subset(sub_south2,select=-c(HPRICE))
str(sub_south2)

sub_far2<-sub_far1
sub_far2$PRICE_LEVEL<-ifelse(sub_far2$HPRICE>178000,1,0)
sub_far2<-subset(sub_far2,select=-c(HPRICE))
str(sub_far2)

### North Models ###

#Linear Regression

plot(sub_north1$SQFT,sub_north$HPRICE)
sub_north1_regression<-lm(HPRICE~SQFT,data=sub_north1)
abline(sub_north1_regression,col="blue",lwd=2)
summary(sub_north1_regression)

#Multiple Regression

multiple_regression<-lm(HPRICE~.,data=sub_north1)
summary(multiple_regression)

#Linear Probability Model

indata1<-sub_north2
train_idx1<-sample(nrow(indata1),round(.8*nrow(indata1)))
train1<-indata1[train_idx1,]
test1<-indata1[-train_idx1,]
testy1<-test1$PRICE_LEVEL

lm01<-lm(PRICE_LEVEL~.,data=train1)
summary(lm01)
yhat01<-predict(lm01,newdata=test1)
conf_table(yhat01,testy1,"LPM")
auc_plot(yhat01,testy1,"LPM")

#Logistic Regression

logit1<-glm(PRICE_LEVEL~.,data=train1,family=binomial(link=logit))
summary(logit1)
loghat1<-predict(logit1,newdata=test1,type="response")
conf_table(loghat1,testy1,"LOGIT")
auc_plot(loghat1,testy1,"LOGIT")

#Random Forest

train1$PRICE_LEVEL <- as.factor(train1$PRICE_LEVEL)
test1$PRICE_LEVEL <-  as.factor(test1$PRICE_LEVEL)

rf01 <- randomForest(formula=PRICE_LEVEL ~ ., data=train1, mtry=5,ntree=500)
summary(rf01)
rfhat01 <- predict(rf01, newdata=test1, type="prob")
rfhat01 <- rfhat01[,2]
conf_table(rfhat01, testy1, "RANDOMFOREST")
auc_plot(rfhat01, testy1, "RANDOMFOREST")

#Find the best mtry

oob.values <- vector(length = 12)
for (i in 1:12) {temp.model <- randomForest(formula=PRICE_LEVEL ~ ., data=train1, mtry=i,ntree=500) 
oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]}
cbind(1:12, oob.values)

#The best mtry is 1.

#Find the best ntree

rf_tree <- randomForest(formula=PRICE_LEVEL ~ ., data=train1, mtry=i,ntree=1000)
Trees <- rep(1:nrow(rf_tree$err.rate))
Error.rate <- rf_tree$err.rate[,"OOB"]
plot(Trees, Error.rate, col="skyblue")

#Tree might be 100.

rf11 <- randomForest(formula(PRICE_LEVEL~.),data=train1,mtry=1,ntree=100)
summary(rf11)
rfhat11 <- predict(rf11,newdata=test1,type="prob")
rfhat11=rfhat11[,2]
conf_table(rfhat11,testy1,"RANDOMFOREST")
auc_plot(rfhat11,testy1,"RANDOMFOREST")

### West Models ###

#Linear Regression

plot(sub_west1$SQFT,sub_west$HPRICE)
sub_west1_regression<-lm(HPRICE~SQFT,data=sub_west1)
abline(sub_west1_regression,col="blue",lwd=2)
summary(sub_west1_regression)

#Multiple Regression

multiple_regression<-lm(HPRICE~.,data=sub_west1)
summary(multiple_regression)

#Linear Probability Model

indata2<-sub_west2
train_idx2<-sample(nrow(indata2),round(.8*nrow(indata2)))
train2<-indata2[train_idx2,]
test2<-indata2[-train_idx2,]
testy2<-test2$PRICE_LEVEL

lm02<-lm(PRICE_LEVEL~.,data=train2)
summary(lm02)
yhat02<-predict(lm02,newdata=test2)
conf_table(yhat02,testy2,"LPM")
auc_plot(yhat02,testy2,"LPM")

#Logistic Regression

logit2<-glm(PRICE_LEVEL~.,data=train2,family=binomial(link=logit))
summary(logit2)
loghat2<-predict(logit2,newdata=test2,type="response")
conf_table(loghat2,testy2,"LOGIT")
auc_plot(loghat2,testy2,"LOGIT")

#Random Forest

train2$PRICE_LEVEL <- as.factor(train2$PRICE_LEVEL)
test2$PRICE_LEVEL <-  as.factor(test2$PRICE_LEVEL)

rf02 <- randomForest(formula=PRICE_LEVEL ~ ., data=train2, mtry=5,ntree=500)
summary(rf02)
rfhat02 <- predict(rf02, newdata=test2, type="prob")
rfhat02 <- rfhat02[,2]
conf_table(rfhat02, testy2, "RANDOMFOREST")
auc_plot(rfhat02, testy2, "RANDOMFOREST")

#Find the best mtry

oob.values <- vector(length = 12)
for (i in 1:12) {temp.model <- randomForest(formula=PRICE_LEVEL ~ ., data=train2, mtry=i,ntree=500) 
oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]}
cbind(1:12, oob.values)

#The best mtry is 1.

#Find the best ntree

rf_tree <- randomForest(formula=PRICE_LEVEL ~ ., data=train2, mtry=i,ntree=1000)
Trees <- rep(1:nrow(rf_tree$err.rate))
Error.rate <- rf_tree$err.rate[,"OOB"]
plot(Trees, Error.rate, col="skyblue")

#Tree might be 100.

rf12 <- randomForest(formula=PRICE_LEVEL~.,data=train2,mtry=1,ntree=100)
summary(rf12)
rfhat12 <- predict(rf12,newdata=test2,type="prob")
rfhat12=rfhat12[,2]
conf_table(rfhat12,testy2,"RANDOMFOREST")
auc_plot(rfhat12,testy2,"RANDOMFOREST")

### Southwest Models ###

#Linear Regression

plot(sub_southwest1$SQFT,sub_southwest$HPRICE)
sub_southwest1_regression<-lm(HPRICE~SQFT,data=sub_southwest1)
abline(sub_southwest1_regression,col="blue",lwd=2)
summary(sub_southwest1_regression)

#Multiple Regression

multiple_regression<-lm(HPRICE~.,data=sub_southwest1)
summary(multiple_regression)

#Linear Probability Model

indata3<-sub_southwest2
train_idx3<-sample(nrow(indata3),round(.8*nrow(indata3)))
train3<-indata3[train_idx3,]
test3<-indata3[-train_idx3,]
testy3<-test3$PRICE_LEVEL

lm03<-lm(PRICE_LEVEL~.,data=train3)
summary(lm03)
yhat03<-predict(lm03,newdata=test3)
conf_table(yhat03,testy3,"LPM")
auc_plot(yhat03,testy3,"LPM")

#Logistic Regression

logit3<-glm(PRICE_LEVEL~.,data=train3,family=binomial(link=logit))
summary(logit3)
loghat3<-predict(logit3,newdata=test3,type="response")
conf_table(loghat3,testy3,"LOGIT")
auc_plot(loghat3,testy3,"LOGIT")

#Random Forest

train3$PRICE_LEVEL <- as.factor(train3$PRICE_LEVEL)
test3$PRICE_LEVEL <-  as.factor(test3$PRICE_LEVEL)

rf03 <- randomForest(formula=PRICE_LEVEL ~ ., data=train3, mtry=5,ntree=500)
summary(rf03)
rfhat03 <- predict(rf03, newdata=test3, type="prob")
rfhat03 <- rfhat03[,2]
conf_table(rfhat03, testy3, "RANDOMFOREST")
auc_plot(rfhat03, testy3, "RANDOMFOREST")

#Find the best mtry

oob.values <- vector(length = 12)
for (i in 1:12) {temp.model <- randomForest(formula=PRICE_LEVEL ~ ., data=train3, mtry=i,ntree=500) 
oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]}
cbind(1:12, oob.values)

#the best mtry is 2.

#Find the best ntree

rf_tree <- randomForest(formula=PRICE_LEVEL ~ ., data=train3, mtry=i,ntree=1000)
Trees <- rep(1:nrow(rf_tree$err.rate))
Error.rate <- rf_tree$err.rate[,"OOB"]
plot(Trees, Error.rate, col="skyblue")

#Tree might be 570.

rf13 <- randomForest(formula=PRICE_LEVEL~.,data=train3,mtry=2,ntree=570)
summary(rf13)
rfhat13 <- predict(rf13,newdata=test3,type="prob")
rfhat13=rfhat13[,2]
conf_table(rfhat13,testy3,"RANDOMFOREST")
auc_plot(rfhat13,testy3,"RANDOMFOREST")

### South Models ###

#Linear Regression

plot(sub_south1$SQFT,sub_south1$HPRICE)
sub_south1_regression<-lm(HPRICE~SQFT,data=sub_south1)
abline(sub_south1_regression,col="blue",lwd=2)
summary(sub_south1_regression)

#Multiple Regression

multiple_regression<-lm(HPRICE~.,data=sub_south1)
summary(multiple_regression)

#Linear Probability Model

indata4<-sub_south2
train_idx4<-sample(nrow(indata4),round(.8*nrow(indata4)))
train4<-indata4[train_idx4,]
test4<-indata4[-train_idx4,]
testy4<-test4$PRICE_LEVEL

lm04<-lm(PRICE_LEVEL~.,data=train4)
summary(lm04)
yhat04<-predict(lm04,newdata=test4)
conf_table(yhat04,testy4,"LPM")
auc_plot(yhat04,testy4,"LPM")

#Logistic Regression

logit4<-glm(PRICE_LEVEL~.,data=train4,family=binomial(link=logit))
summary(logit4)
loghat4<-predict(logit4,newdata=test4,type="response")
conf_table(loghat4,testy4,"LOGIT")
auc_plot(loghat4,testy4,"LOGIT")

#Random Forest

train4$PRICE_LEVEL <- as.factor(train4$PRICE_LEVEL)
test4$PRICE_LEVEL <-  as.factor(test4$PRICE_LEVEL)

rf04 <- randomForest(formula=PRICE_LEVEL ~ ., data=train4, mtry=5,ntree=500)
summary(rf04)
rfhat04 <- predict(rf04, newdata=test4, type="prob")
rfhat04 <- rfhat04[,2]
conf_table(rfhat04, testy4, "RANDOMFOREST")
auc_plot(rfhat04, testy4, "RANDOMFOREST")

#Find the best mtry

oob.values <- vector(length = 12)
for (i in 1:12) {temp.model <- randomForest(formula=PRICE_LEVEL ~ ., data=train4, mtry=i,ntree=500) 
oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]}
cbind(1:12, oob.values)

#The best mtry is 2.

#Find the best ntree

rf_tree <- randomForest(formula=PRICE_LEVEL ~ ., data=train4, mtry=i,ntree=1000)
Trees <- rep(1:nrow(rf_tree$err.rate))
Error.rate <- rf_tree$err.rate[,"OOB"]
plot(Trees, Error.rate, col="skyblue")

#Tree might be 120.

rf14 <- randomForest(formula=PRICE_LEVEL~.,data=train4,mtry=2,ntree=120)
summary(rf14)
rfhat14 <- predict(rf14,newdata=test4,type="prob")
rfhat14=rfhat14[,2]
conf_table(rfhat14,testy4,"RANDOMFOREST")
auc_plot(rfhat14,testy4,"RANDOMFOREST")

### Other Models ###

#Linear Regression

plot(sub_far1$SQFT,sub_far1$HPRICE)
sub_far1_regression<-lm(HPRICE~SQFT,data=sub_far1)
abline(sub_far1_regression,col="blue",lwd=2)
summary(sub_far1_regression)

#Multiple Regression

multiple_regression<-lm(HPRICE~.,data=sub_far1)
summary(multiple_regression)

#Linear Probability Model

indata5<-sub_far2
train_idx5<-sample(nrow(indata5),round(.8*nrow(indata5)))
train5<-indata5[train_idx5,]
test5<-indata5[-train_idx5,]
testy5<-test5$PRICE_LEVEL

lm05<-lm(PRICE_LEVEL~.,data=train5)
summary(lm05)
yhat05<-predict(lm05,newdata=test5)
conf_table(yhat05,testy5,"LPM")
auc_plot(yhat05,testy5,"LPM")

#Logistic Regression

logit5<-glm(PRICE_LEVEL~.,data=train5,family=binomial(link=logit))
summary(logit5)
loghat5<-predict(logit5,newdata=test5,type="response")
conf_table(loghat5,testy5,"LOGIT")
auc_plot(loghat5,testy5,"LOGIT")

#Random Forest

train5$PRICE_LEVEL <- as.factor(train5$PRICE_LEVEL)
test5$PRICE_LEVEL <-  as.factor(test5$PRICE_LEVEL)

rf05 <- randomForest(formula=PRICE_LEVEL ~ ., data=train5, mtry=5,ntree=500)
summary(rf05)
rfhat05 <- predict(rf05, newdata=test5, type="prob")
rfhat05 <- rfhat05[,2]
conf_table(rfhat05, testy5, "RANDOMFOREST")
auc_plot(rfhat05, testy5, "RANDOMFOREST")

#Find the best mtry

oob.values <- vector(length = 12)
for (i in 1:12) {temp.model <- randomForest(formula=PRICE_LEVEL ~ ., data=train5, mtry=i,ntree=500) 
oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]}
cbind(1:12, oob.values)

#the best mtry is 2.

#Find the best ntree

rf_tree <- randomForest(formula=PRICE_LEVEL ~ ., data=train5, mtry=i,ntree=1000)
Trees <- rep(1:nrow(rf_tree$err.rate))
Error.rate <- rf_tree$err.rate[,"OOB"]
plot(Trees, Error.rate, col="skyblue")

#Tree might be 190.

rf15 <- randomForest(formula=PRICE_LEVEL~.,data=train5,mtry=2,ntree=190)
summary(rf15)
rfhat15 <- predict(rf15,newdata=test5,type="prob")
rfhat15=rfhat15[,2]
conf_table(rfhat15,testy5,"RANDOMFOREST")
auc_plot(rfhat15,testy5,"RANDOMFOREST")
