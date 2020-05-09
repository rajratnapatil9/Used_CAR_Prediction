autodata <- read.table(file = "autos.csv", sep = "," ,fill = TRUE, quote = "",
                      header = TRUE, na.strings=c("","NA"))

str(autodata)

#head(autodata)
#dim(autodata)
#371521     20

#Cleaning Data
autodata$nrOfPictures<-NULL
autodata$seller<-NULL
autodata$offerType<-NULL
autodata$abtest<-NULL
autodata$dateCrawled<-NULL
autodata$monthOfRegistration<-NULL

#Removing the outliers
autodata<-subset(autodata, autodata$price>200 & autodata$price<150000)
autodata <-subset(autodata, autodata$yearOfRegistration>=1990 & autodata$yearOfRegistration<2016)
autodata <- subset(autodata, autodata$powerPS>25 & autodata$powerPS<600)

#sep <- strsplit(as.character(autodata$name),split = "_")
#autodata$model1 <- as.factor(sapply(sep,"[[",1))

#As the data is in German we need to change the Column values to English
autodata$gearbox<-gsub("manuell","manual",autodata$gearbox)
autodata$gearbox<-gsub("automatik","automatic",autodata$gearbox)
autodata$vehicleType<-gsub("andere","others",autodata$vehicleType)
autodata$vehicleType<-gsub("cabrio","convertible",autodata$vehicleType)
autodata$vehicleType<-gsub("kleinwagen","small_Car",autodata$vehicleType)
autodata$fuelType<-gsub("benzin","petrol",autodata$fuelType)
autodata$fuelType<-gsub("andere","others",autodata$fuelType)
autodata$fuelType<-gsub("electro","electro",autodata$fuelType)
autodata$notRepairedDamage<-gsub("nein","no",autodata$notRepairedDamage)
autodata$notRepairedDamage<-gsub("ja","yes",autodata$notRepairedDamage)

autodata = autodata[complete.cases(autodata$gearbox),]
table(autodata$gearbox)

#Handling the NAs
autodata$vehicleType <- as.character(autodata$vehicleType)
autodata$vehicleType[is.na(autodata$vehicleType)] <- "others"

autodata$fuelType <- as.character(autodata$fuelType)
autodata$fuelType[is.na(autodata$fuelType)] <- "others"

autodata$notRepairedDamage <- as.character(autodata$notRepairedDamage)
autodata$notRepairedDamage[is.na(autodata$notRepairedDamage)] <- "Unknown"

autodata = autodata[complete.cases(autodata$model),]

#to check no of nulls in each column
colSums(is.na(autodata))
anyNA(autodata)

#Age of car
autodata$Car_Age<-2016-autodata$yearOfRegistration
table(autodata$Car_Age)

#further removing columns which are not needed  
autodata$yearOfRegistration<-NULL
autodata$postalCode<-NULL
autodata$lastSeen<-NULL
autodata$dateCreated<-NULL
autodata$name<-NULL


str(autodata)
dim(autodata)
#280081     10

head(autodata,5)

#write.table(autodata, file = "autodataclean_phase3.csv", sep = ",")

#Correlations
#install.packages("corrplot")
library(corrplot)
au<-autodata[,c("price","Car_Age","kilometer","powerPS")]
#cor(au)
#head(au)
corrplot.mixed(cor(au))

autodata$model<-as.character(autodata$model)
autodata$brand<-as.character(autodata$brand)
str(autodata)

#Dealing with top 5 brands
autodata_upd<- autodata[(autodata$brand=="bmw" | autodata$brand=="volkswagen" | autodata$brand=="mercedes_benz" | autodata$brand=="opel" |
                           autodata$brand=="audi"),]
#Removing outdated Models
autodata_upd1<-autodata_upd[!(autodata_upd$model=="andere" | autodata_upd$model=="80
" | autodata_upd$model=="90" | autodata_upd$model=="100" | autodata_upd$model=="200" | 
autodata_upd$model=="gl"| autodata_upd$model=="g_klasse" | autodata_upd$model=="kaefer" | 
autodata_upd$model=="i3" | autodata_upd$model=="200" | autodata_upd$model=="amarok" | 
  autodata_upd$model=="kadett" | autodata_upd$model=="calibra"),]

autodata_upd2<-autodata_upd1[(autodata_upd1$fuelType=="petrol" | autodata_upd1$fuelType=="diesel"),]

dim(autodata_upd2)
head(autodata_upd2)
names(as.factor(str(autodata_upd2$notRepairedDamage)))
length(autodata_upd2$notRepairedDamage)
names(str(as.factor(autodata_upd2$model)))
#111503     10

#Regression Analysis
smp_size <- floor(0.7 * nrow(autodata_upd2))
train_ind <- sample(seq_len(nrow(autodata_upd2)), size = smp_size)
train<- autodata_upd2[train_ind,]
test <- autodata_upd2[-train_ind, ]

#Model Selection
summary(lm(sqrt(price)~.-model,data=train))
Model_simple<- lm(price~vehicleType+gearbox+powerPS+kilometer+fuelType+brand+Car_Age, data=train)
summary(Model_simple)
pred_simple<-predict(Model_simple,train)
RSS<-sum((train$price-pred_simple)^2)
TSS<- sum((train$price-mean(train$price))^2)
(R2<- (TSS-RSS)/TSS)
(R2<- 1-RSS/TSS)
(RSME<-sqrt(RSS/nrow(train)))
(RSE<- sqrt(RSS/(nrow(train) -7 -1)))

Model_log<- lm(log(price)~vehicleType+gearbox+powerPS+kilometer+fuelType+brand+Car_Age, data=train)
summary(Model_log)
pred_log<-exp(predict(Model_log,train))
RSS<-sum((train$price-pred_log)^2)
TSS<- sum((train$price-mean(train$price))^2)
(R2<- 1-RSS/TSS)
(R2<- 1-RSS/TSS)
(RSME<-sqrt(RSS/nrow(train)))
(RSE<- sqrt(RSS/(nrow(train) -7 -1)))

Model_sqrt<- lm(sqrt(price)~vehicleType+gearbox+powerPS+kilometer+fuelType+brand+Car_Age+I(Car_Age^2), data=train)
summary(Model_sqrt)
pred_sqrt<-(predict(Model_sqrt,train))^2
RSS<-sum((train$price-pred_sqrt)^2)
TSS<- sum((train$price-mean(train$price))^2)
(R2<- 1-RSS/TSS)
(RSME<-sqrt(RSS/nrow(train)))
(RSE<- sqrt(RSS/(nrow(train) -16 -1)))

#MSPE (mean squared prediction error):
pred_sqrt_test<-(predict(Model_sqrt,test)^2)
MSPE<-mean((train$price - pred_sqrt) ^ 2)
MSPE



#################
RSS.test<-sum((test$price-pred_sqrt_test)^2)
TSS.test<- sum((test$price-mean(test$price))^2)
(R2.test<- 1-RSS.test/TSS.test)
(RSME.test<-sqrt(RSS.test/nrow(test)))
(RSE.test<- sqrt(RSS.test/(nrow(test) -16 -1)))


#Checking Multicollinearilty
install.packages("car")
library(carData)
library(car)
vif(Model_sqrt)
#No Multicollinearity

Error.df<- data.frame(brand=test$brand,True_Price= test$price,Pred_Price=pred_sqrt_test)
head(Error.df,10)


write.csv(Error.df, file = "Error.df.csv")
write.csv(test,file = "test.csv")

plot(Model_simple,1)

head(test,1)
