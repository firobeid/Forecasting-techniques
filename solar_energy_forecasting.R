install.packages('gbm')
install.packages('Metrics')
install.packages('caret')
library(gbm)
library(Metrics)
library(caret)
library(ggplot2)
theme_set(theme_bw())


data <- read.csv('Project_2_dataset.csv')
data = transform.data.frame(data)
n = floor(nrow(data)/2) #number of training data
x_train<-data[1:n,8:10]
y_train<-data[2:(n+1),4]
x_test<-data[(n+1):(nrow(data)-1),8:10]
y_test = data[(n+2):nrow(data), 4]

model <- gbm.fit(x_train,y_train,distribution = 'gaussian',n.trees = 100000,
                 interaction.depth = 1,n.minobsinnode = 10,shrinkage = 0.0001,bag.fraction = 0.2,verbose = TRUE)#train gbm model
summary(model)

best.iter <- gbm.perf(model,method="OOB",plot.it = TRUE, oobag.curve = TRUE) #calculate optimal parameters

y_predictions <- predict(model,x_test,best.iter)

rmse(actual = y_test, predicted = y_predictions)

postResample(pred = y_predictions, obs = y_test)

#Last dat point in the set with unkown y label
x_unknown_y = data[nrow(data),8:10]
predict_unkown_label = predict(model,x_unknown_y,best.iter)
predict_unkown_label

y_predictions[y_predictions<0]=0

residuals = y_test - y_predictions
hist(residuals)

data$SPI = data$GHI/data$Clearsky.GHI
data$SPI[is.na(data$SPI)] = 0
head(data$SPI)


data$GHI[1:nrow(data)-1] + data$SP[1:nrow(data)-1]*(diff(data$Clearsky.GHI))


