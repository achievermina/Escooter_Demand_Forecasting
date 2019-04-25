library(ggplot2)
library(car)
library(Metrics)

Escooter_raw=read.csv("portland_demographic_and_escooter.csv")
Escooter_raw$ES_ride2 = Escooter_raw$ES_ride/2
Escooter_raw$density_times_ratio = Escooter_raw$pop_density*Escooter_raw$ratio_of_2050

N=nrow(Escooter_raw)
index = sample(2,N,replace = TRUE, prob = c(0.7,0.3))

ES_train = Escooter_raw[index==1,]
ES_test = Escooter_raw[index==2,]
ES_train$density_times_ratio = ES_train$pop_density*ES_train$ratio_of_2050
ES_test$density_times_ratio = ES_test$pop_density*ES_test$ratio_of_2050
ES_train$ES_ride2 = ES_train$ES_ride/2
ES_test$ES_ride2 = ES_test$ES_ride/2

#################################################

Escooter_raw$medianAgeSquare = (Escooter_raw$Age_median)*(Escooter_raw$Age_median)
Escooter_raw$femaleAgeSquare = (Escooter_raw$Age_female)*(Escooter_raw$Age_female)
Escooter_raw$maleAgeSquare = (Escooter_raw$Age_male)*(Escooter_raw$Age_male)
Escooter_raw$medianAgeSquareroot = sqrt(Escooter_raw$Age_median)
Escooter_raw$femaleAgeSquareroot = sqrt(Escooter_raw$Age_female)
Escooter_raw$maleAgeSquareroot = sqrt(Escooter_raw$Age_male)


Household_Income_median
log(Laborforce_participation)
log(Health_Insurance)
log(pop_density)+
  +log(Health_Insurance)
pop_2017


full.model <- lm(ES_ride ~ (pop_density + Health_Insurance  + Household_Income_median + Laborforce_participation + ratio_of_2050 + Age_median ), data=Escooter_raw)
reduced.model <- step(full.model, direction="backward")
summary(reduced.model)

log.full.model <- lm(log(ES_ride) ~ (log(density_times_ratio)+log(Laborforce_participation)+log(Health_Insurance) +log(Household_Income_median) +log(Unemployment_Rate)  ), data=Escooter_raw)
log.reduced.model <- step(log.full.model, direction="backward")
summary(log.reduced.model)

min.model <- lm(ES_ride ~ 1,data=Escooter_raw)
fwd.model <- step(min.model, direction="forward", scope=( ~ density_times_ratio + Health_Insurance  + Household_Income_median + Laborforce_participation  + Age_median+Unemployment_Rate ))
summary(fwd.model)

plot(ES_ride ~ ratio_of_2050+pop_density+Laborforce_participation, data = Escooter_raw)
abline(Mod1)
plot(log(ES_ride) ~ log(ratio_of_2050)+log(pop_density), data = Escooter_raw)

abline(Mod12)


scatterplot.matrix(~mpg+disp+drat+wt|cyl, data=mtcars,
                   main="Three Cylinder Options")


############with whole data


Mod1 <- lm(ES_ride ~ ratio_of_2050+pop_density+Laborforce_participation, data=Escooter_raw) 
summary(Mod1)

Mod2 <- lm(ES_ride ~ ratio_of_2050+pop_density+Laborforce_participation+Unemployment_Rate, data=Escooter_raw) 
summary(Mod2)

Mod3 <- lm(log(ES_ride) ~ log(ratio_of_2050)+log(pop_density)+log(Laborforce_participation), data=Escooter_raw) 
summary(Mod3)


Mod4 <- lm(log(ES_ride) ~ log(density_times_ratio)+log(Laborforce_participation)+log(Health_Insurance)+log(Household_Income_median), data=Escooter_raw) 
summary(Mod4)


Mod5 <- lm(ES_ride ~ ratio_of_2050+pop_density+Age_median, data=Escooter_raw) 
summary(Mod5)

########################################################################


Mod1 <- lm(ES_ride ~ ratio_of_2050+pop_density+Laborforce_participation, data=ES_train) 
summary(Mod1)

Mod2 <- lm(ES_ride ~ ratio_of_2050+pop_density+Laborforce_participation+log(Unemployment_Rate), data=ES_train) 
summary(Mod2)

Mod3 <- lm(log(ES_ride) ~ log(ratio_of_2050)+log(pop_density)+log(Laborforce_participation), data=ES_train) 
summary(Mod3)

Mod4 <- lm(log(ES_ride) ~ log(density_times_ratio)+log(Laborforce_participation)+log(Health_Insurance)+log(Household_Income_median), data=ES_train) 
summary(Mod4)

Mod5 <- lm(ES_ride ~ ratio_of_2050+pop_density+Age_median, data=ES_train) 
summary(Mod5)

real_data = ES_test$ES_ride

predict1 = predict(Mod1,newdata = ES_test)
RMSE1 = sqrt(sum((predict1-real_data)^2)/nrow(ES_test))

predict2 = predict(Mod2,newdata = ES_test)
RMSE2 = sqrt(sum((predict2-real_data)^2)/nrow(ES_test))

predict3 = predict(Mod3,newdata = ES_test)
RMSE3 = sqrt(sum((exp(predict3)-real_data)^2)/nrow(ES_test))
MSE3= mse(y_pred = exp(predict3), y_true = ES_test$ES_ride)


predict4 = predict(Mod4,newdata = ES_test)
RMSE4 = sqrt(sum((exp(predict4)-real_data)^2)/nrow(ES_test))
MSE4= MSE(y_pred = exp(predict4), y_true = ES_test$ES_ride)


predict5 = predict(Mod5,newdata = ES_test)
RMSE5 = sqrt(sum((predict5-real_data)^2)/nrow(ES_test))
MSE5= MSE(y_pred = predict5, y_true = ES_test$ES_ride)

RMSE1
RMSE2 
RMSE3 
RMSE4 
RMSE5 
min(RMSE1, RMSE2, RMSE3, RMSE4, RMSE5)
predict1
predict2
predict5
exp(predict4)
exp(predict3)
real_data

predict4
log(real_data)

plot(ES_ride ~  ratio_of_2050+pop_density+Age_median, data = Escooter_raw)
abline(Mod5)


#my error prediction is 00% of mean 
#real mean 
#
#############################


newyork = read.csv("newyork_demographic.csv",header=TRUE)
newyork$ES_prediction = predict(Mod5,newdata = newyork)
write
write.csv(newyork,"newyork_ES_prediction.csv")
