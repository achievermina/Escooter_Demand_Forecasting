library(ggplot2)
library(car)
library(Metrics)
library(rsq)

Escooter_raw=read.csv("portland_demographic_and_escooter_v2.csv")
Escooter_raw=na.omit(Escooter_raw)
Escooter_raw$density_times_ratio = Escooter_raw$pop_density*Escooter_raw$ratio_of_2050

############with whole data
#ratio_of_2050, pop_density, Laborforce_participation, Unemployment_Rate, Health_Insurance, Household_Income_median









######
#adjusted R
#1-(1-rsq^2)*(no-1)/(no-predictor-1)

avg_actual = mean(Escooter_raw$ES_ride)
actual = Escooter_raw$ES_ride
ss_total <- sum((actual - avg_actual)^2)
no = nrow(Escooter_raw)

Mod1 <- lm((ES_ridership_to_trip/120) ~ ratio_of_2050+pop_density+Laborforce_participation, data=Escooter_raw) 
summary(Mod1)

predict1 = predict(Mod1,newdata =Escooter_raw )
sse <- sum((actual - predict1)^2)
rsq1= 1-sse/ss_total
rsq1


Mod2 <- lm((ES_ridership_to_trip/120) ~ ratio_of_2050+pop_density+Laborforce_participation+Unemployment_Rate, data=Escooter_raw) 
summary(Mod2)

predict2 = predict(Mod2,newdata =Escooter_raw )
sse <- sum((actual - predict2)^2)
rsq2= 1-sse/ss_total
rsq2

Mod3 <- lm(log((ES_ridership_to_trip/120)) ~ log(ratio_of_2050)+log(pop_density)+log(Laborforce_participation), data=Escooter_raw) 
summary(Mod3)

predict3 = predict(Mod3,newdata =Escooter_raw )
predict3 = exp(predict3)

sse <- sum((actual - predict3)^2)
rsq3= 1-sse/ss_total
rsq3


Mod4 <- lm(log(ES_ride) ~ log(density_times_ratio)+log(Laborforce_participation)+log(Health_Insurance)+log(Household_Income_median), data=Escooter_raw) 
summary(Mod4)
predict4 = predict(Mod4,newdata =Escooter_raw )
Escooter_raw$predict4 = exp(predict4)
write.csv(Escooter_raw,"portland_ride_residuals.csv")



sse <- sum((actual - predict4)^2)
rsq4= 1-sse/ss_total
rsq4

plot()

Mod42 <- lm(log(ES_ride) ~ log(ratio_of_2050)+log(pop_density)+log(Household_Income_median)+log(Unemployment_Rate), data=Escooter_raw) 
summary(Mod42)
predict42 = predict(Mod42,newdata =Escooter_raw )
predict42 = exp(predict42)
sse <- sum((actual - predict42)^2)
rsq42= 1-sse/ss_total
rsq42


Mod5 <- lm(ES_ride ~ ratio_of_2050+pop_density+Age_median, data=Escooter_raw) 
summary(Mod5)
predict5 = predict(Mod5,newdata =Escooter_raw )

sse <- sum((actual - predict5)^2)
rsq5= 1-sse/ss_total
rsq5




Mod73 <- lm(log((ES_ridership_to_trip/120)) ~ log(density_times_ratio)+log(modetoWork_pt)+log(modetoWork_Auto), data=Escooter_raw) 
summary(Mod73)

Mod74 <- lm((ES_ridership_to_trip/120) ~ density_times_ratio + modetoWork_Auto + modetoWork_pt + Household_Income_median, data=Escooter_raw) 
summary(Mod74)

Mod75 <- lm(log((ES_ridership_to_trip/120)) ~ log(density_times_ratio) + log(Laborforce_participation) + log(modetoWork_pt), data = Escooter_raw)
summary(Mod75)

Mod76 <- lm((ES_ridership_to_trip/120) ~ pop_density + Household_Income_median + 
              modetoWork_pt + modetoWork_Auto + ratio_of_2050, data = Escooter_raw)

summary(Mod76)


Mod77 <- lm((ES_ridership_to_trip/120) ~ density_times_ratio + Household_Income_median + modetoWork_pt + modetoWork_Auto, 
            data = Escooter_raw)
summary(Mod77)


Mod78<- lm(log((ES_ridership_to_trip/120)) ~ log(density_times_ratio) + 
             log(Health_Insurance) + log(modetoWork_pt) + log(modetoWork_Auto) + 
             log(Household_Income_median), data = Escooter_raw)
summary(Mod78)
########################################################################

#+log(modetoWork_pt)
#+log(modetoWork_pt)
#+ Age_median+avg_income_2018 +modetoWork_pt +modetoWork_Auto
full.model <- lm((ES_ridership_to_trip/120) ~ (pop_density + Health_Insurance  + Household_Income_median + Laborforce_participation + ratio_of_2050  ), data=Escooter_raw)
reduced.model <- step(full.model, direction="backward")
summary(reduced.model)

full.model <- lm((ES_ridership_to_trip/120) ~ (density_times_ratio + Health_Insurance  + Household_Income_median + Laborforce_participation +modetoWork_pt+modetoWork_Auto    ), data=Escooter_raw)
reduced.model <- step(full.model, direction="backward")
summary(reduced.model)

log.full.model <- lm(log((ES_ridership_to_trip/120)) ~ (log(density_times_ratio)+log(Laborforce_participation)+log(Health_Insurance) +log(Household_Income_median) +log(Unemployment_Rate)  ), data=Escooter_raw)
log.reduced.model <- step(log.full.model, direction="backward")
summary(log.reduced.model)

min.model <- lm((ES_ridership_to_trip/120) ~ 1,data=Escooter_raw)
fwd.model <- step(min.model, direction="forward", scope=( ~ density_times_ratio + Health_Insurance  + Household_Income_median +modetoWork_Auto+ Laborforce_participation  + Age_median+Unemployment_Rate+avg_income_2018 ))
summary(fwd.model)



scatterplot.matrix(~mpg+disp+drat+wt|cyl, data=mtcars,
                   main="Three Cylinder Options")

#############################
#my error prediction is 00% of mean 
#real mean 
#





newyork = read.csv("newyork_demographic_2.csv",header=TRUE)
newyork$ES_prediction = predict(Mod5,newdata = newyork)
write
write.csv(newyork,"newyork_ES_prediction_withMode_model73.csv")

