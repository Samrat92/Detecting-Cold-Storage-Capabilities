setwd("C:/Users/Sam/Documents/R/Directories")
getwd()

data = read.csv("Cold_Storage_Temp_Data.csv", header = TRUE)

summary(data)
str(data)
names(data)

install.packages("ggplot2")
library(ggplot2)

plot(Temperature~Season, data = data, main = "Seasonal Comparison of Temperature", col = "light blue")

winter_data = subset(data, Season == "Winter")
mean(winter_data$Temperature)
hist(winter_data$Temperature, col = "blue",
     main = "Temperature Distribution in Winter",
     xlab = "Temperature")

summer_data = subset(data, Season == "Summer")
mean(summer_data$Temperature)
hist(summer_data$Temperature, col = "orange",
     main = "Temperature Distribution in Summer",
     xlab = "Temperature")

rainy_data = subset(data, Season == "Rainy")
mean(rainy_data$Temperature)
hist(rainy_data$Temperature, col = "purple",
     main = "Temperature Distribution in Monsoon",
     xlab = "Temperature")

data_mean = mean(data$Temperature)

data_sd = sd(data$Temperature)

data_var = var(data$Temperature)

sqrt(data_var)



pnorm(2, mean = data_mean, sd = data_sd)

pnorm(4, mean = data_mean, sd = data_sd, lower.tail = FALSE)

prob =1 -(pnorm(4, mean = data_mean, sd = data_sd) - pnorm(2, mean = data_mean, sd = data_sd))


if (prob > 0.025 && prob <= 0.05){
        penalty = "10 %"
}else if (prob > 0.05){
        penalty = "25 %"
}else {
        penalty = "0 %"
}
       
print(penalty)

                                                                    
new_data = read.csv("Cold_Storage_Mar2018.csv", header = TRUE)

attach(new_data)

new_data_mean = mean(Temperature)
new_data_sd = sd(Temperature)

Mu = 3.9

## Z Test

zstat = (new_data_mean - Mu)/(new_data_sd/sqrt(length(Temperature)))

P.Ho = pnorm(zstat, lower.tail = FALSE)

## Null Hypothesis is rejected

## T Test
## We perform a one sample T test

tstat = (new_data_mean - Mu)/(new_data_sd/sqrt(length(Temperature)))

P2.Ho = 1 - pt(tstat,34)

t.test(Temperature, alternative = c("greater"), mu = Mu, conf.level = 0.9)

print(data_sd)
## Since in both the tests P-value < Alpha Null Hyppothesis is rejected. Hence, corrective action is required at the Cold Storage Plant.



