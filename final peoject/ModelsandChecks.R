# loading libraries
library(readr)
library(stringr)
library(tidyverse)
library(dplyr)
library(car)
library(stargazer)

#################################### Bhargavi's tests:  ##############################################
aqi <- read.csv("income_mortality_aqi.csv", header = TRUE, sep = ",")
aqiByState <- aggregate(cbind(Unhealthy.Days, Very.Unhealthy.Days, Hazardous.Days, Median.AQI, Max.AQI, rate_100k) ~ region, data = aqi, mean)

# information about the regions of the US are from this site: https://www.mappr.co/political-maps/us-regions-map/
west = c("California", "Colorado", "Nevada", "Hawaii", "Alaska", "Oregon", "Utah", "Idaho", "Montana", "Wyoming", "Washington")
midwest = c("Minnesota", "Wisconsin", "Illinois", "Ohio", "Indiana", "Michigan", "Missouri", "Iowa", "Kansas", "Nebraska", "North Dakota", "South Dakota")
southwest = c("New Mexico", "Arizona", "Oklahoma", "Texas")
southeast = c("Georgia", "North Carolina", "South Carolina", "Virginia", "West Virginia", "Kentucky", "Tennessee", "Mississippi", "Alabama", "Delaware", "Maryland", "Florida", "Louisiana", "Arkansas")
northeast = c("Massachusetts", "Rhode Island", "Connecticut", "Vermont", "New Hampshire", "Maine", "Pennsylvania", "New Jersey", "New York")

# getting subsets of the aqiByState data frame using the region vectors made above
aqiW <- aqiByState[aqiByState$region %in% west, ]
aqiMW <- aqiByState[aqiByState$region %in% midwest, ]
aqiSW <- aqiByState[aqiByState$region %in% southwest, ]
aqiSE <- aqiByState[aqiByState$region %in% southeast, ]
aqiNE <- aqiByState[aqiByState$region %in% northeast, ]

# comparing the number of hazardous days between the west and midwest regions
t.test(aqiW$Hazardous.Days, aqiMW$Hazardous.Days)
# p-value = 0.1269   ->  not statistically significant
# the differences in number of hazardous days between the two regions is not caused by spatial disparity

# comparing the number of deaths between the southwest and northeast regions
t.test(aqiSW$rate_100k, aqiNE$rate_100k)
# p-value = 0.1971   ->  not statistically significant
# the differences in number of deaths between the two regions is not caused by spatial disparity

# comparing the median aqi between the west and southeast regions
t.test(aqiW$Median.AQI, aqiSE$Median.AQI)
# p-value = 0.4825   ->  not statistically significant
# the differences in median air quality between the two regions is not caused by spatial disparity



###################################### Rafel's tests:  ##############################################

data <- read_csv("income_mortality_aqi.csv")

# filtering for the model 
data <- data %>%
  select(-c(FIPS, subregion, STATE_FIPS, State, County, County, CNTY_FIPS))

# cleaning the rank column to only include the state's ranking
data$Rank.within.US <- data$Rank.within.US %>%
  str_extract("\\d+") %>%
  as.numeric()

# creating first model 
full <- lm(Deaths ~ region + Good.Days + Moderate.Days + Unhealthy.for.Sensitive.Groups.Days + Unhealthy.Days + Very.Unhealthy.Days + Hazardous.Days + Median.AQI + Days.CO + Days.NO2 + Days.Ozone + Days.PM2.5 + Value..Dollars., data = data)
summary(full)
# statistically significant variables are moderate days, unhealthy for sensitive group days, very unhealthy days, hazardous days and median income

# checking for multicollinearity 
car::vif(full)
# looks like region is colinear and so is median AQI and days ozone

# first, adding a region column to the data 
data <- data %>% 
  mutate(geo = 
           case_when(region %in% west ~ "W",
                     region %in% midwest ~ "MW",
                     region %in% southwest ~ "SW",
                     region %in% southeast ~ "SE",
                     region %in% northeast ~ "NE")
  )

# now creating second model including region in the model 
# first taking out the NAs 
data <- data %>%
  na.omit()
mod2 <- lm(Deaths ~ Good.Days + Moderate.Days + Unhealthy.for.Sensitive.Groups.Days + Unhealthy.Days + Very.Unhealthy.Days + Hazardous.Days + Median.AQI + Days.CO + Days.NO2 + Days.Ozone + Days.PM2.5 + Value..Dollars. + factor(geo), data = data)
summary(mod2)
# moderate days, unhealthy for sensitive, very unhealthy, hazardous, median income, NE, SE, and W are statistically significant 

# checking multicollinearity again 
car::vif(mod2)
# looks like days ozone is colinear 

# redoing the model without days ozone 
mod3 <- lm(Deaths ~ Good.Days + Moderate.Days + Unhealthy.for.Sensitive.Groups.Days + Unhealthy.Days + Very.Unhealthy.Days + Hazardous.Days + Median.AQI + Days.CO + Days.NO2 + Days.PM2.5 + Value..Dollars. + factor(geo), data = data)
summary(mod3)
# moderate days, unhealthy for sensitive days, very unhealthy, hazardous, median income, NE, SE, and W are stat sig

# checking multicollinearity again 
car::vif(mod3)
# looks good

# looking for interactions to add to the final model 
add1(mod3, scope = .~. + .^2, test="F")

# sorting these interactions to put stat sig p values on top 
add1.test <- add1(mod3, scope = .~. + .^2, test="F")
add1.test[order(add1.test$`Pr(>F)`),]
# lots of interactions look significant, lets look at hazardous days and median income 
mod4 <- lm(Deaths ~ Good.Days + Moderate.Days + Unhealthy.for.Sensitive.Groups.Days + Unhealthy.Days + Very.Unhealthy.Days + Hazardous.Days * Value..Dollars. + Median.AQI + Days.CO + Days.NO2 + Days.PM2.5 + factor(geo), data = data)
summary(mod4)
# is SS but the effect is very low

# lets try unhealthy days and geographic region as the interaction 
mod5 <- lm(Deaths ~ Good.Days + Moderate.Days + Unhealthy.for.Sensitive.Groups.Days + Unhealthy.Days * factor(geo) + Very.Unhealthy.Days + Hazardous.Days + Value..Dollars. + Median.AQI + Days.CO + Days.NO2 + Days.PM2.5, data = data)
summary(mod5)
# looks like the interaction effect is SS for all regions and causes a huge difference
# this will be our final model 
final_reg_table <- stargazer(mod5, type = "html", title = "Regression Results", align = TRUE, style = "qje", out = "LING_regression_table.html")



###################################### Lama's Assumption Checks:  ##############################################


# Now we will do some model assumptions checks
# Since mod5 is the best model we came up with it would be best if we did some model checks on it

# 1st model check -> run a linerity test
car::ncvTest(mod5)
#The result of the Non-constant Variance Score Test suggests that there might be heteroscedasticity in the model, meaning that the variance of the residuals may not be constant across all levels of the independent variables. Specifically, the test is checking whether the variance of the residuals changes as the predicted values (fitted values) of the response variable change. A significant p-value (p < 0.05) indicates evidence of non-constant variance, which can lead to biased standard errors and incorrect statistical inference.

#To confirm if there is heteroscedasticity in the model, I will plot the residuals against the fitted values and look for any patterns that suggest the variance of the residuals changes as the fitted values change.
plot(mod5$fitted.values, mod5$residuals, 
     xlab = "Fitted values", ylab = "Residuals", 
     main = "Residual plot for mod5")
# there is no clear pattern, however I can see that there is cluttered plotting between fitted values 0 -500 and then it changes drastically and starts getting really scattered 
# since we have most values between 0 - 500 it is worth zooming in on that and once we do it is clear that we have a negaative slope kind of happening A negative slope in the residuals plot suggests that the variance of the residuals is decreasing as the fitted values increase. This is an indication of heteroscedasticity, which violates one of the assumptions of linear regression that the variance of the residuals should be constant across all values of the independent variable.

# last model assumption check to make is to check the normality of the model 5

qqnorm(mod5$residuals)
qqline(mod5$residuals)
#the points on the QQ-plot fall approximately along the reference line, thus the residuals are normally distributed, and we can assume that the model residuals are also normally distributed.

