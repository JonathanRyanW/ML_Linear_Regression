data <- read.csv("bikeshare.csv")
head(data)

library(ggplot2)
library(dplyr)

#Create a scatter plot of count vs temp. Set a good alpha value
ggplot(data, aes(x = temp, y = count)) +
  geom_point(aes(color = temp), alpha = 0.5)

class(data$datetime)

#Converting datetime into actual date and time
data$datetime <- strptime(data$datetime, format = "%Y-%m-%d %H:%M:%S")
data$datetime <- as.POSIXct(data$datetime)

#Plot count vs datetime with a color gradient based on temperature.
ggplot(data, aes(x = datetime, y = count)) +
  geom_point(aes(color = temp))

#What is the correlation between temp and count?
cor <- cor(data$temp, data$count)

#Create a boxplot with the y count and the x axis begin a box for each season.
ggplot(data, aes(count, color = as.factor(season))) +
  geom_boxplot() +
  coord_flip()

#Create an "hour" column that takes the hour from the datetime column
data$hour <- format(data$datetime, "%H")
data$hour <- as.numeric(data$hour)

#Create a scatterplot of count vs hour, with color scale based on temp
#Only use bike data where workingday==1
ggplot(filter(data, workingday == 1), aes(x = hour, y = count)) +
  geom_point(aes(color = temp), position=position_jitter(w=1, h=0), alpha = 0.5) +
  scale_color_gradientn(colors =c("blue", "purple", "sky blue", "green",
                                  "yellow", "orange", "red"))

#Now create the same plot for non working days
ggplot(filter(data, workingday == 0), aes(x = hour, y = count)) +
  geom_point(aes(color = temp), position=position_jitter(w=1, h=0), alpha = 0.5) +
  scale_color_gradientn(colors =c("blue", "purple", "sky blue", "green",
                                  "yellow", "orange", "red"))

#Use lm() to build a model that predicts count based solely on the temp feature
#name it temp.model

temp.model <- lm(count ~ temp, data)

#Get the summary of the temp.model
summary(temp.model)

#How many bike rentals is predicted if the temperature was 25 degrees Celsius?
#Calculate this two ways:
#Using the coefficients values
prediction1 <- 6.0462 + 9.1705 * 25

#Using the predict() function
df <- data.frame(temp = c(25))
prediction2 <- predict(temp.model, df)

#Use sapply and as.numeric to change the hour column to numeric values
for (i in 2:13) {
  data[,i] <- as.numeric(data[,i])
}
sapply(data, class)

#Build a model to predict count based off of the following features:
#season, holiday workingday weather temp humidity windspeed hour (factor)

model.all <- lm(count ~ season + holiday + workingday + weather + temp
                + humidity + windspeed + hour, data)

#Get the summary of the model
summary(model.all)

#The project has been done successfully

"Something interesting came out, if we change hour into factor, we will get a
model with Adjusted R-squared:  0.6229"

model.all2 <- lm(count ~ season + holiday + workingday + weather + temp
                + humidity + windspeed + as.factor(hour), data)
summary(model.all2)