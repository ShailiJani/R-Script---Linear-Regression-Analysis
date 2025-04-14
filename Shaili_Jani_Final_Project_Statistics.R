#ggplot
#basic statistics 
library(ggplot2)
library(dplyr)
library(MASS)
data <- Birthweights
View(data)
head(data)
summary(data)

#Histogram of bwt
ggplot(data = Birthweights, aes(x = bwt)) +
  geom_histogram(binwidth = 100, fill = "yellow", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Birth Weight",
    x = "Birth Weight (grams)",
    y = "Frequency"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


#
ggplot(data = Birthweights, aes(x = bwt, fill = factor(smoke))) +
  geom_histogram(position = "fill", binwidth = 100) +
  labs(
    title = "Histogram of Birth Weight by Smoking Status (Proportion)",
    x = "Birth Weight (grams)",
    y = "Proportion",
    fill = "Smoking Status"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


#One sample t test 
t.test(data$bwt, mu=3400)

#Scatter plot between smoking status categorical variable independent variable and 
ggplot(Birthweights, aes(x = factor(smoke), y = bwt)) +
  geom_point(colour = "blue") +
  labs(
    title = "Scatter Plot of Smoking Status vs Birth Weight",
    x = "Smoking Status (0 = Non-Smoker, 1 = Smoker)",
    y = "Birth Weight (grams)"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Making box plot for better explanation of relation between mother being smoker and birth weights of new born babies
ggplot(Birthweights, aes(x = factor(smoke), y = bwt)) +
  geom_boxplot(fill = "lightblue", colour = "darkblue") +
  labs(
    title = "Box Plot of Smoking Status vs Birth Weight",
    x = "Smoking Status (0 = Non-Smoker, 1 = Smoker)",
    y = "Birth Weight (lb)"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Correlation between smoke and bwt
cor(Birthweights$smoke, Birthweights$bwt)

#Linear regression model
linear_model <- lm(bwt ~ smoke, data = Birthweights)
summary(linear_model)


#Multiple linear regression model

MV_Model <- lm(bwt ~ gestation + region + age + height + weight + smoke, data = Birthweights)

print(MV_Model)

summary(MV_Model)

#Create a new data frame for prediction
newdata = data.frame(gestation = 351 , region = 'southwest', age = 34, height = 168, weight = 56.2, smoke = 1)
print(newdata)


#Prediction
MV_prediction <- predict(MV_Model, newdata)
print(MV_prediction)







