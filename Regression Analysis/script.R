library(car)
cars
head(cars)
help(scatterplotMatrix)
/ / scatterplotMatrix(states_info, spread = FALSE, smoother.args = list(lty = 2), main = "Scatter Plot Matrix")

scatter.smooth(x = cars$speed, y = cars$dist, main = "Dist ~ Speed") # scatterplot

par(mfrow = c(1, 2)) # divide graph area in 2 columns
boxplot(cars$speed, main = "Speed", sub = paste("Outlier rows: ", boxplot.stats(cars$speed)$out)) # box plot for 'speed'
boxplot(cars$dist, main = "Distance", sub = paste("Outlier rows: ", boxplot.stats(cars$dist)$out)) # box plot for 'distance'

install.packages("e1071")
library(e1071)
# divide graph area in 2 columns
par(mfrow = c(1, 2))
# density plot for 'speed'
plot(density(cars$speed), main = "Density Plot: Speed", ylab = "Frequency", sub = paste("Skewness:", round(e1071::skewness(cars$speed), 2)))
polygon(density(cars$speed), col = "red")
plot(density(cars$dist), main = "Density Plot: Distance", ylab = "Frequency", sub = paste("Skewness:", round(e1071::skewness(cars$dist), 2))) # density plot for 'dist'
polygon(density(cars$dist), col = "red")

# calculate correlation between speed and distance
cor(cars$speed, cars$dist)

# build linear regression model on full data
linearMod <- lm(dist ~ speed, data = cars)
print(linearMod)

# model summary
summary(linearMod)

# demo of how to calculate t-statistic and p-values
# capture model summary as an object
model_summary <- summary(linearMod)

# model coefficients
model_coeffs <- model_summary$coefficients

# get beta estimate for speed
beta.estimate <- model_coeffs["speed", "Estimate"]

# get std.error for speed
std_error <- model_coeffs["speed", "Std. Error"]

# calc t statistic
t_value <- beta.estimate / std_error
p_value <- 2 * pt(-abs(t_value), df = nrow(cars) - ncol(cars)) # calc p Value
f_statistic <- linearMod$fstatistic[1] # fstatistic
f <- summary(linearMod)$fstatistic # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower = FALSE)

# AIC => 419.1569
AIC(linearMod)

# BIC => 424.8929
BIC(linearMod)

# Create Training and Test data
# setting seed to reproduce results of random sampling
set.seed(100)

# sample chooses a random sample
# from 1:all records from cars, 80% of rows
no_of_records <- sample(1:nrow(cars), 0.8 * nrow(cars))
# model training data
training_data <- cars[no_of_records,]
training_data
# test data
testing_data <- cars[-no_of_records,]
testing_data

# Build the model on training data
# lm(formula, data) where
# formula describes the model to be fit
lr_model <- lm(dist ~ speed, data = training_data)

# predict distance from testing data
dist_predicted <- predict(lr_model, testing_data)

# model summary
summary(lr_model)

# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals = testing_data$dist, predicted = dist_predicted))
head(actuals_preds)

correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

# MAPE
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals)) / actuals_preds$actuals)
mape

# Package DAAG includes CVlm whic is
# Cross-Validation For Linear Regression function
install.packages("DAAG")
library(DAAG)
cvResults <- suppressWarnings(CVlm(data = cars, form.lm = dist ~ speed, m = 5, dots = FALSE, seed = 29, legend.pos = "topleft", printit = FALSE, main = "Small symbols are predicted values while bigger ones are actuals."));
# performs the CV
attr(cvResults, 'ms') # => 251.2783 mean squared error

# Example shown in slides for 
# height and weight of men
height <- c(63, 64, 66, 69, 69, 71, 71, 72, 73, 75)
weight <- c(127, 121, 142, 157, 162, 156, 169, 165, 181, 208)
my_data <- data.frame(height = height, weight = weight)
my_data
fit <- lm(weight ~ height, data = mydata)
summary(fit)

# predict weight from height.
# Having an equation for
# predicting weight from height can help
# you to identify overweight or
# underweight individuals.
fit <- lm(weight ~ height, data = women)
summary(fit)
# Because a height of 0 is impossible, you wouldn’t 
# try to give a physical interpretation
# to the intercept. It merely becomes an adjustment constant

# Actual values
women$height

# list the predicted values in the fitted model
fitted(fit)

# List the residual values in the fitted model
residuals(fit)
# show scatter plot 
plot(women$height, women$weight, xlab = "Height (in inches)", ylab = "Weight (in pounds)")
# Add regression line
abline(fit)
# From the Pr(>|t|) column,
# you see that the regression coefficient(3.45) is significantly different from zero
# (p < 0.001) indicates that there’s an expected increase of 3.45 pounds of weight
# for every 1 inch increase in height.
# multiple R-squared is also the squared correlation between the actual 
# and predicted value
# The residual standard error (1.53 pounds) can be thought of as the 
# average error in predicting weight from height using this model.


# Polynomial regression -----------------------------------------------
fit2 <- lm(weight ~ height + I(height ^ 2), data = women)
summary(fit2)
# The prediction equation now is 
# Weight = 261.88 - 7.35 × Height + 0.083 × Height2

# The curve provides a better fit
plot(women$height, women$weight, xlab = "Height (in inches)", ylab = "Weight (in lbs)")
lines(women$height, fitted(fit2))

# this enhanced plot provides the scatter plot of weight with height, 
# box plots for each variable in their respective margins, the linear 
# line of best fit, and a smoothed fit line. 
# spread=FALSE suppresses spread and asymmetry information. 
# smoother.args=list(lty=2) specifies the fit be rendered as a dashed line. 
# pch = 19 options display points as filled circles(the default is open circles) . 
# You can tell that the two variables are roughly symmetrical and that a curved line
# will fit the data points better than a straight line
install.packages("car")
library(car)
scatterplot(weight ~ height, data = women,
  spread = FALSE, smoother.args = list(lty = 2), pch = 19,
  main = "Women Age 30-39",
  xlab = "Height (inches)",
  ylab = "Weight (lbs.)")

# use the state.x77 dataset in the base package for this example. Suppose you
# want to explore the relationship between a state’s murder rate and other characteristics
# of the state, including population, illiteracy rate, average income, and frost levels
# Note ... lm() function requires a data frame (and the state.x77 dataset is
# contained in a matrix) so we can convert it with this code

# A good first step in multiple regression is to examine the 
# relationships among the variables two at a time. 
# The bivariate correlations are provided by the cor() function,
# and scatter plots are generated from the scatterplotMatrix() 
# function in the car package
states <- as.data.frame(state.x77[, c("Murder", "Population",
"Illiteracy", "Income", "Frost")])
cor(states)
library(car)
help(scatterplotMatrix)
scatterplotMatrix(states, spread = FALSE, smoother.args = list(lty = 2),
main = "Scatter Plot Matrix")

states <- as.data.frame(state.x77[, c("Murder", "Population",
"Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
data = states)
summary(fit)

# Multiple linear regression with interactions -----------------------
# We use : to indicate an interaction between predictor variables
fit <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(fit)
# Pr(>|t|) column that the interaction between horsepower and
# car weight is significant. What does this mean ? A significant interaction between two
# predictor variables tells you that the relationship between one predictor and the
# response variable depends on the level of the other predictor

# You can visualise interactions using the effect() function in the effects package.
# This means we can change values for wt and view the change graphically
install.packages("effects")
library(effects)
plot(effect("hp:wt", fit,, list(wt = c(2.2, 3.2, 4.2))), multiline = TRUE)

# evaluating the statistical assumptions in a regression analysis. 
# The most common approach is to apply the plot() function
# to the object returned by the lm() . Doing so produces four graphs that are useful
# for evaluating the model fit.
fit <- lm(weight ~ height, data = women)
# 4 plots in one graph
par(mfrow = c(2, 2))
plot(fit)

# Diagnostic plots for the quadratic fit.
fit2 <- lm(weight ~ height + I(height ^ 2), data = women)
par(mfrow = c(2, 2))
plot(fit2)

# Dropping point 13 and 15
newfit <- lm(weight ~ height + I(height ^ 2), data = women[-c(13, 15),])
par(mfrow = c(2, 2))
plot(newfit)

# let ’s apply the basic approach to the states multiple regression problem
states <- as.data.frame(state.x77[, c("Murder", "Population",
                                    "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
par(mfrow = c(2, 2))
plot(fit)
# the model assumptions appear to be well satisfied
# with the exception that Nevada is an outlier

# Global validation of linear model assumption
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)

library(car)
states <- as.data.frame(state.x77[, c("Murder", "Population",
"Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
qqPlot(fit, labels = row.names(states), id.method = "identify",
simulate = TRUE, main = "Q-Q Plot")