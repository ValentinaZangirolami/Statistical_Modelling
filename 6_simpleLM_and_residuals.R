################################################################################
#                      Simple linear regression models                         #
#                          & Analysis of residuals                             #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################
#load packages
require(alr4)
require(ggplot2)
#load data
data("Heights")

# Data involve Mother and Daughter heights (we already used these data for 
#the first exercise)
head(Heights)

#There are two variables: mheight (mother's height) and dheight (daughter's height). 
#Heights are expressed in inches. 

#Main summary statistics about the two variables:
summary(Heights)
nrow(Heights)
#There are 1375 observations.

#Our goal: We would like to find out if there exists a relationship between
#dheight and mheight

#Now, let's see the scatter plot of the above two variables
theme_set(theme_bw())  ## sets default white background theme for ggplot
ggplot(data = Heights, aes(x=mheight, y=dheight)) + geom_point(col="red", alpha = 0.5) + 
  labs(x="Mother's height", y="Daughter's height", title = "Plot Mother and Daughter Heights")
#The relationship among variables seems to be linear. 

#Let's add the regression line
ggplot(data = Heights, aes(x=mheight, y=dheight)) + geom_point(col="red", alpha = 0.5) + geom_smooth(method = "lm") + 
  labs(x="Mother's height", y="Daughter's height", title = "Plot Mother and Daughter Heights")

# Let's focus on the linear regression model and check the estimated coefficients
mod <- lm(dheight ~ mheight, data=Heights)
summary(mod)

#We can observe that the median of the residual is around 0. 
#The p-values for each coefficient bring us to reject the null hypothesis
#The R2 is equal to 0.2408
#Also the p-value of the Ftest is less than all the conventional alpha (reject null hypothesis)

#Confident Intervals for each regression coefficient with 95% confidence level
confint(mod)
#you can verify the confidence level by looking the first line.

#Analysis of residuals
par(mfrow=c(2,2))
plot(mod)

#In the first plot (Residual vs Fitted), we can observe the relationship between residuals and
#fitted values.
#Q-Q Residuals allows us to check the gaussian assumption of errors
#Scale-Location is used to verify if the assumption of homoschedasticity holds within data
#Residuals vs Leverage can highlights potential issues as outliers.

## Prediction

# new data for mheight
new_data <- data.frame(mheight=c(75, 60.6, 68, 57.4))

# check range of mheight values
cat('Min:', min(Heights$mheight), 'Max:', max(Heights$mheight))

# confidence interval
(prev_ic <- predict(mod, new_data, interval = "confidence"))

# Plot

plot(Heights$mheight, Heights$dheight, xlim=range(new_data$mheight), ylab = "Daughter's Height", xlab = "Mother's Height")
matlines(new_data$mheight, prev_ic, lty=c(1,2,2), col="violet")

rm(list=ls())

#### Simulating Data - Analysis of residuals
set.seed(123)
x <- sort(runif(50))

# Issues with Linearity assumption

y1 <- x^2+rnorm(50,sd=.05)
y2 <- -x^2+rnorm(50,sd=.05)
y3 <- (x-.5)^3+rnorm(50,sd=.01)

plot(x,residuals(lm(y1~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y2~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y3~x)),xlab="X",ylab="residuals")

# Diagnostic plot
par(mfrow=c(2, 2))
plot(lm(y1~x))

# Transformation of the independent variable
par(mfrow=c(2, 2))
plot(lm(y1~poly(x, 2)))

# Issues with Homoscedasticity assumption
y1 <- x+x*rnorm(50,sd=.03)
y2 <- x+(1-x)*rnorm(50,sd=.03)
y3 <- x+c(rnorm(15,sd=.1),rnorm(20,sd=.4),rnorm(15,sd=.02))
y4 <- x+c(rnorm(20,sd=.5),rnorm(15,sd=.1),rnorm(15,sd=.4))

plot(x,residuals(lm(y1~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y2~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y3~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y4~x)),xlab="X",ylab="residuals")

# Issues with Gaussian assumption

y1 <- x+.05*rt(50,df=2)
y2 <- x+.03*(rchisq(50,1)-.5)
y3 <- x-.03*(rchisq(50,1)-.5)
y4 <- x+.07*rt(50,df=2)

plot(x,residuals(lm(y1~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y2~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y3~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y4~x)),xlab="X",ylab="residuals")

# qqplot

qqnorm(residuals(lm(y1~x)), pch = 1, frame = FALSE)
qqline(residuals(lm(y1~x)), col = "steelblue", lwd = 2)

qqnorm(residuals(lm(y2~x)), pch = 1, frame = FALSE)
qqline(residuals(lm(y2~x)), col = "steelblue", lwd = 2)

# transformations of Y

qqnorm(residuals(lm(log(y2)~x)), pch = 1, frame = FALSE)
qqline(residuals(lm(log(y2)~x)), col = "steelblue", lwd = 2)

qqnorm(residuals(lm(sqrt(y2)~x)), pch = 1, frame = FALSE)
qqline(residuals(lm(sqrt(y2)~x)), col = "steelblue", lwd = 2)