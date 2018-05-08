install.packages("forecast")
library(forecast)
getwd()
setwd("C:/Users/Akshay/Documents/")

ts_honey <-  read.csv("Honeywell.csv")


ts_honey <- ts_honey[c(-1,-3)]

#ts_honey <- ts(ts_honey)

ts_honey

plot.ts(ts_honey)

mod1 <- HoltWinters(ts_honey, alpha=0.15, beta=FALSE, gamma=FALSE)
mod2 <- HoltWinters(ts_honey, alpha=0.35, beta=FALSE, gamma=FALSE)

mod3 <- HoltWinters(ts_honey, alpha=0.55, beta=FALSE, gamma=FALSE)
mod4 <- HoltWinters(ts_honey, alpha=0.75, beta=FALSE, gamma=FALSE)
modauto <- HoltWinters(ts_honey,  beta=FALSE, gamma=FALSE)
modauto
mod4$fitted
temp1 <- mod1$fitted
temp2 <- mod2$fitted
temp3 <- mod3$fitted
temp4 <- mod4$fitted
tempauto <- modauto$fitted

temp <- as.data.frame(temp)

mean((ts_honey[-1,] - temp1[,1]))^2
mean((ts_honey[-1,] - temp2[,1]))^2
mean((ts_honey[-1,] - temp3[,1]))^2
mean((ts_honey[-1,] - temp4[,1]))^2
mean((ts_honey[-1,] - tempauto[,1]))^2


mod1$SSE
mod2$SSE
mod3$SSE
mod4$SSE
modauto$SSE

modforecast <- forecast(mod4, h = 4)
modforecast
plot(modforecast)

###Part 2
ts_honey <-  read.csv("Honeywell.csv")

ts_honey <- ts_honey[c(-1,-3)]


mod1 <- HoltWinters(ts_honey, alpha=0.75, beta=0.15, gamma=FALSE)
mod2 <- HoltWinters(ts_honey, alpha=0.75, beta=0.25, gamma=FALSE)

mod3 <- HoltWinters(ts_honey, alpha=0.75, beta=0.45, gamma=FALSE)
mod4 <- HoltWinters(ts_honey, alpha=0.75, beta=0.85, gamma=FALSE)
modauto <- HoltWinters(ts_honey,   gamma=FALSE)

temp1 <- mod1$fitted
temp2 <- mod2$fitted
temp3 <- mod3$fitted
temp4 <- mod4$fitted
tempauto <- modauto$fitted

temp4
temp <- as.data.frame(temp)
ts_honey[-1,]
mean((ts_honey[c(-1,-2),] - temp1[,1]))^2
mean((ts_honey[c(-1,-2),] - temp2[,1]))^2
mean((ts_honey[c(-1,-2),] - temp3[,1]))^2
mean((ts_honey[c(-1,-2),] - temp4[,1]))^2
mean((ts_honey[c(-1,-2),] - tempauto[,1]))^2

ts_honey
mod1$SSE
mod2$SSE
mod3$SSE
mod4$SSE
modauto$SSE

modforecast <- forecast(mod4, h = 4)
modforecast
plot(modforecast)



#mean((training.data - predict(training.model))^2)

####Part 3
ts_honey <-  read.csv("Honeywell.csv")
ts_honey <- ts_honey[c(-1,-3)]
num <- seq(1:124)
ts_honey[,2] <- num
temp <- ts_honey[,1] 
ts_honey[,1] <- ts_honey[,2]
ts_honey[,2] <- temp

#View(ts_honey)

df <- lm(ts_honey$V2 ~ ts_honey$Close)
View(ts_honey)
plot(df)


library(ggplot2)

ggplot(ts_honey,aes(x=V2,y=Close)) + geom_point(aes(color = Close)) + scale_colour_gradient(high='red',low = "blue") + geom_smooth(method='lm',formula=y~x)


summary(df)
new <- data.frame(x = seq(124, 247, 1))
new

prediction <- predict(df, newdata = new,se.fit = TRUE)
prediction$fit

mean((ts_honey[,2] - prediction$fit)^2)

sse <- sum(df$residuals^2)

sse



summary(df)

X <- resid(df)

hist(X)
View(X)

#Plotting probability plot
qqnorm(X) 
qqline(X)
#Performing the chi square test

X <- X^2
X <- sqrt(X)

chisq <- chisq.test(X) 
chisq 
chisq$observed

ts_honey <-  read.csv("Honeywell.csv")
View(ts_honey)
ts_honey
plot(ts_honey$Date,X, type = "p")
prediction$fit
plot(X,prediction$fit, type = "p")
abline(df)

#04/16/2018	147.39	148.13	146.44	146.75	2,565,472


