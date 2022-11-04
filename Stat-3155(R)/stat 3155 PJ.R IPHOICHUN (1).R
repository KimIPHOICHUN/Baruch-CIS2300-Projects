#https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset#

bike=read.csv("BikeSharing.csv")
dim(bike); head(bike)

head(bike)
# temp as x, cnt as y
latest = lm(cnt ~ temp, data = bike); summary(latest)

#plot
plot(cnt ~ temp, data=bike)
abline(latest,col='red')
# Coefficient of determination Rsquare
summary(latest)$r.squared

# transformation data for x and y(sqrt)
latest_for_trans = lm(sqrt(cnt)~temp,data=bike); summary(latest_for_trans)

#plot
plot(sqrt(cnt)~temp, data=bike)
abline(latest_for_trans,col='red')

# Coefficient of determination Rsquare
summary(latest_for_trans)$r.squared


#State the fitted regression line
latest_for_trans = lm(sqrt(cnt) ~ temp, data = bike);summary(latest_for_trans) #se =6.192, r2 = 0.1729
latest_for_trans$coefficients
#plot
plot(sqrt(cnt)~temp, data = bike)
abline(latest_for_trans,col='red')



#Confindnece level
confint(latest_for_trans,level=0.95)

# Test whether B1 is different from zero or not
#large t stats and small p value
# p < 0.05
#it is significant 
#reject null hypothesis
#B1 is different from 0

#8 predict
new.x = seq(0.02,1,by=.2)
predict(latest_for_trans,newdata = data.frame(temp = new.x),level=0.90,interval = "confidence")
lines(new.x,ci.90[,2],col = "blue")
lines(new.x,ci.90[,3],col = "blue")

#9Construct a residual plot
# Residual plot versus predicted value
plot(latest_for_trans$residuals ~ latest_for_trans$fitted.values)
qqnorm(latest_for_trans$residuals,main = "Residuals"); qqline(latest_for_trans$residuals)



bike=read.csv("BikeSharing.csv")
dim(bike); head(bike)


#############PART 2
#####model 1
#Fit regression equation (hum,wind,temp)
model1 = lm(cnt ~ temp + hum + windspeed, data = BikeSharing); summary(model1)
# cnt = 189.15 + 369.55temp - 306.91hum + 45.17 windspeed

# ANOVA table
anova(model1)
anova(update(model1,~1),model1)
#SST = 68120548 Df = 1999
#SSE = 49583667 Df = 1996
#SSR = 18536880 Df = 3

# F critical value given alpha=0.05; Model 1
qf(0.95,3,nrow(bike)-3-1)
# p-value of F test; Model 3
1-pf(248.74, 3,nrow(bike)-3-1)
