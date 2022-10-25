

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
