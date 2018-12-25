#Linear Regression Problems with Solutions
#Question 1
#using Library insuranceData for this problem
library(insuranceData)
data("AutoCollision")
names(AutoCollision)
attach(AutoCollision)
summary(AutoCollision)

#building simple linear regression model
claims.fit <- lm(Severity ~ Claim_Count, data = AutoClaims)
summary(claims.fit)
#there is a relationship between claim count and severity since the p-value for the t-stastic is less than 0.05 
#11.37% of the variability in severity is explained using claim count
#the relationship is negative since the coefficient for claim count -0.1724

#making prediction for Severity when claim count is 3
predict(claims.fit, data.frame(Claim_Count=3), interval = "confidence")
predict(claims.fit, data.frame(Claim_Count=3), interval = "prediction")
#prediction is 324.0133 95% confidence interval is (266.45,381.5765) 95% prediction interval is (103.9941,544.0324)

#plotting the response and predictor
plot(Claim_Count, Severity)
abline(claims.fit, col = "red", lwd = 2)

#plotting diagnostic plots
par(mfrow=c(2,2))
plot(claims.fit)
par(mfrow=c(1,1))
plot(predict(claims.fit), rstudent(claims.fit))
#there appears to be an outlier at observation 4 and a few high leverage points


#Question 2
data("AutoBi")
attach(AutoBi)
names(AutoBi)
summary(AutoBi)
AutoBi$SEATBELT <- as.factor(SEATBELT)
AutoBi$ATTORNEY <- as.factor(ATTORNEY)
AutoBi$CLMSEX <- as.factor(CLMSEX)
AutoBi$CLMINSUR <- as.factor(CLMINSUR)
summary(AutoBi)

#create a multiple linear regression model
loss.fit <- lm(LOSS~CLMAGE+SEATBELT+CLMSEX+CLMINSUR+MARITAL+ATTORNEY, data = AutoBi)
summary(loss.fit)
#there is a relationship between the predictors and the response since the p-value of the F-statistic is close to zero
#significant predictors are claimant age, seatbelts, and attorney2
#dropping predictors that are not statistically significant
loss.fit2 <- lm(LOSS~CLMAGE+SEATBELT+ATTORNEY, data = AutoBi)
summary(loss.fit2)
#there should be four equations based on the model

#confidence intervals for coefficients
confint(loss.fit2)

#diagnostic plots
par(mfrow=c(2,2))
plot(loss.fit2)
par(mfrow=c(1,1))
plot(predict(loss.fit2), rstudent(loss.fit2))
#there are a lot of outliers and high leverage points

#tranforming response variable
loss.fit3 <- lm(log(LOSS)~CLMAGE+SEATBELT+ATTORNEY, data = AutoBi)
summary(loss.fit3)

#diagnostic plots
par(mfrow=c(2,2))
plot(loss.fit3)
par(mfrow=c(1,1))
plot(predict(loss.fit3), rstudent(loss.fit3))
#high leverage points not improved but fewer outliers

#Question 3
install.packages("CASdatasets", repos = "http://dutangc.free.fr/pub/RRepos/", type = "source")
library(CASdatasets)
rm(list = ls())
data("asiacomrisk")
asiacomrisk <- na.omit(asiacomrisk)
attach(asiacomrisk)
names(asiacomrisk)
summary(asiacomrisk)

#creating a multiple linear regression model
commloss.fit <- lm(FGU~TIV+DR+CountryStatus, data = asiacomrisk)
summary(commloss.fit)

#creating a multiple linear regression model including interaction term
commloss.fit2 <- lm(FGU~TIV*DR+CountryStatus, data = asiacomrisk)
summary(commloss.fit2)

#removing predictors that are not statistically significant
commloss.fit3 <- lm(FGU~TIV*DR, data = asiacomrisk)
summary(commloss.fit3)

#creating a polynomial regression model
commloss.fit4 <- lm(FGU~poly(DR,3)+CountryStatus, data = asiacomrisk)
summary(commloss.fit4)

#Question 4
#creating simulated data
set.seed(500)
x1 <- rpois(1000, 5)
x2 <- runif(1000, 16, 90)
eps <- rnorm(1000, 0, sd = 400)
y <- 100 + 1000*x1 + 10*x2 + eps
#the value of Beta 0 is 100 and the value of Beta 1 is 1000 and Beta 2 is 10

#linear regression model
sim.fit <- lm(y~x1+x2)
summary(sim.fit)

# increasing variablility
set.seed(500)
epsi <- rnorm(1000, 0, sd = 4000)
yi <- 100 + 1000*x1 + 10*x2 + epsi

#linear regression model
sim.fit2 <- lm(yi~x1+x2)
summary(sim.fit2)

# decreasing variablility
set.seed(500)
epsd <- rnorm(1000, 0, sd = 50)
yd <- 100 + 1000*x1 + 10*x2 + epsd

#linear regression model
sim.fit3 <- lm(yd~x1+x2)
summary(sim.fit3)

#confidence intervals
confint(sim.fit)
confint(sim.fit2)
confint(sim.fit3)

#Classification questions
#Question 5
library(insuranceData)
data("ClaimsLong")
head(ClaimsLong)
dim(ClaimsLong)
names(ClaimsLong)
attach(ClaimsLong)
summary(ClaimsLong)
cor(ClaimsLong)


#creating logistic model using full data set and all predictors besides numclaims
claim.fit <- glm(claim~.-numclaims, data = ClaimsLong, family = binomial)
summary(claim.fit)
#all predictors are statistically significant

#confusion matrix
claim.probs <- predict(claim.fit, type = "response")
claim.pred <- rep(0, length(claim.probs))
claim.pred[claim.probs > 0.5] <- 1
table(claim.pred, claim)
(102870)/length(claim)
mean(claim.pred != claim)

#overall fraction of correct predictions is 0.85725
#test error rate 14.275%
#It appears that the logistic regression model only predicts a claim not occurring.

#changing prediction criteria
claim.pred2 <- rep(0, length(claim.probs))
claim.pred2[claim.probs > 0.15] <- 1
table(claim.pred2, claim)
mean(claim.pred2 != claim)
(5834)/(11296+5834)
#test error rate 35.10%

#creating training data and test data
ClaimsLong.train <- ClaimsLong[1:90000,]
ClaimsLong.test <- ClaimsLong[90001:nrow(ClaimsLong),]
claim.test <- claim[90001:length(claim)]

#fitting a second logistic regression model
claim.fit2 <- glm(claim~.-numclaims, data = ClaimsLong.train, family = binomial)
summary(claim.fit2)

#confusion matrix
claim.probs3 <- predict(claim.fit2, ClaimsLong.test, type = "response")
claim.pred3 <- rep(0, length(claim.probs3))
claim.pred3[claim.probs3 > 0.5] <- 1
table(claim.pred3, claim.test)
(25842)/length(claim.test)
mean(claim.pred3 != claim.test)
#test error rate of 13.86%
#0% of actually claims that were predicted or sensitivity

#new prediction criteria
claim.pred4 <- rep(0, length(claim.probs3))
claim.pred4[claim.probs3 > 0.15] <- 1
table(claim.pred4, claim.test)
(1151+19251)/length(claim.test)
mean(claim.pred4 != claim.test)
1151/(1151+3007)
#test error rate of 31.99%
#Sensitivity of 27.68%

#KNN 
library(class)
train.X <- ClaimsLong.train[,-(5:6)]
test.X <- ClaimsLong.test[,-(5:6)]
train.claim <- claim[1:90000]

#using knn to make predictions k = 1
set.seed(500)
knn.pred <- knn(train.X, test.X, train.claim, k = 1)

#confusion matrix
table(knn.pred, claim.test)
(25837)/length(claim.test)
mean(knn.pred != claim.test)
#test error rate 13.88%
#Sensitivity of 0%

#using knn to make predictions k = 5
set.seed(500)
knn.pred2 <- knn(train.X, test.X, train.claim, k = 5)

#confusion matrix
table(knn.pred2, claim.test)
(25842)/length(claim.test)
mean(knn.pred2 != claim.test)
#test error rate 13.86%
#Sensitivity of 0%

#Best models in terms of error rate are logistic with 0.5 decision threshold to predict a claim and KNN with K = 3. Based on sensitivity, logistic with decision threshold of 0.15 is best


#Question 6
#using insuranceData package
data("dataCar")
head(dataCar)
dim(dataCar)
names(dataCar)
attach(dataCar)
summary(dataCar)
dataCar$veh_age <- as.factor(veh_age)
dataCar$agecat <- as.factor(agecat)
dataCar$clm <- as.factor(clm)
summary(dataCar)

#creating training data and test data
dataCar.train <- dataCar[1:50000,]
dataCar.test <- dataCar[50001:nrow(dataCar),]
clm.test <- clm[50001:length(clm)]

#logistic regression model
clm.fit <- glm(clm~veh_value+exposure+veh_age+gender+area+agecat, data = dataCar.train, family = binomial)
summary(clm.fit)

#reduced logistic regression model
clm.fit2 <- glm(clm~veh_value+exposure+agecat, data = dataCar.train, family = binomial)
summary(clm.fit2)

#confusion matrix
clm.probs <- predict(clm.fit2, dataCar.test, type = "response")
clm.pred <- rep(0, length(clm.probs))
clm.pred[clm.probs > 0.5] <- 1
table(clm.pred, clm.test)
(16540)/length(clm.test)
mean(clm.pred != clm.test)
#test error rate of 7.37%
#0% of actually claims that were predicted or sensitivity

#new prediction criteria
clm.pred2 <- rep(0, length(clm.probs))
clm.pred2[clm.probs > 0.15] <- 1
table(clm.pred2, clm.test)
(15728+111)/length(clm.test)
mean(clm.pred2 != clm.test)
111/(1204+111)
#test error rate of 11.30%
#Sensitivity of 8.44%

#KNN 
library(class)
train.X <- dataCar.train[,c(1,2,10)]
test.X <- dataCar.test[,c(1,2,10)]
train.clm <- clm[1:50000]

#using knn to make predictions k = 1
set.seed(500)
knn.pred <- knn(train.X, test.X, train.clm, k = 1)

#confusion matrix
table(knn.pred, clm.test)
(15283+123)/length(clm.test)
mean(knn.pred != clm.test)
123/(1192+123)
#test error rate 13.72%
#Sensitivity of 9.35%

#using knn to make predictions k = 3
set.seed(500)
knn.pred2 <- knn(train.X, test.X, train.clm, k = 3)

#confusion matrix
table(knn.pred2, clm.test)
(16230+37)/length(clm.test)
mean(knn.pred2 != clm.test)
37/(1278+37)
#test error rate 8.90%
#Sensitivity of 2.81%

#lowest test error rate is the original logistic model
#the highest sensitivity is the KNN with k=1

#Question 7
library(CASdatasets)
data("ustermlife")
