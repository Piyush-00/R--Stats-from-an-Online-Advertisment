social <- read.csv("social2.csv")

#Describing the Dataset

head(social)
str(social)
colSums(is.na(social))

library(psych)
describe(social)

User.ID <- social[,1]
Gender <- social[,2]
Age <- social[,3]
Purchased <- social[,4]

#Using Data Visualization

library(ggplot2)
ggplot(social, aes(Purchased, fill=Gender)) + geom_bar() + facet_grid(.~Gender)

#Creating the Logistic Regression model

logit <- glm(Purchased ~ Gender + Age, family=binomial)
summary(logit)

#Recreating the model, with significant variables

logit2 <- glm(Purchased ~ Age, family=binomial)
summary(logit2)

#Comparing the two Models

AIC(logit, logit2)
BIC(logit, logit2)

#Getting the coefficients and standard deviations of the variables

library(arm)
se.coef(logit2)

b0 <- logit2$coeff[1]
b1 <- logit2$coeff[2]

se.b0 <- se.coef(logit2)[1]
se.b1 <- se.coef(logit2)[2]

#Predicting Probabilities

logit2.prob <- predict(logit2, type = "response")
logit2.prob

#Predicting Odds

odds.pred <- logit2$fitted.values/(1-logit2$fitted.values)
odds.pred

#Calculating the odds ratio

exp(b1)

#Calculating the 95% COnfidence Interval

LB.95 <- b1 - qnorm(0.975,0,1)*se.b1
UB.95 <- b1 + qnorm(0.975,0,1)*se.b1
CI.95 <- c(LB.95, UB.95)  
CI.95

#Calculating Odds ratio for 95% Confidence Intervals

exp(CI.95) 

#Plotting the Logistic Regression Curve
x.Age <- seq(min(Age), max(Age), 0.05)
prob.pur <- predict(logit2, data.frame(Age = x.Age), type = "response")
plot(Age, Purchased, xlab = "Estimated Age", ylab = "Estimated Probability of a Successful Purchase")
lines(x.Age, prob.pur, col = "red", lwd = 2)

#Pearson Residual
r <- residuals(logit2,"pearson")
r
qqnorm(r)
qqline(r, col="red") 

#DEviance Residuals
dev.res <- residuals(logit2,type="deviance")
qqnorm(dev.res)
qqline(dev.res)

#Likelihood Ratio Test (LRT)
G <- logit2$null.deviance-logit2$deviance
G
df <- logit2$df.null-logit2$df.residual
pchisq(G, df, lower=FALSE)

#Hosmer Lemeshow Goodness-of-fit Test
library(ResourceSelection)
hoslem.test(logit2$y, fitted(logit2), g=5)


