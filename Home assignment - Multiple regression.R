#### Multiple regression ####

# load packages #
library(psych) # for describe
library(lm.beta) # for lm.beta
library(car) # for scatter3d
library(ggplot2) # for ggplot
library(rgl) # for scatter3d 

#Check the dataset
View(home_sample_1)

describe(home_sample_1)
describe(home_sample_1$sex)
describe(home_sample_1$age)

#OBS - fault ditected, max in age 222.00
#Correct this so that the stat will not be missleading 
#clean data 
home_sample_1_cleaned = home_sample_1
home_sample_1_cleaned[home_sample_1_cleaned[, "age"] == "22", "age"] = 22
home_sample_1_cleaned[, "age"] = as.numeric(as.character(home_sample_1_cleaned[, "age"]))
home_sample_1_cleaned[home_sample_1_cleaned[, "age" ] == 222, "age"] = 22 
describe(home_sample_1_cleaned[, "age"])

# histograms
hist(home_sample_1_cleaned$pain, breaks = 30)
hist(home_sample_1_cleaned$sex, breaks = 30)
hist(home_sample_1_cleaned$age, breaks = 30)

# scatterplots
plot(pain ~ age, data = home_sample_1_cleaned)
plot(pain ~ sex, data = home_sample_1_cleaned)

table(home_sample_1_cleaned$sex)
plot(home_sample_1_cleaned$pain ~ home_sample_1_cleaned$sex)

mod1 = lm(pain ~ sex + age, data = home_sample_1_cleaned)
summary(mod1)

#Variable sex is automatically labelled as "Sexmale" by R
#Look at the estimated mean of the female
mean(home_sample_1_cleaned[home_sample_1_cleaned$sex=="female",]$pain)
#Recieved a value of 4.794587
#Is this value somehow connected to the intercept?

#Conclusion: The man/male is the baseline 

plot(pain ~ age, data = home_sample_1_cleaned)
abline(lm(pain ~ age, data = home_sample_1_cleaned))

plot(pain ~ sex, data = home_sample_1_cleaned)
abline(lm(pain ~ sex, data = home_sample_1_cleaned))

summary(mod1)

require(lm.beta)

AIC(mod1)
confint(mod1)
lm.beta(mod1)

mod2 = lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = home_sample_1_cleaned)

summary(mod2)

#### Hierarchical regression ####

#Check the dataset
View(home_sample_1_cleaned)

summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared
#the variance explained has increased 

#Analysis of variance 
anova(mod1, mod2)

AIC(mod1)
AIC(mod2)

plot(mod2)

#### Model diagnostics #####

#lodain packages 
library(psych) # for describe
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(lmtest) # bptest
library(sandwich)

#Check the dataset
View(home_sample_1_cleaned)

#Identifying outliers
mod1 = lm(pain ~ sex + age, data = home_sample_1_cleaned)
plot(home_sample_1_cleaned$pain ~ home_sample_1_cleaned$age)

#Identifying outliers with high leverage
plot(home_sample_1_cleaned$pain ~ home_sample_1_cleaned$age)
abline(lm(pain ~ age, data = home_sample_1_cleaned))

plot(home_sample_1_cleaned$pain ~ home_sample_1_cleaned$sex)
abline(lm(pain ~ sex, data = home_sample_1_cleaned))

#residuals VS levrage 
plot(mod2, which = 5)

#Cook´s distance 
plot(mod2, which = 4)

# Check Normality
plot(mod2, which = 2)

#histogram 
hist(residuals(mod2), breaks = 20)

# skew and kurtosis
describe(residuals(mod2))

#Check Linearity
residualPlots(mod2)

#Check Homoscedasticty
plot(mod2, which = 3)
ncvTest(mod2) # NCV test
bptest(mod2) # Breush-Pagan test

#Check Multicollinearity
vif(mod2)

lm.beta(mod2)
confint(mod2)

require(lsr)
standardCoefs(mod1)
standardCoefs(mod2)

## Model selection ##
##Comparing model performance on the training set and the test set

rand_vars = as.data.frame(matrix(rnorm(mean = 0, sd = 1, n = 50 *nrow(home_sample_1_cleaned)), ncol = 50))
home_sample_1_cleaned_withrandomvars = cbind(home_sample_1_cleaned, rand_vars)

training_set = home_sample_1_cleaned_withrandomvars[1:80, ] # training set, using half of the data
test_set = home_sample_1_cleaned_withrandomvars[81:160, ] # test set, the other half of the dataset

mod1_train <- lm(pain ~ age + sex, data = training_set)
mod1_rand_train <- lm(pain ~ age + sex + V1 +
V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 +
V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 +
V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 +
V33 + V34 + V35 + V36 + V37 + V38 + V39 + V40 + V41 + V42 +
V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50, data = training_set)

summary(mod1_train)
summary(mod1_rand_train)

pred_train <- predict(mod1_train)
pred_train_rand <- predict(mod1_rand_train)
RSS_train = sum((training_set[, "pain"] - pred_train)^2)
RSS_train_rand = sum((training_set[, "pain"] - pred_train_rand)^2)
RSS_train

summary(mod1_train)$adj.r.squared
summary(mod1_rand_train)$adj.r.squared

AIC(mod1_train)
AIC(mod1_rand_train)

anova(mod1_train, mod1_rand_train)

#Backward regression 
mod_back_train = step(mod1_rand_train, direction = "backward")

anova(mod1_train, mod_back_train)

summary(mod1_train)$adj.r.squared
summary(mod_back_train)$adj.r.squared

AIC(mod1_train)
AIC(mod_back_train)

#Testing performance on the test set
# calculate predicted values
pred_test <- predict(mod1_train, test_set)
pred_test_back <- predict(mod_back_train, test_set)

#calculate the sum of squared residuals
RSS_test = sum((test_set[, "pain"] - pred_test)^2)
RSS_test_back = sum((test_set[, "pain"] - pred_test_back)^2)
RSS_test
RSS_test_back

#This test reveals that the backward regression model has more error than the model only using sex and age.


