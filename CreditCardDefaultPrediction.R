setwd('G:/Imarticus/DSP_Online_2/Project2BankCreditCardDefaultPrediction/Dataset')

ccd = read.csv('BankCreditCard.csv')

summary(ccd)
str(ccd)

# Straight off the bat we will remove the Customer Id column from this data

ccd = subset(ccd, select = -Customer.ID)
str(ccd)

# We see some categories that are in integer datatype.
# We will convert them into factor
ccd$Gender = as.factor(ccd$Gender)
ccd$Academic_Qualification = as.factor(ccd$Academic_Qualification)
ccd$Marital = as.factor(ccd$Marital)

summary(ccd$Gender)
summary(ccd$Academic_Qualification)

# In Academic Qualification, category 6 is 'unknown'
# We will remove that group from the dataset to avoid confusion
ccd =subset(ccd, ccd$Academic_Qualification!='6')


summary(ccd$Marital)
barplot(table(ccd$Marital))
# We notice there are some observations with category as 0.
# This category is not part of the data description.
# Hence, as an anomaly, this must be removed.

str(ccd)

# For now, we have converted factor=0 to factor=3.
ccd$Marital[ccd$Marital=="0"] = 3

# We have to convert the y variable to factor as well
ccd$Default_Payment = as.factor(ccd$Default_Payment)

# Need to check for correlation among independent variables
# To do so, we need to convert all integer types to numeric

library(dplyr)
ccd = ccd %>% mutate_if(is.integer, as.numeric)
str(ccd)

ccd_numeric = ccd[sapply(ccd, is.numeric)]
str(ccd_numeric)

ccd_numeric.scaled = as.data.frame(scale(ccd_numeric))

cor(ccd_numeric)

corrplot::corrplot(ccd_numeric.scaled, is.corr = T)

library(ggplot2)

library(ggcorrplot)
ggcorrplot(ccd_numeric)

# Performing Logistic Regression
# Splitting the data

library(caTools)
set.seed(1234)
sample = sample.split(ccd, 0.8)
train = subset(ccd, sample==T)
test = subset(ccd, sample==F)

# Training the model using logistic regression

log_model = glm(Default_Payment~., family=binomial(),train)
summary(log_model)

log_preds = predict(log_model,test, type = "response")

log_preds = ifelse(log_preds>=0.5,'1','0')
log_preds = as.factor(log_preds)

library(caret)
confusionMatrix(test$Default_Payment,log_preds)



opt_model = step(log_model, direction = 'both')
summary(opt_model)

opt_preds = predict(opt_model,test, type = "response")

opt_preds = ifelse(opt_preds>=0.5,'1','0')
opt_preds = as.factor(opt_preds)


confusionMatrix(test$Default_Payment,opt_preds)




# Decision Tree

# Training the model using Decision Tree
library(rpart)    
library(rpart.plot)
dt_model = rpart(Default_Payment~.,test,method = 'class')

test$dt_preds = predict(dt_model, test, type = 'class')
confusionMatrix(test$Default_Payment,test$dt_preds)
rpart.plot(dt_model)


# Naive Bayes Model
library(e1071)
NB_model = naiveBayes(Default_Payment~.,data=train)
NB_preds = predict(NB_model, test, type = 'class')
confusionMatrix(test$Default_Payment, NB_preds)



