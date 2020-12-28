ccd = read.csv('BankCreditCard.csv')

str(ccd)
summary(ccd)


                        # Data Cleaning


# Straight off the bat we will remove the Customer Id column from this data
ccd = subset(ccd, select = -Customer.ID)


# Now we look into the other features
summary(ccd$Marital)
barplot(table(ccd$Marital))


# We see some categorical variables are in integer data type.
# We will convert them into factor.
# We have to convert the target variable 'Default Payment' to factor as well
cols = c('Marital','Gender','Academic_Qualification', 'Default_Payment')
ccd[cols] = lapply(ccd[cols], as.factor)


summary(ccd$Gender)
summary(ccd$Academic_Qualification)







# Need to check for correlation among independent variables
# To do so, we need to convert all integer types to numeric
library(dplyr)
ccd = ccd %>% mutate_if(is.integer, as.numeric)
str(ccd)

ccd_numeric = ccd[sapply(ccd, is.numeric)]
cor(ccd_numeric)
str(ccd_numeric)

ccd_scaled = as.data.frame(scale(ccd_numeric))
cor(ccd_scaled)

# Upon observing the data, we see we can perform feature reduction
# We can add the amounts from Jan to June for Repayment Status, 
# Bill Amount and Previous Payment

ccd$Total_Repayment_Default = 
    ccd$Repayment_Status_Jan+ 
    ccd$Repayment_Status_Feb+
    ccd$Repayment_Status_March+
    ccd$Repayment_Status_April+
    ccd$Repayment_Status_May+
    ccd$Repayment_Status_June

    
ccd$Total_Bill_Amount = 
    ccd$Jan_Bill_Amount+
    ccd$Feb_Bill_Amount+
    ccd$March_Bill_Amount+
    ccd$April_Bill_Amount+
    ccd$May_Bill_Amount+
    ccd$June_Bill_Amount


ccd$Total_Previous_Payment = 
    ccd$Previous_Payment_Jan+
    ccd$Previous_Payment_Feb+
    ccd$Previous_Payment_March+
    ccd$Previous_Payment_April+
    ccd$Previous_Payment_May+
    ccd$Previous_Payment_June


str(ccd)

ccd = ccd[,c(1:5,24:27)]
str(ccd)

ccd_numeric = ccd[sapply(ccd, is.numeric)]
cor(ccd_numeric)
str(ccd_numeric)
# We notice there is no significant multi-collinearity between the variables.


                    # Exploratory Data Analysis


plot(ccd$Credit_Amount, ccd$Default_Payment, col = ccd$Default_Payment)

boxplot(ccd$Credit_Amount~ccd$Default_Payment, horizontal = T, col = ccd$Default_Payment)

newplot = function(x)
{
    boxplot(ccd[,x]~ccd$Default_Payment, 
            horizontal = T, 
            col = 'maroon',
            xlab = x,
            ylab = 'Default Payment')
    
    
}

newplot('Total_Repayment_Default')

newplot('Total_Bill_Amount')

newplot('Total_Previous_Payment')


                    # Performing Logistic Regression


# Splitting the data

library(caTools)
set.seed(1234)
sample = sample.split(ccd, 0.8)
train = subset(ccd, sample==T)
test = subset(ccd, sample==F)


                        # Logistic Regression

log_model = glm(Default_Payment~.,family=binomial(),train)
summary(log_model)

log_preds = predict(log_model,test, type = "response")

log_preds = ifelse(log_preds>=0.5,'1','0')
log_preds = as.factor(log_preds)

library(caret)
confusionMatrix(test$Default_Payment,log_preds)
# Accuracy of 80% is very poor.


opt_model = step(log_model, direction = 'both')
summary(opt_model)

opt_preds = predict(opt_model,test, type = "response")

opt_preds = ifelse(opt_preds>=0.5,'1','0')
opt_preds = as.factor(opt_preds)

confusionMatrix(test$Default_Payment,opt_preds)


                        # Decision Tree


library(rpart)    
library(rpart.plot)
dt_model = rpart(Default_Payment~.,test,method = 'class')

test$dt_preds = predict(dt_model, test, type = 'class')

confusionMatrix(test$Default_Payment,test$dt_preds)
# Accuracy still at 80%

rpart.plot(dt_model)


                        # Naive Bayes Model


library(e1071)
NB_model = naiveBayes(Default_Payment~.,data=train)
NB_preds = predict(NB_model, test, type = 'class')

confusionMatrix(test$Default_Payment, NB_preds)
# Accuracy still at 80%


                        # Conclusion

print("We have successfully created a Credit Card Default Prediction algorithm with 80% accuracy")
print("The model is ready for deployment.")
