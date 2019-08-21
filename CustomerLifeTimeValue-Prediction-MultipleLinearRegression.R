
#################################################################################################################
############   PREDICTING CUSTOMER LIFETIME VALUE BY USING VARIOUS MACHINE LEARNING ALGORITHMS   ################
#################################################################################################################

# Required Library Packages and installation
# install.packages("magrittr") 
# install.packages("dplyr")
# install.packages('gglplot2')
# install.packages("MASS")
library(magrittr) 
library(dplyr) 
library(ggplot2)
library(MASS)

#Setting the Working Directory and Reading the Data
setwd("")
customerDataSet <- read.csv("Project_Modeled_Data.csv")
str(customerDataSet)
summary(customerDataSet)

#Choosing the Necessary Data Set ( Excluding the Customer ID ) 
miniDataSet <- customerDataSet[,2:23]
str(miniDataSet)
summary(miniDataSet)

#################################### Exploratory Data Analysis ########################################################

# DATA HAS BEEN CHECKED AND VALIDATED FOR ANY NULLS or MISSING VALUES 
# Checking for missing values
colSums(sapply(miniDataSet, is.na))

# Validing the Income Range. 35.8% of the population have more than $50,000 Income
# 35.822% of population are having more than 50,000 Income
sum((miniDataSet[, 'Income',] >= 50000)/ dim(miniDataSet)[1])*100 

# Plotting to Understand Income against Education
ggplot(data=miniDataSet,aes(Income, color= 'red')) +
  geom_density()  +
  facet_wrap(~Education) + 
  theme(axis.text.x = element_text(angle = 90, color = 'blue' ))

# Plotting to Understand Income against Vehicle Class  
ggplot(data=miniDataSet,aes(Income, color= 'red')) +
  geom_density()  +
  facet_wrap(~Vehicle.Class) + 
  theme(axis.text.x = element_text(angle = 90, color = 'blue' ))

# Plotting to Understand Total Claim Amount against Vehicle Size
ggplot(data=miniDataSet,aes(Total.Claim.Amount, color= 'red')) +
  geom_density()  +
  facet_wrap(~Vehicle.Size) + 
  theme(axis.text.x = element_text(angle = 90, color = 'blue' ))

# Plotting to Understand Total Claim Amount against Vehicle Class
ggplot(data=miniDataSet,aes(Total.Claim.Amount, color= 'red')) +
  geom_density()  +
  facet_wrap(~Vehicle.Class) + 
  theme(axis.text.x = element_text(angle = 90, color = 'blue' ))

ggplot(data=miniDataSet,aes(Income, color= 'red')) +
  geom_density()  +
  facet_wrap(~Marital.Status) + 
  theme(axis.text.x = element_text(angle = 90, color = 'blue' ))

miniDataSet %>% select(State, Income) %>% ggplot(aes(factor(State), Income)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('State')

miniDataSet %>% select(Coverage, Income) %>% ggplot(aes(factor(Coverage), Income)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Coverage')

miniDataSet %>% select(Education, Income) %>% ggplot(aes(factor(Education), Income)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Education')

miniDataSet %>% select(Education, Customer.Lifetime.Value) %>% ggplot(aes(factor(Education), Customer.Lifetime.Value)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Education')

miniDataSet %>% select(Coverage, Customer.Lifetime.Value) %>% ggplot(aes(factor(Coverage), Customer.Lifetime.Value)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Coverage')

miniDataSet %>% select(Vehicle.Class, Customer.Lifetime.Value) %>% ggplot(aes(factor(Vehicle.Class), Customer.Lifetime.Value)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Vehicle.Class')

miniDataSet %>% select(State, Total.Claim.Amount) %>% ggplot(aes(factor(State), Total.Claim.Amount)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('State')

miniDataSet %>% select(State, Customer.Lifetime.Value) %>% ggplot(aes(factor(State), Customer.Lifetime.Value)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('State')

miniDataSet %>% select(State, Monthly.Premium.Auto) %>% ggplot(aes(factor(State), Monthly.Premium.Auto)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('State')

miniDataSet %>% select(Vehicle.Size, Coverage, Income) %>% filter(Vehicle.Size == c('Medsize', 'Large', 'Small')) %>% 
  arrange(Coverage) %>% group_by(Vehicle.Size, Coverage) %>% summarize(Count = n()) %>% ggplot(aes(Coverage, Count)) + 
  geom_bar(aes(fill = Vehicle.Size), position = 'dodge', stat = 'identity') + 
  theme_classic() +theme(axis.text.x = element_text(angle = 90, hjust =1))

miniDataSet %>% select(Vehicle.Class, Coverage, Customer.Lifetime.Value) %>% filter(Vehicle.Class == c('Two-Door Car','Four-Door Car','SUV','Luxury SUV','Luxury Car','Sports Car')) %>% 
  arrange(Coverage) %>% group_by(Vehicle.Class, Coverage) %>% summarize(Count = n()) %>% ggplot(aes(Coverage, Count)) + 
  geom_bar(aes(fill = Vehicle.Class), position = 'dodge', stat = 'identity') + 
  theme_classic() +theme(axis.text.x = element_text(angle = 90, hjust =1))

## Before Managing Outlier
boxplot(miniDataSet$Customer.Lifetime.Value, data = miniDataSet , xlab = "Customer LifeTime Value")

ggplot(miniDataSet, aes(x=Customer.Lifetime.Value)) + geom_histogram(bins = 20, col = 'blue') + theme_classic()
ggplot(miniDataSet, aes(x=Income)) + geom_histogram(bins = 30, col = 'white') + theme_classic()
ggplot(miniDataSet, aes(x=Monthly.Premium.Auto)) + geom_histogram(bins = 30, col = 'white') + theme_classic()
ggplot(miniDataSet, aes(x=Months.Since.Policy.Inception)) + geom_histogram(bins = 30, col = 'green') + theme_classic()


################################### DATA PREPROCESSING   #################################################################

############  MANAGING OUTLIERS  ######################

OutlierManagement <- function(x){
  quantiles <- quantile( x, c(.00, .97 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
miniDataSet$Customer.Lifetime.Value <- OutlierManagement(miniDataSet$Customer.Lifetime.Value)
#write.csv(customerDataSet$Customer.Lifetime.Value, "outliers1.csv")
# min(miniDataSet$Customer.Lifetime.Value) - 1898.008
# max(miniDataSet$Customer.Lifetime.Value) - 26199.54

## After Managing Outlier
boxplot(miniDataSet$Customer.Lifetime.Value, data = miniDataSet , xlab = "Customer LifeTime Value")


############ Factoring the Data ######################

# miniDataSet$State = as.factor(miniDataSet$State)
# miniDataSet$Gender = as.factor(miniDataSet$Gender)
# miniDataSet$Vehicle.Size = as.factor(miniDataSet$Vehicle.Size)
# miniDataSet$Policy = as.factor(miniDataSet$Policy)
# miniDataSet$Coverage = as.factor(miniDataSet$Coverage)
# miniDataSet$EmploymentStatus = as.factor(miniDataSet$EmploymentStatus)
# miniDataSet$Response = as.factor(miniDataSet$Response)
# miniDataSet$Education = as.factor(miniDataSet$Education)
# miniDataSet$Location.Code = as.factor(miniDataSet$Location.Code)
# miniDataSet$Marital.Status = as.factor(miniDataSet$Marital.Status)
# miniDataSet$Policy.Type = as.factor(miniDataSet$Policy.Type)
# miniDataSet$Renew.Offer.Type = as.factor(miniDataSet$Renew.Offer.Type)
# miniDataSet$Sales.Channel = as.factor(miniDataSet$Sales.Channel)
# miniDataSet$Vehicle.Class =as.factor(miniDataSet$Vehicle.Class)
str(miniDataSet)


miniDataSet$State = factor(miniDataSet$State,
                           levels = c('Washington','Arizona','Nevada','Oregon','California'),
                           labels = c(1, 2, 3, 4, 5))

miniDataSet$Gender = factor(miniDataSet$Gender,
                            levels = c('M', 'F'),
                            labels = c(1,2))

miniDataSet$Vehicle.Size = factor(miniDataSet$Vehicle.Size,
                                  levels = c('Large','Medsize','Small'),
                                  labels = c(1,2,3))

miniDataSet$Policy = factor(miniDataSet$Policy,
                            levels = c('Corporate L1','Corporate L2','Corporate L3','Personal L1','Personal L2' ,'Personal L3', 'Special L1', 'Special L2', 'Special L3'),
                            labels = c(1,2,3,4,5,6,7,8,9))

miniDataSet$Coverage = factor(miniDataSet$Coverage,
                              levels = c('Basic','Extended','Premium'),
                              labels = c(1,2,3))

miniDataSet$EmploymentStatus = factor(miniDataSet$EmploymentStatus, 
                                      levels = c('Employed','Unemployed','Disabled','Medical Leave','Retired'),
                                      labels = c(1,2,3,4,5))

miniDataSet$Response = factor(miniDataSet$Response,
                              levels = c('No','Yes'),
                              labels = c(1,2))

miniDataSet$Education = factor(miniDataSet$Education,
                               levels = c('High School or Below', 'Bachelor','College', 'Master', 'Doctor' ),
                               labels = c(1,2,3,4,5))

miniDataSet$Location.Code = factor(miniDataSet$Location.Code,
                                   levels = c('Rural', 'Suburban', 'Urban'),
                                   labels = c(1,2,3))

miniDataSet$Marital.Status = factor(miniDataSet$Marital.Status,
                                    levels = c('Single','Married','Divorced'),
                                    labels = c(1,2,3))

miniDataSet$Policy.Type = factor(miniDataSet$Policy.Type,
                                 levels = c('Corporate Auto', 'Personal Auto', 'Special Auto'),
                                 labels = c(1,2,3))

miniDataSet$Renew.Offer.Type = factor(miniDataSet$Renew.Offer.Type,
                                      levels = c('Offer1','Offer2','Offer3','Offer4'),
                                      labels = c(1,2,3,4))

miniDataSet$Sales.Channel = factor(miniDataSet$Sales.Channel,
                                   levels = c('Agent', 'Call Center', 'Branch','Web'),
                                   labels = c(1,2,3,4))

miniDataSet$Vehicle.Class = factor(miniDataSet$Vehicle.Class,
                                   levels = c('Two-Door Car','Four-Door Car','SUV','Luxury Car','Luxury SUV', 'Sports Car'),
                                   labels = c(1,2,3,4,5,6))

############  Splitting the Dataset into trainingData  and testData ###########################

library(caTools)
set.seed(101)
split = sample.split(miniDataSet$Customer.Lifetime.Value, SplitRatio = 0.75)
trainingData = subset(miniDataSet, split == TRUE)
testData = subset(miniDataSet, split == FALSE )

########### BUILDING / VALIDATING MULTIPLE LINEAR REGRESSION MODEL FOR PREDICTING CLV ###################

# Validating the Appropriate attributes with P-Value > 0.05 ( Backward Elimination )
# Transforming the Response Variable to Predict Customer Lifetime Value
# Validating the Linear Regression for (Target Variable) ^2
library(MASS)
Regressor <- lm(formula = (Customer.Lifetime.Value)^2 ~ ., 
                data = trainingData)
summary(Regressor)
stepAIC(Regressor)


StepRegressor <- lm(formula = (Customer.Lifetime.Value)^2 ~ Coverage + EmploymentStatus + 
                      Monthly.Premium.Auto + Number.of.Open.Complaints + Number.of.Policies + 
                      Renew.Offer.Type + Response, data = trainingData)
summary(StepRegressor)
extractAIC(StepRegressor)  

# Residual standard error: 142300000 on 6836 degrees of freedom
# Multiple R-squared:  0.118,	Adjusted R-squared:  0.1163 
# F-statistic: 70.36 on 13 and 6836 DF,  p-value: < 2.2e-16
# AIC  = 266020.5

# Validating the Linear Regression for log(Target Variable)
Regressor <- lm(formula = log(Customer.Lifetime.Value) ~ ., 
                data = trainingData)
summary(Regressor)
stepAIC(Regressor)

StepRegressor <- lm(formula = log(Customer.Lifetime.Value) ~ Coverage + EmploymentStatus + 
                      Marital.Status + Monthly.Premium.Auto + Number.of.Open.Complaints + 
                      Number.of.Policies + Renew.Offer.Type + Vehicle.Class + Policy, 
                    data = trainingData)
summary(StepRegressor)
extractAIC(StepRegressor)

# Residual standard error: 0.5459 on 6822 degrees of freedom
# Multiple R-squared:  0.2576,	Adjusted R-squared:  0.2547 
# F-statistic: 87.69 on 27 and 6822 DF,  p-value: < 2.2e-16
# AIC = -7803.597

# Validating the Linear Regression for sqrt(Target Variable)
Regressor <- lm(formula = sqrt(Customer.Lifetime.Value) ~ ., 
                data = trainingData)
summary(Regressor)
stepAIC(Regressor)

StepRegressor <- lm(formula = sqrt(Customer.Lifetime.Value) ~ EmploymentStatus + 
                      Marital.Status + Monthly.Premium.Auto + Number.of.Open.Complaints + 
                      Number.of.Policies + Renew.Offer.Type + Vehicle.Class + Policy + 
                      Sales.Channel, data = trainingData)
summary(StepRegressor)
extractAIC(StepRegressor)

# Residual standard error: 25.21 on 6821 degrees of freedom
# Multiple R-squared:  0.2118,	Adjusted R-squared:  0.2085 
# F-statistic: 65.44 on 28 and 6821 DF,  p-value: < 2.2e-16
# AIC : 45450.53

# Validating Linear Regression for 1/(Target Variable)

Regressor <- lm(formula = 1/Customer.Lifetime.Value ~ ., 
                data = trainingData)
summary(Regressor)
stepAIC(Regressor)


#### BASED ON THE OUTPUT FROM BACKWARD ELIMINATION and fitting the model with identified significant variables ####

StepRegressor <- lm(formula = 1/Customer.Lifetime.Value ~ Location.Code + Education + 
                      Coverage + EmploymentStatus + Marital.Status + Monthly.Premium.Auto + 
                      Number.of.Open.Complaints + Number.of.Policies + Renew.Offer.Type + 
                      Vehicle.Class, data = trainingData)
summary(StepRegressor)
extractAIC(StepRegressor)

# Residual standard error: 8.954e-05 on 6824 degrees of freedom
# Multiple R-squared:  0.3332,	Adjusted R-squared:  0.3308 
# F-statistic: 136.4 on 25 and 6824 DF,  p-value: < 2.2e-16
# AIC : -127624

# PREDICTING CUSTOMER LIFETIME VALUE FOR THE TEST DATASET
# Preparing the Test Data Set by removing CustomerLifeTime Value column.

PredictedValue = predict(StepRegressor, newdata = testData)
summary(PredictedValue)

# Validating the Predicted Values and Converting / Transforming the Predicted values
PredictedValuecheck <- 1/PredictedValue
summary(PredictedValuecheck)
write.csv(PredictedValuecheck, "MLRPredictedValueCheck.csv")

# plotting the values to validate the data
ggplot(testData, aes(testData$Customer.Lifetime.Value, PredictedValuecheck, color = 'red')) +
  geom_point(color = 'red') + geom_abline(color = 'blue') + 
  ggtitle('Plotting Predicted Customer Lifetime Values') +
  xlab('Actual Values') + ylab('Predicted values')

#Validating random values to check the accuracy of prediction.

# Random check on the Prediction - Actual Value = 7328.977
# pred = predict(StepRegressor, data.frame(
#     Location.Code = 'Suburban',  Education = 'Master',
#     EmploymentStatus = 'Employed' ,
#     Marital.Status = 'Married',
#     Coverage = 'Extended' ,
#     Renew.Offer.Type = 'Offer2',
#     Vehicle.Class = 'Four-Door Car',
#     Number.of.Policies = 8,
#     Monthly.Premium.Auto = 91 ,
#     Number.of.Open.Complaints = 0
#   ))
# pred = 1/pred # Predicted value = 8849.627














