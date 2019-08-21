#################################################################################################################
############   PREDICTING CUSTOMER LIFETIME VALUE BY USING MACHINE LEARNING ALGORITHMS   ################
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


############################## Random Forest Model ##############################################

library(randomForest)
library(caret)

# After multiple retries, reducing the number of trees to 20 and mtry = 5
randomforestfitting <- randomForest(Customer.Lifetime.Value ~ ., trainingData, ntree = 20, mtry = 5)
summary(randomforestfitting)

# Predicting Training data with all variables.
randomforestpredictionfulltrain <- predict(randomforestfitting, newdata = trainingData)

# Checking R Sqaured value 
R2 <- 1- (sum((trainingData$Customer.Lifetime.Value-randomforestpredictionfulltrain)^2) / sum((trainingData$Customer.Lifetime.Value - mean(trainingData$Customer.Lifetime.Value))^2))
print(R2 * 100) # 91.6743 %

#Checking RMSE
caret::RMSE(randomforestpredictionfulltrain, trainingData$Customer.Lifetime.Value) # RMSE : 1996.349

# Checking / Validating Variable Importance
varImp = importance(randomforestfitting)
varImpPlot(randomforestfitting)


#Fitting the model using only the Important Variables.
randomforestfittingwithimp <- randomForest(Customer.Lifetime.Value ~ Number.of.Policies + Monthly.Premium.Auto + 
                                             Vehicle.Class + Total.Claim.Amount + Months.Since.Last.Claim + Months.Since.Policy.Inception +
                                             Income + Policy + State + Education + Sales.Channel, trainingData, ntree = 20, mtry = 5
)

# Predicting on Train Data
randomForestpredictionTrain <- predict(randomforestfittingwithimp, newdata = trainingData)
summary(randomForestpredictionTrain)

# Checking R Sqaured value 
R2 <- 1- (sum((trainingData$Customer.Lifetime.Value-randomForestpredictionTrain)^2) / sum((trainingData$Customer.Lifetime.Value - mean(trainingData$Customer.Lifetime.Value))^2))
print(R2 * 100) # R Squared Value :- 92.83247 %


# Predicting for Test Data
randomForestpredictionTest <- predict(randomforestfittingwithimp, newdata =  testData)
summary(randomForestpredictionTest)

#Calculating R Squared for Test data
R2 <- 1- (sum((testData$Customer.Lifetime.Value-randomForestpredictionTest)^2) / sum((testData$Customer.Lifetime.Value - mean(testData$Customer.Lifetime.Value))^2))
print(R2 * 100) # R Squared Value : 71.52612

testData$predictedvalues <- randomForestpredictionTest
write.csv(testData, 'RandomForestpredictedvalues.csv')

# plotting Actual v/s Predicted Values for Training Data Set
ggplot(trainingData, aes(trainingData$Customer.Lifetime.Value, randomForestpredictionTrain, color = 'red')) +
  geom_point(color = 'red') + geom_abline(color = 'blue') + 
  ggtitle('Plotting Predicted Customer Lifetime Values') +
  xlab('Actual Values') + ylab('Predicted values')

# plotting Actual v/s Predicted Values for Test Data Set
ggplot(testData, aes(testData$Customer.Lifetime.Value, randomForestpredictionTest, color = 'red')) +
  geom_point(color = 'red') + geom_abline(color = 'blue') + 
  ggtitle('Plotting Predicted Customer Lifetime Values') +
  xlab('Actual Values') + ylab('Predicted values')
