
##############################################################################################################
############   PREDICTING CUSTOMER LIFETIME VALUE BY USING MACHINE LEARNING ALGORITHMS   #####################
##############################################################################################################

############################ XG BOOST #################################################################

#install.packages('Metrics')
#install.packages('xgboost')
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(Metrics)
library(Matrix)
library(caTools)

# Cannot use the old dataset for XGBoost and hence reading the data again.
setwd("F:\\Deepak\\EPABA - IIMA\\Project")

customerDataSet <- read.csv("Project_modeled_Data.csv")

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


############  MANAGING OUTLIERS  ######################

miniDataSet$Customer.Lifetime.Value <- OutlierManagement(miniDataSet$Customer.Lifetime.Value)

#######################################################

#Splitting the dataset

set.seed(101)
split <- sample.split(miniDataSet, SplitRatio = 0.75)
xgtraindataset <- subset(miniDataSet, split == TRUE)
xgTestDataSet <- subset(miniDataSet, split == FALSE)

#str(xgtraindataset)
#str(xgTestDataSet)

# Standardizing the Train Dataset
xgtrainingDataset <- sparse.model.matrix(Customer.Lifetime.Value ~., data = xgtraindataset)
head(xgtrainingDataset)
xgtrain_label <- xgtraindataset[,"Customer.Lifetime.Value"]
training_matrix <- xgb.DMatrix(data = as.matrix(xgtrainingDataset), label = xgtrain_label)

#Standardizing the Test Dataset
xgtestingDataSet <- sparse.model.matrix(Customer.Lifetime.Value ~., data = xgTestDataSet)
head(xgtestingDataSet)
xgtest_label <- xgTestDataSet[,"Customer.Lifetime.Value"]
testing_matrix <- xgb.DMatrix(data = as.matrix(xgtestingDataSet), label = xgtest_label)

#Standardizing minidataset for complete CLV Prediction and Model Validation
xgminiDataSet <- sparse.model.matrix(Customer.Lifetime.Value ~., data = miniDataSet)
head(xgminiDataSet)
xgmini_label <- miniDataSet[,"Customer.Lifetime.Value"]
minidataset_matrix <- xgb.DMatrix(data = as.matrix(xgminiDataSet), label = xgmini_label)

# Updating the Parameters
xgb_params <- list("objective" = "reg:linear", "eval_metric" = "rmse" , "verbose" = 1 )
watchlist <- list(train = training_matrix, test = testing_matrix)

# Applying eXtreme Gradient Boost Model
xgboostmodel <- xgb.train(params = xgb_params, data = training_matrix, watchlist = watchlist, 
                          nround = 100, eta = 0.1, max.depth = 6 )


# Checking the importance of the variables
ValidateImportance <- xgb.importance(feature_names = names(training_matrix), model = xgboostmodel)
print(ValidateImportance)

# plotting to find the Important Variables for predicting Customer Life Time Value
xgb.plot.importance(importance_matrix = ValidateImportance)
#xgb.plot.tree(model = ValidateImportance )


################ Working with Important Variables ##############################################

minidataimp <- customerDataSet[,c(12,10,8,21,20,22,23,5,11,15 )]
str(minidataimp)
summary(minidataimp)

############  MANAGING OUTLIERS  ######################

minidataimp$Customer.Lifetime.Value <- OutlierManagement(minidataimp$Customer.Lifetime.Value)

#######################################################

#Splitting the dataset
library(caTools)


set.seed(101)
split <- sample.split(minidataimp, SplitRatio = 0.75)
xgtraindataset <- subset(minidataimp, split == TRUE)
xgTestDataSet <- subset(minidataimp, split == FALSE)

#str(xgtraindataset)
#str(xgTestDataSet)

# Standardizing the Train Dataset
xgtrainingDataset <- sparse.model.matrix(Customer.Lifetime.Value ~., data = xgtraindataset)
head(xgtrainingDataset)
xgtrain_label <- xgtraindataset[,"Customer.Lifetime.Value"]
training_matrix <- xgb.DMatrix(data = as.matrix(xgtrainingDataset), label = xgtrain_label)

#Standardizing the Test Dataset
xgtestingDataSet <- sparse.model.matrix(Customer.Lifetime.Value ~., data = xgTestDataSet)
head(xgtestingDataSet)
xgtest_label <- xgTestDataSet[,"Customer.Lifetime.Value"]
testing_matrix <- xgb.DMatrix(data = as.matrix(xgtestingDataSet), label = xgtest_label)

#Standardizing minidataimp for complete CLV Prediction and Model Validation
xgminiDataSet <- sparse.model.matrix(Customer.Lifetime.Value ~., data = minidataimp)
head(xgminiDataSet)
xgmini_label <- minidataimp[,"Customer.Lifetime.Value"]
minidataset_matrix <- xgb.DMatrix(data = as.matrix(xgminiDataSet), label = xgmini_label)

# Updating the Parameters
xgb_params <- list("objective" = "reg:linear", "eval_metric" = "rmse" , "verbose" = 1 )
watchlist <- list(train = training_matrix, test = testing_matrix)

# Applying eXtreme Gradient Boost Model
xgboostmodel <- xgb.train(params = xgb_params, data = training_matrix, watchlist = watchlist, 
                          nround = 100, eta = 0.1, max.depth = 6 )


# Predicting the Values from Test Matrix
# Exporting the data to Excel for calculating R^Sqaured
# Calculating the R Sqaured values for Test Matrix We get
# R Squared value : ranging between 69.11% to 79.78% for Training and Test data set.

pred_xgboost <- predict(xgboostmodel, newdata = testing_matrix)
write.csv(pred_xgboost, "CLVPredictedValues.csv")
write.csv(xgTestDataSet, "xgTestDataSet.csv")

# Calculating RMSE for Predicted Values
rmse(xgTestDataSet$Customer.Lifetime.Value, pred_xgboost)
postResample(xgTestDataSet$Customer.Lifetime.Value, pred_xgboost)

# RMSE     Rsquared          MAE 
# 2702.0871193    0.7771907 1256.8595801

#Predicting for the entire dataset
pred_xgboostfull <- predict(xgboostmodel, newdata = minidataset_matrix)
write.csv(pred_xgboostfull, "pred_xgboostfull.csv")

# Calculating RMSE
rmse( miniDataSet$Customer.Lifetime.Value, pred_xgboostfull)
postResample(miniDataSet$Customer.Lifetime.Value, pred_xgboostfull)

############ WITH OUTLIERS ########
# RMSE     Rsquared          MAE 
# 3102.4602138    0.7978388 1354.5685411
# R Squared Value : 79.78%

########### WITHOUT OUTLIERS #############

# RMSE     Rsquared          MAE 
# 2271.3391328    0.8402656 1047.6633110
# R Squared Value : 84.02%

###########################################

# Plotting the values to look at the Residuals.

ggplot(xgTestDataSet, aes(xgTestDataSet$Customer.Lifetime.Value, pred_xgboost, color = 'red')) +
  geom_point(color = 'red') + geom_abline(color = 'blue') + 
  ggtitle('Plotting Predicted Customer Lifetime Values') +
  xlab('Actual Values') + ylab('Predicted values')


###### XG BOOST MODEL IS STABLE AND CAN BE USED FOR BETTER CUSTOMER LIFETIME VALUE PREDICTION #######
