#############################################################
## Create combined machine learning comparison 
## original author - ferratlauric@gmail.com - September 2018
## adapted by Anita Lynam - g26482@hotmail.co.uk - July 2019
#############################################################

#####################################################################################################
# useful web links
# https://www.r-project.org/conferences/useR-2013/Tutorials/kuhn/user_caret_2up.pdf
# https://medium.com/all-things-ai/in-depth-parameter-tuning-for-gradient-boosting-3363992e9bae
# https://stackoverflow.com/questions/15613332/using-caret-package-to-find-optimal-parameters-of-gbm
# https://topepo.github.io/caret/available-models.html
# https://astro.temple.edu/~msobel/courses_files/StochasticBoosting(gradient).pdf
# https://topepo.github.io/caret/random-hyperparameter-search.html
# https://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf
# https://topepo.github.io/caret/variable-importance.html 
# https://cran.r-project.org/web/packages/gbm/vignettes/gbm.pdf
# https://topepo.github.io/caret/variable-importance.html
# https://www.rdocumentation.org/packages/caret/versions/6.0-81/topics/varImp
# https://www.rdocumentation.org/packages/caret/versions/6.0-81/topics/train
#####################################################################################################


##############################
# install libraries
# install.packages("recipes")
# install.packages("caret")
# install.packages("DMwR")
# install.packages("kernlab")
# install.packages("randomForest")
# install.packages("pROC")
# install.packages("gbm")
# install.packages("gbm")
# install.packages("purrr")
# install.packages("PRROC")
# remotes::install_github("ddsjoberg/dca")
# install.packages("corrplot")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("Rcpp")
# install.packages("rlang")
# install.packages("readstata13")
##############################
#Load libraries
library(recipes)
library(caret)
library(knitr)
library(kernlab)
library(DMwR)
library(randomForest)
library(pROC)	
library(ggplot2)
library(readstata13)
library(rpart)
library(rpart.plot)
library(gbm)
library(gridExtra)
library(dplyr) # for data manipulation
library(tidyr)
library(purrr) # for functional programming (map)
library(pROC) # for Precision-Recall curve calculations
library(PRROC) # for Precision-Recall curve calculations
library(corrplot)
library(Rcpp)
library(rlang)
library(dca)
# export to word office
library(readxl)
library(officer) 
library(rvg)
library(flextable)
library(cowplot)

library(compareGroups)
############################## 
# 1 - Source file 
##############################
setwd("/Users/laf225/Desktop/AnitaML")


#name the data files
dataFile3 = "GADModelV2.dta"
dataFile3Bis <- "updatedDare.xlsx"
dataFile4 = "YDXValidationGADModelData.dta"

#load Stata datasets
data_train = read.dta13(dataFile3)
updatedDare <- read_excel(dataFile3Bis)
data_train <-  data_train %>% left_join(updatedDare, by = c("ID" = "Patient ID")) 


dataset_val =  read.dta13(dataFile4)
YDX_lipid_BP_data <- read_excel("YDX lipid_BP data.xlsx")
data_train <- data_train %>% dplyr::rename(Fasting_Glucose =fastingGlucose,
                                        Fasting_HDL = HDLChol,
                                        Fasting_Triglycerides = Triglycerides,
                                        Fasting_Cholesterol = TotalChol)
dataset_val <- dataset_val %>% left_join(YDX_lipid_BP_data, by = c("Subject_UID" = "Subject_UID")) 

X2005 <- read_excel("2005.xls") %>% select("n", "PatientID", "SystolicBP", "DiastolicBP", "TotalChol", "HDLChol", "HDLRatio", "LDL", "Trigs", "specimendate", "chol", "trig", "hdl" )
X2012 <- read_excel("2012.xls") %>% select("n", "PatientID", "SystolicBP", "DiastolicBP", "TotalChol", "HDLChol", "HDLRatio", "LDL", "Trigs", "specimendate", "chol", "trig", "hdl" )
X2014 <- read_excel("2014.xls") %>% select("n", "PatientID", "SystolicBP", "DiastolicBP", "TotalChol", "HDLChol", "HDLRatio", "LDL", "Trigs", "specimendate", "chol", "trig", "hdl" )
X2016 <- read_excel("2016.xls") %>% select("n", "PatientID", "SystolicBP", "DiastolicBP", "TotalChol", "HDLChol", "HDLRatio", "LDL", "Trigs", "specimendate", "chol", "trig", "hdl" )

NHSdata <- rbind(X2005,X2012,X2014,X2016)
dim(NHSdata)

NHSdata$specimendate <- as.Date(NHSdata$specimendate,"%d/%m/%Y")
data_train_t <- data_train %>% left_join(NHSdata, by = c("ID" = "PatientID")) %>% select(c("ID","insulinRequire","bmi","AgeatDiagnosis","GADPositive975","sexMale","Fasting_Cholesterol","Fasting_HDL","Fasting_Triglycerides", "TotalChol", "HDLChol",'DateSeen', "specimendate", "chol", "trig", "hdl")) 

head(data_train_t)

# detach(package:plyr)
data_train_t_new <- data_train_t %>%
  dplyr::arrange(ID,specimendate) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::filter((DateSeen + 1 * 365) >= specimendate) %>%
  mutate(Fasting_Cholesterol =  dplyr::last(na.omit(chol))) %>% 
  mutate(Fasting_HDL =  dplyr::last(na.omit(hdl))) %>%
  mutate(Fasting_Triglycerides = dplyr::last(na.omit(trig))) %>% 
  slice(n()) %>% 
  ungroup()
# View(data_train_t_new )
dim(data_train_t_new)


data_train_r <- data_train %>% left_join(data_train_t_new, by = c("ID" = "ID")) %>% 
  mutate(Fasting_Cholesterol = case_when (
    !is.na(Fasting_Cholesterol.x) ~ Fasting_Cholesterol.x,
    !is.na(Fasting_Cholesterol.y) ~ Fasting_Cholesterol.y,
    TRUE ~ na_dbl),
    Fasting_HDL = case_when (
      !is.na(Fasting_HDL.x) ~ Fasting_HDL.x,
      !is.na(Fasting_HDL.y) ~ Fasting_HDL.y,
      TRUE ~ na_dbl),
    Fasting_Triglycerides = case_when (
      !is.na(Fasting_Triglycerides.x) ~ Fasting_Triglycerides.x,
      !is.na(Fasting_Triglycerides.y) ~ Fasting_Triglycerides.y,
      TRUE ~ na_dbl))
dim(data_train_r)

myvars = c("insulinRequire.x","bmi.x","AgeatDiagnosis.x","GADPositive975.x","sexMale.x","Fasting_Cholesterol","Fasting_HDL","Fasting_Triglycerides")  

data_train_r <- data_train_r[data_train_r$Fasting_Triglycerides<25,] # remove one observation with one fasting_HDL equal to 51.2
data_train_r <- data_train_r[data_train_r$ID != "3-4-457",] # too many problems for this patient in NHS data
data_train_r = data_train_r %>%  select(myvars)

colMeans(is.na(data_train_r))

names(data_train_r) <-  c("insulinRequire","bmi","AgeatDiagnosis","GADPositive975","sexMale","Fasting_Cholesterol","Fasting_HDL","Fasting_Triglycerides")  
#create a datset containing a subset of variables to include in the model
myvars = c("insulinRequire","bmi","AgeatDiagnosis","GADPositive975")  
myvars = c("insulinRequire","bmi","AgeatDiagnosis","GADPositive975","sexMale","Fasting_Cholesterol","Fasting_HDL","Fasting_Triglycerides")  
data_train = data_train_r[myvars]
dataset_val = dataset_val[myvars]


summary(data_train)
summary(dataset_val)
dim(data_train)
dim(dataset_val)
# deletion ofobservations with na
dataset_val <- dataset_val[dataset_val$Fasting_HDL<20,] # remove one observation with one fasting_HDL equal to 104

colSums(is.na(data_train))
colSums(is.na(dataset_val))
data_train <- data_train %>% drop_na()
dataset_val <- dataset_val %>% drop_na()
dim(data_train)
dim(dataset_val)
summary(data_train)
summary(dataset_val)


library(DataExplorer)
# DataExplorer::create_report(data_train, y = "insulinRequire")
# DataExplorer::create_report(dataset_val, y = "insulinRequire")
data1 <-  dataset_val
data2 <- data_train
data1$name <- "YDX"
data2$name <- "DARE"
dataall <- rbind(data1,data2)
dataall$insulinRequire <- factor(dataall$insulinRequire)
dataall$sexMale <- factor(dataall$sexMale)
dataall$GADPositive975 <- factor(dataall$GADPositive975)
# DataExplorer::create_report(dataall, y = "name")

# data_temp <- dataset_val
# dataset_val <- data_train
# data_train <- data_temp
##############################
# 2 - Set up the model training 
##############################

#set seed for reproductivity
seedchoice <- 7 

# Data need to be put in a good shape to be used in the caret framework:
# factorise string data and mumeric data which need to be factorised
# no missing data (always possible to impute when it is not the case)

#identify factor variables and view the levels
is.fact2 = sapply(data_train, is.factor)
factors2.df <- data_train[, is.fact2]
lapply(factors2.df, levels)

is.fact3 = sapply(dataset_val, is.factor)
factors3.df <- dataset_val[, is.fact3]
lapply(factors3.df, levels)

#amend class to factor for training and validation datasets
variable_factors <- c("insulinRequire","GADPositive975","sexMale")
data_train <- data_train %>%  mutate_at(variable_factors, factor)
dataset_val = dataset_val %>%  mutate_at(variable_factors, factor)


#rename the levels of the facor variable. This is required to run the training models for each of the five imputed datasets
feature6.names = names(data_train)
for (f in feature6.names) {
  if (class(data_train[[f]]) == "factor") {
    levels6 <- unique(c(data_train[[f]]))
    data_train[[f]] <- factor(data_train[[f]],
                                labels = make.names(levels6))
  }
}


feature7.names = names(dataset_val)
for (f in feature7.names) {
  if (class(dataset_val[[f]]) == "factor") {
    levels7 <- unique(c(dataset_val[[f]]))
    dataset_val[[f]] <- factor(dataset_val[[f]],
                               labels = make.names(levels7))
  }
}



#create standardised variables for the continuous variables
# model formulas
# formula.model <- formula(insulinRequire ~ bmi + AgeatDiagnosis + GADPositive975)

formula.model <- formula(insulinRequire ~ bmi + AgeatDiagnosis + GADPositive975 + sexMale + Fasting_Cholesterol + Fasting_HDL + Fasting_Triglycerides)
Youroutcomevariable <- as.character(formula.model)[2]

# standardized data sets
standardized <-recipe(formula.model , data = data_train) %>% 
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

trained_rec <- prep(standardized, training = data_train)
data_train <- bake(trained_rec, new_data = data_train)
dataset_val  <- bake(trained_rec, new_data = dataset_val)



# prepare training scheme ####################################################################################

#routines, fits each model and calculates a resampling based performance measure.
# The traincontrol function controls the computational nuances of the train function
# repeatedcv (repeated cross validation) method is a resampling method Control that creates multiple versions 
# of the folds and aggregates the results
# number is the k number of folds for the repeatedcv
# repeats is the number of complete sets of folds to compute
# The summmary funcion is a function to compute performance metrics across resamples. 
# twoClassSummary computes sensitivity, specificity and the area under the ROC curve
# sampling is the type of additional sampling that is conducted after resampling 
# (usually to resolve class imbalances). 
# SMOTE (Chawla et. al. 2002) is a well-known algorithm to fight unbalanced classification problem. 
# The general idea of this method is to artificially generate new examples of the minority class using 
# the nearest neighbors of these cases. Furthermore, the majority class examples are also under-sampled, 
# leading to a more balanced dataset.
sampling <- NULL

# sampling = "smote"
#for use in default and grid search optimised models
control <- trainControl(method = "repeatedcv", number = 10, repeats = 5,classProbs = TRUE,summaryFunction = twoClassSummary, sampling = sampling, savePredictions = TRUE)
#for use in random search optimised models
control_Rand_Search <- trainControl(method = "repeatedcv", number = 10, repeats = 5,classProbs = TRUE,summaryFunction = twoClassSummary, sampling = sampling, savePredictions = TRUE, search = "random")
##########################################################################

##############################
# 3 - Train the models 
##############################

# The train function sets up a grid of tuning parameters for a number of classification and regression 
# ROC  used to select the optimal model using the largest value.


# train the Gradient bootstrap Machine model (Stochastic Gradient Boosting)
# utils::browseVignettes("gbm")
# verbose is an argument of the gmb package, indicating whether or not to print out progress and 
# performance indicators



#build all the Gbm (Stochastic gradient boosting model) models
#tuning parameters: n.trees (number of  iterations), interaction depth (complexity), shrinkage (learning rate), n.minobsinnode (min number of training det damples in a node to commence splitting)
#learning rate shrinks the contribution of each tree by learning_rate

getModelInfo()$gbm$parameters
#Shrinkage: the smaller the number, the better the predictive value, the more trees required, and the more computational cost.
#the smaller the shrinkage, the more trees you should have

# Fetch max Value for interaction.depth 
floor(sqrt(NCOL(data_train)))
#set up the grid
gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 7, 10),
                        n.trees = c(10, 50,100,200), 
                        shrinkage = seq(from = 0.01, to = 0.1, by = 0.01),
                        n.minobsinnode = c(5,20,50,100,200)) 

#tune the hyper-parameters using  Grid Search
set.seed(seedchoice)
modelGbm_CC_GADA_Lr <- train(formula.model, data = data_train, method = "gbm", trControl = control, verbose = FALSE,metric = 'ROC',tuneGrid=gbmGrid)
#random search Independently draws from a uniform density from the same configuration space as would be spanned by a regular grid,
#we do not use random hyperparameter search for gbm models as it may be inefficients 


# train the SVM model
# Support Vector Machines with Radial Basis Function Kernel (SVM classifier using a non-linear kernel)
#RBF is a reasonable first choice, it can handle nonlinear relationships
#C is the penalty parameter of the error term. It controls the trade off between smooth decision boundary (small c) and classifying the training points correctly.
#larger values of C focus attention more on (correctly classified) points near the decision boundary (wiggly boundary), while smaller values involve data further away (wider margins).
# sigma the radius/spread/decision boundary of the kernel
#When gamma is low, the 'curve' of the decision boundary is very low and thus the decision region is very broad. 
#When gamma is high, the 'curve' of the decision boundary is high, which creates islands of decision-boundaries around data points.

#using training dataset and default parameters
getModelInfo()$svmRadial$parameters
svmGrid <-  expand.grid(sigma = c(0.0001,0.001,0.01, 0.1, 1, 10, 100),
                        C = seq(from = 0.1, to = 2, by = 0.1)) 

#using training dataset and tune the hyper-parameters using Caret Grid Search
set.seed(seedchoice)
modelSvm_CC_GADA_Lr <- train(formula.model, data = data_train, method = "svmRadial", trControl = control, verbose = TRUE,metric = 'ROC',tuneGrid=svmGrid)


# train the Random forest model
#parameter mtry is the number of variables available for splitting at each tree node
#The default is the square root of the number of predictor variables (rounded down)
#as we are only using three variables we do not optimise the parameters
#For mtry refer to http://code.env.duke.edu/projects/mget/export/HEAD/MGET/Trunk/PythonPackage/dist/TracOnlineDocumentation/Documentation/ArcGISReference/RandomForestModel.FitToArcGISTable.html

#using training dataset and default parameters
getModelInfo()$rf$parameters

rfGrid <-  expand.grid(mtry = seq(from = 1, to = 5, by = 1))
set.seed(seedchoice)
modelRf_CC_GADA <- train(formula.model, data = data_train, method = 'rf', trControl = control,metric = 'ROC', tuneGrid = rfGrid)


# train a logistic regression model
#using training dataset
#there are no tuning parameters for glm method within caret
set.seed(seedchoice)
modelLG_CC_GADA <- train(formula.model, data = data_train, method = "glm", family = "binomial", trControl = control,metric = 'ROC')


# train neural network
getModelInfo()$nnet$parameters
#size parameter is the number of units in hidden layer (nnet fit a single hidden layer neural network) 
#decay parameter is the regularization parameter to avoid over-fitting
nnetGrid <-  expand.grid(size = seq(from = 2, to = 10, by = 1),
                         decay = c(1,0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001, 0.0000001))

#tune the hyper-parameters using Caret Grid Search
set.seed(seedchoice)
modelnnet_CC_GADA_Lr <- train(formula.model, data = data_train, method = "nnet", trControl = control,metric = 'ROC', tuneGrid = nnetGrid)


# train a k-nearest-neighbours
#based on euclidean distance
getModelInfo()$knn$parameters
#k parameter is the number of neighbours. 
knnGrid <-  expand.grid(k = seq(from = 1, to = 200, by = 1))
#tune the hyper-parameters using Caret Grid Search
set.seed(seedchoice)
modelknn_CC_GADA_Lr <- train(formula.model, data = data_train, method = "knn", trControl = control,metric = 'ROC', tuneGrid = knnGrid)


getModelInfo()$earth$parameters
#Shrinkage: the smaller the number, the better the predictive value, the more trees required, and the more computational cost.
#the smaller the shrinkage, the more trees you should have


#set up the grid
# create a tuning grid
marsgrid <- expand.grid(
  degree = 1:3, 
  nprune = seq(1, 100, length.out = 100) %>% floor()
)

#tune the hyper-parameters using  Grid Search
set.seed(seedchoice)
modelMARS_CC_GADA_Lr <- train(formula.model, data = data_train, method = "earth", glm=list(family='binomial'),trControl = control,,metric = 'ROC',tuneGrid=marsgrid)
# view the final models##############################################################################
summary(modelknn_CC_GADA_Lr)
summary(modelnnet_CC_GADA_Lr) 
summary(modelLG_CC_GADA)
summary(modelRf_CC_GADA)
summary(modelSvm_CC_GADA_Lr)
summary(modelGbm_CC_GADA_Lr)
summary(modelMARS_CC_GADA_Lr)
######################################################

# collect resamples
#compare the models for the CC existing GADA model (hyperparameter grid search) for comparison
#no grid search for RF or LG models
results_grid_CC_GADA <- resamples(list(LogisticRegression = modelLG_CC_GADA, StochasticGradientBoosting = modelGbm_CC_GADA_Lr, SupportVectorMachine = modelSvm_CC_GADA_Lr, NeuralNetwork = modelnnet_CC_GADA_Lr,RandomForest = modelRf_CC_GADA,
                                       KNearestNeighbours = modelknn_CC_GADA_Lr, logisticregressionMARS = modelMARS_CC_GADA_Lr))
summary(results_grid_CC_GADA)
#check character string for the performance measure used to sort or computing the between-model correlations
results_grid_CC_GADA$metrics
#visualizing resampling results across models
xyplot(results_grid_CC_GADA, what = "BlandAltman")

# boxplots of results and save as pdf
pdf(paste0("bwplot_sampling", sampling, "formula_",as.character(formula.model)[3],".pdf"))
bwplot(results_grid_CC_GADA)
dev.off()

# dot plots of results (includes 95% CI)
# average performance value (with two-sided confidence limits) for each model
pdf(paste0("dotplot_sampling", sampling, "formula_",as.character(formula.model)[3],".pdf"))
dotplot(results_grid_CC_GADA)
dev.off()

#trellis scatterplot of results
pdf(paste0("splom_sampling", sampling, "formula_",as.character(formula.model)[3],".pdf"))
splom(results_grid_CC_GADA)
dev.off()

#test for a difference in the average resampled area under the ROC curve
diffs <- diff(results_grid_CC_GADA, metric = "ROC")
summary(diffs)

#calculate the 95% CI for the resampling ROC AUC
test  <- results_grid_CC_GADA$values

m = mean(test$`RandomForest~ROC`)
s = sd(test$`RandomForest~ROC`)
l = length(test$`RandomForest~ROC`)
m+c(-1.96,1.96)*s/sqrt(length(l))


m = mean(test$`LogisticRegression~ROC`)
s = sd(test$`LogisticRegression~ROC`)
l = length(test$`LogisticRegression~ROC`)
m + (c(-1.96,1.96)*(s/sqrt(length(l))))

m = mean(test$`StochasticGradientBoosting~ROC`)
s = sd(test$`StochasticGradientBoosting~ROC`)
l = length(test$`StochasticGradientBoosting~ROC`)
m + (c(-1.96,1.96)*(s/sqrt(length(l))))

m = mean(test$`SupportVectorMachine~ROC`)
s = sd(test$`SupportVectorMachine~ROC`)
l = length(test$`SupportVectorMachine~ROC`)
m + (c(-1.96,1.96)*(s/sqrt(length(l))))

m = mean(test$`NeuralNetwork~ROC`)
s = sd(test$`NeuralNetwork~ROC`)
l = length(test$`NeuralNetwork~ROC`)
m + (c(-1.96,1.96)*(s/sqrt(length(l))))

m = mean(test$`KNearestNeighbours~ROC`)
s = sd(test$`KNearestNeighbours~ROC`)
l = length(test$`KNearestNeighbours~ROC`)
m + (c(-1.96,1.96)*(s/sqrt(length(l))))

m = mean(test$`logisticregressionMARS~ROC`)
s = sd(test$`logisticregressionMARS~ROC`)
l = length(test$`logisticregressionMARS~ROC`)
m + (c(-1.96,1.96)*(s/sqrt(length(l))))
#################################
# 4 - Perform external validation 
#################################

#for the Gbm grid search model (validation)
probsTestGbmGridVal = predict(modelGbm_CC_GADA_Lr, newdata=dataset_val, type = "prob")
dataset_val =data.frame(dataset_val,probsTestGbmGridVal$X2)
predTestGbmGridVal = log(as.numeric(probsTestGbmGridVal$X2)/(1-as.numeric(probsTestGbmGridVal$X2)))
#then create a roc object and calculate the ROC on the validation dataset
roc_objTestGbmGridval <- roc(dataset_val[,Youroutcomevariable], predTestGbmGridVal)
AUC_objTestGbmGridval = auc(roc_objTestGbmGridval)
AUC_objTestGbmGridval
ci.auc(roc_objTestGbmGridval)


#for the Svm grid search model (validation)
probsTestSvmGridVal = predict(modelSvm_CC_GADA_Lr, newdata=dataset_val, type = "prob")
dataset_val =data.frame(dataset_val,probsTestSvmGridVal$X2)
predTestSvmGridVal = log(as.numeric(probsTestSvmGridVal$X2)/(1-as.numeric(probsTestSvmGridVal$X2)))
#then create a roc object and calculate the ROC on the validation dataset
roc_objTestSvmGridval <- roc(dataset_val[,Youroutcomevariable], predTestSvmGridVal)
AUC_objTestSvmGridval = auc(roc_objTestSvmGridval)
AUC_objTestSvmGridval
ci.auc(roc_objTestSvmGridval)

#for the knn grid search model (validation)
probsTestknnGridVal = predict(modelknn_CC_GADA_Lr, newdata=dataset_val, type = "prob")
dataset_val =data.frame(dataset_val,probsTestknnGridVal$X2)
probsTestknnGridVal$X2[probsTestknnGridVal$X2 == 1] <- 0.999999
probsTestknnGridVal$X2[probsTestknnGridVal$X2 == 0] <- 0.000001
predTestknnGridVal = log(as.numeric(probsTestknnGridVal$X2)/(1-as.numeric(probsTestknnGridVal$X2)))
#then create a roc object and calculate the ROC on the validation dataset
roc_objTestknnGridval <- roc(dataset_val[,Youroutcomevariable], predTestknnGridVal)
AUC_objTestknnGridval = auc(roc_objTestknnGridval)
AUC_objTestknnGridval
ci.auc(roc_objTestknnGridval)

#for the nnet grid search model (validation)
probsTestnnetGridVal = predict(modelnnet_CC_GADA_Lr, newdata=dataset_val, type = "prob")
dataset_val =data.frame(dataset_val,probsTestnnetGridVal$X2)
predTestnnetGridVal = log(as.numeric(probsTestnnetGridVal$X2)/(1-as.numeric(probsTestnnetGridVal$X2)))
#then create a roc object and calculate the ROC on the validation dataset
roc_objTestnnetGridval <- roc(dataset_val[,Youroutcomevariable], predTestnnetGridVal)
AUC_objTestnnetGridval = auc(roc_objTestnnetGridval)
AUC_objTestnnetGridval
ci.auc(roc_objTestnnetGridval)

#for the rf model (validation)
probsTestRfVal = predict(modelRf_CC_GADA, newdata=dataset_val, type = "prob")
dataset_val =data.frame(dataset_val,probsTestRfVal$X2)
probsTestRfVal$X2[probsTestRfVal$X2 == 1] <- 0.999999
probsTestRfVal$X2[probsTestRfVal$X2 == 0] <- 0.000001
predTestRfVal = log(as.numeric(probsTestRfVal$X2)/(1-as.numeric(probsTestRfVal$X2)))
#then create a roc object and calculate the ROC on the validation dataset
roc_objTestRfval <- roc(dataset_val[,Youroutcomevariable], predTestRfVal)
AUC_objTestRfval = auc(roc_objTestRfval)
AUC_objTestRfval
ci.auc(roc_objTestRfval)

#for the logistic regression model (validation)
probsTestlgVal = predict(modelLG_CC_GADA, newdata=dataset_val, type = "prob")
dataset_val =data.frame(dataset_val,probsTestlgVal$X2)
predTestLGVal = log(as.numeric(probsTestlgVal$X2)/(1-as.numeric(probsTestlgVal$X2)))
#then create a roc object and calculate the ROC on the validation dataset
roc_objTestLGval <- roc(dataset_val[,Youroutcomevariable], predTestLGVal)
AUC_objTestLGval = auc(roc_objTestLGval)
AUC_objTestLGval
ci.auc(roc_objTestLGval)

#for the logistic regression model MARS (validation)
probsTestmarsVal = predict(modelMARS_CC_GADA_Lr, newdata=dataset_val, type = "prob")
dataset_val =data.frame(dataset_val,probsTestmarsVal$X2)
# predTestMARSVal = log(as.numeric(probsTestmarsVal$X2)/(1-as.numeric(probsTestmarsVal$X2)))
#then create a roc object and calculate the ROC on the validation dataset
roc_objTesMARSGval <- roc(dataset_val[,Youroutcomevariable], probsTestmarsVal$X2)
AUC_objTestMARSval = auc(roc_objTesMARSGval)
AUC_objTestMARSval
ci.auc(roc_objTesMARSGval)

#plot the roc curves
plot(roc_objTestRfval, col = "gray85",main = "",add=FALSE)
plot(roc_objTestLGval, col = "gray45", add = TRUE)
plot(roc_objTestSvmGridval, co = "black", add = TRUE)
plot(roc_objTestGbmGridval, col = "gray85",  lty = 3, add = TRUE)
plot(roc_objTestnnetGridval, col = "black",lty = 3, add = TRUE)
plot(roc_objTestknnGridval, col = "gray45", lty = 3, add = TRUE)
plot(roc_objTesMARSGval, col = "gray99", lty = 3, add = TRUE)


model=c('LR','GBM','SVM','RF','Nnet','Knn','LGMARS')
AUC = c(AUC_objTestLGval, AUC_objTestGbmGridval, AUC_objTestSvmGridval, AUC_objTestRfval, AUC_objTestnnetGridval, AUC_objTestknnGridval,AUC_objTestMARSval)
ValResults = data.frame(model, AUC)

myft <- regulartable(ValResults)
myft <- theme_vanilla(myft)
myft <- autofit(myft)
doc <- read_docx()
doc <- body_add_flextable(doc, value = myft)
print(doc, target = paste0("extAUC_sampling", sampling, "formula_",as.character(formula.model)[3],".docx"))

#use prediction-recall curve to validate the models########################################################
#calculate the AUPRC for the validation dataset
prRFval = pr.curve(1-dataset_val$probsTestRfVal.X2,dataset_val$probsTestRfVal.X2, curve = TRUE)
prLGval =  pr.curve(1-dataset_val$probsTestlgVal.X2,dataset_val$probsTestlgVal.X2, curve = TRUE) 
prSVMval = pr.curve(1-dataset_val$probsTestSvmGridVal.X2,dataset_val$probsTestSvmGridVal.X2, curve = TRUE) 
prGBMval = pr.curve(1-dataset_val$probsTestGbmGridVal.X2,dataset_val$probsTestGbmGridVal.X2, curve = TRUE) 
prNNval = pr.curve(1-dataset_val$probsTestnnetGridVal.X2,dataset_val$probsTestnnetGridVal.X2, curve = TRUE)
prKNNval = pr.curve(1-dataset_val$probsTestknnGridVal.X2,dataset_val$probsTestknnGridVal.X2, curve = TRUE)
prMARSval = pr.curve(1-dataset_val$probsTestmarsVal,dataset_val$probsTestmarsVal.X2, curve = TRUE)
#return the AUPRC
prRFval
prLGval
prSVMval
prGBMval
prNNval
prKNNval
prMARSval
model=c('LG','GBM','SVM','RF','Nnet','Knn','LGMARS')
pr = c(prLGval$auc.integral, prGBMval$auc.integral, prSVMval$auc.integral, prRFval$auc.integral, prNNval$auc.integral, prKNNval$auc.integral,prMARSval$auc.integral)
ValResults = data.frame(model, pr)

myft <- regulartable(ValResults)
myft <- theme_vanilla(myft)
myft <- autofit(myft)
doc <- read_docx()
doc <- body_add_flextable(doc, value = myft)
print(doc, target = paste0("extpr_sampling", sampling, "formula_",as.character(formula.model)[3],".docx"))

# plot PR curve for the test curve in red, without legend
pdf(paste0("prcurve", sampling, "formula_",as.character(formula.model)[3],".pdf"))
plot(prRFval, color = "gray85",auc.main=FALSE, main = "") 
plot( prLGval, color = "gray45", add = TRUE)
plot( prSVMval, color = "black", add = TRUE)
plot( prGBMval, color = "gray85", lty = 3, add = TRUE)
plot( prNNval, color = "black", lty = 3, add = TRUE)
plot( prKNNval, color = "gray45", lty = 3,add = TRUE)
plot(prMARSval, col = "gray99", lty = 3, add = TRUE)
dev.off()

#######plot the calibration plots with loess smoother #########################################
#for logistic regression
#create 10 risk groups
dataset_val %>% mutate(quintile=ntile(dataset_val$probsTestlgVal.X2,10)) -> dataset_val_10
Youroutcomevariable_num <- paste(Youroutcomevariable,"num")
dataset_val_10[as.numeric(dataset_val_10[,Youroutcomevariable])== 1,Youroutcomevariable_num] <- 0
dataset_val_10[as.numeric(dataset_val_10[,Youroutcomevariable])== 2,Youroutcomevariable_num] <- 1

#average the observed and expected probabilities of patients in each risk group 
obs = aggregate(as.numeric(dataset_val_10[,Youroutcomevariable_num]), list(dataset_val_10$quintile),mean)
exptd = aggregate(dataset_val_10$probsTestlgVal.X2, list(dataset_val_10$quintile),mean)
obsn =aggregate(as.formula(paste0(Youroutcomevariable ,"~ quintile")), dataset_val_10, length)

#CIs for scatter points
lci = obs - (1.96*(((obs*(1-obs))/obsn[,Youroutcomevariable])^.5))
lci[lci<0]<-0

uci = obs + (1.96*(((obs*(1-obs))/obsn[,Youroutcomevariable])^.5))
uci[uci>1]<-1

LR_Cali_Plot = data.frame(exptd$x,obs$x, uci$x, lci$x)

calLG <- ggplot(LR_Cali_Plot, aes(x= exptd$x, y=obs$x)) +
  geom_point(size = 2) + 
  geom_smooth(data = dataset_val_10,aes(x = probsTestlgVal.X2, y =`insulinRequire num`) ,span = 0.8,se=FALSE, col = "grey60") +
  geom_abline(slope=1, intercept=0, lty=2 ) +
  scale_x_continuous(name = "Expected", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  scale_y_continuous(name = "Observed", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  geom_errorbar(aes(ymin=lci$x, ymax=uci$x), width=0.02) +
  theme_bw()
calLG
#for SVM
#create 10 risk groups
dataset_val %>% mutate(quintile=ntile(dataset_val$probsTestSvmGridVal.X2,10)) -> dataset_val_10_SVM
dataset_val_10_SVM[as.numeric(dataset_val_10_SVM[,Youroutcomevariable])== 1,Youroutcomevariable_num] <- 0
dataset_val_10_SVM[as.numeric(dataset_val_10_SVM[,Youroutcomevariable])== 2,Youroutcomevariable_num] <- 1
#average the observed and expected probabilities of patients in each risk group 
obs_SVM = aggregate(as.numeric(dataset_val_10_SVM[,Youroutcomevariable_num]), list(dataset_val_10_SVM$quintile),mean)
exptd_SVM = aggregate(dataset_val_10_SVM$probsTestSvmGridVal.X2, list(dataset_val_10_SVM$quintile),mean)
obsn_SVM =aggregate(as.formula(paste0(Youroutcomevariable ,"~ quintile")), dataset_val_10_SVM, length)


#CIs for scatter points
lci_SVM = obs_SVM- (1.96*(((obs_SVM*(1-obs_SVM))/obsn_SVM[,Youroutcomevariable])^.5))
lci_SVM[lci_SVM<0]<-0

uci_SVM = obs_SVM + (1.96*(((obs_SVM*(1-obs_SVM))/obsn_SVM[,Youroutcomevariable])^.5))
uci_SVM[uci_SVM>1]<-1
SVM_Cali_Plot = data.frame(exptd_SVM$x,obs_SVM$x, uci_SVM$x, lci_SVM$x)

calSVM <- ggplot(SVM_Cali_Plot, aes(x= exptd_SVM$x, y=obs_SVM$x)) +
  geom_point(size = 2) + 
  geom_smooth(data = dataset_val_10,aes(x = probsTestSvmGridVal.X2, y =`insulinRequire num`) ,span = 0.8,se=FALSE, col = "grey60") +
  geom_abline(slope=1, intercept=0, lty=2 ) +
  scale_x_continuous(name = "Expected", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  scale_y_continuous(name = "Observed", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  geom_errorbar(aes(ymin=lci_SVM$x, ymax=uci_SVM$x), width=0.02) +
  theme_bw()
calSVM
#for Random Forest    
#create 10 risk groups
dataset_val %>% mutate(quintile=ntile(dataset_val$probsTestRfVal.X2,10)) -> dataset_val_10_RF
dataset_val_10_RF[as.numeric(dataset_val_10_RF[,Youroutcomevariable])== 1,Youroutcomevariable_num] <- 0
dataset_val_10_RF[as.numeric(dataset_val_10_RF[,Youroutcomevariable])== 2,Youroutcomevariable_num] <- 1
#average the observed and expected probabilities of patients in each risk group 
obs_RF = aggregate(as.numeric(dataset_val_10_RF[,Youroutcomevariable_num]), list(dataset_val_10_RF$quintile),mean)
exptd_RF = aggregate(dataset_val_10_RF$probsTestRfVal.X2, list(dataset_val_10_RF$quintile),mean)
obsn_RF =aggregate(as.formula(paste0(Youroutcomevariable ,"~ quintile")), dataset_val_10_RF, length)


#CIs for scatter points
lci_RF = obs_RF- (1.96*(((obs_RF*(1-obs_RF))/obsn_RF[,Youroutcomevariable])^.5))
lci_RF[lci_RF<0]<-0

uci_RF = obs_RF + (1.96*(((obs_RF*(1-obs_RF))/obsn_RF[,Youroutcomevariable])^.5))
uci_RF[uci_RF>1]<-1
RF_Cali_Plot = data.frame(exptd_RF$x,obs_RF$x, uci_RF$x, lci_RF$x)

calRF <- ggplot(RF_Cali_Plot, aes(x= exptd_RF$x, y=obs_RF$x)) +
  geom_point(size = 2) + 
  geom_smooth(data = dataset_val_10,aes(x = probsTestRfVal.X2, y =`insulinRequire num`) ,span = 0.8,se=FALSE, col = "grey60") +
  geom_abline(slope=1, intercept=0, lty=2 ) +
  scale_x_continuous(name = "Expected", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  scale_y_continuous(name = "Observed", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  geom_errorbar(aes(ymin=lci_RF$x, ymax=uci_RF$x), width=0.02) +
  theme_bw()
calRF
#for GBM   
#create 10 risk groups
dataset_val %>% mutate(quintile=ntile(dataset_val$probsTestGbmGridVal.X2,10)) -> dataset_val_10_GBM
dataset_val_10_GBM[as.numeric(dataset_val_10_GBM[,Youroutcomevariable])== 1,Youroutcomevariable_num] <- 0
dataset_val_10_GBM[as.numeric(dataset_val_10_GBM[,Youroutcomevariable])== 2,Youroutcomevariable_num] <- 1
#average the observed and expected probabilities of patients in each risk group 
obs_GBM = aggregate(as.numeric(dataset_val_10_GBM[,Youroutcomevariable_num]), list(dataset_val_10_GBM$quintile),mean)
exptd_GBM = aggregate(dataset_val_10_GBM$probsTestGbmGridVal.X2, list(dataset_val_10_GBM$quintile),mean)
obsn_GBM =aggregate(as.formula(paste0(Youroutcomevariable ,"~ quintile")), dataset_val_10_GBM, length)


#CIs for scatter points
lci_GBM = obs_GBM- (1.96*(((obs_GBM*(1-obs_GBM))/obsn_GBM[,Youroutcomevariable])^.5))
lci_GBM[lci_GBM<0]<-0

uci_GBM = obs_GBM + (1.96*(((obs_GBM*(1-obs_GBM))/obsn_GBM[,Youroutcomevariable])^.5))
uci_GBM[uci_GBM>1]<-1
GBM_Cali_Plot = data.frame(exptd_GBM$x,obs_GBM$x, uci_GBM$x, lci_GBM$x)

calGBM <-ggplot(GBM_Cali_Plot, aes(x= exptd_GBM$x, y=obs_GBM$x)) +
  geom_point(size = 2) + 
  geom_smooth(data = dataset_val_10,aes(x = probsTestGbmGridVal.X2, y =`insulinRequire num`) ,span = 0.8,se=FALSE, col = "grey60") +
  geom_abline(slope=1, intercept=0, lty=2 ) +
  scale_x_continuous(name = "Expected", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  scale_y_continuous(name = "Observed", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  geom_errorbar(aes(ymin=lci_GBM$x, ymax=uci_GBM$x), width=0.02) +
  theme_bw()
calGBM
#for KNN  
#create 10 risk groups
dataset_val %>% mutate(quintile=ntile(dataset_val$probsTestknnGridVal.X2,10)) -> dataset_val_10_KNN
dataset_val_10_KNN[as.numeric(dataset_val_10_KNN[,Youroutcomevariable])== 1,Youroutcomevariable_num] <- 0
dataset_val_10_KNN[as.numeric(dataset_val_10_KNN[,Youroutcomevariable])== 2,Youroutcomevariable_num] <- 1
#average the observed and expected probabilities of patients in each risk group 
obs_KNN = aggregate(as.numeric(dataset_val_10_KNN[,Youroutcomevariable_num]), list(dataset_val_10_KNN$quintile),mean)
exptd_KNN = aggregate(dataset_val_10_KNN$probsTestknnGridVal.X2, list(dataset_val_10_KNN$quintile),mean)
obsn_KNN =aggregate(as.formula(paste0(Youroutcomevariable ,"~ quintile")), dataset_val_10_KNN, length)


#CIs for scatter points
lci_KNN = obs_KNN- (1.96*(((obs_KNN*(1-obs_KNN))/obsn_KNN[,Youroutcomevariable])^.5))
lci_KNN[lci_KNN<0]<-0

uci_KNN = obs_KNN + (1.96*(((obs_KNN*(1-obs_KNN))/obsn_KNN[,Youroutcomevariable])^.5))
uci_KNN[uci_KNN>1]<-1
KNN_Cali_Plot = data.frame(exptd_KNN$x,obs_KNN$x, uci_KNN$x, lci_KNN$x)

calKNN <- ggplot(KNN_Cali_Plot, aes(x= exptd_KNN$x, y=obs_KNN$x)) +
  geom_point(size = 2) + 
  geom_smooth(data = dataset_val_10,aes(x = probsTestknnGridVal.X2, y =`insulinRequire num`) ,span = 0.8,se=FALSE, col = "grey60") +
  geom_abline(slope=1, intercept=0, lty=2 ) +
  scale_x_continuous(name = "Expected", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  scale_y_continuous(name = "Observed", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  geom_errorbar(aes(ymin=lci_KNN$x, ymax=uci_KNN$x), width=0.02) +
  theme_bw()
calKNN
#for NN  
#create 10 risk groups
dataset_val %>% mutate(quintile=ntile(dataset_val$probsTestnnetGridVal.X2,10)) -> dataset_val_10_NN
dataset_val_10_NN[as.numeric(dataset_val_10_NN[,Youroutcomevariable])== 1,Youroutcomevariable_num] <- 0
dataset_val_10_NN[as.numeric(dataset_val_10_NN[,Youroutcomevariable])== 2,Youroutcomevariable_num] <- 1
#average the observed and expected probabilities of patients in each risk group 
obs_NN = aggregate(as.numeric(dataset_val_10_NN[,Youroutcomevariable_num]), list(dataset_val_10_NN$quintile),mean)
exptd_NN = aggregate(dataset_val_10_NN$probsTestnnetGridVal.X2, list(dataset_val_10_NN$quintile),mean)
obsn_NN =aggregate(as.formula(paste0(Youroutcomevariable ,"~ quintile")), dataset_val_10_NN, length)


#CIs for scatter points
lci_NN = obs_NN- (1.96*(((obs_NN*(1-obs_NN))/obsn_NN[,Youroutcomevariable])^.5))
lci_NN[lci_NN<0]<-0

uci_NN = obs_NN + (1.96*(((obs_NN*(1-obs_NN))/obsn_NN[,Youroutcomevariable])^.5))
uci_NN[uci_NN>1]<-1
NN_Cali_Plot = data.frame(exptd_NN$x,obs_NN$x, uci_NN$x, lci_NN$x)

calNN <- ggplot(NN_Cali_Plot, aes(x= exptd_NN$x, y=obs_NN$x)) +
  geom_point(size = 2) + 
  geom_smooth(data = dataset_val_10,aes(x = probsTestnnetGridVal.X2, y =`insulinRequire num`) ,span = 0.8,se=FALSE, col = "grey60") +
  geom_abline(slope=1, intercept=0, lty=2 ) +
  scale_x_continuous(name = "Expected", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  scale_y_continuous(name = "Observed", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  geom_errorbar(aes(ymin=lci_NN$x, ymax=uci_NN$x), width=0.02) +
  theme_bw()
calNN

#for LGMARS
#create 10 risk groups
dataset_val %>% mutate(quintile=ntile(dataset_val$probsTestmarsVal.X2,10)) -> dataset_val_10_MARS
dataset_val_10_MARS[as.numeric(dataset_val_10_MARS[,Youroutcomevariable])== 1,Youroutcomevariable_num] <- 0
dataset_val_10_MARS[as.numeric(dataset_val_10_MARS[,Youroutcomevariable])== 2,Youroutcomevariable_num] <- 1
#average the observed and expected probabilities of patients in each risk group 
obs_MARS = aggregate(as.numeric(dataset_val_10_MARS[,Youroutcomevariable_num]), list(dataset_val_10_MARS$quintile),mean)
exptd_MARS = aggregate(dataset_val_10_MARS$probsTestmarsVal.X2, list(dataset_val_10_MARS$quintile),mean)
obsn_MARS =aggregate(as.formula(paste0(Youroutcomevariable ,"~ quintile")), dataset_val_10_MARS, length)


#CIs for scatter points
lci_MARS = obs_MARS- (1.96*(((obs_MARS*(1-obs_MARS))/obsn_MARS[,Youroutcomevariable])^.5))
lci_MARS[lci_MARS<0]<-0

uci_MARS = obs_MARS + (1.96*(((obs_MARS*(1-obs_MARS))/obsn_MARS[,Youroutcomevariable])^.5))
uci_MARS[uci_MARS>1]<-1
MARS_Cali_Plot = data.frame(exptd_MARS$x,obs_MARS$x, uci_MARS$x, lci_MARS$x)

calMARS <- ggplot(MARS_Cali_Plot, aes(x= exptd_MARS$x, y=obs_MARS$x)) +
  geom_point(size = 2) + 
  geom_smooth(data = dataset_val_10,aes(x = probsTestmarsVal.X2, y =`insulinRequire num`) ,span = 0.8,se=FALSE, col = "grey60") +
  geom_abline(slope=1, intercept=0, lty=2 ) +
  scale_x_continuous(name = "Expected", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  scale_y_continuous(name = "Observed", breaks = c(0.0, 0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  geom_errorbar(aes(ymin=lci_MARS$x, ymax=uci_MARS$x), width=0.02) +
  theme_bw()
calMARS


plot_grid(calGBM,calKNN,calLG,calMARS,calNN,calRF,calSVM, labels = c('A', 'B','C','D','E','F','G'), label_size = 12)
ggsave(paste0("calibration_sampling", sampling, "formula_",as.character(formula.model)[3],".pdf"))
#calcluate Calibration slope for each model

dataset_val2 <- dataset_val
dataset_val2[,Youroutcomevariable] <- as.numeric(dataset_val2[,Youroutcomevariable])-1

calslopeGBM <- glm(formula(paste0(Youroutcomevariable," ~ probsTestGbmGridVal.X2")), family=gaussian, data=dataset_val2)$coefficients
calslopeSVM <-glm(formula(paste0(Youroutcomevariable," ~ probsTestSvmGridVal.X2")), family=gaussian, data=dataset_val2)$coefficients
calslopeKNN <-glm(formula(paste0(Youroutcomevariable," ~ probsTestknnGridVal.X2")), family=gaussian, data=dataset_val2)$coefficients
calslopeNN <-glm(formula(paste0(Youroutcomevariable," ~ probsTestnnetGridVal.X2")), family=gaussian, data=dataset_val2)$coefficients
calslopeRF <-glm(formula(paste0(Youroutcomevariable," ~ probsTestRfVal.X2")), family=gaussian, data=dataset_val2)$coefficients
calslopeLG <-glm(formula(paste0(Youroutcomevariable," ~ probsTestlgVal.X2")), family=gaussian, data=dataset_val2)$coefficients
calslopeMARS <-glm(formula(paste0(Youroutcomevariable," ~ probsTestmarsVal.X2")), family=gaussian, data=dataset_val2)$coefficients

#calcluate Calibration in the large for each model###########################################################
#predicted risks are understated if _b[_cons] > 0 or overstated if _b[_cons] < 0
callargeGBM <- summary(glm(formula(paste0(Youroutcomevariable," ~  offset(probsTestGbmGridVal.X2)")), family=gaussian, data=dataset_val2))$coefficients[1]
callargeSVM <- summary(glm(formula(paste0(Youroutcomevariable," ~  offset(probsTestSvmGridVal.X2)")), family=gaussian, data=dataset_val2))$coefficients[1]
callargeKNN <- summary(glm(formula(paste0(Youroutcomevariable," ~  offset(probsTestknnGridVal.X2)")), family=gaussian, data=dataset_val2))$coefficients[1]
callargeNN <- summary(glm(formula(paste0(Youroutcomevariable," ~  offset(probsTestnnetGridVal.X2)")), family=gaussian, data=dataset_val2))$coefficients[1]
callargeRF <- summary(glm(formula(paste0(Youroutcomevariable," ~  offset(probsTestRfVal.X2)")), family=gaussian, data=dataset_val2))$coefficients[1]
callargeLG <- summary(glm(formula(paste0(Youroutcomevariable," ~  offset(probsTestlgVal.X2)")), family=gaussian, data=dataset_val2))$coefficients[1]
callargeMARS <- summary(glm(formula(paste0(Youroutcomevariable," ~  offset(probsTestmarsVal.X2)")), family=gaussian, data=dataset_val2))$coefficients[1]

#calcluate overall misCalibration for each model#############################################################
#the slope coefficient beta of the linear predictors reflects the deviations from the ideal slope of 1. 
#If p is significant then there is deviation from zero
mc1 = glm(formula(paste0(Youroutcomevariable," ~  probsTestGbmGridVal.X2+ offset(probsTestGbmGridVal.X2)")), family=gaussian, data=dataset_val2)
mc2 = glm(formula(paste0(Youroutcomevariable," ~  probsTestSvmGridVal.X2 + offset(probsTestSvmGridVal.X2)")), family=gaussian, data=dataset_val2)
mc3 = glm(formula(paste0(Youroutcomevariable," ~  probsTestknnGridVal.X2 + offset(probsTestknnGridVal.X2)")), family=gaussian, data=dataset_val2)
mc4 = glm(formula(paste0(Youroutcomevariable," ~  probsTestnnetGridVal.X2 + offset(probsTestnnetGridVal.X2)")), family=gaussian, data=dataset_val2)
mc5 = glm(formula(paste0(Youroutcomevariable," ~  probsTestRfVal.X2 + offset(probsTestRfVal.X2)")), family=gaussian, data=dataset_val2)
mc6 = glm(formula(paste0(Youroutcomevariable," ~  probsTestlgVal.X2 + offset(probsTestlgVal.X2)")), family=gaussian, data=dataset_val2)
mc7 = glm(formula(paste0(Youroutcomevariable," ~  probsTestmarsVal.X2 + offset(probsTestmarsVal.X2)")), family=gaussian, data=dataset_val2)

calpvalueGBM <- summary(mc1)$coefficients[2,c(4)]
calpvalueSVM <-summary(mc2)$coefficients[2,c(4)]
calpvalueKNN <-summary(mc3)$coefficients[2,c(4)]
calpvalueNN <-summary(mc4)$coefficients[2,c(4)]
calpvalueRF <- summary(mc5)$coefficients[2,c(4)]
calpvalueLG <- summary(mc6)$coefficients[2,c(4)]
calpvalueMARS <- summary(mc7)$coefficients[2,c(4)]

model=c('LR','GBM','SVM','RF','Nnet','Knn','LGMARS')
intercept = c(calslopeLG[1], calslopeGBM[1], calslopeSVM[1], calslopeRF[1], calslopeNN[1], calslopeKNN[1],calslopeMARS[1])
slope = c(calslopeLG[2], calslopeGBM[2], calslopeSVM[2], calslopeRF[2], calslopeNN[2], calslopeKNN[2],calslopeMARS[2])
inthelarge = c(callargeLG, callargeGBM, callargeSVM, callargeRF, callargeNN, callargeKNN,callargeMARS)
pvalue = c(calpvalueLG, calpvalueGBM, calpvalueSVM, calpvalueRF, calpvalueNN, calpvalueKNN,calpvalueMARS)
ValResults = data.frame(model, intercept, slope, inthelarge, pvalue)

myft <- regulartable(ValResults)
myft <- theme_vanilla(myft)
myft <- autofit(myft)
doc <- read_docx()
doc <- body_add_flextable(doc, value = myft)
print(doc, target = paste0("calibrationplot_sampling", sampling, "formula_",as.character(formula.model)[3],".docx"))

#Decision curve analysis
dataset_val_10$probsTestlgVal.X1 = 1-dataset_val_10$probsTestlgVal.X2 

dcaLR = dca(data = dataset_val_10, outcome = "insulinRequire num", predictors = c("probsTestlgVal.X2"))
dcaSVM = dca(data = dataset_val_10, outcome = "insulinRequire num", predictors = c("probsTestSvmGridVal.X2"))
dcaRF = dca(data = dataset_val_10, outcome = "insulinRequire num", predictors = c("probsTestRfVal.X2"))
dcaGBM = dca(data = dataset_val_10, outcome = "insulinRequire num", predictors = c("probsTestGbmGridVal.X2"))
dcaKNN = dca(data = dataset_val_10, outcome = "insulinRequire num", predictors = c("probsTestknnGridVal.X2"))
dcaNN = dca(data = dataset_val_10, outcome = "insulinRequire num", predictors = c("probsTestnnetGridVal.X2"))
dcaMARS = dca(data = dataset_val_10, outcome = "insulinRequire num", predictors = c("probsTestmarsVal.X2"))

#https://www.mskcc.org/sites/default/files/node/4511/documents/v3-worked-example-of-decision-curve-analysis-using-r.pdf

threshold <-  dcaLR$net.benefit$threshold*100
probtest <- dcaLR$net.benefit$probsTestlgVal.X2
model <- rep("LR",length(probtest))
dcadata <- data.frame(model,probtest,threshold)
threshold <- dcaSVM$net.benefit$threshold*100
probtest <- dcaSVM$net.benefit$probsTestSvmGridVal.X2
model <- rep("SVM",length(probtest))
dcadata <- rbind(dcadata, data.frame(model,probtest,threshold))
threshold <- dcaRF$net.benefit$threshold*100
probtest <- dcaRF$net.benefit$probsTestRfVal.X2
model <- rep("RF",length(probtest))
dcadata <- rbind(dcadata, data.frame(model,probtest,threshold))
threshold <- dcaGBM$net.benefit$threshold*100
probtest <- dcaGBM$net.benefit$probsTestGbmGridVal.X2
model <- rep("GBM",length(probtest))
dcadata <- rbind(dcadata, data.frame(model,probtest,threshold))
threshold <- dcaKNN$net.benefit$threshold*100
probtest <- dcaKNN$net.benefit$probsTestknnGridVal.X2
model <- rep("KNN",length(probtest))
dcadata <- rbind(dcadata, data.frame(model,probtest,threshold))
threshold <- dcaNN$net.benefit$threshold*100
probtest <- dcaNN$net.benefit$probsTestnnetGridVal.X2
model <- rep("NN",length(probtest))
dcadata <- rbind(dcadata, data.frame(model,probtest,threshold))
threshold <- dcaMARS$net.benefit$threshold*100
probtest <- dcaMARS$net.benefit$probsTestmarsVal.X2
model <- rep("MARS",length(probtest))
dcadata <- rbind(dcadata, data.frame(model,probtest,threshold))
threshold <- dcaMARS$net.benefit$threshold*100
probtest <- dcaMARS$net.benefit$all
model <- rep("all",length(probtest))
dcadata <- rbind(dcadata, data.frame(model,probtest,threshold))
threshold <- dcaMARS$net.benefit$threshold*100
probtest <- dcaMARS$net.benefit$none
model <- rep("none",length(probtest))
dcadata <- rbind(dcadata, data.frame(model,probtest,threshold))


ggplot(dcadata, aes(x = threshold, y = probtest, group = model, linetype = model, colour = model)) +
  geom_line()  +
  coord_cartesian(ylim =c(-0.25,0.20)) +
  theme(legend.key.width = unit(1.5,"cm")) +
  xlab("Probability threshold") +
  ylab("Net benefit") +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.7, 0.8))
ggsave(paste0("decision_curve_analysis",sampling, "formula_",as.character(formula.model)[3],".pdf"), units = "in", height = 6.5, width = 6.5)

#correlation matrix of predictions - validation dataset
predMatrixVal = data.frame(dataset_val$probsTestGbmGridVal.X2 ,dataset_val$probsTestSvmGridVal.X2, dataset_val$probsTestknnGridVal.X2 , dataset_val$probsTestnnetGridVal.X2 ,
                           dataset_val$probsTestRfVal.X2, dataset_val$probsTestlgVal.X2, dataset_val$probsTestmarsVal.X2)
names(predMatrixVal)[1] <-"GBM"
names(predMatrixVal)[2] <-"SVM"
names(predMatrixVal)[3] <-"KNN"
names(predMatrixVal)[4] <-"NN"
names(predMatrixVal)[5] <-"RF"
names(predMatrixVal)[6] <-"LR"
names(predMatrixVal)[7] <-"LRMARS"

MVal = cor(predMatrixVal)
pdf(paste0("corrplot", sampling, "formula_",as.character(formula.model)[3],".pdf"))
corrplot(MVal, method="number",tl.cex = 1)
dev.off()

library(GGally)
ggpairs(predMatrixVal,diag=list(continuous=wrap("barDiag", binwidth=0.1)),labeller = "label_parsed", axisLabels = "show",xlab = "Predicted probability to have T1D", ylab = "Predicted probability to have T1D") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0("corrplot_scatter", sampling, "formula_",as.character(formula.model)[3],".pdf"),units = "in",height = 9, width = 9)
#create a variable importance dataframe###########################################################
# Svm and KNN do not have built-in variable importance score
Model = c('Logistic Regression','Stochastic Gradient Boosting', 'Neural Network', 'Random Forest', 'GLM MARS')
# calculate the variable importance scores
# varImp function provides the variable importance
LGImp <- varImp(modelLG_CC_GADA, scale = FALSE)
LGImp

gmbImp <- varImp(modelGbm_CC_GADA_Lr, scale = FALSE)
gmbImp

nnetImp <- varImp(modelnnet_CC_GADA_Lr, scale = FALSE)
nnetImp

rfImp <- varImp(modelRf_CC_GADA, scale = FALSE)
rfImp

MarsImp <- varImp(modelMARS_CC_GADA_Lr, scale = FALSE)
MarsImp



impdata <- data.frame(LG = LGImp$importance,GBM = gmbImp$importance, NNET = nnetImp$importance,LGMARS = MarsImp$importance)
impdata <- cbind(rownames(impdata),impdata)
names(impdata) <- c("variables","LR","GBM","NNET","LGMARS")
impdata <- impdata %>% gather(models,values, -variables) %>%
  dplyr::group_by(models) %>% 
  dplyr::mutate(values =values/sum(values) * 100) %>% 
  dplyr::ungroup()

ggplot(impdata,aes(x = variables, y = values)) +
geom_bar(stat="identity",width=0.06) +
  coord_flip()+ ylab("Scaled variable importance score") +
  xlab("") + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank()) + 
  facet_grid(models~.)

ggsave(paste0("parameters_importance", sampling, "formula_",as.character(formula.model)[3],".pdf"))


#################################
# 5 - save the objects for future use
#################################

save(data_train,dataset_val,ValResults,control,modelLG_CC_GADA, modelRf_CC_GADA, modelSvm_CC_GADA_Lr, modelGbm_CC_GADA_Lr, 
     modelnnet_CC_GADA_Lr,modelknn_CC_GADA_Lr,impdata, results_grid_CC_GADA,gmbImp, rfImp, nnetImp,dataset_val_10, 
     file = paste0("results_sampling", sampling, "formula_",as.character(formula.model)[3],".Rdata"))





