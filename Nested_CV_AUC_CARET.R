# Function for doing a k-fold cross-validation for each C in CC
nested_cv <- function(data,  k,formula.model,  seed = NULL,sampling = sampling) {
  # Set the seed, if given
  if (!is.null(seed)) {
    set.seed(seed)
  }

  folds <- createFolds(data$insulinRequire, k = k)
  auc <- lapply(1:k, function(k) {
      train_and_validate( data, folds[[k]], sampling = sampling,formula.model = formula.model)
    })
  auc
}



train_and_validate <- function( data, fold, sampling = sampling,formula.model = formula.model) {
  data_train <- data[-fold,]
  data_val <- data[fold,]
  #for use in default and grid search optimised models
  control <- trainControl(method = "repeatedcv", number = 5, repeats = 1,classProbs = TRUE,summaryFunction = twoClassSummary, sampling = sampling, savePredictions = TRUE)
  #for use in random search optimised models
  control_Rand_Search <- trainControl(method = "repeatedcv", number = 5, repeats = 1,classProbs = TRUE,summaryFunction = twoClassSummary, sampling = sampling, savePredictions = TRUE, search = "random")
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
  floor(sqrt(NCOL(data)))
  #set up the grid
  gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 7, 10),
                          n.trees = c(10, 50,100,200), 
                          shrinkage = seq(from = 0.01, to = 0.1, by = 0.02),
                          n.minobsinnode = c(5,10,20)) 
  
  #tune the hyper-parameters using  Grid Search
  
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
  svmGrid <-  expand.grid(sigma = c(0.01, 0.1, 1, 10, 100),
                          C = seq(from = 0.1, to = 1, by = 0.05)) 
  
  #using training dataset and tune the hyper-parameters using Caret Grid Search
  
  modelSvm_CC_GADA_Lr <- train(formula.model, data = data_train, method = "svmRadial", trControl = control, verbose = TRUE,metric = 'ROC',tuneGrid=svmGrid)
  
  
  # train the Random forest model
  #parameter mtry is the number of variables available for splitting at each tree node
  #The default is the square root of the number of predictor variables (rounded down)
  #as we are only using three variables we do not optimise the parameters
  #For mtry refer to http://code.env.duke.edu/projects/mget/export/HEAD/MGET/Trunk/PythonPackage/dist/TracOnlineDocumentation/Documentation/ArcGISReference/RandomForestModel.FitToArcGISTable.html
  
  #using training dataset and default parameters
  
  modelRf_CC_GADA <- train(formula.model, data = data_train, method = 'rf', trControl = control,metric = 'ROC')
  
  
  # train a logistic regression model
  #using training dataset
  #there are no tuning parameters for glm method within caret
  
  modelLG_CC_GADA <- train(formula.model, data = data_train, method = "glm", family = "binomial", trControl = control,metric = 'ROC')
  
  
  # train neural network
  getModelInfo()$nnet$parameters
  #size parameter is the number of units in hidden layer (nnet fit a single hidden layer neural network) 
  #decay parameter is the regularization parameter to avoid over-fitting
  nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                           decay = c(0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001, 0.0000001))
  
  #tune the hyper-parameters using Caret Grid Search
  
  modelnnet_CC_GADA_Lr <- train(formula.model, data = data_train, method = "nnet", trControl = control,metric = 'ROC', tuneGrid = nnetGrid)
  
  
  # train a k-nearest-neighbours
  #based on euclidean distance
  getModelInfo()$knn$parameters
  #k parameter is the number of neighbours. 
  knnGrid <-  expand.grid(k = seq(from = 1, to = 100, by = 1))
  #tune the hyper-parameters using Caret Grid Search
  
  modelknn_CC_GADA_Lr <- train(formula.model, data = data_train, method = "knn", trControl = control,metric = 'ROC', tuneGrid = knnGrid)
  
  marsgrid <- expand.grid(
    degree = 1:3, 
    nprune = seq(1, 20, length.out = 20) %>% floor()
  )
  
  #tune the hyper-parameters using  Grid Search
  
  modelMARS_CC_GADA_Lr <- train(formula.model, data = data_train, method = "earth", glm=list(family='binomial'),trControl = control,,metric = 'ROC',tuneGrid=marsgrid)
  #################################
  # 4 - Perform validation 
  #################################
  
  #for the Gbm grid search model (validation)
  probsTestGbmGridVal = predict(modelGbm_CC_GADA_Lr, newdata=data_val, type = "prob")
  data_val =data.frame(data_val,probsTestGbmGridVal$X2)
  predTestGbmGridVal = log(as.numeric(probsTestGbmGridVal$X2)/(1-as.numeric(probsTestGbmGridVal$X2)))
  #then create a roc object and calculate the ROC on the validation dataset
  roc_objTestGbmGridval <- roc(data_val[,Youroutcomevariable], predTestGbmGridVal)
  AUC_objTestGbmGridval = auc(roc_objTestGbmGridval)
  AUC_objTestGbmGridval
  ci.auc(roc_objTestGbmGridval)
  
  
  #for the Svm grid search model (validation)
  probsTestSvmGridVal = predict(modelSvm_CC_GADA_Lr, newdata=data_val, type = "prob")
  data_val =data.frame(data_val,probsTestSvmGridVal$X2)
  predTestSvmGridVal = log(as.numeric(probsTestSvmGridVal$X2)/(1-as.numeric(probsTestSvmGridVal$X2)))
  #then create a roc object and calculate the ROC on the validation dataset
  roc_objTestSvmGridval <- roc(data_val[,Youroutcomevariable], predTestSvmGridVal)
  AUC_objTestSvmGridval = auc(roc_objTestSvmGridval)
  AUC_objTestSvmGridval
  ci.auc(roc_objTestSvmGridval)
  
  #for the knn grid search model (validation)
  probsTestknnGridVal = predict(modelknn_CC_GADA_Lr, newdata=data_val, type = "prob")
  data_val =data.frame(data_val,probsTestknnGridVal$X2)
  probsTestknnGridVal$X2[probsTestknnGridVal$X2 == 1] <- 0.999999
  probsTestknnGridVal$X2[probsTestknnGridVal$X2 == 0] <- 0.000001
  predTestknnGridVal = log(as.numeric(probsTestknnGridVal$X2)/(1-as.numeric(probsTestknnGridVal$X2)))
  #then create a roc object and calculate the ROC on the validation dataset
  roc_objTestknnGridval <- roc(data_val[,Youroutcomevariable], predTestknnGridVal)
  AUC_objTestknnGridval = auc(roc_objTestknnGridval)
  AUC_objTestknnGridval
  ci.auc(roc_objTestknnGridval)
  
  #for the nnet grid search model (validation)
  probsTestnnetGridVal = predict(modelnnet_CC_GADA_Lr, newdata=data_val, type = "prob")
  data_val =data.frame(data_val,probsTestnnetGridVal$X2)
  predTestnnetGridVal = log(as.numeric(probsTestnnetGridVal$X2)/(1-as.numeric(probsTestnnetGridVal$X2)))
  #then create a roc object and calculate the ROC on the validation dataset
  roc_objTestnnetGridval <- roc(data_val[,Youroutcomevariable], predTestnnetGridVal)
  AUC_objTestnnetGridval = auc(roc_objTestnnetGridval)
  AUC_objTestnnetGridval
  ci.auc(roc_objTestnnetGridval)
  
  #for the rf model (validation)
  probsTestRfVal = predict(modelRf_CC_GADA, newdata=data_val, type = "prob")
  data_val =data.frame(data_val,probsTestRfVal$X2)
  probsTestRfVal$X2[probsTestRfVal$X2 == 1] <- 0.999999
  probsTestRfVal$X2[probsTestRfVal$X2 == 0] <- 0.000001
  predTestRfVal = log(as.numeric(probsTestRfVal$X2)/(1-as.numeric(probsTestRfVal$X2)))
  #then create a roc object and calculate the ROC on the validation dataset
  roc_objTestRfval <- roc(data_val[,Youroutcomevariable], predTestRfVal)
  AUC_objTestRfval = auc(roc_objTestRfval)
  AUC_objTestRfval
  ci.auc(roc_objTestRfval)
  
  #for the logistic regression model (validation)
  probsTestlgVal = predict(modelLG_CC_GADA, newdata=data_val, type = "prob")
  data_val =data.frame(data_val,probsTestlgVal$X2)
  predTestLGVal = log(as.numeric(probsTestlgVal$X2)/(1-as.numeric(probsTestlgVal$X2)))
  #then create a roc object and calculate the ROC on the validation dataset
  roc_objTestLGval <- roc(data_val[,Youroutcomevariable], predTestLGVal)
  AUC_objTestLGval = auc(roc_objTestLGval)
  AUC_objTestLGval
  ci.auc(roc_objTestLGval)
  
  
 
  
  #for the logistic regression model (validation)
  probsTestMARSVal = predict( modelMARS_CC_GADA_Lr, newdata=data_val, type = "prob")
  data_val =data.frame(data_val,probsTestMARSVal$X2)
  predTestMARSVal = log(as.numeric(probsTestMARSVal$X2)/(1-as.numeric(probsTestMARSVal$X2)))
  #then create a roc object and calculate the ROC on the validation dataset
  roc_objTestMARSval <- roc(data_val[,Youroutcomevariable], predTestMARSVal)
  AUC_objTestMARSval = auc(roc_objTestMARSval)
  AUC_objTestMARSval
  ci.auc(roc_objTestMARSval)
  res <- c(  AUC_objTestGbmGridval,  AUC_objTestSvmGridval,  AUC_objTestknnGridval,  AUC_objTestnnetGridval,AUC_objTestRfval,AUC_objTestLGval,AUC_objTestMARSval)
}
  
  
sampling <- NULL  
res_none_l <- nested_cv(data_train, 5,formula.model, sampling = sampling)
res_none <- do.call(rbind, res_none_l) 
res <- apply(res_none,2,mean)
resvar <- apply(res_none,2,sd)
res -1.96*(resvar/sqrt(5))
res + 1.96*(resvar/sqrt(5))
models <- c("Gbm",  "Svm",  "knn",  "nnet","Rf","LG","MARS")
res <- data.frame(models,res)
names(res) <- c("Models", "AUC")
myft <- regulartable(res)
myft <- theme_vanilla(myft)
myft <- autofit(myft)
doc <- read_docx()
doc <- body_add_flextable(doc, value = myft)
print(doc, target = paste0("nestedCV_sampling", sampling, "formula_",as.character(formula.model)[3],".docx"))
