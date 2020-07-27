# code-machine-learning-comparison

Code generated to provide the results of 
Lynam, A.L., Dennis, J.M., Owen, K.R. et al. Logistic regression has similar performance to optimised machine learning algorithms in a clinical setting: application to the discrimination between type 1 and type 2 diabetes in young adults. Diagn Progn Res 4, 6 (2020). https://doi.org/10.1186/s41512-020-00075-2

The code can easely adapted to your dataset by following the next steps
main.R will generate most of the figures and results
- depending of your dataset you might have to change the l81 to l 255 to load and set up your data in a good format.

Nested_CV_AUC_CARET.R will generate nested cross validation to robustely estimate the performance of each model when hyperparameter have been optimised.
- in l8 "folds <- createFolds(data$insulinRequire, k = k)" please do change insulinRequire by your outcome
-change the outcomes labels to X1 and X2
