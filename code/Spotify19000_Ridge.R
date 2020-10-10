dataset <- read.csv("Project/Spotify19000.csv")
head(dataset)

library(dplyr)
dataset <- dataset %>% distinct()

keys <- c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B')
dataset$key <- factor(dataset$key, labels = keys)
rm(keys)

dataset$audio_mode <- factor(ifelse(dataset$audio_mode==0, 'minor', 'major'))

dataset$song_duration_ms <- dataset$song_duration_ms/60000
names(dataset)[names(dataset) == 'song_duration_ms'] <- 'song_duration'

dataset$time_signature <- factor(dataset$time_signature)

dataset <- dataset[, c(-1,-2)]


#Features are standardized using built-in methods in R's function glmnet()


###################
###The least square method finds the coefficients that best fit the data. One more 
#condition to be added is that it also finds the unbiased coefficients. Unbiased 
#means that least squares doesn't consider which independent variable is more important than 
#others, i.e. simply finds the coefficients for a given data set. Therefore,  
#this kind of model becomes more complex as new variables are added. It can be said 
#that an OLS model has the lowest bias and the highest variance. To overcome this 
#issue, we fit a ridge regression to our model. We are trying to find coefficient
#estimates that would minimze the RSS in a linear model, by adding a second term
# lambda*sum((beta_j)^2) - a shrinkage penalty. This penalty is small when b_1, .. b_p
#are close to zero, so it has the effect of shrinking the estimates of beta_j towards
#0. The tuning parameter lambda serves to control the relative impact of these two terms 
#on the regression coefficient estimates. As lambda increases, the flexibility of the ridge 
#regression fit decreases, leading to decreased variance but increased bias. This too
#may resolve the issue of multicollinearity, so we too use the "energy variable".

###We divide our data into a training and testing set.
set.seed(42)
dataset <- dataset[sample(nrow(dataset)),] #randomly shuffle data
split <- round(nrow(dataset) *0.80)
train_set <- dataset[1:split, ]
test_set <- dataset[(split+1):nrow(dataset),]


###Creation of model matrix
x_train <- model.matrix(~., data=train_set[,-14])
x_test <- model.matrix(~., data=test_set[,-14])

library(glmnet)


###Ridge regression w/ 5-fold CV:

###We modify the grid so we compute model fits for a particular value of lambda that is not 
#one of the original grid values.
grid <- 10^seq(4, -4, length=100)
ridge_cv<-cv.glmnet(x_train[,-1], train_set$song_popularity, alpha=0, lambda=grid,
                type.measure = 'mse', standardize=T, K=5) #-1 not to include intercept

str(ridge_cv)

###We see that with lambda large, we have a constant error and there is no change in
#the plot. As more regressors are pushed to zero, the MSE in the validation set slightly
#incerases and after a certain "threshold" there seems to be no change in the 
#behaviour of our model, i.e. just a high constant MSE in the validation set. 
#As we decrease the lambda parameter, we can see that MSE decreases. This starts from
#setting lambda to approxiamtely 2, and stops when lambda is approximately 0 and we
#have a constant decrease in MSE. 
plot(ridge_cv) 

###The most appropriate values for lambda are plotted with two verical lines: one 
#for the "best" value of lambda and other for 1 standard error above it.
plot(ridge_cv, xlim=c(-6,6))

###The optimal value of lambda is very close to 0, which means that there is very
#little-to-no regularization effect and the model we fit will be equivalent to
#the previous full model. This is because ridge does not shrink any variables to
#exactly 0, hence all predictors will be used in the fit.


###Prediction on test set
pred_ridge <- predict(ridge_cv, newx=x_test[,-1], lambda=lambda_min)

MSE_ridge <- mean((pred_ridge-test_set$song_popularity)^2)
MSE_ridge 

RMSE_ridge <- sqrt(MSE_ridge)
RMSE_ridge



###The RMSE of Ridge Regression is 20.22731 which is not lower than the previous
#errors obtained. Now, we inspect whether the choice of variables for our model 
#poses a problem.