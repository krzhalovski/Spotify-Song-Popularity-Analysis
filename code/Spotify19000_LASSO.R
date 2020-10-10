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
###LASSO

###We saw that ridge regression with a wise choice of  lambda  didn't really outperform least 
#squares. Now, we're interested in whether the LASSO can yield either a more accurate or 
#a more interpretable model. In order to fit a lasso model, we again use the glmnet() function; 
#however, this time we use the argument alpha=1. This type of regression analysis is a shrinkage 
#and variable selection method for linear regression models. The goal of LASSO regression is to 
#obtain the subset of predictors that minimizes prediction error for a quantitative response 
#variable. LASSO does this by imposing a constraint on the model parameters that causes regression 
#coefficients for some variables to shrink toward zero. Variables with a regression coefficient 
#equal to zero after the shrinkage process are excluded from the model. Variables with non-zero 
#regression coefficients variables are most strongly associated with the response variable.

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

###We modify the grid so we compute model fits for a particular value of that is not 
#one of the original grid values.
grid <- 10^seq(4, -6, length=100) 
lasso_cv <- cv.glmnet(x_train[,-1], train_set$song_popularity, alpha=1, lambda=grid,
                type.measure = 'mse', standardize=T, K=5)

#-1 not to include intercept
str(lasso_cv)

###We see that with lambda very large, we have a constant error and there is no change in
#the plot. As more regressors are pushed to zero, the MSE in the validation set slightly
#incerases and after a certain "threshold" there seems to be no change in the 
#behaviour of our model, i.e. just a high constant MSE in the validation set. 
#As we decrease the lambda parameter, i.e. set it to 0 we can see that MSE decreases.
plot(lasso_cv)

###This continues until lambda is approximately -4 and we start to see a stabilization in 
#decreased MSE and a constant afterwards.
plot(lasso_cv, xlim=c(-6,2))

lambda_best <- lasso_cv$lambda.min
lambda_1se <- lasso_cv$lambda.1se

###We see that with an optimal value for lambda of 0.0351, LASSO has discarded 
#most variables: 
coefs <- coef(lasso_cv, lambda="lambda.min")
coefs

###And it has left us the following to work with, which in comparison to our reduced
#model, does not include song duration, audio mode, and speechiness:
coef_df <- data.frame(name = coefs@Dimnames[[1]][coefs@i + 1], coefficient = coefs@x) 
coef_df[order(coef_df$coefficient, decreasing = T),]
#and hence, the minimum value of the RMSE is reached.


m_lasso <- glmnet(x_train[,-1], train_set$song_popularity, alpha=1, lambda=grid, 
                  type.measure = 'mse', standardize=T, K=5)

plot(m_lasso, xvar="lambda", label=TRUE) 
###By plotting the solid black line of ridge regression for lambda within 1 standard 
#error, we seem to get the least squares estimates.
abline(v=log(lasso_cv$lambda.1se), lty=1) 

###Plotting the optimal vlaue for lambda in a blue dashed line, we get the estimates
#used to fit our LASSO model.
abline(v=log(lasso_cv$lambda.min), lty=2, col=4) #minimum lambda



###Prediction on test set
pred_lasso <- predict(lasso_cv, newx=x_test[,-1],lambda=lambda_best)

MSE_lasso <- mean((pred_lasso-test_set$song_popularity)^2)
MSE_lasso 

RMSE_lasso <- sqrt(MSE_lasso)
RMSE_lasso
 
##The RMSE of LASS 20.16621 which in comparison to the Ridge regression and
#previous linear models, is the least we got . Despite producing the lowest 
#error, it is not a very significant reduction. We conclude that these types 
#of models do not perform well on our data. That is why, we conduct an analysis
#on how correctly can a model classify songs to highly popular and not so popular,
#i.e. having a popularity score > 75 or not.