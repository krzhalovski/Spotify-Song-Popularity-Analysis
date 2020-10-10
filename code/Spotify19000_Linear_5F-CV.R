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

######################
### LINEAR REGRESSION - 
#we estimate models, to check significance of regression
######################


###H0: beta_1 = beta_2 = ... = beta_p = 0
mod_h0 <- lm(song_popularity ~ 1, data = dataset)
summary(mod_h0)

RSS_h0 <- sum(residuals(mod_h0)^2)

MSE_h01 <- mean(residuals(mod_h0)^2)
MSE_h01

RMSE_h01 <- sqrt(MSE_h01)
RMSE_h01



##############

###H1: full model
mod_full <- lm(song_popularity ~ ., data = dataset)

###Judging by the correlation plot given previously in our analysis, we need to
#check whether it is appropriate to use all of them in our models. For this, we
#use R's function vif() which stands for Variance Inflation Factors and assesses
#how much the variance of an estimated regression coefficient increases if the
#predictors are correlated. If no factors are correlated, VIF's will be all 1s.
car::vif(mod_full)

###We see that all VIF values are around 1, excluding acousticness with a VIF of 2.0557,
#energy with 3.9067, and loudness with 3.0310. Since energy has the highest VIF, we
#decide to drop it from our dataset and refit our model.
dataset<-dataset[,-4]

mod_full <- lm(song_popularity ~ ., data = dataset)

###Now, by checking the VIF values of the coefficient, we can observe that all of
#them aroung 1. This justifies our choice to drop one of the predictors at the mere 
#beginning.
car::vif(mod_full)


###Checking summary of full model:
summary(mod_full)

###We can see that the p-value of the F-statistic is < 2.2e-16, 
#which is highly significant. This means that, at least, one of the 
#predictor variables is significantly related to the target variable, in
#our case - song popularity. To see which predictor variables are significant, 
#we examine the coefficients table, which shows the estimate of regression beta 
#coefficients and the associated t-statitic p-values:
summary(mod_full)$coefficients

###For a given the predictor, the t-statistic evaluates whether or not there 
#is significant association between the predictor and the outcome variable, 
#that is whether the beta coefficient of the predictor is significantly 
#different from zero. It can be seen that, changing the following variables
#is significantly associated to changes in song popularity:
# danceability
# instrumentalness
# liveness
# speechiness
# tempo
# audio valence
#i.e. they all have a p-value <0.001, meaning that there is less than a 0.1% 
#chance that the coefficient might be equal to 0 and thus be insignificant.


###But the estimates of the regression coefficients are subject to sampling uncertainty. 
#Therefore, we will never exactly estimate the true value of these parameters from sample 
#data in an empirical application. However, we may construct confidence intervals,
#at a 95% level:
confint(mod_full)
#For some of the estimates, the interval does not contain the value zero. 
#Those that we suspect can be left out of the model fit are:
# song duration
# acousticness
# key
# time signature

###The overall quality of the model can be assesed by examining the Root Mean Squared 
#Error:
RSS_full <- sum(residuals(mod_full)^2) #6060176

MSE_full <- mean(residuals(mod_full)^2) #406.0147
MSE_full

RMSE_full <- sqrt(MSE_full) #20.14981
RMSE_full



###What we are basically doing is performing a hypothesis test
#under H0: beta_1 = beta_2 = ... = beta_p = 0 , vs the alternative
#hypothesis H1: b_0 + b1x1 + .. + bpxp + Ei

#technical: Under H0, (RSS0 - RSSfull / p) / (RSS/n-p-1) ~ Fp, n-p-1
v1 <- 12
v2 <- nrow(dataset)-12-1
F_obs <- ((RSS_h0 - RSS_full) / v1 ) / (RSS_full/v2)
F_obs

p_value <- 1 - pf(F_obs, v1, v2) # df_1 = 3; df_2 = n-p-1 / 200-3-1   
p_value

###Since the p-value is 0, we can reject our primary hypothesis that
#all coefficients can be estimated as 0, and continue our analysis by using
#the full model for prediction to increase the predictive capability of our
#model.


###As we've seen in the data exploration, there isn't a big correlation
#between the independent variables we chose to work with, but we suspect that
#some of them influence others. We suspect that the effect of acousticness on 
#popularity also depends on instrumentalness, similarly to how valance has an effect 
#on popularity as it is dependent on loudness and danceability. We too expect that 
#the effect of loudness on popularity depends on liveness too.
mod_full_inter <- lm(song_popularity ~ . + acousticness:instrumentalness +
                       audio_valence:loudness:danceability +
                       liveness:loudness, data=dataset)

summary(mod_full_inter)

RSS_full_inter <- sum(residuals(mod_full_inter)^2)

MSE_full_inter <- mean(residuals(mod_full_inter)^2)
MSE_full_inter

RMSE_full_inter <- sqrt(MSE_full_inter) #19.97706
RMSE_full_inter

#We can see that the RMSE is decreased by not even 0.5%, which isn't significant. 
#Hence, we continue using our original model.
rm(mod_full_inter, MSE_full_inter, RMSE_full_inter, RSS_full_inter)


#######################


###We analyze the residuals of our full model:
par(mfrow=c(2,2))
plot(mod_full)
par(mfrow=c(1,1))

###Residuals vs Fitted
#There isn't a clear pattern in the residuals and they are symetrically
#distributed, and that there aren't any clear patterns. Since, there isn't
#any evidence of heteroscedasticity (non-constant variance in the errors),
#we conclude that regression assumptions are satisfied.

###Normal Q-Q
#Notice the points fall along a line in the middle of the graph, but curve 
#off in the extremities. This means our data has more extreme values, but
#by applying various transformation to the response variable yielded even
#worse results.

###Scale-Location
#The presence of heteroscedasticity can be seen also from how the 
#studentized residuals spread along the ranges of predicted variables.
#We can see that the residuals are spread equally, even though our
#horizontal line isn't quite straight.

###Residuals vs Leverage
#We can see that there are points which are outside the red dashed Cook's
#distance line. These are points that would be influential in the model and
#removing them would likely alter the regression results.
dataset <- dataset[c(-6504,-9718,-14452), ]
dim(dataset)

mod_full <- lm(song_popularity ~ ., data = dataset)

summary(mod_full)

RSS_full <- sum(residuals(mod_full)^2)

MSE_full <- mean(residuals(mod_full)^2)
MSE_full

RMSE_full <- sqrt(MSE_full) #20.14781 -> no significant decrease after removing observations
RMSE_full



#Although there is a change of sign in some predictors, there is no real change in their significance, nor 
#in performance of the model. However, we leave them out of the dataset. As we can see,
#many of the variables used in our multiple regression model are in fact not associated
#with the response (i.e. they are non-significant) and including such irrelevant variables
#leads to unnecessary complexity in the resulting model. This was also suspected when
#we constructed confidence intervals for the estimated coefficients of the regression.
#Some of the intervals did not contain 0, which leads to the rejection of the null hypothesis 
#(beta_i = 0) i.e. they should have a say in predicting song popularity.
confint(mod_full)

#This is the reason why we perform a feature selection using backward selection. We start with all 
#variables in the model, and remove the variable with the largest p-value. The new (p-1)-variable 
#model is fit, and the variable with the largest p-value is removed. Here, we consider Akaike 
#Information Criterion (AIC) and remove model that shows lowest AIC.
step_mod <- step(mod_full, steps=10,  trace=1, direction="backward") 

#We observe that we have excluded 2 predictors from the fit (time_signature and 
#acousticness), such that the selected model is:
#song popularity = b_0 + b_1*song_duration + b_2*key + b_3*audio_mode + b_4*speechiness
#                   + b_5*tempo + b_6*loudness + b_7*liveness + b_8*danceability
#                   + b_9*audio_valence + b_10*instrumentalness

mod_bw <- lm(song_popularity ~ song_duration + key + audio_mode + speechiness + tempo +
               loudness + liveness + danceability + audio_valence +
               instrumentalness, data=dataset)

summary(mod_bw)
###The coefficients that have the largest effect on our target varialbe are:
# Danceability with 8.481296, meaning with each unit increase, song popularity increases
#by roughly 8.48;
# Instrumentalness with -8.017524, meaning with each unit increase, song popularity decreases
#by roughly 8.02;
# Audio valence with -7.460624, meaning with each unit increase, song popularity decreases
#by roughly 7.46.

#And how our model performed using accurate metrics:
RSS_bw <- sum(residuals(mod_bw)^2)

MSE_bw <- mean(residuals(mod_bw)^2) 
MSE_bw

RMSE_bw <- sqrt(MSE_bw) #20.14996
RMSE_bw


###########################

###Comparing full and reduced model:
v1 <- 12
v2 <- nrow(dataset)-12-1
F_obs <- ((RSS_bw - RSS_full) / v1 ) / (RSS_full/v2)
F_obs

p_value <- 1 - pf(F_obs, v1, v2) 
p_value

###Since the p-value is 0.99, we can't reject our null hypothesis (that some of the coefficients
#are 0s and do not contribute to the effect of song_popularity). Hence, we continue
#by using the reduced model obtained through backward selection.







###Once we are done with training our model, we just can't assume that it is going to 
#work well on data that it has not seen before. In other words, we cant be sure that 
#the model will have the desired accuracy and variance in production environment. 
#We need some kind of assurance of the accuracy of the predictions that our model is 
#putting out. For this, we need to validate our model. We use 5-Fold Cross Validaion,
#We split the entire data randomly into 5 folds, because using 5 or 10 as these values
#have been shown empirically to yield test error rate estimates that suffer neither 
#from excessively high bias nor from very high variance. We fit the model using the K-1
#i.e. 4 folds, and validate the model using the the remaining K-th fold. We repeat this
#process until every K-fold serves as the test set. Then take the average of the scores,
#which will be the performance metric for the model. putting computational issues aside,
#we choose K-fold CV instead of LOOCV, because it often gives more accurate estimates of
#the test error rate. To perform this, we use R's built-in functions glm() to fit the 
#linear model, and cv.glm() to do cross-validation. We compare the results of the full 
#model and the reduced obtained by backward selection. We get two errors in the delta component. 
#The first one is the average mean-squared error that we obtain from doing K-fold CV. 
#The second is the average mean-squared error that you obtain from 
#doing K-fold CV, but with a bias correction because we have not used LOOCV. 

library(boot)
mod_full <- glm(song_popularity ~ song_duration + key + audio_mode + speechiness + 
                  tempo + loudness + liveness + danceability + audio_valence +
                  instrumentalness, data=dataset)

mod_full_cv <- cv.glm(dataset, mod_full, K=5) 
cv_full <- mod_full_cv$delta[1]



mod_bw <- glm(song_popularity ~ song_duration + key + audio_mode + speechiness + 
                   tempo + loudness + liveness + danceability + audio_valence +
                   instrumentalness, data=dataset)

mod_cv_bw <- cv.glm(dataset, mod_bw, K=5) 
cv_bw <- mod_cv_bw$delta[1]



err <- matrix(c(sqrt(cv_full), sqrt(cv_bw)), ncol=1, byrow=TRUE)
rownames(err) <- c("Full model","Reduced Model")
colnames(err) <- "Root Mean Squared Error"
err <- as.table(err)
err

###Though not significantly lower than the full model - 20.18614, the 
#backward selected model turned out to fit the data better, with a RMSE of
#20.18325. Because we couldn't get a lower error on, we suspect that the reason 
#for this is either removing the energy variable (although it seemed justified
#at the time), or that the variable selection is not well, so we further use 
#Ridge regression to impose a penalty term on predictors to impose a bias on the
#esitmators and LASSO for the latter.