dataset <- read.csv("Project/Spotify19000.csv")
head(dataset)

library(dplyr)
dataset <- dataset %>% distinct()

keys <- c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B')
dataset$key <- factor(dataset$key, labels = keys)

dataset$audio_mode <- factor(ifelse(dataset$audio_mode==0, 'minor', 'major'))

dataset$song_duration_ms <- dataset$song_duration_ms/60000
names(dataset)[names(dataset) == 'song_duration_ms'] <- 'song_duration'

dataset$time_signature <- factor(dataset$time_signature)

dataset <- dataset[, c(-1,-2)]#, -6)]


# We will now use logistic regression to build a model that predicts the 
#probability a song is highly popular among users (i.e. has a popularity 
#score above 75). We will begin by transforming the popularity score into
#a factor considering the model that includes all the explanatory variables.
dataset$song_popularity <- factor(ifelse(dataset$song_popularity>75, 'high', 'low'))

#Using the contrasts() function, we see that R created a dummy variable with a 1 for 
#songs that are not very popular. NOPE: We use R's relevel() function to re-order the levels 
#of the factor, so to have an easier interpretation of the model we build.
contrasts(dataset$song_popularity) 
dataset$song_popularity <- relevel(dataset$song_popularity, ref = 2)
contrasts(dataset$song_popularity) 

###We firt buld the null model, where the intercept is the log-odds of "success",
#estimated without reference to any predictors i.e. the predictors have no
#effect on song popularity:
mod_null <- glm(song_popularity ~ +1, data=dataset, family="binomial")
summary(mod_null)

###And continue with the full model:
mod_full <- glm(song_popularity ~ ., data=dataset, family="binomial")

###As we did in the linear regression case, we have to check whether some of
#the predictors have a high VIF value. 
car::vif(mod_full)

###We see that energy does not have a high VIF value as it did in the previous
#case, so we do not exclude it.


summary(mod_full)
#Using a logistic regression, it gives us the estimate, standard errors, z-score
#and p-values on each of the coefficients. In comparison with the full linear
#regression model, we can see that different predictors are significant.
#Take for instance the predictors where the p-value < 0.001:
# acousticness
# danceability
# energy
# instrumentalness
# loudness
# audio valence
#which are different from the regression fit, excluding danceability and 
#instrumentalness.

#The estimates now give the change in the log odds of the outcome for a one unit increase
#in the predictor variable, but not all of them are significant:
# For every one unit change in song duration, the log odds of a song being popular
#increase by 0.06305
# For every one unit change in acousticness, the log odds of a song being popular
#decrease by 0.9296;
# For every one unit change in danceability, the log odds of a song being popular
#increase by 1.709; etc.

#technical: log(pi / 1-pi) = b_0 + b_1*x_1....


###We also see two forms of deviance - the null deviance and the residual deviance. 
#Deviance is a measure of goodness of fit of a generalized linear model:
#dev = -2l(theta, y), were l=loglikelihood. The null deviance shows how well the 
#response variable is predicted by a model that includes only the intercept 
#i.e. b_1=b_2=b_n = 0. Residual deviance reflects the saturated model, and it has 
#reduced by 575.2 points on 14899 degrees of freedom. This is equivalent to comparing a
#pair of nested models: H_0: no relationship between the X variables and the Y variable
#i.e. the predictions are no closer to the actual Y values than you would expect by 
#chance vs H_1: full model, all X variables relate with Y.

anova(mod_null, mod_full, test="Chisq")
#The results from our hypothesis testing tells us that the predictors should not be
#removed from the model since Pr(>Chi) is very small (<2.2e-16).

#technical: D = (RSSnull - RSSfull / (dimRSSfull-dimRSSnull)) / (RSSfull/n-p_full-1) ~chi q, n-p_full-1



#The logit function is the natural log of the odds that the response variable (in our
#case song popularity) equals one of the categories (high popularity vs low popularity).
#The type="response" option tells R to output probabilities of the form P(Y = 1|X), 
#as opposed to other information such as the logit. These values correspond to the 
#probability of the song being very popular i.e. P(Y = "high" | X):
log_prob <- predict(mod_full, type="response")


###The following two commands create a vector of class predictions based on whether 
#the predicted probability of a song being more popular is greater than or less than 
#0.5. Given these predictions, we can see how many observations were correctly or 
#incorrectly classified.
glm_pred <- rep("low",nrow(dataset)) 
glm_pred[log_prob>.5] <- "high"
table(dataset$song_popularity,glm_pred)
###We see that our model has predicted all songs in our dataset to have low popularity.
TP<-0
TN<-13945
FP<-0
FN<-981


#We report:

#Overall error rate = FP + FN / all observations:
err <- (FP+FN)/14926*100 #6.5724%

#Accuracy = TP+TN/all
acc <- (TP+TN)/14926*100 #93.4275%

#True Positive Rate = TP / TP + FN 
tpr <- TP/(TP+FN)*100 #0

#False Positive Rate = FP / FP + TN
fpr <- FP/(FP+TN)*100 #0

#Specificity = 1 - FPR
spec <- (1 - (fpr/100))*100 #100%

#Precision = TP/TP+FP
prec <- TP/(TP+FP)*100 #0


###A ROC curve is constructed by plotting the true positive rate (TPR) against the 
#false positive rate (FPR). The true positive rate is the proportion of observations 
#that were correctly predicted to be positive out of all positive observations 
#(TP/(TP + FN)). It shows the trade-off between sensitivity (or TPR) and specificity 
#(1 - FPR), and ideally it would be closer to the top-left corner and indicate good
#performance. This is effective for imbalanced binary classification, as it focuses
#on the minority class. We calculate the area under the ROC curve - AUC, which is 
#equivalent to the probability that a randomly chosen positive instance is ranked 
#higher than a randomly chosen negative instance. So we can see that our model's 
#predictions are 72.3% correct.
library(pROC)
roc_out <- roc(dataset$song_popularity, log_prob, levels=c("low", "high"))
plot(roc_out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
coords(roc_out, "best", transpose=TRUE) #thresh = 0.068

#By lowering the threshold, we are to get the following results:
log_prob_th <- predict(mod_full, type="response")
glm_pred_th <- rep("low",nrow(dataset)) 
glm_pred_th[log_prob_th>.068] <- "high"
table(dataset$song_popularity,glm_pred_th)

TP<-715
TN<-8371
FP<-5574
FN<-266

#Overall error rate = FP + FN / all observations:
err_th <- (FP+FN) / 14926 * 100 #39.1263%

#True Positive Rate = TP / TP + FN
tpr_th <-  TP/(TP+FN) *100 #72.8848%

#False Positive Rate = FP / FP + TN
fpr_th <- FP/(FP+TN)*100 #39.9713%

#Specificity = 1 - FPR
spec_th <- (1-(fpr_th/100))*100 #60.0286

#Precision = TP/TP+FP
prec_th <- 715/(715+5574)*100 #11.3690%




###As observed in the linear regression analysis, many of the variables used not associated
#with the response (i.e. they are non-significant) and including such irrelevant variables
#leads to unnecessary complexity in the resulting model. This is the reason why we perform
#a feature selection using backward selection:
step_mod <- step(mod_full, steps=10,  trace=1, direction="backward") 

#Our variable selection is based on AIC, and accordingly the model that would, in theory,
#maximize its predictive capability is:
# song popularity = b_0 + b_1*song_duration + b_2*audio_valence + b_3*acousticness +
#                         b_4*danceability + b_5*energy + b_6*instrumentalness +
#                         b_7*loudness

mod_full_bw <- glm(song_popularity ~ song_duration + audio_valence + acousticness +
                        danceability + energy + instrumentalness + loudness, 
                        data=dataset, family='binomial')

summary(mod_full_bw)


#Comparing full and reduced model obtained by backward selection:
anova(mod_full_bw, mod_full, test="Chisq")
#Since the results from our hypothesis testing shows Pr(>Chi) = 
#0.3171, we can safely remove some of the predictors.

#technical: D = (RSSred - RSSfull / (dimRSSfull-dimRSSred)) / (RSSred/n-p_full-1) ~chi q, n-p_full-1



#The following approach considers a train-validation (test) set split:
set.seed(42)
dataset <- dataset[sample(nrow(dataset)),] #randomly shuffle data
split <- round(nrow(dataset) *0.80)
train_set <- dataset[1:split, ]
test_set <- dataset[(split+1):nrow(dataset),]

mod_lr<- glm(song_popularity ~ song_duration + audio_valence + acousticness +
                      danceability + energy + instrumentalness + loudness,
                          data=train_set, family='binomial')

summary(mod_lr)

###As mentioned, the estimates give the change in the log odds of the outcome for 
#a one unit increase in the predictor variable:
# For every one unit change in song duration, the log odds of a song being popular
#increases by 0.08;
# For every one unit change in valence, the log odds of a song being popular
#decreases by 0.9;
# For every one unit change in acousticness, the log odds of a song being popular
#decreases by 0.75;
# For every one unit change in danceability, the log odds of a song being popular
#increases by 1.87;
# For every one unit change in energy, the log odds of a song being popular
#decreases by 2.53;
# For every one unit change in instrumentalness, the log odds of a song being popular
#decreases by 3.82;
# For every one unit change in loudness, the log odds of a song being popular
#increases by 0.23.


###95% confidence interval for the coefficients in terms of their effect on the log-odds 
#of a song being popular:
exp(confint(mod_lr))

#technical: A level C confidence interval for the odds ratio exp(b1 )is obtained by transforming 
#the confidence interval for the slope: (exp((b1) - z*SE(b1)), exp((b1) + z*SE(b1)))

#P(Y=high | X) 
pred_lr_train <- predict(mod_lr, train_set, type = 'response')


###Results on training set:
roc_out <- roc(train_set$song_popularity, pred_lr_train, levels=c("high", "low"))
plot(roc_out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
coords(roc_out, "best", transpose = TRUE) #best thresh is 0.06911304

train_pred <- rep("low",nrow(train_set)) 
train_pred[pred_lr_train>.0691] <- "high"
table(train_set$song_popularity, train_pred)

TP<-565
TN<-6758
FP<-4398
FN<-220
  
#Overall error rate = FP + FN / all observations:
err_train <- (FP+FN)/11941*100 #38.67%

#Accuracy = TP+TN/all
acc_train <- (TP+TN)/11941*100 #61.32%

#True Positive Rate = TP / TP + FN 
tpr_train <- TP/(TP+FN)*100 #71.97%

#False Positive Rate = FP / FP + TN
fpr_train <- FP/(FP+TN)*100 #39.42%

#Specificity = 1 - FPR
spec_train <- (1 - (fpr_train/100))*100 #60.57%

#Precision = TP/TP+FP
prec_train <- TP/(TP+FP)*100 #12.24%

#An the model precicts correctly 71.7% of the data.



###Results on test data:
pred_lr_test <- predict(mod_lr, newdata = test_set, type = 'response')

roc_out <- roc(test_set$song_popularity, pred_lr_test, levels=c("high", "low"))
plot(roc_out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
###We use the same threshold as in the training set.

test_pred <- rep("low",nrow(test_set)) 
test_pred[pred_lr_test>.0691] <- "high"
table(test_set$song_popularity, test_pred)

TP<-144
TN<-1675
FP<-1114
FN<-52

#Overall error rate = FP + FN / all observations: (accuracy)
err_test <- (FP+FN)/2985*100 #39.06%

#Accuracy = TP+TN/all
acc_test <- (TP+TN)/2985*100 #60.93%

#True Positive Rate = TP / TP + FN 
tpr_test <- TP/(TP+FN)*100 #73.46%

#False Positive Rate = FP / FP + TN
fpr_test <- FP/(FP+TN)*100 #39.94%

#Specificity = 1 - FPR
spec_test <- (1 - (fpr_test/100))*100 #60.05%

#Precision = TP/TP+FP
prec_test <- TP/(TP+FP)*100 #11.44%

###We see that the overall error rate is roughly 40%, and it's accuracy is 60.93%. With respect
#to the training set, it has an increase of 1% in error rate, and decrease of 1% in accuracy.
#Precision, which tells us how much of the retrieved instances that are relevant (i.e. number of 
#songs that are actually popular and are predicted as such), isn't even 1%, but the relevant 
#instances that are retrieved is shown by the TPR equaling to 73%, higher by 1% than the one obtained
#in training set results. Judging also by the ROC curve, we see that the model's predictions 
#are correct 72% of the time by setting the threshold to 0.0691, the coordinates of the ROC curve
#on the training set. 




###We now try to fit a non-linear boundary between classifiers. To do so, we 
#perform a Quadratic Discriminant Analysis on our dataset. The QDA classifier 
#results from assuming that the observations from each class are drawn from a 
#Gaussian distribution, and plugging estimates for the parameters into Bayes' 
#theorem in order to perform prediction. It assumes that each class has its own 
#covariance matrix.

qda_red <- qda(song_popularity ~ song_duration + audio_valence + acousticness +
                 danceability + energy + instrumentalness + loudness, data=train_set)
qda_red

### pi1_hat = 0.9342 - 93.42% of training data corresponds to song having a popularity
#score below 80, pi2_hat = 0.0657 - 6.57% highly popular songs

### QDA also provides us the group means - average of each predictor within each class,
#and are used by QDA as estimates of mu_j: These suggest that when songs have a high
#popularity score, their song duration, danceability, enery and loudness are greater
#than songs with low popularity score. We expected valence to be greater too,
#but we suspect that this might be due to classes being unbalanced...unless
#people do feel better with pleasing music, but not too energetic and danceable.
#On the contrary, songs with low popularity tend to have greater acousticness and
#instrumentalness values, which does not come as a surprise.

qda_pred <- predict(qda_red,test_set)$class 
table(test_set$song_popularity, qda_pred)

TP<-28
TN<-2645
FP<-144
FN<-168

#Overall error rate = FP + FN / all observations:
err_qda <- (FP+FN)/2985*100 #10.4522%

#Accuracy = TP+TN/all
acc_qda <- (TP+TN)/2985*100 #89.5477%

#True Positive Rate = TP / TP + FN 
tpr_qda <- TP/(TP+FN)*100 #14.2857%

#False Positive Rate = FP / FP + TN
fpr_qda <- FP/(FP+TN)*100 #5.1631%

#Specificity = 1 - FPR
spec_qda <- (1 - (fpr_qda/100))*100 #94.8368%

#Precision = TP/TP+FP
prec_qda <- TP/(TP+FP)*100 #16.2790%

###In comparison to our logistic model, using QDA has yielded us an almost multiplicatively
#smaller error of 10.4% and an accuracy of 89% which is almost 20 percent larger
#than the one obtained previously. TPRand FPR have decreased, but specificity has
#increased to almost 95%, as well as precision of returning relevant instances at
#a rate of 16%.



###Additionally, we perform a K-Nearest Neighbour classification for the test set from training set. 
#Unlike most algorithms, K-NN is a non-parametric model which means that it does not make any 
#assumptions about the dataset. This makes the algorithm more effective since it can handle realistic 
#data. K-NN is a lazy algorithm, meaning that it memorizes the training data set instead of learning 
#a discriminative function from the training data. For each observation of the validation set, the K
#nearest (in Euclidean distance) validation set vectors are found, and the classification is decided 
#by majority vote. In our case, we will fit the model using a value of one for K, meaning that it will
#classify a song based on most similar features with another already classified observation in the
#training set. To perform this kind of modeling, we have to use the original numerical predictors we had 
#in the dataset, prior to modifications.
dataset$key <- as.numeric(dataset$key) - 1
dataset$audio_mode <- as.numeric(dataset$audio_mode) - 1
dataset$time_signature <- ifelse(dataset$time_signature==0, 0, ifelse(dataset$time_signature==1, 1, 
                                 ifelse(dataset$time_signature==3, 3, ifelse(dataset$time_signature==4, 4, 5))))

train_set <- dataset[1:split, ]
test_set <- dataset[(split+1):nrow(dataset),]

library(class)
knn_pred <- knn(train_set[,-14], test_set[,-14], train_set$song_popularity, k=1)
table(test_set$song_popularity, knn_pred)
TP<-24
TN<-2617
FP<-172
FN<-172

#Overall error rate = FP + FN / all observations:
err_knn <- (FP+FN)/2985*100 #11.5242%

#Accuracy = TP+TN/all
acc_knn <- (TP+TN)/2985*100 #88.4757%

#True Positive Rate = TP / TP + FN 
tpr_knn <- TP/(TP+FN)*100 #12.2448%

#False Positive Rate = FP / FP + TN
fpr_knn <- FP/(FP+TN)*100 #6.1670%

#Specificity = 1 - FPR
spec_knn <- (1 - (fpr_knn/100))*100 #93.8329%

#Precision = TP/TP+FP
prec_knn <- TP/(TP+FP)*100 #12.2448%



###When setting K=1, the overal error rate drops to neary 11.5%, and it's accuracy
#boosts to 88.47%, which is far better than what we got using logistic regresion.
#Precision, which tells us how much of the retrieved instances that are relevant (i.e. number of 
#songs that are actually popular and are predicted as such) is now 12.24% which
#is not much and  specificity is 93.8%. We get improved results than the logistic
#regression model.

err <- matrix(c(acc_test, acc_qda, acc_knn, prec_test, prec_qda, prec_knn,
                tpr_test, tpr_qda, tpr_knn), ncol=3, byrow=TRUE)
rownames(err) <- c("Accuracy","Precision","Recall / TPR")
colnames(err) <- c("Logistic Regression", "QDA", "1-NN")
err <- as.table(err)
err



###We see that it is quite difficult to determine if a song will be a popular or not,
#and there appear to be other factors at play that are not necessarily included in this 
#dataset. Other factors that might influence if a song will be popular or not could be:
#whether an artist has had previous hits, the genre of the music, collaborations between
#artists, etc.