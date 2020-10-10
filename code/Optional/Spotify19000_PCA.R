dataset <- read.csv("Project/Spotify19000.csv")
head(dataset)

library(dplyr)
dataset <- dataset %>% distinct()

dataset$song_duration_ms <- dataset$song_duration_ms/60000
names(dataset)[names(dataset) == 'song_duration_ms'] <- 'song_duration'




###One way you can take a train/test split of a dataset is to order the dataset randomly, 
#then divide it into the two sets. This ensures that the training set and test set are 
#both random samples and that any biases in the ordering of the dataset (e.g. if it had 
#originally been ordered by price or size) are not retained in the samples we take for 
#training and testing your models.
set.seed(42)
rows <- sample(nrow(dataset))
dataset <- dataset[rows, c(-1,-2)]
dataset[,c(1:5, 7:8, 10:11, 13:14)]<- scale(dataset[,c(1:5, 7:8, 10:11, 13:14)])

split <- round(nrow(dataset) *0.80)
train_set <- dataset[1:split, ]
test_set <- dataset[(split+1):nrow(dataset),]



#####PCA non-factorized 

#Model matrix
x_train <- model.matrix(~., data=train_set[,-14])
x_test <- model.matrix(~., data=test_set[,-14])

#Principal components
prcmp <- princomp(x_train[,-1], cor=TRUE) 
#take out 1s for intercept, obtained from correlation matrix

str(prcmp)

#To interpret the results, we need to determine how many PCs to examine. Although PCA 
#returns as many compomnents as there are variables, we need components to explain
#at least 90% of the data.
plot(prcmp, main="Components", npcs=13, ylim=c(0,3)) #default is 10 principal components
summary(prcmp) 

#We look for a drop in the percentage of explained variation, and ignore the PCs that
#explain little variation. Here, what is immediately recognizable is that the first principal
#component explains a large proportion of the variance, almost double than the others. Since
#we have 13, if each variable contributed equally, they would contribute approximately 6.7%.
#We would like to use as many components to get at least 90% of the variability. 
##Cumulative proportion of variance tells us that including 10 components we can 
#explain 91.59% of our data. 


###We give on explanation of the first couple of components. Loadings are interpreted as 
#the coefficients of the linear combination of the initial variables from which the 
#principal components are constructed:

barplot(prcmp$loadings[,1], cex.names=0.7, ylim=c(-0.6,0.6))#, las=2)
#Looking at the first component, we see that there are huge loading for variables related
#to how the song is carried out in terms of instrumentalness and acousticness, as well as
#the perceptual measure of intensity and loudness. This is easy to understand, since energetic
#songs tend to feel loud. We can say that the first principal component divides the songs into
#to instrumental and non-instrumental. 

barplot(prcmp$loadings[,2], cex.names=0.7, ylim=c(-0.6,0.6))
#The second component somehow divides the songs to songs with a high degree of danceability
#and valance, and those that do not posses these qualities. This seems natural, since we
#know there are songs which help us release a lot of energy and positive feelings 
#by allowing us to dance. We suspect that these are the songs we normally listen to at clubs.

barplot(prcmp$loadings[,3], cex.names=0.7, ylim=c(-0.6,0.6))
#The third principal component shows an audio mode and key extremization, telling us they aren't
#much dependent of other variables, but they might be between each other. 
#Apart from that, it allows to define songs that have more lyrics and less-to-none.

barplot(prcmp$loadings[,4], cex.names=0.7, ylim=c(-0.6, 0.6))
#Fourth principal components shows a separation between songs that tend to last longer,
#and songs that do not. The loadings also tell us that the negative influences on the component
#might lead to lowered popularity when we fit the model.

barplot(prcmp$loadings[,5], cex.names=0.7, ylim=c(-0.6, 0.6))
barplot(prcmp$loadings[,6], cex.names=0.7, ylim=c(-0.6, 0.6))
barplot(prcmp$loadings[,7], cex.names=0.7, ylim=c(-0.6, 0.6))
barplot(prcmp$loadings[,8], cex.names=0.7, ylim=c(-0.6, 0.6))
barplot(prcmp$loadings[,9], cex.names=0.7, ylim=c(-0.6, 0.6))
barplot(prcmp$loadings[,10], cex.names=0.7, ylim=c(-0.6, 0.6))


###Prediction on test set:
prcmp_test <- predict(prcmp, newdata=x_test)
x_prcmp<-prcmp$scores[,1:10]


pc <- lm(train_set$song_popularity~., data=as.data.frame(x_prcmp))
summary(pc)

pc_predict<- predict(pc, newdata=as.data.frame(prcmp_test))


MSE_pc <- mean((pc_predict-test_set$song_popularity)^2)

RMSE_pc <- sqrt(MSE_pc)
RMSE_pc



###LINEAR MODEL
dataset_lm <- dataset[c(-6504,-9718,-14452), ]

keys <- c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B')
dataset_lm$key <- factor(dataset_lm$key, labels = keys)
rm(keys)

dataset_lm$audio_mode <- factor(ifelse(dataset$audio_mode==0, 'minor','major'))

dataset_lm$time_signature <- factor(dataset$time_signature)



train_set_lm <- dataset_lm[1:split, ]
test_set_lm <- dataset_lm[(split+1):nrow(dataset_lm),]

mod_lm <- lm(song_popularity ~ song_duration + key + audio_mode + speechiness + 
               tempo + loudness + liveness + danceability + audio_valence +
               instrumentalness, data=train_set_lm)
summary(mod_lm)

pred_lm <- predict(mod_lm, newdata=test_set_lm) 

MSE_lm <- mean((pred_lm - test_set$song_popularity)^2)

RMSE_lm <- sqrt(MSE_lm)
RMSE_lm 



##### NO SIGNIFICANT DECREASE IN ERROR #####