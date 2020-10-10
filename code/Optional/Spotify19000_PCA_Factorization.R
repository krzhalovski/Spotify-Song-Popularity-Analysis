dataset <- read.csv("Project/Spotify19000.csv")
head(dataset)

library(dplyr)
dataset <- dataset %>% distinct()

dataset$song_duration_ms <- dataset$song_duration_ms/60000
names(dataset)[names(dataset) == 'song_duration_ms'] <- 'song_duration'

keys <- c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B')
dataset$key <- factor(dataset$key, labels = keys)
rm(keys)

dataset$audio_mode <- factor(ifelse(dataset$audio_mode==0, 'minor','major'))

dataset$time_signature <- factor(dataset$time_signature)

set.seed(42)
rows <- sample(nrow(dataset))
dataset <- dataset[rows, c(-1,-2)]
dataset[,c(1:5, 7:8, 10:11, 13:14)]<- scale(dataset[,c(1:5, 7:8, 10:11, 13:14)])

split <- round(nrow(dataset) *0.80)
train_set <- dataset[1:split, ]
test_set <- dataset[(split+1):nrow(dataset),]



#Model matrix
x_train <- model.matrix(~., data=train_set[,-14])
x_test <- model.matrix(~., data=test_set[,-14])

#Principal components
prcmp_f <- princomp(x_train[,-1], cor=TRUE) 
summary(prcmp_f)
plot(prcmp_f, npcs=26, ylim=c(0,3.5))

###>90% of the data is explained with the first 20 components.

#Model
prcmp_f_test <- predict(prcmp_f, newdata=x_test)
x_prcmp_f <- prcmp_f$scores[,1:20]

pc_f <- lm(train_set$song_popularity~., data=as.data.frame(x_prcmp_f))
summary(pc_f)

pc_predict_f<- predict(pc_f, newdata=as.data.frame(prcmp_f_test))


MSE_pc_f <- mean((pc_predict_f-test_set$song_popularity)^2)
MSE_pc_f

MAE_pc_f <- mean(abs(pc_predict_f-test_set$song_popularity))
MAE_pc_f



###Linear fit:
dataset_lm <- dataset[c(-6504,-9718,-14452), ]

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

