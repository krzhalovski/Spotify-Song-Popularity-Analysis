dataset <- read.csv("Project/Spotify19000.csv")
head(dataset)

###We can see that we are working with 18835 observations. Two of these features are 
# represented as factors:  names and the artists of the songs. Judging by their count, 
# we see that "song_name" has 13070 levels which is less than the number of observations. 
# This doesn't seem too strange, since there are a lot of songs titled the same. But even 
# so, the songs are collected from different playlists so we need to search for duplicates 
# and remove them. Of course, prior to any analysis, we check whether our data has any 
# missing values, counting column-wise
str(dataset)

sapply(dataset, function(x) sum(is.na(x)))

library(dplyr)
dataset <- dataset %>% distinct()

dim(dataset)


###As mentioned earlier, the data was already cleaned up and tidied, but some additional 
# modifications had to be made. We created factors for the 'audio_mode' and 'key' variables 
# to make the data easier to interpret. Anyone with at least a bit of musical knowledge 
# would prefer and actually find it easier to understand the analysis if the person could 
# see the keys (C, C#, etc.) and modes (major, minor) instead of going back to the 
# description of features to check their numerical values. We too transform the song 
# duration from miliseconds to minutes.
keys <- c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B')
dataset$key <- factor(dataset$key, labels = keys)
dataset$audio_mode <- factor(ifelse(dataset$audio_mode==0, 'minor', 'major'))
dataset$song_duration_ms <- dataset$song_duration_ms/60000
names(dataset)[names(dataset) == 'song_duration_ms'] <- 'song_duration'
head(dataset)


###We begin our analysis by checking which artists are a part of Spotify playlists the 
# most. Throughout the years, Lady Gaga has been dominant in the pop genre since 2010s, so 
# the fact that she showed up first in our list does not surprise us. Following is Drake, 
# a major influencer on all generations, bringig his fusion of hip hop and contemporary 
# R&B with trap and dancehall music. Kanye West has too shown to be a remarkable 
# alternative and experimental hip hop artist, with his fair share in the pop genre. 
# Eminem is one of the most famous artists in hip hop making his debut in 1996. By looking 
# at the entire list, we can see that all artists are from different time periods, genres 
# and cultures, so we can say that our data is not biased towards a specific type of music 
# nor a time period.
t <- as.data.frame(summary(dataset$artist_name), columns = 'Number of Songs')
#t
colnames(t) <- 'Number of Songs'
head(t, n=20)
#rm(t)


###To review our dataset's content and shape, we provide the summaries and conclude that 
# there is a lot of skewness in most of the predictors, except maybe for the duration of 
# the song and valence. We can observe a negative skew in song popularity (our target 
# variable) as well. The results are represented as follows, and we continue with a more 
# detailed analysis.
summary(dataset)


###By taking a a look at how the popularity scores are distributed, one thing is 
# noticeable instantly. The data is somewhat negatively skewed, and the majority of 
# the songs have a popularity score more than 40, with the mean popularity score at 
# a value of 48.75.  
hist(dataset$song_popularity, probability =  TRUE, main = "Popularity Distribution", xlab="Song Popularity")
dens <- density(dataset$song_popularity, adjust=0.5, na.rm=TRUE)
lines(dens)
abline(v=mean(dataset$song_popularity),col="blue")


###We can assume that the audio mode, key, tempo and time signature of a song are 
# composition elements, which we can make us of to describe and classify music.  

### In simple terms, the emotional center of music comes from one of two places: the 
# major chord or the minor chord. If you're listening to music and you can sense happiness 
# and you're at easet, you're probably listening to a song that uses mostly major chords 
# to create that feeling. Any chord has a relative major or minor version. If we wanted 
# to get moodier and possibly sad, we could think of contrasting those major chords into 
# minors. It looks like people lean more towards songs with a major mode than those with 
# a minor mode. Does this mean people like happier songs?
barplot(table(dataset$audio_mode), xlab="Audio Mode", ylab="Number of Songs")

###Additionaly to the audio mode, the association of musical keys with specific 
# emotional or qualitative characteristic was fairly common prior to the 20th century. 
# When Mozart or Beethoven or Schubert wrote a piece in a Ab major, for example, they 
# were well aware that this was the 'key of the grave' and knew that many in their 
# audiences were as well. In a study by Spotify's Kenny Ning, it was shown that more than 
# a third of 30 million songs observed are in one of four keys: G major, C major, D major, 
# or A major. This reveals the kind of sounds we tend to see more commonly in music: bold, 
# upbeat majors tend to outvote the moodier minors. In our data, the most common key is C, 
# but very close to it are G and C# (Db). 
barplot(table(dataset$key), xlab="Pitch Key", ylab="Number of Songs")

###Popular songs tend to have a faster tempo with an average BPM of 121.105. 
# This is commonly known as 'allegro', ranging from 120 - 156 BPM. Modern music tempos 
# are in this range: techno (120-140 BPM), house (115-130 BPM), hip-hop (80-120), and 
# similar.  
par(mfrow=c(1,2))

hist(dataset$tempo, prob=TRUE, xlab="Tempo", main = "")
abline(v=mean(dataset$tempo),col="blue")

dataset$tempo_factor <- factor(ifelse(dataset$tempo > 176, 'Presto', ifelse(dataset$tempo<=176 & dataset$tempo>156, 'Vivace', ifelse(dataset$tempo<=156 & dataset$tempo>120, 'Allegro', ifelse(dataset$tempo<=120 & dataset$tempo>108, 'Moderato', ifelse(dataset$tempo<=108 & dataset$tempo>76, 'Andante', 'Andagio'))))))

barplot(table(dataset$tempo_factor), ylim=c(0,6000), xlab="Tempo", ylab="Number of Songs")

par(mfrow=c(1,1))

par(mfrow=c(1,2))

###Categories of tempo:
par(mfrow=c(1,2))

boxplot(dataset$song_popularity~dataset$tempo_factor, xlab="Tempo", ylab="Song Popularity")
boxplot(dataset$tempo~dataset$key, xlab="Key", ylab="Tempo")

par(mfrow=c(1,1))
dataset$tempo_factor<-NULL

###Most common time signature: 4x4
barplot(table(dataset$time_signature), xlab="Time Signature", ylab="Number of Songs")

###Average duration of a song:
hist(dataset$song_duration, prob=TRUE, breaks=50, xlim=c(0,8), xlab="Song Duration", main = "")
abline(v=mean(dataset$song_duration),col="blue")


###Univariate distribution of numerical variables:
par(mfrow=c(2,2))

hist(dataset$acousticness, prob=TRUE, xlab="Acousticness", main = "")
lines(density(dataset$acousticness, adjust=0.5, na.rm=TRUE))
abline(v=mean(dataset$acousticness),col="blue")

hist(dataset$danceability, prob=TRUE, xlab="Danceability", main = "")
lines(density(dataset$danceability, adjust=0.5, na.rm=TRUE))
abline(v=mean(dataset$danceability),col="blue")

hist(dataset$energy, prob=TRUE, xlab="Energy", main = "")
lines(density(dataset$energy, adjust=0.5, na.rm=TRUE))
abline(v=mean(dataset$energy),col="blue")

hist(dataset$instrumentalness, prob=TRUE, xlab="Instrumentalness", main = "")
lines(density(dataset$instrumentalness, adjust=0.5, na.rm=TRUE))
abline(v=mean(dataset$instrumentalness),col="blue")

par(mfrow=c(1,1))


par(mfrow=c(2,2))

hist(dataset$liveness, prob=TRUE, xlab="Liveness", main = "")
lines(density(dataset$liveness, adjust=0.5, na.rm=TRUE))
abline(v=mean(dataset$liveness),col="blue")


hist(dataset$loudness, prob=TRUE, xlab="Loudness", main = "")
lines(density(dataset$loudness, adjust=0.5, na.rm=TRUE))
abline(v=mean(dataset$loudness),col="blue")


hist(dataset$speechiness, prob=TRUE, xlab="Speechiness", main = "")
lines(density(dataset$speechiness, adjust=0.5, na.rm=TRUE))
abline(v=mean(dataset$speechiness),col="blue")


hist(dataset$audio_valence, prob=TRUE, xlab="Audio Valence", main = "")
lines(density(dataset$audio_valence, adjust=0.5, na.rm=TRUE))
abline(v=mean(dataset$audio_valence),col="blue")

par(mfrow=c(1,1))

###Danceability is most dense in the interval [0.5, 0.8] telling us that popular songs 
# are quite danceable.  Energy peaks around 0.7 and is most dense around the same interval.
# Audio valance has a symmetrical distribution and it too might be correlated with 
# danceability and energy, considering happy songs make people energetic and wanting to 
# dance. The majority of the songs tend to have very low liveness, i.e. they are not 
# recorded during a live session, and songs are between -10 and 0 dB. Acousticness and 
# speechiness seem to have a similar distribution, saying that most songs are acoustic 
# and have less spoken words (which too might be concluded looking at the instrumentalness 
# over the songs).  



### We can divide the song popularity into low and high, based on whether it is below or 
# above 75. Continuing our analysis, we observe that:
# 981 songs are very popular, with a rating above 75, which comprises approximately 61% 
#of our total data.  
# Songs that are more popular tend to last 3-4 minutes, and their acousticness level is 
#below 0.1 meaning that the more a song is popular, the less likely that it's acoustic.  
# Danceability and energy have a very similar mass, saying when songs are more popular 
#they have a moderate-to-high value in both of them, i.e. the songs are more danceable 
#and energetic.  
# Instrumentalness is most likely to be 0 when a song is very popular, and when liveness 
#is more than 0.8 we can say that there is strong likelihood that the song is 
#studio-recorded. Our values of liveness for songs with popularity > 75 are concentrated 
#arlond 0.1.  
# Popular songs tend to be louder, mostly ranging from -6 to -4 dB, and contain 
#less-to-no lyrics.  
# The average tempo is around 120 BPM, with popular songs having a tempo in the range 
#of (90,130). No surprise, people like to listen to moderately cheerful songs. 


cat("Number of highly popular songs: ", length(which(dataset$song_popularity > 75)))
cat("Percentage of highly popular songs: ", length(which(dataset$song_popularity > 75))/length(dataset))
pop_factor <- factor(ifelse(dataset$song_popularity > 75, 'high','low'))
p <- pop_factor =='high'
rm(pop_factor)

par(mfrow=c(2,2))

plot(dataset$song_duration, dataset$song_popularity, col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Duration in minutes", xlim=c(0,6))

plot(dataset$acousticness, dataset$song_popularity, col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Acousticness")

plot(dataset$danceability, dataset$song_popularity, col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Danceability", xlim=c(0.5,0.9))
#plot(dataset$danceability, dataset$song_popularity, col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Danceability")

plot(dataset$energy, dataset$song_popularity, col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Energy", xlim=c(0.5,0.9))
#plot(dataset$energy, dataset$song_popularity, col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Energy")

par(mfrow=c(1,1))


par(mfrow=c(2,2))

plot(dataset$instrumentalness, dataset$song_popularity, col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Instrumentalness")

plot(dataset$liveness, dataset$song_popularity, col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Liveness", xlim=c(0,0.2))
#plot(dataset$liveness, dataset$song_popularity, col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Liveness")

plot(dataset$loudness, dataset$song_popularity, col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Loudness")

plot(dataset$speechiness, dataset$song_popularity, col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Speechiness")

par(mfrow=c(1,1))


par(mfrow=c(2,2))

plot(dataset$tempo, dataset$song_popularity, col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Tempo", xlim=c(80,140))
#plot(dataset$tempo, dataset$song_popularity, col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Tempo")

plot(dataset$audio_valence, dataset$song_popularity,  col=p+1, pch=p*15+1, ylab="Song Popularity", xlab="Valence")

par(mfrow=c(1,1))



### Correlation colour and numerical plots:
library(corrplot)
library(RColorBrewer)
dataset_cor <-cor(scale(dataset[c(-1, -2, -8, -11, -14)]))

corrplot(dataset_cor, type="lower", order="hclust",col=brewer.pal(n=8, name="RdYlBu"))

corrplot(dataset_cor, method="number")

###We can observe in the strongest tones the existing correlations between the 
# independent variables. Energy and loudness of the tracks are directly proportional, 
# meaning if the loudness of a track increases then chances of it being energetic are 
# quite higher. As we suspected, valence is positively correlated with danceability and 
# energy. Interestingly enough, energy and acousticness are negatively correlated with 
# each other, as are loudness and acousticness. Hence, we face a problem of 
# multicollinearity.


###The strongest correlation is between loudness and energy, so we decide to inspect it.
library(WVPlots)
ScatterHist(dataset, "loudness", "energy", smoothmethod="lm", title="Loudness vs Energy")

#We can see that as the values of loudness are closer to 1 (above 0.6 approximately), the 
#probability that our song has a high content of rhythms is higher. Popular music's 
#energy level plateaued slightly during the '80s. Other than that, we've seen a consistent 
#progress in the energy level of our favorite songs.

###The second highest positive correlation is between the variables "dance" and "valance". 
#Psychologists put a spin on that concept, using the word "valence" to describe whether 
#something is likely to make someone feel happy (positive valence) or sad (negative valence). 

ScatterHist(dataset, "audio_valence", "danceability", smoothmethod="lm", title="Valence vs Danceability")
