library(tidyverse)
library(tidyr)
library(ggplot2)
library(data.table)     
library(RColorBrewer)
library(hrbrthemes)

pl_track_fts <- read.table("./data/track_fts_4000")
names(pl_track_fts) <- gsub("_",".",names(pl_track_fts))
#Correlation Matrix of all audio features 
pl_track_fts <- pl_track_fts %>% mutate(key = as.factor(key), 
                                        mode = as.factor(mode))
fts_cor<- cor(pl_track_fts[ ,c("danceability","energy","loudness", "valence",
                               "speechiness","acousticness", "instrumentalness","liveness",
                             "duration.ms.x","tempo")])
melted_fts <- reshape2::melt(fts_cor)
head(melted_fts)
ggplot(data = melted_fts, aes(x = Var1, y = Var2, fill = value)) + geom_tile()+
  scale_fill_distiller(palette = "RdPu")+ 
  ggtitle("A Heatmap of Correlation of Spotify Sound Features of Song Tracks")+
  theme_ipsum() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title=element_text( hjust=0.5, vjust=0.5))

# colnames(pl_track_fts)
# g = ggplot(data = pl_track_fts, aes(x = valence, y = danceability))
# g + geom_point()
# 
# g = ggplot(data = pl_track_fts, aes(x = valence, y = energy))
# g + geom_point()
# 
# test <- pl_track_fts %>% group_by(pid) %>% 
#       summarise(danceability_mean = mean(danceability),
#                 energy_mean = mean(energy))

#analyze the diversity of a playlist: Counting number of non-repeat artist in playlists
library(dplyr)
total_artist <- pl_track_fts %>% arrange(pid, name) %>% 
  group_by(pid) %>% 
  summarize(total.n.artist = n_distinct(artist.name))

tracks1 <-pl_track_fts %>% arrange(pid) %>% 
  group_by(pid,name, artist.name) %>% 
  mutate(repeat.artist = lag(artist.name)) %>% filter(!is.na(repeat.artist)) 

repeat_artist <- tracks1 %>% 
  group_by(pid,name) %>%
  summarise(repeat.n.artist = n_distinct(repeat.artist))
n_artist <- left_join(total_artist, repeat_artist, by = "pid") 
# mutate(d = total.n.artist -repeat.n.artist)
n_artist[is.na(n_artist$repeat.n.artist),]$repeat.n.artist = 0
n_artist <- n_artist %>% mutate(d = total.n.artist -repeat.n.artist)

g = n_artist %>% ggplot(aes(x = d)) 
g + geom_density() + 
    xlab("Number of Non-repeated Tracks ") +
    ylab("Proportion of playlists")

# Principal Component Analysis
audio.pca <- prcomp(pl_track_fts[, c("danceability","energy","loudness", "valence",
                                     "speechiness","acousticness", "instrumentalness","liveness",
                                     "duration.ms.x","tempo")],
                    center = TRUE, scale = TRUE)
summary(audio.pca)
#scree plot
plot(audio.pca, type = "line")
str(audio.pca)
library(devtools)
library(ggbiplot)
#ggbiplot(audio.pca)

autoplot(audio.pca, data = pl_track_fts,
         loadings = TRUE, loadings.colour = "blue", loadings.label = TRUE)
# seems danceibility, energy, loudness, valence explain most of the variations in the data

audio.pca <- prcomp(~ danceability + energy + loudness + valence+speechiness +
                      acousticness+ instrumentalness+liveness 
                      + tempo + duration.ms.x,
                                     data = pl_track_fts)
summary(audio.pca)
library(ggfortify)
autoplot(audio.pca, data = pl_track_fts,
         loadings = TRUE, loadings.colour = "blue", loadings.label = TRUE)

