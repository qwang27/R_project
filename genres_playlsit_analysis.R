library(tidyverse)
library(tidyr)
library(ggplot2)
library(data.table)     
library(RColorBrewer)
library(hrbrthemes)

setwd("~/NYCDSA/Intro_to_R/R_project")

genres_pl <- read.table("~/NYCDSA/Intro_to_R/R_project/data/multi_genre_playlist")
pl_track_fts <- read.table("~/NYCDSA/Intro_to_R/R_project/data/track_fts_4000")

names(genres_pl) = tolower(names(genres_pl))
# names(pl_track_fts) <- gsub("_",".",names(pl_track_fts))
names(genres_pl) <- gsub("_",".",names(genres_pl))
# colnames(genres_pl)[17] = "track.id"
# colnames(pl_track_fts)[23] = "playlist"

#check common playlists

# Playlists_common <- genres_pl %>% select(-track.href, -analysis.url, -time.signature) %>% 
#                                   semi_join(pl_track_fts, by = c("id" = "track.id"))

n_distinct(genres_pl$playlist) # n playlist: 347

total_artist <- genres_pl %>% arrange(playlist) %>% 
  group_by(playlist) %>% 
  summarize(total.n.artist = n_distinct(artist.name))

tracks1 <-genres_pl %>% arrange(playlist) %>% 
  group_by(playlist, artist.name) %>% 
  mutate(repeat.artist = lag(artist.name)) %>% filter(!is.na(repeat.artist)) 

repeat_artist <- tracks1 %>% 
  group_by(playlist) %>%
  summarise(repeat.n.artist = n_distinct(repeat.artist))
n_artist <- left_join(total_artist, repeat_artist, by = "playlist") 
# mutate(d = total.n.artist -repeat.n.artist)
n_artist[is.na(n_artist$repeat.n.artist),]$repeat.n.artist = 0
n_artist <- n_artist %>% mutate(d = total.n.artist -repeat.n.artist)

g = n_artist %>% ggplot(aes(x = d)) 
g + geom_density() + 
  xlab("Number of Non-repeated Tracks ") +
  ylab("Proportion of playlists") +
  ggtitle("Proportion of non-repeat playlist densities on more recent data")

# Principal Component Analysis
audio.pca <- prcomp(genres_pl[, c("danceability","energy","loudness", "valence",
                                     "speechiness","acousticness", "instrumentalness","liveness",
                                     "duration.ms","tempo")],
                    center = TRUE, scale = TRUE)
summary(audio.pca)
#scree plot
plot(audio.pca, type = "line")
str(audio.pca)
library(devtools)
library(ggbiplot)
#ggbiplot(audio.pca)
library(ggfortify)

autoplot(audio.pca, data = genres_pl,
         loadings = TRUE, loadings.colour = "blue", 
         loadings.label = TRUE)
# seems danceibility, energy, loudness, valence explain most of the variations in the data
summary(audio.pca)

g = ggplot(data = genres_pl, aes(x = duration.ms, y = valence)) 
g + geom_point(aes(shape = genre)) +
  facet_grid(genre~.)
