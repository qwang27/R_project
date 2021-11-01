library(tidyverse)
library(tidyr)
library(ggplot2)
library(data.table)     
library(RColorBrewer)
library(hrbrthemes)
library(plotly)

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

n_artist_0 <- read.table("./data/n_artist_0")
dat <-data.frame(d = c(n_artist$d, n_artist_0$d), 
                 lines = rep(c("More recent data","2017"), c(nrow(n_artist),
                                                             nrow(n_artist_0))))
g = dat %>% ggplot(aes(x = d)) 
g + geom_density(aes(color = lines)) + 
  xlab("Number of Non-repeated Tracks ") +
  ylab("Proportion") +
  ggtitle("Proportion of non-repeat playlists")

g = n_artist %>% ggplot(aes(x = d)) 
g + geom_density() + 
  xlab("Number of Non-repeated Tracks ") +
  ylab("Proportion") +
  ggtitle("Proportion of non-repeat playlists in more recent data")

g = n_artist_0 %>% ggplot(aes(x = d)) 
g + geom_density() + 
  xlab("Number of Non-repeated Tracks ") +
  ylab("Proportion") +
  ggtitle("Proportion of non-repeat playlists in 2017")

n_artist <- n_artist %>%arrange(desc(d))

#diverse vs. popular
pop_div<- genres_pl %>% group_by(playlist) %>% summarise(
  median.pop=median(popularity))%>% 
  left_join(n_artist, by = "playlist") %>% 
  
g = ggplot(pop_div, aes(x = d, y = median.pop))
g + geom_point()+
  xlab("Number of Non-repeat artists played")+
  ylab("popularity") +
  ggtitle("Are playlists with more artists also more popular?")


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

g = ggplot(data = genres_pl, aes(x = energy, y = valence)) 
g + geom_point() +
  facet_grid(genre~.)

g = ggplot(data = genres_pl, aes(x = acousticness, y = energy)) 
g + geom_point() +
  facet_grid(genre~.)

genres_pl$genre <- as.factor(genres_pl$genre)
fig <- plot_ly(genres_pl[genres_pl$genre == "blues",], x = ~valence, y = ~energy, z = ~popularity,
               marker = list(symbol = 'circle',size = 1))
fig <- fig %>% add_markers()
fig <- fig %>% layout (scene = list(xaxis = list(title = 'Valence'),
                                    yaxis = list(title = 'Energy'),
                                    zaxis = list(title = 'Popularity')))
fig

#polar plot of the most popular playlists
pop <- genres_pl %>% group_by(playlist) %>% 
  summarise(avg.pop=mean(popularity)) %>% 
  arrange(desc(avg.pop))
top50 <- pop[1:50,] %>% left_join(genres_pl, by = "playlist") 
bottom50 <- pop[298:347,] %>% left_join(genres_pl, by = "playlist")
g = ggplot(data = top50, aes(x = genre)) 
g+geom_bar(aes(fill = genre))+
  ggtitle("Distribution of genres of songs of the top 50 most popular playlists")

g = ggplot(data = bottom50, aes(x = genre)) 
g+geom_bar(aes(fill = genre))+
  ggtitle("Distributon of genres of songs of the bottom 50 popular playlists")
