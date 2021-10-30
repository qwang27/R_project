library("tidyverse")
library("tidyr")
library("ggplot2")
library("data.table")
library(RColorBrewer)
library(hrbrthemes)

pl_track_fts <- read.table("./data/track_fts_4000")

fts_cor<- cor(pl_track_fts[,
                           c("danceability","energy","loudness", "mode","speechiness","acousticness", "instrumentalness","liveness", "valence","tempo")])
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

#count number of non-repeat artist in playlists
total_artist <- pl_track_fts %>% group_by(pid,name) %>% summarise(total.n.artist = n_distinct(artist_name))
tracks1 <-pl_track_fts %>% arrange(pid) %>% group_by(pid,name, artist_name) %>% 
  mutate(repeat_artist = lag(artist_name)) %>% filter(!is.na(repeat_artist)) 

repeat_artist <- tracks1 %>% 
 group_by(pid,name) %>% summarise(repeat.n.artist = n_distinct(repeat_artist))
n.no.repeat.artist <- full_join(total_artist, repeat_artist, by = "pid") 
# mutate(d = total.n.artist -repeat.n.artist)
n.no.repeat.artist[is.na(n.no.repeat.artist$repeat.n.artist),]$repeat.n.artist = 0
n.no.repeat.artist <- n.no.repeat.artist %>% mutate(d = total.n.artist -repeat.n.artist)


         