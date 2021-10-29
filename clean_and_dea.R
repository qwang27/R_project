library("rjson")
library("jsonlite")
library("tidyverse")
library("tidyr")
library("ggplot2")
library("data.table")

json_file <- "~/NYCDSA/Intro_to_R/R_project/spotify_million_playlist_dataset/data/mpd.slice.0-999.json"
pls <- fromJSON(paste(readLines(json_file), collapse = ""))

names(pls)
class(pls$playlists)
nrow(pls$playlist)


#lass(pls_0)
#library(tidyverse)
# one.Row <- pls$playlists[1,]
# class(one.Row)
# colnames(one.Row)
# one.Row$pid
# head(one.Row$tracks,5)
# class(one.Row$tracks)
# length(one.Row$tracks)
# tracks <- one.Row$tracks[[1]]
# class(tracks)
# #cbind(one.Row$pid, one.Row$tracks[[1]])
# pls_0 <- tracks %>% mutate(pid = one.Row$pid)

# loop over rows of playlists 
pl_df_lst = list()
for (row in 1:nrow(pls$playlists)) {
    pl_row <- pls$playlists[row,]
    tracks <- pl_row$tracks[[1]]
    pl_row.new <- tracks %>% mutate(pid = pl_row$pid, name = pl_row$name)%>% 
      separate(track_uri, c("app","track","track_id"), sep = ":", remove = TRUE) %>% 
      select(-app, -track)
    pl_df_lst[[row]] <- pl_row.new
}

pl_df <- bind_rows(pl_df_lst)

# Total number of records in playlists: 67503
nrow(pl_df)

# check n of unique tracks: 34443
pl_df %>% summarise(n_distinct(track_id))
tracks <- data.frame(track_id = unique(pl_df$track_id))
write.table(pl_df, "slice_0999_all")

source("spotify_test.R")

library(spotifyr)

# ft <- get_track_audio_features(pl_df$track_id[1])
# aud_an <- get_track_audio_analysis(pl_df$track_id[1])

#nrow(pl_df) = 67503
# get track audio features in chunks 

df_fts <- as.data.frame(matrix(nrow = 100, ncol = 19 ))
ft_lst.n <- lst() 

fetch_fts <- function(n, from, to) {
for (row in from:to) {
  track_id = tracks$track_id[row]
  ft <- get_track_audio_features(track_id) %>% mutate(track_id = track_id)
  ft_lst.n[[row]] = ft
}
ft_df.n <- bind_rows(ft_lst.n)
#df_tracks <- bind_rows(df_tracks, ft_df.n)
}
df1 <- read.table("df1")
df2 <- read.table("df2")
df3 <- read.table("df3")
df4 <- read.table("df4")
df5 <- read.table("df5")
df6 <- read.table("df6")

df_fts <- bind_rows(df1, df2, df3, df4, df5, df6)
df_fts = unique(df_fts)
write.table(df_fts, "ft_1-")

fetch_fts(7, 4001, 4002)
pl_track_fts <- inner_join(df_tracks, pl_df, by = "track_id") %>% 
  select(-id, -uri, -track_href, -analysis_url, -time_signature)



library(RColorBrewer)
library(hrbrthemes)
fts_cor<- cor(pl_track_fts[,1:11])
melted_fts <- reshape2::melt(fts_cor)
head(melted_fts)
ggplot(data = melted_fts, aes(x = Var1, y = Var2, fill = value)) + geom_tile()+
  scale_fill_distiller(palette = "RdPu")+
  theme_ipsum() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#