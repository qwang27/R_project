library("rjson")
library("jsonlite")
library("tidyverse")
library("tidyr")
library("ggplot2")
library("data.table")

setwd("~/NYCDSA/Intro_to_R/R_project")

file_list <- list.files(path = "./data/spotify_million_playlist_dataset/data")

for (i in length(file_list)){
 
}

json_file <- "~/NYCDSA/Intro_to_R/R_project/data/spotify_million_playlist_dataset/data/mpd.slice.999000-999999.json"
pls <- fromJSON(paste(readLines(json_file), collapse = ""))






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