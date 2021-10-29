library(spotifyr)
Sys.setenv(SPOTIFY_CLIENT_ID = "05cf1d02089f45408fd07c698398e044")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "c8629d3931914fc1ae3e309af0b9d29f")

access_token<- get_spotify_access_token(
  client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
  client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")
) 

#define scopes
print(spotifyr::scopes)
scopes = c(
  "user-library-read",
  "user-read-recently-played",
  "playlist-read-private",
  "playlist-read-collaborative",
  "user-read-private"
)
#build my authorization
# auth = get_spotify_authorization_code(Sys.getenv("SPOTIFY_CLIENT_ID"),
#                                                 Sys.getenv("SPOTIFY_CLIENT_SECRET"),
#                                                 scopes)

library(dplyr)
library(purrr)
library(knitr)

#get Beatle's favorite keys
# beatles %>% 
#   count(key_mode, sort = TRUE) %>% 
#   head(5) %>% 
#   kable()

#get_user_playlists("05cf1d02089f45408fd07c698398e044", limit = 5, offset, authorization = auth)
