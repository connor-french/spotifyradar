# script to pull data from google sheets, get metadata from spotify API, and wrangle it for use in the shiny app

## allow the script to use my authorization after running it once interactively
options(gargle_oauth_email = TRUE)

library(dplyr)
library(purrr)
library(googledrive)
library(googlesheets4)
library(spotifyr)
library(DBI)
library(here)

source("R/fct_helpers.R")

# track and artist info DB
sp_con <- DBI::dbConnect(RSQLite::SQLite(), here("data", "spotify.db"))

# IFTTT writes a new google sheet every 2000 rows, so I need to detect all sheets related to the saved tracks
file_ids <- drive_ls(path = "IFTTT/Spotify", pattern = "played_tracks") |>
  dplyr::pull(id)

# get the most recent played_time that I have full spotify data for
last_played <- DBI::dbGetQuery(sp_con,
                "
                SELECT MAX(played_at) AS played_at
                  FROM track_info
                "
                ) |>
  mutate(played_at = lubridate::as_datetime(played_at, tz = "EST"))

# read in google sheets and bind rows
## the spreadsheet that IFTTT writes doesn't have column names, so I have to specify them here
cn <- c("played_at", "track_name", "artist_name", "track_id", "track_url")
ct <- c("ccccc")

## read the sheets and bind into a single data frame
s <- purrr::map2_dfr(file_ids, file_ids, function(x, y) {
  f <- read_sheet(x, col_names = cn, col_types = ct) |>
    dplyr::mutate(sheet_id = y)
}) |>
  dplyr::mutate(played_at = lubridate::mdy_hm(played_at, tz = "EST")) |>
  filter(played_at > last_played$played_at) # filter for tracks that haven't been added to the full data set yet

# only want to run this if there are new tracks
if (nrow(s) > 0){
  # get track information from spotifyr

  ## Need a Dev account with Spotify, then set the client id and client secret in your R environment
  ## Since it's just me using this on my personal computer, I'm just writing these to a .Renviron file for use across sessions
  ###Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxx')
  ###Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxx')

  ## get access token
  access_token <- get_spotify_access_token()

  ## get track audio features

  tid_unique <- unique(s$track_id) # only want unique ids


  ### the spotify api only handles requests in batches of up to 100. Although I'm having this script run every day it's running locally, so if I leave my laptop off for a few days it could accumulate more than 100 new listens. Or if I just listen to grindcore all day

  tid_split <- split(tid_unique, ceiling(seq_along(tid_unique)/100))

  ### I'm having the function wait for a couple seconds before querying the each chunk to be nice and so I don't get booted. I don't need it to run quickly

  rate <- rate_delay(pause = 2)
  slow_get_track <- slowly(get_track_audio_features, rate = rate)
  t <- map_dfr(tid_split, slow_get_track) |>
    na.omit()


  ### rename the id variable to be consistent with my spreadsheet
  t <- t |> rename(track_id = id)

  ### bind to the original data frame to get additional data for artist and genre information
  st <- t |>
    left_join(s, by = "track_id")


  # get artist and genre information

  ## filter for unique artists, so I'm not over-querying
  ## I'm hoping the duplicated() function behaves the same each time, so I'm not re-querying the same artists the next time this happens
  su <- st |>
    filter(!duplicated(artist_name)) |>
    select(artist_name, track_id)

  ## get artist ID info for tracks
  ### the limit for this one is 50
  utid_split <- split(su$track_id, ceiling(seq_along(su$track_id)/50))

  rate <- rate_delay(pause = 2)
  slow_get_artist <- slowly(get_artist_from_track, rate = rate)
  a <- map_dfr(utid_split, ~slow_get_artist(.x, su$artist_name)) |>
    # some artists get duplicated because their names get listed twice for a track, or some tracks have multiple artists
    filter(!duplicated(name))


  ## the limit for this one is 50
  aid_split <- split(a$id, ceiling(seq_along(a$id) / 50))

  slow_get_artist_info <- slowly(get_artists, rate = rate)
  ai <- map_dfr(aid_split, ~slow_get_artist_info(.x)) |>
    # remove followers.href which is all NA
    select(-followers.href) |>
    # change "id" to "artist_id" and "name" to "artist_name" for consistency and clarity
    rename(artist_id = id,
           artist_name = name)

  # add artist ID to the track data set as a key between the track and artist data
  si <- st |>
    left_join(ai |> select(artist_name, artist_id), by = "artist_name") |>
    filter(!duplicated(track_id))

  # get artist IDs from the DB so I don't duplicate artist info
  artist_ids <- DBI::dbGetQuery(sp_con,
                                "
                              SELECT artist_id FROM artist_info
                              ") |>
    filter(!duplicated(artist_id))

  # unnest by genre
  gi <- ai |>
    select(-images) |> # putting images in a different table
    filter(artist_id %notin% artist_ids$artist_id) |> # remove artists who are already in the database
    tidyr::unnest(genres, keep_empty = TRUE)

  # get image links for each artist
  # even though image urls are duplicated, I want to keep the size info in case I decide to do some sort of image classification machine learning thing and I need the info

  im <- ai |>
    select(artist_name, artist_id, images) |>
    filter(artist_id %notin% artist_ids$artist_id) |>
    tidyr::unnest(images)


  # write track and artist info to db table
  DBI::dbAppendTable(sp_con, "track_info", si)
  DBI::dbAppendTable(sp_con, "artist_info", gi)
  DBI::dbAppendTable(sp_con, "artist_images", im)

} else print("No new tracks listened to")



