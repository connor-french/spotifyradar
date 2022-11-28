#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# just a lil helpful function
`%notin%` <- Negate(`%in%`)

# function to get artist IDs from tracks for obtaining artist info (convoluted, I know)
# since tracks can have multiple artists, I'm only retaining the main artist in the original track list
get_artist_from_track <- function(id, artist_name) {
  get_tracks(id) |>
    pull(artists) |>
    bind_rows() |>
    filter(name %in% artist_name)
}


