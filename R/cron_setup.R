# cron setup script
## you need to get a Spotify dev account and add your client ID and secret to your .Renviron before running this. See README for more details.

library(here)

sp_id <- Sys.getenv("SPOTIFY_CLIENT_ID")
sp_secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET")

wd <- here()

r_script <- cron_rscript(
  rscript = here("R", "get_data.R"),
  rscript_args = c(sp_id, sp_secret, wd),
  workdir = wd
)

# use dry_run = TRUE first to double check that the arguments are correct before adding it
cron_add(
  r_script,
  frequency = "daily",
  at = "10:05",
  id = "get_spotify_data"
)
