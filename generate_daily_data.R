## generate_daily_data.R
##
## Run this DAILY. It does NOT loop MLB seasons itself -- it reads the
## players_full.json produced once by build_players_pool.R, picks today's
## player from it, and pulls that one player's bio details fresh.
##
## Requires: httr, jsonlite
## Requires: players_full.json to already exist (run build_players_pool.R first)

if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")

library(httr)
library(jsonlite)

POOL_FILE <- "players_full.json"

if (!file.exists(POOL_FILE)) {
  stop("players_full.json not found. Run build_players_pool.R first to build the historical pool.")
}

people <- fromJSON(POOL_FILE, flatten = TRUE)
people <- people[order(people$id), ]
row.names(people) <- NULL
cat("Loaded", nrow(people), "players from", POOL_FILE, "\n")

# autocomplete pool for the front end: id + fullName is all it needs
write_json(people[, c("id", "fullName")], "players.json", auto_unbox = TRUE, pretty = TRUE)
cat("Wrote players.json\n")

## ---- deterministically pick today's player --------------------------------

hash_index <- function(str, length) {
  h <- 0
  for (code in utf8ToInt(str)) {
    h <- (h * 31 + code) %% 4294967296
  }
  (h %% length) + 1  # R is 1-indexed
}

date_str <- as.character(Sys.Date())
idx <- hash_index(date_str, nrow(people))
chosen <- people[idx, ]

cat("Today's pick (", date_str, "):", chosen$fullName, "( id", chosen$id, ")\n")

## ---- pull fresh bio details for just that one player -----------------------
## (bio fields like birth country / bats-throws / debut date don't change and
## work even for retired players; we do NOT rely on currentTeam here since
## retired players won't have one -- team history comes from the pool instead)

detail_url <- paste0("https://statsapi.mlb.com/api/v1/people/", chosen$id)
detail_resp <- GET(detail_url, timeout(15))
stop_for_status(detail_resp)

detail_data <- fromJSON(content(detail_resp, "text", encoding = "UTF-8"), flatten = TRUE)
person <- detail_data$people[1, ]

get_field <- function(row, field, default = "Unknown") {
  if (field %in% names(row) && !is.na(row[[field]]) && nzchar(as.character(row[[field]]))) {
    as.character(row[[field]])
  } else {
    default
  }
}

last_name <- get_field(person, "lastName", default = NA)
if (is.na(last_name)) {
  parts <- strsplit(person$fullName, " ")[[1]]
  last_name <- parts[length(parts)]
}

debut_year <- get_field(person, "mlbDebutDate")
if (debut_year != "Unknown") debut_year <- substr(debut_year, 1, 4)

number <- get_field(person, "primaryNumber")
if (number != "Unknown") number <- paste0("#", number)

today_out <- list(
  id           = chosen$id,
  fullName     = chosen$fullName,
  lastName     = last_name,
  teams        = chosen$teams,                          # full career team history from the pool
  seasonsSpan  = paste0(chosen$firstSeason, "-", chosen$lastSeason),
  position     = chosen$position,
  bats         = paste0(get_field(person, "batSide.description", "?"), " / ", get_field(person, "pitchHand.description", "?")),
  number       = number,
  debut        = debut_year,
  birthCountry = get_field(person, "birthCountry"),
  headshotUrl  = paste0("https://img.mlbstatic.com/mlb-photos/image/upload/w_240,q_auto:best/v1/people/", chosen$id, "/headshot/67/current"),
  date         = date_str
)

write_json(today_out, "today.json", auto_unbox = TRUE, pretty = TRUE)
cat("Wrote today.json\n")
cat("Done.\n")
