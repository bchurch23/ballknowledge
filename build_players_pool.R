## build_players_pool.R
##
## Loops the MLB Stats API season-by-season and merges every player it finds
## into ONE deduplicated pool, keyed by player id, with their full team/season
## history attached. Run this ONCE (or occasionally to refresh) -- it's slow
## because it's ~150 separate API calls. generate_daily_data.R (run daily)
## just reads the players_full.json this produces; it does not re-loop seasons.
##
## Output: players_full.json
##   [{ id, fullName, teams: "Team A, Team B", firstSeason, lastSeason,
##      seasonsPlayed: <n>, position }, ...]
##
## Requires: httr, jsonlite

if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")

library(httr)
library(jsonlite)

# ---- config -----------------------------------------------------------

START_SEASON <- 1990                 # MLB "modern era" start; set to 1876 if you want the very earliest
END_SEASON   <- as.integer(format(Sys.Date(), "%Y"))
CACHE_DIR    <- "season_cache"       # raw per-season pulls, so a crash/rerun doesn't refetch everything
OUT_FILE     <- "players_full.json"

dir.create(CACHE_DIR, showWarnings = FALSE)

# ---- pull each season, with a local cache so reruns are cheap ---------

all_rows <- list()

for (season in START_SEASON:END_SEASON) {
  cache_file <- file.path(CACHE_DIR, paste0(season, ".json"))

  if (file.exists(cache_file)) {
    raw <- tryCatch(fromJSON(cache_file, flatten = TRUE), error = function(e) NULL)
  } else {
    url <- paste0("https://statsapi.mlb.com/api/v1/sports/1/players?season=", season)
    resp <- tryCatch(GET(url, timeout(15)), error = function(e) NULL)

    if (is.null(resp) || status_code(resp) != 200) {
      cat("season", season, "-- request failed, skipping\n")
      next
    }
    txt <- content(resp, "text", encoding = "UTF-8")
    writeLines(txt, cache_file)
    raw <- tryCatch(fromJSON(txt, flatten = TRUE), error = function(e) NULL)
    Sys.sleep(0.2)  # be polite to the API
  }

  people <- raw$people
  if (is.null(people) || nrow(people) == 0) {
    cat("season", season, "-- no players returned, skipping\n")
    next
  }

  team_col <- if ("currentTeam.name" %in% names(people)) people$"currentTeam.name" else NA
  pos_col  <- if ("primaryPosition.name" %in% names(people)) people$"primaryPosition.name" else NA

  all_rows[[length(all_rows) + 1]] <- data.frame(
    id       = people$id,
    fullName = people$fullName,
    season   = season,
    team     = team_col,
    position = pos_col,
    stringsAsFactors = FALSE
  )

  cat("season", season, "-- ", nrow(people), "players\n")
}

if (length(all_rows) == 0) stop("No seasons returned any data -- check network/API access.")

combined <- do.call(rbind, all_rows)
cat("\nTotal season-player rows before merging:", nrow(combined), "\n")

# ---- merge by id: one row per player, teams/seasons combined ----------

ids <- unique(combined$id)
cat("Unique players:", length(ids), "\n")

merged <- vector("list", length(ids))

for (i in seq_along(ids)) {
  pid <- ids[i]
  rows <- combined[combined$id == pid, ]

  teams <- unique(rows$team[!is.na(rows$team) & nzchar(rows$team)])
  seasons <- sort(unique(rows$season))
  # most frequently listed position for this player, ignoring NA
  pos_table <- table(rows$position[!is.na(rows$position)])
  position <- if (length(pos_table) > 0) names(pos_table)[which.max(pos_table)] else "Unknown"

  merged[[i]] <- list(
    id            = pid,
    fullName      = rows$fullName[1],
    teams         = paste(teams, collapse = ", "),
    firstSeason   = min(seasons),
    lastSeason    = max(seasons),
    seasonsPlayed = length(seasons),
    position      = position
  )

  if (i %% 1000 == 0) cat("merged", i, "/", length(ids), "\n")
}

write_json(merged, OUT_FILE, auto_unbox = TRUE, pretty = TRUE)
cat("\nWrote", OUT_FILE, "with", length(merged), "players\n")
cat("Per-season raw responses cached in ./", CACHE_DIR, "/ (delete that folder to force a full refetch)\n", sep = "")
