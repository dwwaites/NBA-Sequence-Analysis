# NBA SSS/STS sequence analysis: 2023-24 regular season
#
# Research question:
#   How often does each team create Score-Stop-Score (SSS) and
#   Stop-Score-Stop (STS) sequences, how are those sequences related to game
#   results, and how much does each defense suppress opponents relative to the
#   opponents' usual production?
#
# This script is designed to run from top to bottom in a fresh R session from
# the repository root. Raw play-by-play is cached in data/ after the first
# successful download, so later runs do not download it again. Because the RDS
# is large, data/ should normally be listed in .gitignore; the script recreates
# the directory and downloads the file automatically when it is absent.

library(tidyverse)


# -----------------------------------------------------------------------------
# 1. Helper functions
# -----------------------------------------------------------------------------

# Confirm that the cached/downloaded object contains data and every field used
# by the analysis. Failing here produces a useful message instead of a later,
# harder-to-diagnose error inside filter(), mutate(), or summarise().
validate_nba_pbp <- function(pbp, source_label) {
  required_columns <- c(
    "season_type",
    "game_id",
    "game_date",
    "game_play_number",
    "period_number",
    "clock_display_value",
    "type_text",
    "shooting_play",
    "scoring_play",
    "score_value",
    "team_id",
    "home_team_id",
    "away_team_id",
    "home_team_abbrev",
    "away_team_abbrev",
    "home_score",
    "away_score"
  )

  if (!is.data.frame(pbp) || nrow(pbp) == 0L) {
    stop(
      source_label,
      " did not contain a non-empty play-by-play data frame.",
      call. = FALSE
    )
  }

  missing_columns <- setdiff(required_columns, names(pbp))

  if (length(missing_columns) > 0L) {
    stop(
      source_label,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(pbp)
}

# Read the local cache when it exists. Otherwise, download the season RDS to a
# temporary file, validate it, and move it into the cache. The temporary file
# prevents an interrupted download from masquerading as a valid cache later.
load_cached_nba_pbp <- function(
    cache_path = file.path("data", "nba_pbp_2024.rds"),
    download_timeout = 300) {
  data_dir <- dirname(cache_path)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  if (file.exists(cache_path)) {
    cached_pbp <- tryCatch(
      readRDS(cache_path),
      error = function(error) {
        stop(
          "Could not read the cached play-by-play file at ", cache_path,
          ". Delete the invalid file and rerun the script. Original error: ",
          conditionMessage(error),
          call. = FALSE
        )
      }
    )

    validate_nba_pbp(cached_pbp, paste0("Cached file ", cache_path))
    return(cached_pbp)
  }

  pbp_url <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/",
    "releases/download/espn_nba_pbp/play_by_play_2024.rds"
  )
  download_path <- tempfile(
    pattern = "nba_pbp_2024_",
    tmpdir = data_dir,
    fileext = ".rds"
  )
  on.exit(unlink(download_path), add = TRUE)

  old_timeout <- getOption("timeout")
  if (is.null(old_timeout) || !is.finite(old_timeout)) {
    old_timeout <- 60
  }
  options(timeout = max(download_timeout, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  message("Play-by-play cache not found; downloading 2023-24 NBA data once.")
  download_status <- tryCatch(
    download.file(
      url = pbp_url,
      destfile = download_path,
      mode = "wb",
      method = "libcurl"
    ),
    error = function(error) {
      stop(
        "NBA play-by-play download failed. Original error: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )

  if (download_status != 0L || !file.exists(download_path)) {
    stop(
      "NBA play-by-play download failed (status ", download_status, ").",
      call. = FALSE
    )
  }

  downloaded_pbp <- tryCatch(
    readRDS(download_path),
    error = function(error) {
      stop(
        "The downloaded play-by-play file could not be read. Original error: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
  validate_nba_pbp(downloaded_pbp, "Downloaded play-by-play file")

  if (!file.rename(download_path, cache_path)) {
    stop(
      "The download was valid, but it could not be saved to ", cache_path, ".",
      call. = FALSE
    )
  }

  # Read the finalized cache so first and later runs use the same code path.
  cached_pbp <- readRDS(cache_path)
  validate_nba_pbp(cached_pbp, paste0("Cached file ", cache_path))
  cached_pbp
}

# max(..., na.rm = TRUE) returns -Inf when every value is missing. This helper
# returns NA instead, which is safer when extracting final scores.
safe_max <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    max(x, na.rm = TRUE)
  }
}

# Mark events that close a possession.
#
# Notes on the rule:
# - A made non-free-throw shot ends a possession.
# - A defensive rebound ends the shooter's possession.
# - A turnover or offensive foul ends the committing team's possession.
# - A made final free throw ends a possession. A missed final free throw is
#   closed by the later defensive rebound (or by the end of the period).
# - End Period closes a live possession but is later prevented from creating a
#   sequence across quarters.
mark_possession_ends <- function(pbp) {
  pbp %>%
    mutate(
      type_text_clean = replace_na(type_text, ""),
      is_free_throw = str_detect(
        type_text_clean,
        regex("Free Throw", ignore_case = TRUE)
      ),
      is_final_free_throw = str_detect(
        type_text_clean,
        regex(
          "Free Throw - (1 of 1|2 of 2|3 of 3)",
          ignore_case = TRUE
        )
      ),
      possession_end =
        (
          coalesce(shooting_play, FALSE) &
            coalesce(scoring_play, FALSE) &
            !is_free_throw
        ) |
          type_text_clean == "Defensive Rebound" |
          str_detect(type_text_clean, regex("Turnover", ignore_case = TRUE)) |
          type_text_clean == "Offensive Foul" |
          type_text_clean == "End Period" |
          (is_final_free_throw & coalesce(scoring_play, FALSE))
    )
}

# Convert event-level play-by-play into one row per possession.
build_possessions <- function(pbp) {
  marked_pbp <- pbp %>%
    arrange(game_id, period_number, game_play_number) %>%
    mark_possession_ends() %>%
    group_by(game_id) %>%
    mutate(
      # The event that ends a possession stays with that possession. The next
      # event begins the next possession.
      possession = lag(cumsum(possession_end), default = 0L) + 1L
    ) %>%
    ungroup()

  possessions <- marked_pbp %>%
    group_by(game_id, possession) %>%
    summarise(
      game_date = first(game_date),
      home_team_id = first(home_team_id),
      away_team_id = first(away_team_id),
      home_team_abbrev = first(home_team_abbrev),
      away_team_abbrev = first(away_team_abbrev),

      end_type_text = last(type_text_clean),
      end_team_id = last(team_id),

      # End Period events usually have no team_id. In that case, use the most
      # recent team-attributed event in the possession as a fallback.
      last_event_team_id = {
        candidate_ids <- team_id[
          !is.na(team_id) & type_text_clean != "End Period"
        ]
        if (length(candidate_ids) == 0) {
          NA
        } else {
          last(candidate_ids)
        }
      },

      # Track points by the team that actually scored them. This prevents a
      # technical free throw by the other team from being credited to the
      # offense whose live-ball possession surrounds the technical foul.
      home_points = sum(
        if_else(team_id == home_team_id, score_value, 0L),
        na.rm = TRUE
      ),
      away_points = sum(
        if_else(team_id == away_team_id, score_value, 0L),
        na.rm = TRUE
      ),
      start_period = first(period_number),
      start_clock = first(clock_display_value),
      end_period = last(period_number),
      end_clock = last(clock_display_value),
      event_count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      # A defensive rebound belongs to the defense, so the possession that just
      # ended belongs to the other team. Most other terminal events are tagged
      # to the offense. Period-end possessions use the fallback described above.
      offense_team_id = case_when(
        end_type_text == "Defensive Rebound" &
          end_team_id == home_team_id ~ away_team_id,
        end_type_text == "Defensive Rebound" &
          end_team_id == away_team_id ~ home_team_id,
        end_type_text == "End Period" ~ last_event_team_id,
        TRUE ~ end_team_id
      ),
      points_scored = case_when(
        offense_team_id == home_team_id ~ home_points,
        offense_team_id == away_team_id ~ away_points,
        TRUE ~ NA_integer_
      ),
      offense = case_when(
        offense_team_id == home_team_id ~ home_team_abbrev,
        offense_team_id == away_team_id ~ away_team_abbrev,
        TRUE ~ NA_character_
      ),
      scored = points_scored > 0
    ) %>%
    # This removes administrative-only rows such as an End Period event that
    # immediately follows an already completed possession.
    filter(!is.na(offense)) %>%
    select(
      -end_type_text,
      -end_team_id,
      -last_event_team_id,
      -home_points,
      -away_points
    )

  list(
    events = marked_pbp,
    possessions = possessions
  )
}

# Add rolling three-possession SSS and STS indicators.
add_sequence_flags <- function(possessions) {
  possessions %>%
    arrange(game_id, start_period, possession) %>%
    # Grouping by period prevents a sequence from spanning a quarter boundary.
    group_by(game_id, start_period) %>%
    mutate(
      next_offense = lead(offense),
      next_scored = lead(scored),
      third_offense = lead(offense, 2),
      third_scored = lead(scored, 2),

      score_stop_score = coalesce(
        scored &
          !next_scored &
          third_scored &
          offense == third_offense &
          offense != next_offense,
        FALSE
      ),

      stop_score_stop = coalesce(
        !scored &
          next_scored &
          !third_scored &
          offense == third_offense &
          offense != next_offense,
        FALSE
      ),

      sequence_team = case_when(
        score_stop_score ~ offense,
        stop_score_stop ~ next_offense,
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()
}


# -----------------------------------------------------------------------------
# 2. Load and define the analysis sample
# -----------------------------------------------------------------------------

# The sportsdataverse repository labels the 2023-24 NBA season as 2024. The
# first run downloads its RDS directly; subsequent runs use data/ without
# contacting GitHub.
nba_pbp <- load_cached_nba_pbp()

# Optional data inspection when the script is run interactively.
if (interactive()) {
  glimpse(nba_pbp)
  print(dim(nba_pbp))
}

league_pbp <- nba_pbp %>%
  filter(
    season_type == 2,
    !home_team_abbrev %in% c("EAST", "WEST"),
    !away_team_abbrev %in% c("EAST", "WEST")
  ) %>%
  mutate(
    game_date = as.Date(game_date),

    # The 2023 In-Season Tournament championship (LAL-IND on 2023-12-09)
    # appears in the regular-season feed but did not count in league standings.
    # Excluding it restores the official 1,230-game, 82-games-per-team sample.
    is_non_standings_cup_final =
      game_date == as.Date("2023-12-09") &
      (
        (home_team_abbrev == "LAL" & away_team_abbrev == "IND") |
          (home_team_abbrev == "IND" & away_team_abbrev == "LAL")
      )
  ) %>%
  filter(!is_non_standings_cup_final) %>%
  select(-is_non_standings_cup_final) %>%
  arrange(game_id, period_number, game_play_number)

game_index <- league_pbp %>%
  distinct(
    game_id,
    game_date,
    home_team_abbrev,
    away_team_abbrev
  ) %>%
  arrange(game_date, game_id)

team_game_counts <- bind_rows(
  game_index %>% transmute(game_id, team = home_team_abbrev),
  game_index %>% transmute(game_id, team = away_team_abbrev)
) %>%
  count(team, name = "games") %>%
  arrange(team)

# Stop early if the sample is not the expected official regular season.
if (
  nrow(game_index) != 1230L ||
    nrow(team_game_counts) != 30L ||
    any(team_game_counts$games != 82L)
) {
  stop(
    "The sample is not 1,230 games with 82 games per team. ",
    "Inspect team_game_counts before interpreting results.",
    call. = FALSE
  )
}

thunder_games <- game_index %>%
  filter(home_team_abbrev == "OKC" | away_team_abbrev == "OKC")

# -----------------------------------------------------------------------------
# 3. Inspect one game before running the league-wide pipeline
# -----------------------------------------------------------------------------

# Use the original example when it exists; otherwise use OKC's first game. The
# as.character() comparison works whether hoopR stores game_id as text or number.
requested_example_game_id <- "401584709"

example_game_id <- if (
  requested_example_game_id %in% as.character(thunder_games$game_id)
) {
  requested_example_game_id
} else {
  as.character(first(thunder_games$game_id))
}

example_game_raw <- league_pbp %>%
  filter(as.character(game_id) == example_game_id)

example_build <- build_possessions(example_game_raw)
example_game_events <- example_build$events
example_possessions <- example_build$possessions %>%
  add_sequence_flags()

# These views are useful in RStudio but are skipped in non-interactive runs.
if (interactive()) {
  example_game_events %>%
    filter(possession_end) %>%
    select(
      game_play_number,
      period_number,
      clock_display_value,
      team_id,
      type_text,
      text,
      scoring_play,
      score_value,
      home_score,
      away_score,
      possession,
      possession_end
    ) %>%
    View()

  example_possessions %>%
    select(
      game_id,
      possession,
      offense,
      points_scored,
      scored,
      start_period,
      start_clock,
      end_clock,
      score_stop_score,
      stop_score_stop,
      sequence_team
    ) %>%
    View()
}

example_sequence_summary <- example_possessions %>%
  filter(score_stop_score | stop_score_stop) %>%
  group_by(sequence_team) %>%
  summarise(
    score_stop_score = sum(score_stop_score),
    stop_score_stop = sum(stop_score_stop),
    total_sequences = score_stop_score + stop_score_stop,
    .groups = "drop"
  )

example_sequence_summary


# -----------------------------------------------------------------------------
# 4. Build possessions and sequence indicators for the whole league
# -----------------------------------------------------------------------------

league_build <- build_possessions(league_pbp)
league_pbp_marked <- league_build$events
league_possessions <- league_build$possessions %>%
  add_sequence_flags()

# Count SSS and STS sequences by team in every game.
team_sequences <- league_possessions %>%
  filter(score_stop_score | stop_score_stop) %>%
  group_by(game_id, sequence_team) %>%
  summarise(
    sss = sum(score_stop_score),
    sts = sum(stop_score_stop),
    total_sequences = sss + sts,
    .groups = "drop"
  ) %>%
  rename(team = sequence_team)

# Create exactly two perspectives per game: one home-team row and one away-team
# row. Keeping games with zero sequences here prevents selection bias.
team_games <- bind_rows(
  game_index %>%
    transmute(
      game_id,
      game_date,
      team = home_team_abbrev,
      opponent = away_team_abbrev,
      home_away = "Home"
    ),
  game_index %>%
    transmute(
      game_id,
      game_date,
      team = away_team_abbrev,
      opponent = home_team_abbrev,
      home_away = "Away"
    )
)

team_game_summary <- team_games %>%
  left_join(team_sequences, by = c("game_id", "team")) %>%
  mutate(
    sss = replace_na(sss, 0L),
    sts = replace_na(sts, 0L),
    total_sequences = replace_na(total_sequences, 0L)
  )

# Each game-team key must be unique before using a self-join to find the
# opponent's totals. This check protects against accidental many-to-many joins.
duplicate_game_team_keys <- team_game_summary %>%
  count(game_id, team) %>%
  filter(n != 1L)

stopifnot(nrow(duplicate_game_team_keys) == 0L)

opponent_sequence_lookup <- team_game_summary %>%
  select(game_id, team, sss, sts, total_sequences) %>%
  rename(
    opponent = team,
    opponent_sss = sss,
    opponent_sts = sts,
    opponent_sequences = total_sequences
  )

rows_before_opponent_join <- nrow(team_game_summary)

team_game_summary <- team_game_summary %>%
  left_join(
    opponent_sequence_lookup,
    by = c("game_id", "opponent")
  ) %>%
  mutate(
    sss_differential = sss - opponent_sss,
    sts_differential = sts - opponent_sts,
    sequence_differential = total_sequences - opponent_sequences
  )

stopifnot(
  nrow(team_game_summary) == rows_before_opponent_join,
  !anyNA(team_game_summary$opponent_sss),
  !anyNA(team_game_summary$opponent_sts),
  !anyNA(team_game_summary$opponent_sequences),
  all(
    team_game_summary$sequence_differential ==
      team_game_summary$sss_differential +
      team_game_summary$sts_differential
  )
)


# -----------------------------------------------------------------------------
# 5. Add final scores and game outcomes
# -----------------------------------------------------------------------------

league_game_results <- league_pbp %>%
  group_by(game_id) %>%
  summarise(
    final_home_score = safe_max(home_score),
    final_away_score = safe_max(away_score),
    .groups = "drop"
  )

rows_before_score_join <- nrow(team_game_summary)

team_game_summary <- team_game_summary %>%
  left_join(league_game_results, by = "game_id") %>%
  mutate(
    team_score = if_else(
      home_away == "Home",
      final_home_score,
      final_away_score
    ),
    opponent_score = if_else(
      home_away == "Home",
      final_away_score,
      final_home_score
    ),
    point_differential = team_score - opponent_score,
    result = case_when(
      point_differential > 0 ~ "Win",
      point_differential < 0 ~ "Loss",
      TRUE ~ "Tie"
    )
  )

stopifnot(
  nrow(team_game_summary) == rows_before_score_join,
  !anyNA(team_game_summary$team_score),
  !anyNA(team_game_summary$opponent_score)
)

analysis_dimensions <- team_game_summary %>%
  summarise(
    rows = n(),
    teams = n_distinct(team),
    games = n_distinct(game_id),
    average_sequences = mean(total_sequences)
  )

analysis_dimensions


# -----------------------------------------------------------------------------
# 6. Team rankings and OKC game-level analysis
# -----------------------------------------------------------------------------

team_season_summary <- team_game_summary %>%
  group_by(team) %>%
  summarise(
    games = n(),
    avg_sss = mean(sss),
    avg_sts = mean(sts),
    avg_total_sequences = mean(total_sequences),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_total_sequences)) %>%
  mutate(sequence_rank = row_number()) %>%
  select(
    sequence_rank,
    team,
    games,
    avg_sss,
    avg_sts,
    avg_total_sequences
  )

team_season_summary

team_sequence_rankings <- team_game_summary %>%
  group_by(team) %>%
  summarise(
    games = n(),
    sequences_for = mean(total_sequences),
    sequences_against = mean(opponent_sequences),
    sequence_differential = mean(sequence_differential),
    .groups = "drop"
  ) %>%
  arrange(desc(sequence_differential)) %>%
  mutate(rank = row_number())

team_sequence_rankings

# The league-wide table already contains the OKC perspective, so a second OKC
# possession pipeline is unnecessary.
okc_game_summary <- team_game_summary %>%
  filter(team == "OKC") %>%
  transmute(
    game_id,
    game_date,
    home_away,
    opponent,
    okc_sss = sss,
    okc_sts = sts,
    opp_sss = opponent_sss,
    opp_sts = opponent_sts,
    okc_total = total_sequences,
    opp_total = opponent_sequences,
    sequence_differential,
    okc_score = team_score,
    opp_score = opponent_score,
    point_differential,
    result
  )

okc_result_summary <- okc_game_summary %>%
  group_by(result) %>%
  summarise(
    avg_okc_sequences = mean(okc_total),
    avg_opp_sequences = mean(opp_total),
    avg_sequence_diff = mean(sequence_differential),
    games = n(),
    .groups = "drop"
  )

okc_result_summary

okc_sequence_correlation <- cor(
  okc_game_summary$sequence_differential,
  okc_game_summary$point_differential,
  use = "complete.obs"
)

okc_sequence_model <- lm(
  point_differential ~ sequence_differential,
  data = okc_game_summary
)

okc_sequence_correlation
summary(okc_sequence_model)


# -----------------------------------------------------------------------------
# 7. League-wide relationship between sequences and point differential
# -----------------------------------------------------------------------------

# team_game_summary has two mirrored rows per game. Using both rows would leave
# the slope and R-squared unchanged but would incorrectly double the apparent
# sample size and make standard errors and p-values too optimistic. Use one
# independent home-team perspective per game instead.
league_game_model_data <- team_game_summary %>%
  filter(home_away == "Home")

stopifnot(nrow(league_game_model_data) == nrow(game_index))

league_sequence_correlation <- cor(
  league_game_model_data$sequence_differential,
  league_game_model_data$point_differential,
  use = "complete.obs"
)

league_sequence_model <- lm(
  point_differential ~ sequence_differential,
  data = league_game_model_data
)

league_sequence_correlation
summary(league_sequence_model)

# Interpretation caution: SSS/STS sequences are defined partly by scoring and
# stops, so this regression is descriptive. It does not establish that creating
# an additional sequence causes a particular change in final margin.


# -----------------------------------------------------------------------------
# 8. Defensive suppression effects for every team
# -----------------------------------------------------------------------------

# For each defensive team, compare every opponent-game against that defense with
# the same offensive team's average against all other defenses. The baseline
# deliberately excludes games against the defense being evaluated.
all_team_effects <- map_dfr(
  sort(unique(team_game_summary$team)),
  function(defensive_team_value) {
    opponent_baseline <- team_game_summary %>%
      filter(
        team != defensive_team_value,
        opponent != defensive_team_value
      ) %>%
      group_by(team) %>%
      summarise(
        normal_avg_sequences = mean(total_sequences),
        .groups = "drop"
      )

    team_game_summary %>%
      filter(opponent == defensive_team_value) %>%
      select(
        game_id,
        game_date,
        team,
        total_sequences
      ) %>%
      left_join(opponent_baseline, by = "team") %>%
      mutate(
        defensive_team = defensive_team_value,
        suppression_effect = total_sequences - normal_avg_sequences
      ) %>%
      select(
        game_id,
        game_date,
        defensive_team,
        opponent_team = team,
        opponent_sequences = total_sequences,
        normal_avg_sequences,
        suppression_effect
      )
  }
)

stopifnot(
  nrow(all_team_effects) == nrow(team_game_summary),
  !anyNA(all_team_effects$normal_avg_sequences)
)

team_suppression_rankings <- all_team_effects %>%
  group_by(defensive_team) %>%
  summarise(
    games = n(),
    avg_opponent_sequences = mean(opponent_sequences),
    avg_expected_sequences = mean(normal_avg_sequences),
    avg_suppression_effect = mean(suppression_effect),
    median_suppression_effect = median(suppression_effect),
    .groups = "drop"
  ) %>%
  arrange(avg_suppression_effect) %>%
  mutate(rank = row_number())

# More negative suppression effects mean opponents created fewer sequences than
# expected; positive values mean opponents created more than expected.
team_suppression_rankings


# -----------------------------------------------------------------------------
# 9. OKC suppression details and inference
# -----------------------------------------------------------------------------

okc_opponent_games <- all_team_effects %>%
  filter(defensive_team == "OKC") %>%
  rename(okc_effect = suppression_effect)

okc_effect_by_team <- okc_opponent_games %>%
  group_by(opponent_team) %>%
  summarise(
    games_vs_okc = n(),
    avg_sequences_vs_okc = mean(opponent_sequences),
    avg_sequences_vs_others = first(normal_avg_sequences),
    okc_effect = avg_sequences_vs_okc - avg_sequences_vs_others,
    .groups = "drop"
  ) %>%
  arrange(okc_effect)

okc_effect_by_team

# This is the preferred game-weighted summary: all 82 OKC games contribute one
# observation. The by-team table above gives each of the 29 opponents one row.
okc_suppression_summary <- okc_opponent_games %>%
  summarise(
    games = n(),
    avg_opponent_sequences_vs_okc = mean(opponent_sequences),
    avg_expected_sequences = mean(normal_avg_sequences),
    average_okc_effect = mean(okc_effect),
    median_okc_effect = median(okc_effect),
    sd_okc_effect = sd(okc_effect)
  )

okc_suppression_summary

# The one-sided alternative matches the pre-specified question of whether OKC
# suppresses opponents (mean effect < 0). For a conventional two-sided interval,
# run t.test(okc_opponent_games$okc_effect, mu = 0) as well.
okc_suppression_test <- t.test(
  okc_opponent_games$okc_effect,
  mu = 0,
  alternative = "less"
)

okc_suppression_test


# -----------------------------------------------------------------------------
# 10. Separate SSS and STS relationships
# -----------------------------------------------------------------------------

sss_correlation <- cor(
  league_game_model_data$sss_differential,
  league_game_model_data$point_differential,
  use = "complete.obs"
)

sts_correlation <- cor(
  league_game_model_data$sts_differential,
  league_game_model_data$point_differential,
  use = "complete.obs"
)

league_component_model <- lm(
  point_differential ~ sss_differential + sts_differential,
  data = league_game_model_data
)

league_component_confidence_intervals <- confint(league_component_model)

sss_correlation
sts_correlation
summary(league_component_model)
league_component_confidence_intervals


# -----------------------------------------------------------------------------
# 11. Win-probability model
# -----------------------------------------------------------------------------

# NBA games cannot end in a tie. The check ensures the binary outcome is valid.
stopifnot(!any(league_game_model_data$point_differential == 0))

league_game_model_data <- league_game_model_data %>%
  mutate(win = as.integer(point_differential > 0))

league_win_model <- glm(
  win ~ sequence_differential,
  data = league_game_model_data,
  family = binomial
)

sequence_odds_ratio <- exp(
  coef(league_win_model)["sequence_differential"]
)

win_probability_table <- tibble(
  sequence_differential = c(-10, -5, 0, 5, 10)
) %>%
  mutate(
    predicted_win_probability = predict(
      league_win_model,
      newdata = tibble(
        sequence_differential = sequence_differential
      ),
      type = "response"
    )
  )

summary(league_win_model)
sequence_odds_ratio
win_probability_table

# Interpretation caution: these probabilities describe the relationship between
# full-game sequence differential and the completed-game result. They are not
# pregame forecasts because the predictor is observed during the same game.


# -----------------------------------------------------------------------------
# 12. Robustness checks
# -----------------------------------------------------------------------------

# Spearman correlation is less sensitive to extreme values than Pearson
# correlation and tests whether the relationship is consistently monotonic.
league_sequence_spearman <- cor(
  league_game_model_data$sequence_differential,
  league_game_model_data$point_differential,
  method = "spearman",
  use = "complete.obs"
)

# Restrict the sensitivity analysis to games decided by 20 points or fewer.
non_blowout_model_data <- league_game_model_data %>%
  filter(abs(point_differential) <= 20)

non_blowout_sequence_correlation <- cor(
  non_blowout_model_data$sequence_differential,
  non_blowout_model_data$point_differential,
  use = "complete.obs"
)

non_blowout_sequence_model <- lm(
  point_differential ~ sequence_differential,
  data = non_blowout_model_data
)

robustness_comparison <- tibble(
  sample = c(
    "All games",
    "Games decided by 20 points or fewer"
  ),
  games = c(
    nrow(league_game_model_data),
    nrow(non_blowout_model_data)
  ),
  correlation = c(
    league_sequence_correlation,
    non_blowout_sequence_correlation
  ),
  slope = c(
    unname(coef(league_sequence_model)["sequence_differential"]),
    unname(coef(non_blowout_sequence_model)["sequence_differential"])
  ),
  r_squared = c(
    summary(league_sequence_model)$r.squared,
    summary(non_blowout_sequence_model)$r.squared
  )
)

league_sequence_spearman
robustness_comparison
summary(non_blowout_sequence_model)


# -----------------------------------------------------------------------------
# 13. Visualizations
# -----------------------------------------------------------------------------

okc_sequence_plot <- ggplot(
  okc_game_summary,
  aes(x = sequence_differential, y = point_differential)
) +
  geom_point(alpha = 0.7, color = "#007AC1") +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00") +
  labs(
    title = "OKC Sequence Differential vs. Point Differential",
    subtitle = "2023-24 NBA regular season",
    x = "SSS + STS Differential",
    y = "Point Differential"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Build win-probability predictions across the observed differential range.
win_probability_curve <- tibble(
  sequence_differential = seq(
    min(league_game_model_data$sequence_differential),
    max(league_game_model_data$sequence_differential),
    length.out = 300
  )
)

win_probability_predictions <- predict(
  league_win_model,
  newdata = win_probability_curve,
  type = "link",
  se.fit = TRUE
)

win_probability_curve <- win_probability_curve %>%
  mutate(
    predicted_probability = plogis(win_probability_predictions$fit),
    lower_confidence = plogis(
      win_probability_predictions$fit -
        1.96 * win_probability_predictions$se.fit
    ),
    upper_confidence = plogis(
      win_probability_predictions$fit +
        1.96 * win_probability_predictions$se.fit
    )
  )

win_probability_plot <- ggplot(
  win_probability_curve,
  aes(x = sequence_differential, y = predicted_probability)
) +
  geom_ribbon(
    aes(ymin = lower_confidence, ymax = upper_confidence),
    fill = "#0072B2",
    alpha = 0.18
  ) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(
    data = win_probability_table,
    aes(
      x = sequence_differential,
      y = predicted_win_probability
    ),
    inherit.aes = FALSE,
    color = "#D55E00",
    size = 2.5
  ) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    color = "grey50"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Sequence Differential and Win Probability",
    subtitle = "2023-24 NBA regular season, home-team perspective",
    x = "SSS + STS Differential",
    y = "Modeled Win Probability",
    caption = paste(
      "Shaded area represents the 95% confidence interval.",
      "Probabilities describe completed-game relationships, not pregame forecasts."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

team_sequence_plot_data <- team_sequence_rankings %>%
  arrange(sequence_differential) %>%
  mutate(
    team = factor(team, levels = team),
    bar_group = case_when(
      team == "OKC" ~ "OKC",
      sequence_differential >= 0 ~ "Positive",
      TRUE ~ "Negative"
    )
  )

team_sequence_differential_plot <- ggplot(
  team_sequence_plot_data,
  aes(
    x = team,
    y = sequence_differential,
    fill = bar_group
  )
) +
  geom_hline(
    yintercept = 0,
    color = "grey40",
    linewidth = 0.5
  ) +
  geom_col(width = 0.75) +
  geom_text(
    aes(
      label = sprintf("%+.2f", sequence_differential),
      hjust = if_else(sequence_differential >= 0, -0.15, 1.15)
    ),
    size = 3
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "OKC" = "#007AC1",
      "Positive" = "#71879B",
      "Negative" = "#C97A6A"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.15, 0.18))
  ) +
  labs(
    title = "NBA Team Sequence Differential",
    subtitle = "Average SSS + STS differential per game, 2023-24 regular season",
    x = NULL,
    y = "Average sequence differential per game",
    caption = "Positive values indicate more sequences created than allowed."
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

team_suppression_plot_data <- team_suppression_rankings %>%
  arrange(desc(avg_suppression_effect)) %>%
  mutate(
    defensive_team = factor(
      defensive_team,
      levels = defensive_team
    ),
    bar_group = case_when(
      defensive_team == "OKC" ~ "OKC",
      avg_suppression_effect < 0 ~ "Suppression",
      TRUE ~ "Above expected"
    )
  )

team_suppression_plot <- ggplot(
  team_suppression_plot_data,
  aes(
    x = defensive_team,
    y = avg_suppression_effect,
    fill = bar_group
  )
) +
  geom_hline(
    yintercept = 0,
    color = "grey40",
    linewidth = 0.5
  ) +
  geom_col(width = 0.75) +
  geom_text(
    aes(
      label = sprintf("%+.2f", avg_suppression_effect),
      hjust = if_else(avg_suppression_effect < 0, 1.15, -0.15)
    ),
    size = 3
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "OKC" = "#007AC1",
      "Suppression" = "#5B7F73",
      "Above expected" = "#C97A6A"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.18, 0.18))
  ) +
  labs(
    title = "NBA Defensive Sequence Suppression",
    subtitle = paste(
      "Opponent SSS + STS production relative to each opponent's",
      "average against other defenses"
    ),
    x = NULL,
    y = "Average opponent sequences relative to expected",
    caption = paste(
      "More negative values indicate stronger suppression.",
      "Each opponent baseline excludes games against the evaluated defense."
    )
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

okc_sequence_plot
win_probability_plot
team_sequence_differential_plot
team_suppression_plot

# Figure export can be added after the repository structure is finalized.


# -----------------------------------------------------------------------------
# 14. Key output objects
# -----------------------------------------------------------------------------

# Core data
# analysis_dimensions                    sample size and average sequence count
# team_game_summary                      one row per team-game
# okc_game_summary                       one row per OKC game
# all_team_effects                       suppression observations by team-game

# Rankings and summaries
# team_season_summary                    raw SSS + STS production rankings
# team_sequence_rankings                 sequence-differential rankings
# team_suppression_rankings              defensive-suppression rankings
# okc_result_summary                     OKC results split by game outcome
# okc_suppression_summary                game-weighted OKC suppression summary
# okc_suppression_test                   one-sided OKC suppression test

# League models and robustness
# league_sequence_model                  total sequence-differential model
# league_component_model                 separate SSS and STS model
# league_component_confidence_intervals  component-model confidence intervals
# league_win_model                       sequence differential and win model
# sequence_odds_ratio                    odds multiplier for a +1 differential
# win_probability_table                  selected modeled win probabilities
# league_sequence_spearman               rank-based sequence correlation
# non_blowout_sequence_model             model for games within 20 points
# robustness_comparison                  full-sample and non-blowout comparison

# Final plots
# okc_sequence_plot                      OKC differential scatterplot
# win_probability_plot                   modeled league win-probability curve
# team_sequence_differential_plot        team sequence-differential rankings
# team_suppression_plot                  defensive-suppression rankings
