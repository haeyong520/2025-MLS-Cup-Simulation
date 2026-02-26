################################################################################
## Project: MLS Performance Analytics & Championship Simulation
## Author: Haeyong Chun
## Description:
##    This script analyzes the determinants of team success in Major League Soccer (MLS)
##    using Poisson regression models. It compares the impact of offensive vs. 
##    defensive metrics and performs a Simulation (10,000 runs) to 
##    forecast MLS Cup probabilities based on regular-season team strength.
################################################################################

## 0. Setup: Load Required Libraries -------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, dplyr, tidyr, ggplot2, Hmisc, httr, ggcorrplot)

## 1. Data Loading: Import datasets directly from GitHub -----------------------
# Note: Loading data from raw GitHub URLs ensures reproducibility across different machines.

# [ACTION REQUIRED] Please update the URLs below with your actual GitHub Raw URLs
url_def <- "https://raw.githubusercontent.com/haeyong520/2025-MLS-Cup-Simulation/main/data/STT832_Defensive_Data.xlsx"
url_off <- "https://raw.githubusercontent.com/haeyong520/2025-MLS-Cup-Simulation/main/data/STT832_Offensive_Data.xlsx"
url_pts <- "https://raw.githubusercontent.com/haeyong520/2025-MLS-Cup-Simulation/main/data/STT832_pts.xlsx"
# If you have a separate possession file, add it here similarly.

# Helper function to download and read Excel files from URL
read_excel_from_url <- function(url) {
  temp_file <- tempfile(fileext = ".xlsx")
  GET(url, write_disk(temp_file, overwrite = TRUE))
  data <- read_excel(temp_file)
  unlink(temp_file)
  return(data)
}

# Load datasets
off_raw   <- read_excel_from_url(url_off)
defgk_raw <- read_excel_from_url(url_def)
pts_raw   <- read_excel_from_url(url_pts)

## 2. Data Wrangling & Feature Engineering -------------------------------------

# 2.1. Standardize variable names (Replace special characters like %, -, / with .)
off   <- off_raw   %>% rename_with(make.names)
defgk <- defgk_raw %>% rename_with(make.names)
pts   <- pts_raw   %>% rename_with(make.names)

# 2.2. Add prefixes to distinguish Offensive vs. Defensive metrics
#      (Preserving 'Squad' as the key for merging)
off <- off %>% rename_with(~ paste0("off_", .x), -Squad)
defgk <- defgk %>% rename_with(~ paste0("def_", .x), -Squad)

# 2.3. Derive Performance Metrics: Matches Played (MP), PPG, Win %
pts <- pts %>%
  mutate(
    MP     = W + D + L,      # Total Matches Played
    PPG    = Pts / MP,       # Points Per Game
    WinPct = W / MP          # Winning Percentage
  )

# 2.4. Merge datasets into a single master dataframe
dat <- off %>%
  inner_join(defgk, by = "Squad") %>%
  inner_join(pts,   by = "Squad")

# Define predictor sets for later use
off_vars   <- setdiff(names(off),   "Squad")
defgk_vars <- setdiff(names(defgk), "Squad")

glimpse(dat) # Quick inspection of the merged structure

################################################################################
## 3. Statistical Modeling: Poisson Regression ---------------------------------
################################################################################

# Goal: Model the number of Wins (W) based on offensive metrics.
# We use an offset of log(MP) to account for differences in matches played.

# 3.1. Fit Poisson Model (Wins ~ Offensive Metrics)
# Note: We select key efficiency metrics to avoid multicollinearity issues common in volume stats.
formula_W_off <- as.formula(paste("W ~", paste(off_vars, collapse = " + "), "+ offset(log(MP))"))

# Fit the model (Generalized Linear Model)
fit_W_off <- glm(formula_W_off, data = dat, family = poisson(link = "log"))

# 3.2. Extract Team Strength Parameters
# We define 'Team Strength' as the expected win rate derived from the model.
dat_strength <- dat %>%
  mutate(
    W_hat        = fitted(fit_W_off),   # Predicted Wins
    win_rate_hat = W_hat / MP           # Expected Wins per Match (Strength Proxy)
  ) %>%
  dplyr::select(Squad, win_rate_hat)

# View estimated team strengths
head(dat_strength %>% arrange(desc(win_rate_hat)))

################################################################################
## 4. Monte Carlo Simulation: MLS Cup Playoffs ---------------------------------
################################################################################

# 4.1. Define the Playoff Bracket (Round 1 Matchups)
# Based on actual MLS playoff bracket structure
round1 <- tribble(
  ~match_id, ~teamA,              ~teamB,
  1,         "Philadelphia Union", "Chicago Fire",
  2,         "FC Cincinnati",      "Columbus Crew",
  3,         "Inter Miami",        "Nashville SC",
  4,         "NYCFC",              "Charlotte",
  5,         "Sandiego FC",        "Portland Timbers",
  6,         "Vancouver W'caps",   "FC Dallas",
  7,         "LAFC",               "Austin",
  8,         "Minnesota Utd",      "Seattle Sounders"
)

# 4.2. Helper Functions for Simulation

# Function to retrieve strength for a given team
get_strength <- function(team, strength_df) {
  val <- strength_df$win_rate_hat[match(team, strength_df$Squad)]
  if(is.na(val)) return(0.01) # Fallback for missing teams to avoid errors
  return(val)
}

# Function to simulate a single match outcome
simulate_match <- function(teamA, teamB, strength_df) {
  sA <- get_strength(teamA, strength_df)
  sB <- get_strength(teamB, strength_df)
  
  # Calculate win probability for Team A: Strength A / (Strength A + Strength B)
  # Added a small epsilon (1e-6) to prevent division by zero
  sA <- max(sA, 1e-6)
  sB <- max(sB, 1e-6)
  pA <- sA / (sA + sB)
  
  # Determine winner based on random draw
  winner <- ifelse(runif(1) < pA, teamA, teamB)
  return(winner)
}

# Function to simulate the entire tournament once
simulate_playoff_once <- function(round1, strength_df) {
  
  # Round 1
  winners_R1 <- character(nrow(round1))
  for (i in seq_len(nrow(round1))) {
    winners_R1[i] <- simulate_match(round1$teamA[i], round1$teamB[i], strength_df)
  }
  
  # Split into Conferences (Assuming 1-4 are East, 5-8 are West)
  east_R1_winners <- winners_R1[1:4]
  west_R1_winners <- winners_R1[5:8]
  
  # Conference Semifinals
  east_R2_winners <- c(
    simulate_match(east_R1_winners[1], east_R1_winners[2], strength_df),
    simulate_match(east_R1_winners[3], east_R1_winners[4], strength_df)
  )
  
  west_R2_winners <- c(
    simulate_match(west_R1_winners[1], west_R1_winners[2], strength_df),
    simulate_match(west_R1_winners[3], west_R1_winners[4], strength_df)
  )
  
  # Conference Finals
  east_champ <- simulate_match(east_R2_winners[1], east_R2_winners[2], strength_df)
  west_champ <- simulate_match(west_R2_winners[1], west_R2_winners[2], strength_df)
  
  # MLS Cup Final
  champion <- simulate_match(east_champ, west_champ, strength_df)
  
  return(champion)
}

# 4.3. Run Monte Carlo Simulation
set.seed(832) # Ensure reproducibility
n_sim <- 10000 # Number of iterations

print(paste("Running", n_sim, "simulations..."))

champions <- replicate(n_sim, simulate_playoff_once(round1, dat_strength))

# 4.4. Aggregating Results
champion_table <- table(champions)
champion_df <- as.data.frame(champion_table) %>%
  dplyr::rename(Squad = champions, wins = Freq) %>%
  dplyr::mutate(
    win_prob = wins / n_sim
  ) %>%
  dplyr::arrange(dplyr::desc(win_prob))

# Display top contenders
print(champion_df)

## 5. Visualization ------------------------------------------------------------

# Bar Plot: Simulated Championship Probabilities
ggplot(champion_df, aes(x = reorder(Squad, win_prob), y = win_prob)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = paste0("Projected MLS Cup Win Probabilities (N = ", n_sim, ")"),
    subtitle = "Based on Poisson Regression of Regular Season Offensive Efficiency",
    x = "Team",
    y = "Estimated Probability"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Save the plot
ggsave("simulated_probabilities.png", width = 8, height = 6)

