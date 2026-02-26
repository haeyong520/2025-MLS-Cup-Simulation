install.packages("tidyr")
install.packages("Hmisc")

## 0. Load packages ----------------------------------------------------
library(readxl)   # read Excel files
library(dplyr)    # data wrangling
library(tidyr)    # drop_na, etc.
library(ggplot2)  # visualization
library(Hmisc)

## 1. Import data & clean variable names -------------------------------

# 1-1. Read Excel files
setwd("C:/Users/520ch/OneDrive - Michigan State University/Desktop/Haeyong Chun/MSU/My courses/9. FS25/STT832/Final Project")
off_raw   <- read_excel("STT832_Final_Offensive_Data_2.xlsx")
defgk_raw <- read_excel("STT832_Final_Defensive_Data_2.xlsx")
pos_raw   <- read_excel("STT832_Final_Possesion.xlsx")
pts_raw   <- read_excel("STT832_pts.xlsx")

# 1-2. Make variable names R-friendly (replace %, /, - etc. with .)
off   <- off_raw   %>% rename_with(make.names)
defgk <- defgk_raw %>% rename_with(make.names)
pos   <- pos_raw   %>% rename_with(make.names)
pts   <- pts_raw   %>% rename_with(make.names)

# 1-3. Add prefixes to offensive/defensive/possession variables
#      Keep Squad as is and add off_ / def_ / pos_ to the others
off <- off %>%
  rename_with(~ paste0("off_", .x), -Squad)

defgk <- defgk %>%
  rename_with(~ paste0("def_", .x), -Squad)

pos <- pos %>%
  rename_with(~ paste0("pos_", .x), -Squad)

# 1-4. Compute MP, PPG, WinPct from W/D/L data
pts <- pts %>%
  mutate(
    MP     = W + D + L,      # matches played
    PPG    = Pts / MP,       # points per game
    WinPct = W / MP          # winning percentage
  )

# 1-5. Define lists of offensive/defensive/possession variables
#      (these will be used as predictors in the model)
off_vars   <- setdiff(names(off),   "Squad")  # e.g., off_Sh, off_SoT ...
defgk_vars <- setdiff(names(defgk), "Squad")  # e.g., def_GA, def_PSxG ...
pos_vars   <- setdiff(names(pos),   "Squad")  

# 1-6. Merge all data sets by Squad
dat <- off %>%
  inner_join(defgk, by = "Squad") %>%
  inner_join(pts,   by = "Squad") %>%
  inner_join(pos,   by = "Squad")

# Inspect structure
glimpse(dat)


#################################################################
#################################################################
#################################################################

# Performance outcomes
target_vars <- c("W", "D", "L")

# Automatically detect offensive/defensive/possession variables
# (columns starting with off_, def_, pos_)
off_vars <- grep("^off_", names(dat), value = TRUE)
def_vars <- grep("^def_", names(dat), value = TRUE)
pos_vars <- grep("^pos_", names(dat), value = TRUE)

# Poisson regression: W ~ (offensive variables) + offset(log(MP))
fit_W_off <- glm(
  as.formula(paste("W ~", paste(off_vars, collapse = " + "), "+ offset(log(MP))")),
  data   = dat,
  family = poisson(link = "log")
)

# Poisson regression: W ~ (offensive variables) + offset(log(MP))  (duplicated call)
fit_W_off <- glm(
  as.formula(paste("W ~", paste(off_vars, collapse = " + "), "+ offset(log(MP))")),
  data   = dat,
  family = poisson(link = "log")
)

dat_strength <- dat %>%
  mutate(
    W_hat        = fitted(fit_W_off),   # predicted wins from the model
    win_rate_hat = W_hat / MP          # expected wins per match = team strength proxy
  ) %>%
  select(Squad, win_rate_hat)

head(dat_strength)


library(tibble)

round1 <- tribble(
  ~match_id, ~teamA,              ~teamB,
  1,         "Philadelphia Union",    "Chicago Fire",
  2,         "FC Cincinnati",         "Columbus Crew",
  3,         "Inter Miami",           "Nashville SC",
  4,         "NYCFC",                 "Charlotte",
  5,         "Sandiego FC",           "Portland Timbers",
  6,         "Vancouver W'caps",      "FC Dallas",
  7,         "LAFC",                  "Austin",
  8,         "Minnesota Utd",         "Seattle Sounders"
)


# strength_df: data frame including Squad and win_rate_hat
strength_df <- dat_strength

get_strength <- function(team, strength_df) {
  strength_df$win_rate_hat[match(team, strength_df$Squad)]
}

simulate_match <- function(teamA, teamB, strength_df) {
  sA <- get_strength(teamA, strength_df)
  sB <- get_strength(teamB, strength_df)
  
  # Safeguard: if strength is NA or both are 0, use 0.5
  if (is.na(sA) || is.na(sB) || (sA <= 0 & sB <= 0)) {
    pA <- 0.5
  } else {
    # Bound strengths away from 0 so probabilities are well-defined
    sA <- max(sA, 1e-6)
    sB <- max(sB, 1e-6)
    pA <- sA / (sA + sB)
  }
  
  winner <- ifelse(runif(1) < pA, teamA, teamB)
  return(winner)
}

simulate_playoff_once <- function(round1, strength_df) {
  # ----- Round 1 -----
  winners_R1 <- character(nrow(round1))
  for (i in seq_len(nrow(round1))) {
    winners_R1[i] <- simulate_match(round1$teamA[i],
                                    round1$teamB[i],
                                    strength_df)
  }
  
  # Split into East/West (assuming matches 1–4 East, 5–8 West)
  east_R1_winners <- winners_R1[1:4]
  west_R1_winners <- winners_R1[5:8]
  
  # ----- Conference Semifinals (East) -----
  # (1 vs 2), (3 vs 4)
  east_R2_winners <- c(
    simulate_match(east_R1_winners[1], east_R1_winners[2], strength_df),
    simulate_match(east_R1_winners[3], east_R1_winners[4], strength_df)
  )
  
  # ----- Conference Semifinals (West) -----
  west_R2_winners <- c(
    simulate_match(west_R1_winners[1], west_R1_winners[2], strength_df),
    simulate_match(west_R1_winners[3], west_R1_winners[4], strength_df)
  )
  
  # ----- Conference Finals -----
  east_champ <- simulate_match(east_R2_winners[1], east_R2_winners[2], strength_df)
  west_champ <- simulate_match(west_R2_winners[1], west_R2_winners[2], strength_df)
  
  # ----- MLS Cup Final -----
  champion <- simulate_match(east_champ, west_champ, strength_df)
  
  return(champion)
}

set.seed(832)

n_sim <- 10000   # number of simulation runs (e.g., 10,000 in the report)

champions <- replicate(
  n_sim,
  simulate_playoff_once(round1, strength_df)
)

champions[1:10]  # inspect the first 10 simulated champions

champion_table <- table(champions)
champion_df <- as.data.frame(champion_table) %>%
  dplyr::rename(Squad = champions, wins = Freq) %>%
  dplyr::mutate(
    win_prob = wins / n_sim
  ) %>%
  dplyr::arrange(dplyr::desc(win_prob))

champion_df

library(ggplot2)

ggplot(champion_df, aes(x = reorder(Squad, win_prob), y = win_prob)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste0("Simulated MLS Cup win probabilities (", n_sim, " runs)"),
    x     = "Team",
    y     = "Estimated championship probability"
  )
