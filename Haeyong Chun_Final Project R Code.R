############################################################
## STT 832 Final Project: MLS W/D/L + Offense/Defense Analysis
## - Offensive vs defensive contributions to success
## - Poisson regression + expected points + Simulation
############################################################
install.packages("tidyr")
install.packages("Hmisc")

## 0. Load packages ----------------------------------------------------
library(readxl)   # read Excel files
library(dplyr)    # data wrangling
library(tidyr)    # drop_na, etc.
library(ggplot2)  # visualization
library(Hmisc)

## 1. Import data & clean variable names ---------------------------------

# 1-1. Read Excel files
setwd("C:/Users/520ch/OneDrive - Michigan State University/Desktop/Haeyong Chun/MSU/My courses/9. FS25/STT832/Final Project")
off_raw   <- read_excel("STT832_Final_Offensive_Data_2.xlsx")
defgk_raw <- read_excel("STT832_Final_Defensive_Data_2.xlsx")
pts_raw   <- read_excel("STT832_pts.xlsx")

# 1-2. Make variable names R-friendly (replace %, /, - etc. with .)
off   <- off_raw   %>% rename_with(make.names)
defgk <- defgk_raw %>% rename_with(make.names)
pts   <- pts_raw   %>% rename_with(make.names)

# 1-3. Add prefixes to offensive/defensive/possession variables
#      Keep Squad as is and add off_ / def_ / pos_ to the rest
off <- off %>%
  rename_with(~ paste0("off_", .x), -Squad)

defgk <- defgk %>%
  rename_with(~ paste0("def_", .x), -Squad)


# 1-4. Compute MP, PPG, WinPct from W/D/L data
pts <- pts %>%
  mutate(
    MP     = W + D + L,      # matches played
    PPG    = Pts / MP,       # points per game
    WinPct = W / MP          # winning percentage
  )

# 1-5. Define lists of offensive/defensive/possession variables
#      (these will be used as predictors later)
off_vars   <- setdiff(names(off),   "Squad")  # e.g., off_Sh, off_SoT, ...
defgk_vars <- setdiff(names(defgk), "Squad")  # e.g., def_GA, def_PSxG, ...

# 1-6. Merge all data sets by Squad
dat <- off %>%
  inner_join(defgk, by = "Squad") %>%
  inner_join(pts,   by = "Squad") 

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

# Offensive variables + outcomes only (numeric columns)
off_cor_df <- dat %>%
  dplyr::select(dplyr::all_of(target_vars), dplyr::all_of(off_vars)) %>%
  dplyr::select(where(is.numeric))

cor_with_WDL_off <- function(df, outcomes = c("W","D","L")) {
  vars <- setdiff(names(df), target_vars)  # predictor variables only (offense block)
  res_list <- list()
  
  for (y in outcomes) {
    for (x in vars) {
      # Use only rows where both variables are non-missing
      tmp <- df[, c(y, x)]
      tmp <- tmp[complete.cases(tmp), , drop = FALSE]
      
      if (nrow(tmp) >= 3) {
        ct <- suppressWarnings(cor.test(tmp[[y]], tmp[[x]], use = "pairwise.complete.obs"))
        res_list[[length(res_list) + 1]] <- tibble(
          outcome   = y,
          predictor = x,
          r         = unname(ct$estimate),
          p_value   = ct$p.value,
          n         = nrow(tmp)
        )
      }
    }
  }
  
  bind_rows(res_list)
}

off_cor_tbl <- cor_with_WDL_off(off_cor_df)

# Inspect results: offensive variables significantly correlated with W
off_cor_W_sig <- off_cor_tbl %>%
  filter(outcome == "W", p_value < 0.05) %>%
  arrange(desc(abs(r)))

# Similarly for D and L
off_cor_D_sig <- off_cor_tbl %>%
  filter(outcome == "D", p_value < 0.05) %>%
  arrange(desc(abs(r)))

off_cor_L_sig <- off_cor_tbl %>%
  filter(outcome == "L", p_value < 0.05) %>%
  arrange(desc(abs(r)))

off_cor_W_sig
off_cor_D_sig
off_cor_L_sig


# Defensive+GK variables + outcomes only
def_cor_df <- dat %>%
  dplyr::select(dplyr::all_of(target_vars), dplyr::all_of(def_vars)) %>%
  dplyr::select(where(is.numeric))

# Correlation matrix
def_cor <- cor(def_cor_df, use = "pairwise.complete.obs")

cor_with_WDL_def <- function(df, outcomes = c("W","D","L")) {
  vars <- setdiff(names(df), target_vars)  # predictor variables only (defense block)
  res_list <- list()
  
  for (y in outcomes) {
    for (x in vars) {
      # Use only rows where both variables are non-missing
      tmp <- df[, c(y, x)]
      tmp <- tmp[complete.cases(tmp), , drop = FALSE]
      
      if (nrow(tmp) >= 3) {
        ct <- suppressWarnings(cor.test(tmp[[y]], tmp[[x]], use = "pairwise.complete.obs"))
        res_list[[length(res_list) + 1]] <- tibble(
          outcome   = y,
          predictor = x,
          r         = unname(ct$estimate),
          p_value   = ct$p.value,
          n         = nrow(tmp)
        )
      }
    }
  }
  
  bind_rows(res_list)
}

def_cor_tbl <- cor_with_WDL_def(def_cor_df)

# Inspect results: defensive variables significantly correlated with W
def_cor_W_sig <- def_cor_tbl %>%
  filter(outcome == "W", p_value < 0.05) %>%
  arrange(desc(abs(r)))

# Similarly for D and L
def_cor_D_sig <- def_cor_tbl %>%
  filter(outcome == "D", p_value < 0.05) %>%
  arrange(desc(abs(r)))

def_cor_L_sig <- def_cor_tbl %>%
  filter(outcome == "L", p_value < 0.05) %>%
  arrange(desc(abs(r)))

def_cor_W_sig
def_cor_D_sig
def_cor_L_sig


# Print correlations between outcomes and defensive variables
round(
  def_cor[target_vars, setdiff(colnames(def_cor), target_vars)],
  2
)


install.packages("GGally")
library(GGally)

# Correlation plot for offensive block
GGally::ggcorr(
  off_cor_df,
  method      = c("everything", "pearson"),
  label       = TRUE,
  label_round = 2,
  label_size  = 2.5,
  hjust       = 0.75,
  size        = 2
)

# Correlation plot for defensive block
GGally::ggcorr(
  def_cor_df,
  method      = c("everything", "pearson"),
  label       = TRUE,
  label_round = 2,
  label_size  = 2.5,
  hjust       = 0.75,
  size        = 2
)

####################################################################
## 2. Basic summary & simple visualizations ------------------------

dat_summary <- dat %>%
  dplyr::select(Squad, W, D, L, Pts, PPG, WinPct) %>%
  dplyr::arrange(dplyr::desc(PPG))

dat_summary


## 3. Poisson regression: W ~ offensive indicators + offset(log(MP)) --

# 3-1. Data for the model (offensive predictors + W + MP)
dat_W_off <- dat %>%
  dplyr::select(Squad, W, MP, WinPct, all_of(off_vars)) %>%
  drop_na()

# 3-2. Formula: W ~ off_var1 + off_var2 + ... + offset(log(MP))
formula_W_off <- as.formula(
  paste("W ~", paste(off_vars, collapse = " + "), "+ offset(log(MP))")
)

# 3-3. Fit Poisson regression
fit_W_off <- glm(
  formula_W_off,
  data   = dat_W_off,
  family = poisson(link = "log")
)

summary(fit_W_off)

# 3-4. Predicted wins and observed vs predicted comparison
dat_W_off <- dat_W_off %>%
  mutate(
    W_hat = fitted(fit_W_off)
  )

ggplot(dat_W_off, aes(x = W, y = W_hat, label = Squad)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_text(vjust = -0.3, size = 3) +
  labs(
    title = "Observed W vs Predicted W",
    x     = "Observed wins (W)",
    y     = "Predicted wins (W)"
  )


## 4. Poisson regression: L ~ defensive+GK indicators + offset(log(MP)) -

# 4-1. Data for the model (defensive+GK predictors + L + MP)
dat_L_def <- dat %>%
  dplyr::select(Squad, L, MP, all_of(defgk_vars)) %>%
  drop_na()

# 4-2. Formula: L ~ def_var1 + def_var2 + ... + offset(log(MP))
formula_L_def <- as.formula(
  paste("L ~", paste(defgk_vars, collapse = " + "), "+ offset(log(MP))")
)

# 4-3. Fit Poisson regression
fit_L_def <- glm(
  formula_L_def,
  data   = dat_L_def,
  family = poisson(link = "log")
)

summary(fit_L_def)

# 4-4. Predicted losses and observed vs predicted comparison
dat_L_def <- dat_L_def %>%
  mutate(
    L_hat = fitted(fit_L_def)
  )

ggplot(dat_L_def, aes(x = L, y = L_hat, label = Squad)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_text(vjust = -0.3, size = 3) +
  labs(
    title = "observed L vs predicted L",
    x     = "Observed losses (L)",
    y     = "Predicted losses (L)"
  )



## 5. Expected points from Poisson model vs observed points -------------

# Here we use the offensive Poisson model for W
# Expected points = 3 * predicted wins + 1 * observed draws (simple version)

dat_pts_pred <- dat %>%
  dplyr::select(Squad, W, D, Pts, MP, all_of(off_vars)) %>%
  drop_na()

# Refit W model on this subset (in case rows changed after drop_na)
formula_W_off2 <- as.formula(
  paste("W ~", paste(off_vars, collapse = " + "), "+ offset(log(MP))")
)

fit_W_off2 <- glm(
  formula_W_off2,
  data   = dat_pts_pred,
  family = poisson(link = "log")
)

dat_pts_pred <- dat_pts_pred %>%
  mutate(
    W_hat    = fitted(fit_W_off2),
    Pts_hat  = 3 * W_hat + 1 * D,
    Pts_diff = Pts - Pts_hat
  )

# Observed vs predicted points
ggplot(dat_pts_pred, aes(x = Pts, y = Pts_hat, label = Squad)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_text(vjust = -0.3, size = 3) +
  labs(
    title = "Observed points vs predicted points from offensive Poisson model",
    x     = "Observed points (Pts)",
    y     = "Predicted points (Pts_hat)"
  )

# Over- and under-performing teams relative to the model
ggplot(dat_pts_pred, aes(x = reorder(Squad, Pts_diff), y = Pts_diff)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Offensive model: observed points - predicted points",
    x     = NULL,
    y     = "Points difference (observed - predicted)"
  )

#################################################################
#################################################################
#################################################################

# Performance outcomes
target_vars <- c("W", "D", "L")

# Automatically detect offensive/defensive/possession variables
# (columns starting with off_, def_, pos_)
off_vars <- grep("^off_", names(dat), value = TRUE)
def_vars <- grep("^def_", names(dat), value = TRUE)

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


ggplot(champion_df, aes(x = reorder(Squad, win_prob), y = win_prob)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste0("Simulated MLS Cup win probabilities (", n_sim, " runs)"),
    x     = "Team",
    y     = "Estimated championship probability"
  )

