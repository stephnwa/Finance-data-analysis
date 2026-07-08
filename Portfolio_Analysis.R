
# ══════════════════════════════════════════════════════════════════════════════
# THESIS R CODE — EMPIRICAL DESIGN
# European Energy Transition Portfolio Analysis
#   Portfolio A: Equal-weight long-only benchmark (all 12 firms)
#   Portfolio B: Signal-ranked long-only (top 6 firms, equal-weight)
#   Portfolio C: Signal-weighted long-only (top 6 firms, rank-weighted)
# Author: Stephanie Livinus
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

# ── LIBRARIES ─────────────────────────────────────────────────────────────────
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(slider)
library(janitor)
library(ggplot2)
library(scales)

# ── LOAD DATA ─────────────────────────────────────────────────────────────────
master_data <- read_excel("C:\\Users\\Steph\\Documents\\12 firms data backup.xlsx", sheet = "Master_data") %>% clean_names()
msci_data <- read_excel("C:\\Users\\Steph\\Documents\\12 firms data backup.xlsx", sheet = "MSCI_EU") %>% clean_names()

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 1: DATA PREPARATION
# ══════════════════════════════════════════════════════════════════════════════

# ── 1.1 Clean dates ───────────────────────────────────────────────────────────
master_data <- master_data %>%
  mutate(date = as.Date(date), firm = as.factor(firm)) %>%
  arrange(firm, date)

msci_data <- msci_data %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# ── 1.2 MSCI monthly returns ──────────────────────────────────────────────────
msci_data <- msci_data %>%
  mutate(msci_return = (msci_eu / lag(msci_eu)) - 1)

# ── 1.3 Rolling 12-month volatility ───────────────────────────────────────────
msci_data <- msci_data %>%
  mutate(
    rolling_vol_12m = slide_dbl(
      msci_return,
      ~ sd(.x, na.rm = TRUE),
      .before = 11,
      .complete = TRUE
    )
  )

# ── 1.4 Volatility period classification (median split) ───────────────────────
vol_threshold <- median(msci_data$rolling_vol_12m, na.rm = TRUE)

msci_data <- msci_data %>%
  mutate(
    vol_period = ifelse(
      rolling_vol_12m > vol_threshold,
      "High Volatility",
      "Low Volatility"
    )
  )

cat("=== VOLATILITY PERIOD SPLIT ===\n")
print(table(msci_data$vol_period))
cat("Median threshold:", round(vol_threshold, 6), "\n\n")

# ── 1.5 Merge vol periods into firm data ──────────────────────────────────────
master_data <- master_data %>%
  left_join(msci_data %>% select(date, vol_period), by = "date")

# ── 1.6 Cross-sectional signal normalisation (z-score each month) ─────────────
# SignalZ_i,t = (Signal_i,t - mean_t) / sd_t
master_data <- master_data %>%
  group_by(date) %>%
  mutate(
    signal_z = (signal - mean(signal, na.rm = TRUE)) /
      sd(signal, na.rm = TRUE)
  ) %>%
  ungroup()

# ── 1.7 Forward return — avoids look-ahead bias ───────────────────────────────
# Signal at t is matched to return at t+1
master_data <- master_data %>%
  group_by(firm) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(return_lead = lead(return)) %>%
  ungroup()

cat("Firms in sample:", n_distinct(master_data$firm), "\n")
cat("Date range:", as.character(min(master_data$date)),
    "to", as.character(max(master_data$date)), "\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 2: MAIN LONG-ONLY PORTFOLIO CONSTRUCTION
# ══════════════════════════════════════════════════════════════════════════════

# Number of firms to select each month for Portfolios B and C
N_TOP <- 6   # top 6 out of 12 — practical choice balancing signal strength
# and concentration risk (top 4 would be a robustness check)

# ── 2.1 PORTFOLIO A: Equal-Weight Long-Only Benchmark ─────────────────────────
portfolio_A <- master_data %>%
  filter(!is.na(return_lead)) %>%
  group_by(date, vol_period) %>%
  summarise(
    A_return     = mean(return_lead, na.rm = TRUE),
    n_firms      = n(),
    .groups = "drop"
  )

cat("=== PORTFOLIO A: Equal-Weight ===\n")
cat("Observations:", nrow(portfolio_A), "\n\n")

# ── 2.2 PORTFOLIO B: Signal-Ranked Long-Only (Top 6, Equal-Weight) ────────────
# w_B_i,t = 1/6 if firm i is in top 6, else 0
portfolio_B_firm_level <- master_data %>%
  filter(!is.na(return_lead), !is.na(signal)) %>%
  group_by(date) %>%
  arrange(desc(signal), .by_group = TRUE) %>%          # rank by RAW signal
  mutate(
    signal_rank_desc = row_number(),       # rank 1 = highest raw signal
    in_top6          = signal_rank_desc <= N_TOP,
    B_weight         = ifelse(in_top6, 1 / N_TOP, 0)
  ) %>%
  ungroup()

portfolio_B <- portfolio_B_firm_level %>%
  group_by(date, vol_period) %>%
  summarise(
    B_return    = sum(B_weight * return_lead, na.rm = TRUE),
    n_selected  = sum(in_top6),
    .groups = "drop"
  ) %>%
  filter(!is.na(B_return))

cat("=== PORTFOLIO B: Signal-Ranked Long-Only (Top 6) ===\n")
cat("Observations:", nrow(portfolio_B), "\n\n")

# ── 2.3 PORTFOLIO C: Signal-Weighted Long-Only (Top 6, Rank-Weighted) ─────────
# Weight = rank_score / sum(rank_scores) = rank_score / 21
# This gives: w_max = 6/21 ≈ 28.6%, w_min = 1/21 ≈ 4.8%
portfolio_C_firm_level <- master_data %>%
  filter(!is.na(return_lead), !is.na(signal)) %>%
  group_by(date) %>%
  arrange(desc(signal), .by_group = TRUE) %>%          # rank by RAW signal
  mutate(
    signal_rank_desc = row_number(),
    in_top6          = signal_rank_desc <= N_TOP,
    # Rank score: highest signal = 6, second = 5, ..., 6th = 1
    rank_score       = ifelse(in_top6, (N_TOP + 1) - signal_rank_desc, 0),
    C_weight         = rank_score / sum(rank_score[in_top6], na.rm = TRUE)
  ) %>%
  ungroup()
# Verify weights sum to 1 each month
weight_check <- portfolio_C_firm_level %>%
  group_by(date) %>%
  summarise(weight_sum = sum(C_weight, na.rm = TRUE), .groups = "drop")

cat("=== PORTFOLIO C WEIGHT CHECK ===\n")
cat("All weights sum to 1?", all(abs(weight_check$weight_sum - 1) < 1e-10), "\n")
cat("Min weight sum:", min(weight_check$weight_sum), "\n")
cat("Max weight sum:", max(weight_check$weight_sum), "\n\n")

portfolio_C <- portfolio_C_firm_level %>%
  group_by(date, vol_period) %>%
  summarise(
    C_return   = sum(C_weight * return_lead, na.rm = TRUE),
    n_selected = sum(in_top6),
    .groups = "drop"
  ) %>%
  filter(!is.na(C_return))

cat("=== PORTFOLIO C: Signal-Weighted Long-Only (Top 6, Rank-Weighted) ===\n")
cat("Observations:", nrow(portfolio_C), "\n\n")

# ── 2.4 Verify observation counts match ───────────────────────────────────────
cat("=== OBSERVATION COUNT CHECK (should all match) ===\n")
cat("Portfolio A:", nrow(portfolio_A), "\n")
cat("Portfolio B:", nrow(portfolio_B), "\n")
cat("Portfolio C:", nrow(portfolio_C), "\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 3: SECONDARY DIAGNOSTIC — LONG-SHORT ANALYSIS
# ══════════════════════════════════════════════════════════════════════════════
# ── 3.1 Long-Short B: Ranked (top 6 long, bottom 6 short, equal-weight) ───────
portfolio_LS_B <- portfolio_B_firm_level %>%
  group_by(date) %>%
  mutate(
    in_bottom6   = signal_rank_desc > (12 - N_TOP),  # bottom 6
    LS_B_long_w  = B_weight,
    LS_B_short_w = ifelse(in_bottom6, 1 / N_TOP, 0)
  ) %>%
  summarise(
    LS_B_long_return  = sum(LS_B_long_w  * return_lead, na.rm = TRUE),
    LS_B_short_return = sum(LS_B_short_w * return_lead, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(LS_B_spread = LS_B_long_return - LS_B_short_return) %>%
  left_join(msci_data %>% select(date, vol_period), by = "date") %>%
  filter(!is.na(LS_B_spread))

cat("=== LONG-SHORT B: Spread (diagnostic) ===\n")
cat("Observations:", nrow(portfolio_LS_B), "\n\n")

# ── 3.1b ROBUSTNESS CHECK FOR PORTFOLIO B: Signal-Ranked Long-Only (Top 4, Equal-Weight) ──────
N_TOP4 <- 4
portfolio_B_top4_firm_level <- master_data %>%
  filter(!is.na(return_lead), !is.na(signal)) %>%
  group_by(date) %>%
  arrange(desc(signal), .by_group = TRUE) %>%
  mutate(
    signal_rank_desc = row_number(),
    in_top4          = signal_rank_desc <= N_TOP4,
    B4_weight        = ifelse(in_top4, 1 / N_TOP4, 0)
  ) %>%
  ungroup()

portfolio_B_top4 <- portfolio_B_top4_firm_level %>%
  group_by(date, vol_period) %>%
  summarise(
    B4_return   = sum(B4_weight * return_lead, na.rm = TRUE),
    n_selected  = sum(in_top4),
    .groups = "drop"
  ) %>%
  filter(!is.na(B4_return))

cat("=== ROBUSTNESS: Portfolio B Top 4 ===\n")
cat("Observations:", nrow(portfolio_B_top4), "\n\n")

# ── 3.2 Long-Short C: Weighted (normalised z-score signal weighting) ──────────
 portfolio_LS_C <- master_data %>%
  filter(!is.na(return_lead), !is.na(signal_z)) %>%
  group_by(date) %>%
  mutate(
    long_weight  = ifelse(signal_z > 0,
                          signal_z / sum(signal_z[signal_z > 0], na.rm = TRUE),
                          0),
    short_weight = ifelse(signal_z < 0,
                          abs(signal_z) / sum(abs(signal_z[signal_z < 0]), na.rm = TRUE),
                          0)
  ) %>%
  summarise(
    LS_C_long_return  = sum(long_weight  * return_lead, na.rm = TRUE),
    LS_C_short_return = sum(short_weight * return_lead, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(LS_C_spread = LS_C_long_return - LS_C_short_return) %>%
  left_join(msci_data %>% select(date, vol_period), by = "date") %>%
  filter(!is.na(LS_C_spread))

cat("=== LONG-SHORT C: Spread (diagnostic) ===\n")
cat("Observations:", nrow(portfolio_LS_C), "\n\n")

# ── 3.2b ROBUSTNESS CHECK for Portfolio C: Signal-Weighted Long-Only (Top 4, Rank-Weighted) ───
portfolio_C_top4_firm_level <- master_data %>%
  filter(!is.na(return_lead), !is.na(signal)) %>%
  group_by(date) %>%
  arrange(desc(signal), .by_group = TRUE) %>%
  mutate(
    signal_rank_desc = row_number(),
    in_top4          = signal_rank_desc <= N_TOP4,
    rank_score_4     = ifelse(in_top4, (N_TOP4 + 1) - signal_rank_desc, 0),
    C4_weight        = rank_score_4 / sum(rank_score_4[in_top4], na.rm = TRUE)
  ) %>%
  ungroup()

portfolio_C_top4 <- portfolio_C_top4_firm_level %>%
  group_by(date, vol_period) %>%
  summarise(
    C4_return  = sum(C4_weight * return_lead, na.rm = TRUE),
    n_selected = sum(in_top4),
    .groups = "drop"
  ) %>%
  filter(!is.na(C4_return))

cat("=== ROBUSTNESS: Portfolio C Top 4 ===\n")
cat("Observations:", nrow(portfolio_C_top4), "\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 4: PERFORMANCE METRIC FUNCTIONS
# ══════════════════════════════════════════════════════════════════════════════
sharpe_ratio <- function(r) {
  r <- na.omit(r)
  if (sd(r) == 0) return(NA)
  mean(r) / sd(r)
}

# t-statistic for mean return
t_statistic <- function(r) {
  r <- na.omit(r)
  mean(r) / (sd(r) / sqrt(length(r)))
}

# Maximum drawdown
max_drawdown <- function(r) {
  r    <- na.omit(r)
  cum  <- cumprod(1 + r)
  peak <- cummax(cum)
  dd   <- (cum - peak) / peak
  min(dd)
}

# Win rate
win_rate <- function(r) {
  r <- na.omit(r)
  mean(r > 0)
}

# Master summary function
performance_summary <- function(r, label) {
  r <- na.omit(r)
  data.frame(
    Portfolio    = label,
    Mean_Return  = mean(r),
    Std_Dev      = sd(r),
    Sharpe_Ratio = sharpe_ratio(r),
    t_Statistic  = t_statistic(r),
    Max_Drawdown = max_drawdown(r),
    Win_Rate     = win_rate(r),
    Observations = length(r)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 5: FULL-PERIOD PERFORMANCE — MAIN LONG-ONLY COMPARISON (Table 4.1)
# ══════════════════════════════════════════════════════════════════════════════

r_A      <- portfolio_A$A_return
r_B      <- portfolio_B$B_return
r_C      <- portfolio_C$C_return
r_LS_B   <- portfolio_LS_B$LS_B_spread
r_LS_C   <- portfolio_LS_C$LS_C_spread

full_period_main <- bind_rows(
  performance_summary(r_A,    "Portfolio A — Equal-Weight (all 12)"),
  performance_summary(r_B,    "Portfolio B — Signal-Ranked (top 6, equal-weight)"),
  performance_summary(r_C,    "Portfolio C — Signal-Weighted (top 6, rank-weighted)"),
  performance_summary(portfolio_B_top4$B4_return, "Robustness - Signal-Ranked top 4 B equal-weight"),
  performance_summary(portfolio_C_top4$C4_return, "Robustness - Signal-Ranked top 4 C equal-weight")
  )

cat("══════════════════════════════════════════════════════\n")
cat("TABLE 4.1: MAIN LONG-ONLY FULL-PERIOD SUMMARY\n")
cat("══════════════════════════════════════════════════════\n")
print(full_period_main)
cat("\n")

# ── Secondary: Long-Short diagnostic ─────────────────────────────────────────
full_period_ls <- bind_rows(
  performance_summary(r_LS_B, "Long-Short B — Ranked spread (diagnostic)"),
  performance_summary(r_LS_C, "Long-Short C — Weighted spread (diagnostic)")
)

cat("══════════════════════════════════════════════════════\n")
cat("TABLE 4.X: LONG-SHORT DIAGNOSTIC SUMMARY\n")
cat("══════════════════════════════════════════════════════\n")
print(full_period_ls)
cat("\n")
# ══════════════════════════════════════════════════════════════════════════════
# SECTION 5B: DIRECT BENCHMARK COMPARISON TESTS
# ══════════════════════════════════════════════════════════════════════════════

diff_BA <- r_B - r_A   # Portfolio B minus Portfolio A
diff_CA <- r_C - r_A   # Portfolio C minus Portfolio A
diff_CB <- r_C - r_B   # Portfolio C minus Portfolio B

test_BA <- t.test(diff_BA)
test_CA <- t.test(diff_CA)
test_CB <- t.test(diff_CB)

cat("══════════════════════════════════════════════════════\n")
cat("TABLE: DIRECT BENCHMARK COMPARISON TESTS\n")
cat("══════════════════════════════════════════════════════\n")
cat("B minus A — Mean diff:", round(mean(diff_BA), 6),
    "| t-stat:", round(test_BA$statistic, 3),
    "| p-value:", round(test_BA$p.value, 4), "\n")
cat("C minus A — Mean diff:", round(mean(diff_CA), 6),
    "| t-stat:", round(test_CA$statistic, 3),
    "| p-value:", round(test_CA$p.value, 4), "\n")
cat("C minus B — Mean diff:", round(mean(diff_CB), 6),
    "| t-stat:", round(test_CB$statistic, 3),
    "| p-value:", round(test_CB$p.value, 4), "\n\n")
# Export comparison test results to CSV
comparison_tests <- data.frame(
  Comparison = c("Portfolio B minus Portfolio A", 
                 "Portfolio C minus Portfolio A", 
                 "Portfolio C minus Portfolio B"),
  Mean_Difference = c(mean(diff_BA), mean(diff_CA), mean(diff_CB)),
  t_Statistic = c(test_BA$statistic, test_CA$statistic, test_CB$statistic),
  p_value = c(test_BA$p.value, test_CA$p.value, test_CB$p.value)
)

write.csv(comparison_tests, "table_benchmark_comparisons.csv", row.names = FALSE)
cat("Benchmark comparison tests saved to table_benchmark_comparisons.csv\n")
# ══════════════════════════════════════════════════════════════════════════════
# SECTION 10: DIRECT BENCHMARK COMPARISON & LEDOIT-WOLF SHARPE RATIO TEST
# (Table 4.6 and Table 4.7 in thesis)
# ══════════════════════════════════════════════════════════════════════════════

# ── 10.1 Pairwise mean return difference tests ────────────────────────────────
diff_BA <- r_B - r_A
diff_CA <- r_C - r_A
diff_CB <- r_C - r_B

pairwise_test <- function(diff, label) {
  t  <- t.test(diff, mu = 0)
  data.frame(
    Comparison      = label,
    Mean_Difference = mean(diff),
    t_Statistic     = t$statistic,
    p_value         = t$p.value
  )
}

benchmark_comparisons <- bind_rows(
  pairwise_test(diff_BA, "Portfolio B minus Portfolio A"),
  pairwise_test(diff_CA, "Portfolio C minus Portfolio A"),
  pairwise_test(diff_CB, "Portfolio C minus Portfolio B")
)

cat("══════════════════════════════════════════════════════\n")
cat("TABLE 4.6: DIRECT BENCHMARK COMPARISON TEST\n")
cat("══════════════════════════════════════════════════════\n")
print(benchmark_comparisons)
cat("\n")

write.csv(benchmark_comparisons, "table_benchmark_comparisons.csv", row.names = FALSE)

# ── 10.2 Ledoit-Wolf Sharpe ratio difference test ─────────────────────────────
# install.packages("PeerPerformance")  # run once if not installed
library(PeerPerformance)

test_BA <- sharpeTesting(r_B, r_A, control = list(type = 1, ttype = 2))
test_CA <- sharpeTesting(r_C, r_A, control = list(type = 1, ttype = 2))
test_CB <- sharpeTesting(r_C, r_B, control = list(type = 1, ttype = 2))

lw_results <- data.frame(
  Comparison    = c("SR(B) - SR(A)", "SR(C) - SR(A)", "SR(C) - SR(B)"),
  SR_Difference = c(test_BA$dsharpe, test_CA$dsharpe, test_CB$dsharpe),
  p_value       = c(test_BA$pval,    test_CA$pval,    test_CB$pval)
)

cat("══════════════════════════════════════════════════════\n")
cat("TABLE 4.7: LEDOIT-WOLF SHARPE RATIO DIFFERENCE TEST\n")
cat("══════════════════════════════════════════════════════\n")
print(lw_results)
cat("\n")

write.csv(lw_results, "table_ledoit_wolf_sharpe.csv", row.names = FALSE)
# ══════════════════════════════════════════════════════════════════════════════
# SECTION 6: ANNUAL RETURNS (Table 4.2)
# ══════════════════════════════════════════════════════════════════════════════

annual_A <- portfolio_A %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(A_annual = prod(1 + A_return) - 1, .groups = "drop")

annual_B <- portfolio_B %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(B_annual = prod(1 + B_return) - 1, .groups = "drop")

annual_C <- portfolio_C %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(C_annual = prod(1 + C_return) - 1, .groups = "drop")

annual_B4 <- portfolio_B_top4 %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(B4_annual = prod(1 + B4_return) - 1, .groups = "drop")

annual_C4 <- portfolio_C_top4 %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(C4_annual = prod(1 + C4_return) - 1, .groups = "drop")

annual_returns <- annual_A %>%
  left_join(annual_B, by = "year") %>%
  left_join(annual_C, by = "year") %>%
  left_join(annual_B4,  by = "year") %>%
  left_join(annual_C4,  by = "year")

cat("══════════════════════════════════════════════════════\n")
cat("TABLE 4.2: ANNUAL RETURNS — LONG-ONLY PORTFOLIOS\n")
cat("══════════════════════════════════════════════════════\n")
print(annual_returns)
cat("\n")

# Cumulative end values
cat("=== CUMULATIVE WEALTH (Base = 1) ===\n")
cat("Portfolio A:", round(prod(1 + r_A), 4), "\n")
cat("Portfolio B:", round(prod(1 + r_B), 4), "\n")
cat("Portfolio C:", round(prod(1 + r_C), 4), "\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 7: VOLATILITY PERIOD PERFORMANCE (Table 4.3)
# ══════════════════════════════════════════════════════════════════════════════

regime_summary <- function(ret_vec, period_vec, label) {
  df <- data.frame(r = ret_vec, period = period_vec) %>% na.omit()
  df %>%
    group_by(period) %>%
    summarise(
      Portfolio    = label,
      Mean_Return  = mean(r),
      Std_Dev      = sd(r),
      Sharpe_Ratio = mean(r) / sd(r),
      t_Statistic  = mean(r) / (sd(r) / sqrt(n())),
      Win_Rate     = mean(r > 0),
      Observations = n(),
      .groups = "drop"
    ) %>%
    select(Portfolio, period, everything())
}

vol_period_results <- bind_rows(
  regime_summary(portfolio_A$A_return,         portfolio_A$vol_period,    "Portfolio A — Equal-Weight"),
  regime_summary(portfolio_B$B_return,         portfolio_B$vol_period,    "Portfolio B — Signal-Ranked"),
  regime_summary(portfolio_C$C_return,         portfolio_C$vol_period,    "Portfolio C — Signal-Weighted"),
  regime_summary(portfolio_B_top4$B4_return,     portfolio_B_top4$vol_period,  "Robustness — Signal-Ranked (top 4)"),
  regime_summary(portfolio_C_top4$C4_return,     portfolio_C_top4$vol_period,  "Robustness — Signal-Ranked (top 4)")
)

cat("══════════════════════════════════════════════════════\n")
cat("TABLE 4.3: VOLATILITY PERIOD RESULTS — LONG-ONLY\n")
cat("══════════════════════════════════════════════════════\n")
print(vol_period_results)
cat("\n")

# Long-short diagnostic by volatility period
vol_period_ls <- bind_rows(
  regime_summary(portfolio_LS_B$LS_B_spread, portfolio_LS_B$vol_period, "Long-Short B — Ranked spread"),
  regime_summary(portfolio_LS_C$LS_C_spread, portfolio_LS_C$vol_period, "Long-Short C — Weighted spread")
)

cat("=== VOLATILITY PERIOD: LONG-SHORT DIAGNOSTIC ===\n")
print(vol_period_ls)
cat("\n")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 8: PORTFOLIO B vs A — LONG vs BOTTOM BREAKDOWN (Table 4.4)
# ══════════════════════════════════════════════════════════════════════════════

top_bottom_breakdown <- portfolio_LS_B %>%
  select(date, vol_period,
         LS_B_long_return, LS_B_short_return, LS_B_spread)

breakdown_summary <- bind_rows(
  performance_summary(top_bottom_breakdown$LS_B_long_return,  "B — Top 6 long leg"),
  performance_summary(top_bottom_breakdown$LS_B_short_return, "B — Bottom 6 (excluded from B)"),
  performance_summary(top_bottom_breakdown$LS_B_spread,       "B — Spread (top 6 minus bottom 6)")
)

cat("══════════════════════════════════════════════════════\n")
cat("TABLE 4.4: TOP 6 vs BOTTOM 6 BREAKDOWN (Portfolio B diagnostic)\n")
cat("══════════════════════════════════════════════════════\n")
print(breakdown_summary)
cat("\n")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 9: PORTFOLIO WEIGHT INSPECTION
# ══════════════════════════════════════════════════════════════════════════════

# Which firms appear most often in the top 6?
top6_frequency <- portfolio_B_firm_level %>%
  filter(in_top6 == TRUE) %>%
  group_by(firm) %>%
  summarise(
    months_in_top6   = n(),
    pct_months_top6  = n() / n_distinct(portfolio_B_firm_level$date),
    avg_signal_when_top6 = mean(signal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(months_in_top6))

cat("=== FIRM FREQUENCY IN TOP 6 (Portfolio B) ===\n")
print(top6_frequency)
cat("\n")

# Portfolio C weight range over time
weight_range_C <- portfolio_C_firm_level %>%
  filter(in_top6 == TRUE) %>%
  summarise(
    min_weight    = min(C_weight,   na.rm = TRUE),
    max_weight    = max(C_weight,   na.rm = TRUE),
    mean_weight   = mean(C_weight,  na.rm = TRUE),
    sd_weight     = sd(C_weight,    na.rm = TRUE)
  )

cat("=== PORTFOLIO C WEIGHT RANGE (within top 6) ===\n")
print(weight_range_C)
cat("Note: Theoretical range is 1/21 ≈ 4.8% to 6/21 ≈ 28.6%\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 10: CUMULATIVE RETURN DATA (for Figure 4.1)
# ══════════════════════════════════════════════════════════════════════════════

# Align all three portfolios on the same dates
common_dates <- Reduce(intersect, list(
  as.character(portfolio_A$date),
  as.character(portfolio_B$date),
  as.character(portfolio_C$date)
))

cum_A <- portfolio_A %>%
  filter(as.character(date) %in% common_dates) %>%
  arrange(date) %>%
  mutate(cum_A = cumprod(1 + A_return))

cum_B <- portfolio_B %>%
  filter(as.character(date) %in% common_dates) %>%
  arrange(date) %>%
  mutate(cum_B = cumprod(1 + B_return))

cum_C <- portfolio_C %>%
  filter(as.character(date) %in% common_dates) %>%
  arrange(date) %>%
  mutate(cum_C = cumprod(1 + C_return))

cum_all <- cum_A %>%
  select(date, cum_A) %>%
  left_join(cum_B %>% select(date, cum_B), by = "date") %>%
  left_join(cum_C %>% select(date, cum_C), by = "date") %>%
  pivot_longer(
    cols      = c(cum_A, cum_B, cum_C),
    names_to  = "portfolio",
    values_to = "cumulative_return"
  ) %>%
  mutate(
    portfolio = recode(portfolio,
                       "cum_A" = "Portfolio A (Equal-Weight)",
                       "cum_B" = "Portfolio B (Ranked Top 6)",
                       "cum_C" = "Portfolio C (Weighted Top 6)"
    )
  )

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 11: ROLLING 12-MONTH SHARPE (for Figure 4.2)
# ══════════════════════════════════════════════════════════════════════════════

rolling_df <- cum_A %>%
  select(date) %>%
  left_join(portfolio_A    %>% select(date, A_return),  by = "date") %>%
  left_join(portfolio_B    %>% select(date, B_return),  by = "date") %>%
  left_join(portfolio_C    %>% select(date, C_return),  by = "date") %>%
  arrange(date) %>%
  mutate(
    roll_sharpe_A  = slide_dbl(A_return,  ~ if(sd(.x,na.rm=T)==0) NA else mean(.x,na.rm=T)/sd(.x,na.rm=T), .before=11, .complete=TRUE),
    roll_sharpe_B  = slide_dbl(B_return,  ~ if(sd(.x,na.rm=T)==0) NA else mean(.x,na.rm=T)/sd(.x,na.rm=T), .before=11, .complete=TRUE),
    roll_sharpe_C  = slide_dbl(C_return,  ~ if(sd(.x,na.rm=T)==0) NA else mean(.x,na.rm=T)/sd(.x,na.rm=T), .before=11, .complete=TRUE),
  ) %>%
  pivot_longer(
    cols      = c(roll_sharpe_A, roll_sharpe_B, roll_sharpe_C),                           
    names_to  = "portfolio",
    values_to = "rolling_sharpe"
  ) %>%
  mutate(
    portfolio = recode(portfolio,
                       "roll_sharpe_A"  = "Portfolio A (Equal-Weight)",
                       "roll_sharpe_B"  = "Portfolio B (Ranked top 6)",
                       "roll_sharpe_C"  = "Portfolio C (Weighted top 6)"
    )
  )
# ══════════════════════════════════════════════════════════════════════════════
# SECTION 12: FIGURES
# ══════════════════════════════════════════════════════════════════════════════

portfolio_colours <- c(
  "Portfolio A (Equal-Weight)"    = "#2c3e50",
  "Portfolio B (Ranked Top 6)"    = "#2980b9",
  "Portfolio C (Weighted Top 6)"  = "#e67e22"
)

# ── FIGURE 4.1: Cumulative Returns — Main Long-Only Comparison ────────────────
fig1 <- ggplot(cum_all,
               aes(x = date, y = cumulative_return,
                   colour = portfolio, linetype = portfolio)) +
  geom_line(size = 0.9) +
  geom_hline(yintercept = 1, linetype = "dotted",
             colour = "grey50", size = 0.5) +
  scale_colour_manual(values = portfolio_colours) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash")) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    title    = "Cumulative Returns: Long-Only Portfolios (January 2017 – February 2026)",
    subtitle = "Base = 1.0 at January 2017",
    x        = NULL,
    y        = "Cumulative Growth",
    colour   = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

ggsave("fig1_cumulative_longonly.png", fig1,
       width = 10, height = 6, dpi = 150)

# ── FIGURE 4.2: Annual Returns Bar Chart ──────────────────────────────────────
annual_long <- annual_returns %>%
  pivot_longer(
    cols      = c(A_annual, B_annual, C_annual),
    names_to  = "portfolio",
    values_to = "annual_return"
  ) %>%
  mutate(
    portfolio = recode(portfolio,
                       "A_annual" = "Portfolio A (Equal-Weight)",
                       "B_annual" = "Portfolio B (Ranked Top 6)",
                       "C_annual" = "Portfolio C (Weighted Top 6)"
    ),
    annual_pct = annual_return * 100
  )

fig2 <- ggplot(annual_long,
               aes(x = factor(year), y = annual_pct, fill = portfolio)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(yintercept = 0, colour = "black", size = 0.6) +
  scale_fill_manual(values = c(
    "Portfolio A (Equal-Weight)"   = "#2c3e50",
    "Portfolio B (Ranked Top 6)"   = "#2980b9",
    "Portfolio C (Weighted Top 6)" = "#e67e22"
  )) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Annual Returns by Portfolio (2017–2026)",
    x     = NULL,
    y     = "Annual Return (%)",
    fill  = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold", size = 12),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("fig2_annual_returns_longonly.png", fig2,
       width = 12, height = 6, dpi = 150)

# ── FIGURE 4.3: Rolling 12-Month Sharpe ───────────────────────────────────────
fig3 <- ggplot(rolling_df %>% filter(!is.na(rolling_sharpe)),
               aes(x = date, y = rolling_sharpe, colour = portfolio)) +
  geom_line(size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey40", size = 0.6) +
  scale_colour_manual(values = c(
    "Portfolio A (Equal-Weight)"    = "#2c3e50",
    "Portfolio B (Ranked top 6)"    = "#2980b9",
    "Portfolio C (Weighted top 6)"  = "#e67e22"
    )) +
  labs(
    title    = "Rolling 12-Month Return-to-Volatility Ratio",
    subtitle = "Return-to-Volatility Ratio",
    x        = NULL,
    y        = "Rolling Return-to-volatility Ratio",
    colour   = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    plot.title       = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

ggsave("fig3_rolling_sharpe_longonly.png", fig3,
       width = 10, height = 5, dpi = 150)

# ── FIGURE 4.4: Month-by-Month Return Comparison: Portfolio B vs Portfolio C ──

bc_comparison <- portfolio_B %>%
  select(date, B_return) %>%
  left_join(portfolio_C %>% select(date, C_return), by = "date") %>%
  na.omit()

# Count months where each outperforms
n_B_wins <- sum(bc_comparison$B_return > bc_comparison$C_return)
n_C_wins <- sum(bc_comparison$C_return > bc_comparison$B_return)

fig4_4 <- ggplot(bc_comparison,
                 aes(x = B_return * 100, y = C_return * 100)) +
  geom_point(aes(colour = C_return > B_return), size = 2.5, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey40", size = 0.7) +
  scale_colour_manual(
    values = c("TRUE" = "#e67e22", "FALSE" = "#2980b9"),
    labels = c("TRUE"  = paste("C outperforms B:", n_C_wins, "months"),
               "FALSE" = paste("B outperforms C:", n_B_wins, "months"))
  ) +
  labs(
    title    = "Monthly Return: Portfolio B versus Portfolio C",
    subtitle = paste0("January 2017 – February 2026, n = ", nrow(bc_comparison), " months"),
    x        = "Portfolio B Monthly Return (%)",
    y        = "Portfolio C Monthly Return (%)",
    colour   = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    plot.title       = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

ggsave("fig4_4_BC_scatter.png", fig4_4,
       width = 8, height = 7, dpi = 150)

# ── FIGURE 4.4: Volatility Period Bar Chart — Long-Only ───────────────────────
vol_bar_data_fixed <- vol_period_results %>%
  filter(Portfolio %in% c(
    "Portfolio A — Equal-Weight",
    "Portfolio B — Signal-Ranked",
    "Portfolio C — Signal-Weighted"
  )) %>%
  mutate(
    mean_pct  = Mean_Return * 100,
    Portfolio = factor(Portfolio, levels = c(
      "Portfolio A — Equal-Weight",
      "Portfolio B — Signal-Ranked",
      "Portfolio C — Signal-Weighted"
    ))
  )

fig4 <- ggplot(vol_bar_data_fixed,
               aes(x = period, y = mean_pct, fill = Portfolio)) +
  geom_col(position = position_dodge(width = 0.65), width = 0.6) +
  geom_hline(yintercept = 0, colour = "black", size = 0.7) +
  geom_text(
    aes(label = paste0(round(mean_pct, 2), "%"),
        vjust = ifelse(mean_pct >= 0, -0.4, 1.2)),
    position = position_dodge(width = 0.65),
    fontface = "bold", size = 4
  ) +
  scale_fill_manual(values = c(
    "Portfolio A — Equal-Weight"   = "#2c3e50",
    "Portfolio B — Signal-Ranked"  = "#2980b9",
    "Portfolio C — Signal-Weighted"= "#e67e22"
  )) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 1.8)
  ) +
  labs(
    title = "Mean Monthly Returns by Market Volatility Period — Main Long-Only Portfolios",
    x     = NULL,
    y     = "Mean Monthly Return (%)",
    fill  = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 10),
    plot.title         = element_text(face = "bold", size = 12),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("fig4_vol_period_longonly.png", fig4,
       width = 9, height = 5, dpi = 150)
# ── FIGURE 4.5: Long-Short Diagnostic — Spread by Volatility Period ───────────
ls_vol_bar <- vol_period_ls %>%
  mutate(
    mean_pct = Mean_Return * 100,
    Portfolio = recode(Portfolio,
                       "Long-Short B — Ranked spread"   = "Long-Short B (Ranked)",
                       "Long-Short C — Weighted spread"  = "Long-Short C (Weighted)"
    )
  )

fig5 <- ggplot(ls_vol_bar,
               aes(x = period, y = mean_pct, fill = Portfolio)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.55) +
  geom_hline(yintercept = 0, colour = "black", size = 0.7) +
  geom_text(
    aes(label = paste0(round(mean_pct, 2), "%"),
        vjust = ifelse(mean_pct >= 0, -0.4, 1.2)),
    position = position_dodge(width = 0.6),
    fontface = "bold", size = 4
  ) +
  scale_fill_manual(values = c(
    "Long-Short B (Ranked)"   = "#2980b9",
    "Long-Short C (Weighted)" = "#e67e22"
  )) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Long-Short Spread by Market Volatility Period (Diagnostic)",
    x     = NULL,
    y     = "Mean Monthly Long-Short Return (%)",
    fill  = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold", size = 12),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("fig5_ls_diagnostic_vol.png", fig5,
       width = 8, height = 5, dpi = 150)

# ── FIGURE 4.6: Cumulative Returns — Long-Short Diagnostics ──────────────────
cum_LS_B <- portfolio_LS_B %>%
  arrange(date) %>%
  mutate(cum_LS_B = cumprod(1 + LS_B_spread))

cum_LS_C <- portfolio_LS_C %>%
  arrange(date) %>%
  mutate(cum_LS_C = cumprod(1 + LS_C_spread))

cum_ls_all <- cum_LS_B %>%
  select(date, cum_LS_B) %>%
  left_join(cum_LS_C %>% select(date, cum_LS_C), by = "date") %>%
  pivot_longer(
    cols      = c(cum_LS_B, cum_LS_C),
    names_to  = "portfolio",
    values_to = "cumulative_return"
  ) %>%
  mutate(
    portfolio = recode(portfolio,
                       "cum_LS_B" = "Long-Short B (Raw Signal, Equal-Weight)",
                       "cum_LS_C" = "Long-Short C (Normalised Z-Score, Magnitude-Weighted)"
    )
  )

fig6 <- ggplot(cum_ls_all,
               aes(x = date, y = cumulative_return,
                   colour = portfolio, linetype = portfolio)) +
  geom_line(size = 0.9) +
  geom_hline(yintercept = 1, linetype = "dotted",
             colour = "grey50", size = 0.5) +
  scale_colour_manual(values = c(
    "Long-Short B (Raw Signal, Equal-Weight)"              = "#2980b9",
    "Long-Short C (Normalised Z-Score, Magnitude-Weighted)"= "#e67e22"
  )) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title    = "Cumulative Returns: Long-Short Diagnostic Portfolios",
    subtitle = "Base = 1.0 — benchmark is zero (a flat line at 1.0 means no gain or loss)",
    x        = NULL,
    y        = "Cumulative Growth",
    colour   = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

ggsave("fig6_cumulative_ls_diagnostic.png", fig6,
       width = 10, height = 6, dpi = 150)

# ── FIGURE 4.7: Cumulative Returns — Top 4 Robustness Check ──────────────────
cum_B4 <- portfolio_B_top4 %>%
  arrange(date) %>%
  mutate(cum_B4 = cumprod(1 + B4_return))

cum_C4 <- portfolio_C_top4 %>%
  arrange(date) %>%
  mutate(cum_C4 = cumprod(1 + C4_return))

cum_top4_all <- cum_A %>%
  select(date, cum_A) %>%
  left_join(cum_B  %>% select(date, cum_B),  by = "date") %>%
  left_join(cum_C  %>% select(date, cum_C),  by = "date") %>%
  left_join(cum_B4 %>% select(date, cum_B4), by = "date") %>%
  left_join(cum_C4 %>% select(date, cum_C4), by = "date") %>%
  pivot_longer(
    cols      = c(cum_A, cum_B, cum_C, cum_B4, cum_C4),
    names_to  = "portfolio",
    values_to = "cumulative_return"
  ) %>%
  mutate(
    portfolio = recode(portfolio,
                       "cum_A"  = "Portfolio A (Equal-Weight, all 12)",
                       "cum_B"  = "Portfolio B (Ranked top 6)",
                       "cum_C"  = "Portfolio C (Weighted top 6)",
                       "cum_B4" = "Robustness B (Ranked top 4)",
                       "cum_C4" = "Robustness C (Weighted top 4)"
    )
  )

fig7 <- ggplot(cum_top4_all,
               aes(x = date, y = cumulative_return,
                   colour = portfolio, linetype = portfolio)) +
  geom_line(size = 0.85) +
  geom_hline(yintercept = 1, linetype = "dotted",
             colour = "grey50", size = 0.5) +
  scale_colour_manual(values = c(
    "Portfolio A (Equal-Weight, all 12)" = "#2c3e50",
    "Portfolio B (Ranked top 6)"         = "#2980b9",
    "Portfolio C (Weighted top 6)"       = "#e67e22",
    "Robustness B (Ranked top 4)"        = "#27ae60",
    "Robustness C (Weighted top 4)"      = "#8e44ad"
  )) +
  scale_linetype_manual(values = c("solid","solid","solid","dashed","dashed")) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    title    = "Cumulative Returns: Main Portfolios vs Top 4 Robustness Check",
    subtitle = "Base = 1.0 at January 2017",
    x        = NULL,
    y        = "Cumulative Growth",
    colour   = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

ggsave("fig7_cumulative_top4_robustness.png", fig7,
       width = 10, height = 6, dpi = 150)

# ── FIGURE 4.8: Annual Returns — Top 4 Robustness Check ──────────────────────
annual_top4_long <- annual_returns %>%
  pivot_longer(
    cols      = c(A_annual, B_annual, C_annual, B4_annual, C4_annual),
    names_to  = "portfolio",
    values_to = "annual_return"
  ) %>%
  mutate(
    portfolio = recode(portfolio,
                       "A_annual"  = "Portfolio A",
                       "B_annual"  = "Portfolio B (top 6)",
                       "C_annual"  = "Portfolio C (top 6)",
                       "B4_annual" = "Robustness B (top 4)",
                       "C4_annual" = "Robustness C (top 4)"
    ),
    annual_pct = annual_return * 100
  )

fig8 <- ggplot(annual_top4_long,
               aes(x = factor(year), y = annual_pct, fill = portfolio)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_hline(yintercept = 0, colour = "black", size = 0.6) +
  scale_fill_manual(values = c(
    "Portfolio A"          = "#2c3e50",
    "Portfolio B (top 6)"  = "#2980b9",
    "Portfolio C (top 6)"  = "#e67e22",
    "Robustness B (top 4)" = "#27ae60",
    "Robustness C (top 4)" = "#8e44ad"
  )) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Annual Returns: Main Portfolios vs Top 4 Robustness Check (2017-2026)",
    x     = NULL,
    y     = "Annual Return (%)",
    fill  = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold", size = 12),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("fig8_annual_top4_robustness.png", fig8,
       width = 13, height = 6, dpi = 150)

# ── FIGURE 4.9: Firm Frequency in Top-Six Selection: Portfolio B ──────────────

fig4_9 <- ggplot(top6_frequency,
                 aes(x = reorder(firm, pct_months_top6),
                     y = pct_months_top6 * 100,
                     fill = pct_months_top6 >= 0.5)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(pct_months_top6 * 100, 1), "%")),
            hjust = -0.1, size = 3.5, fontface = "bold") +
  scale_fill_manual(
    values = c("TRUE" = "#2980b9", "FALSE" = "#95a5a6"),
    labels = c("TRUE" = "In top 6 ≥50% of months",
               "FALSE" = "In top 6 <50% of months")
  ) +
  scale_y_continuous(limits = c(0, 90),
                     labels = function(x) paste0(x, "%")) +
  coord_flip() +
  labs(
    title    = "Firm Frequency in Portfolio B Top-Six Selection",
    subtitle = paste0("January 2017 – February 2026, n = ",
                      n_distinct(portfolio_B_firm_level$date), " months"),
    x        = NULL,
    y        = "Percentage of Months in Top 6 (%)",
    fill     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    plot.title       = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("fig4_9_firm_frequency.png", fig4_9,
       width = 9, height = 6, dpi = 150)


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 13: EXPORT ALL TABLES TO CSV
# ══════════════════════════════════════════════════════════════════════════════

write.csv(full_period_main,    "table4_1_fullperiod_longonly.csv",   row.names = FALSE)
write.csv(full_period_ls,      "table4_1b_fullperiod_ls_diag.csv",   row.names = FALSE)
write.csv(annual_returns,      "table4_2_annual_returns.csv",         row.names = FALSE)
write.csv(vol_period_results,  "table4_3_vol_period_longonly.csv",    row.names = FALSE)
write.csv(vol_period_ls,       "table4_3b_vol_period_ls_diag.csv",   row.names = FALSE)
write.csv(breakdown_summary,   "table4_4_top6_bottom6_breakdown.csv", row.names = FALSE)
write.csv(top6_frequency,      "portfolio_B_firm_frequency.csv",      row.names = FALSE)

# Return series
write.csv(portfolio_A,         "portfolio_A_returns.csv",             row.names = FALSE)
write.csv(portfolio_B,         "portfolio_B_returns.csv",             row.names = FALSE)
write.csv(portfolio_C,         "portfolio_C_returns.csv",             row.names = FALSE)
write.csv(portfolio_LS_B,      "portfolio_LSB_returns.csv",           row.names = FALSE)
write.csv(portfolio_LS_C,      "portfolio_LSC_returns.csv",           row.names = FALSE)
write.csv(portfolio_B_top4, "portfolio_B_top4_returns.csv", row.names = FALSE)
write.csv(portfolio_C_top4, "portfolio_C_top4_returns.csv", row.names = FALSE)
write.csv(cum_all, "cumulative_return")
getwd()
# ══════════════════════════════════════════════════════════════════════════════
# SECTION 14: FINAL CONSOLE SUMMARY
# ══════════════════════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════════════════════\n")
cat("ALL OUTPUTS SAVED\n")
cat("══════════════════════════════════════════════════════\n")
cat("MAIN TABLES (Long-Only):\n")
cat("  table4_1_fullperiod_longonly.csv\n")
cat("  table4_2_annual_returns.csv\n")
cat("  table4_3_vol_period_longonly.csv\n")
cat("  table4_4_top6_bottom6_breakdown.csv\n")
cat("DIAGNOSTIC TABLES (Long-Short):\n")
cat("  table4_1b_fullperiod_ls_diag.csv\n")
cat("  table4_3b_vol_period_ls_diag.csv\n")
cat("FIRM-LEVEL:\n")
cat("  portfolio_B_firm_frequency.csv\n")
cat("FIGURES:\n")
cat("  fig1_cumulative_longonly.png\n")
cat("  fig2_annual_returns_longonly.png\n")
cat("  fig3_rolling_sharpe_longonly.png\n")
cat("  fig4_vol_period_longonly.png\n")
cat("  fig5_ls_diagnostic_vol.png\n")
cat("  fig6_cumulative_ls_diagnostic.png\n")
cat("  fig7_cumulative_top4_robustness.png\n")
cat("  fig8_annual_top4_robustness.png\n")
cat("══════════════════════════════════════════════════════\n")
