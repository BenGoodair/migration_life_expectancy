# Additional analyses requested by reviewers (Lancet Public Health and SSM rounds)
#
# This script is additive: it does not change create_life_tables.R. It expects
# the individual-level data frame `df` built there (columns: sex, dobyr,
# deyrbde, died, evyreem, evyrbir, cob_3cats, hiscen71 ... hiscen11,
# census_dereg). Inside the secure environment, run create_life_tables.R up to
# the point where `census_dereg` is created, then source this file.
# Outside it, run create_life_tables.R on the synthetic data first.
#
# What it produces (all written to `out_dir`):
#   1. paired_bootstrap_<period>.csv   e10 by group, plus gaps and contributions,
#                                      with percentile CIs taken from the SAME
#                                      bootstrap replicate for every group
#   2. pooled life expectancy          periods are pooled (deaths and exposure
#                                      summed, then one life table), not averages
#                                      of annual life expectancies
#   3. emigration_timing_sensitivity.csv  census-inferred emigration assigned to
#                                      the earliest / midpoint / latest year of
#                                      the intercensal window instead of at random
#   4. salmon_bias_bounds.csv          how much higher migrant mortality would
#                                      have to be to remove the e10 advantage
#   5. cell_counts_by_decade_ageband.csv  deaths and exposure, with a <10 flag
#   6. age_standardised_mortality.csv  directly standardised rates, ages 10+
#   7. exposure_definition_check.csv   e10 under the two exposure definitions
#
# Groups: All, UK (E&W + Scotland/NI born), EW (England & Wales born),
#         rUK (Scotland & NI born), NonUK (born outside the UK).
# "Contribution" is All minus a reference group. Using EW as the reference
# answers the reviewers' point about Scotland/NI-born being in the comparator.

library(dplyr)
library(tidyr)

if (!exists("df")) stop("Create `df` with create_life_tables.R before sourcing this script.")

if (!exists("n_bootstrap_paired")) n_bootstrap_paired <- 500
out_dir <- "Data/sensitivity"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

study_years <- 1971:2017
max_age <- 105
never <- 9999L

# ---- Period definitions ------------------------------------------------------

period_annual <- function(year) as.character(year)

# Non-overlapping three-year periods, 1972-74 ... 2008-10
period_triad <- function(year) {
  start <- 1972 + 3 * ((year - 1972) %/% 3)
  ifelse(year >= 1972 & year <= 2010, paste0(start, "-", start + 2), NA_character_)
}

# Decades as used in Table 1
period_decade <- function(year) {
  start <- 1971 + 10 * ((year - 1971) %/% 10)
  ifelse(year >= 1971 & year <= 2010, paste0(start, "-", start + 9), NA_character_)
}

# ---- Emigration timing for people absent at the next census -------------------

# rule = "random" reproduces the main analysis; the others fix the year within
# the intercensal window. "earliest" removes the most migrant exposure and so
# gives the highest migrant mortality; "latest" gives the lowest.
assign_census_dereg <- function(df, rule = c("random", "earliest", "midpoint", "latest")) {
  rule <- match.arg(rule)
  pick <- function(lo, hi, n) {
    switch(rule,
           random   = sample(lo:hi, n, replace = TRUE),
           earliest = rep(lo, n),
           midpoint = rep((lo + hi) %/% 2, n),
           latest   = rep(hi, n))
  }
  out <- rep(NA_integer_, nrow(df))
  # Assigned latest window first so that earlier windows take priority,
  # matching the nested ifelse in create_life_tables.R
  w <- which(df$hiscen01 != "Not present at 2001" & df$hiscen11 == "Not present at 2011 Census")
  out[w] <- pick(2001, 2010, length(w))
  w <- which(df$hiscen91 != "Not present at 1991" & df$hiscen01 == "Not present at 2001")
  out[w] <- pick(1991, 2000, length(w))
  w <- which(df$hiscen81 == "Present at 1981" & df$hiscen91 == "Not present at 1991")
  out[w] <- pick(1981, 1990, length(w))
  w <- which(df$hiscen71 == "Present at 1971" & df$hiscen81 == "Not present at 1981")
  out[w] <- pick(1971, 1980, length(w))
  out[!(is.na(df$evyreem) & is.na(df$deyrbde))] <- NA_integer_
  out
}

# ---- Collapse individuals to distinct life-history patterns -------------------

# Individuals with the same sex, country of birth, birth year, entry year, exit
# year and death year contribute identically to every life table, so they are
# collapsed to one row with a count. Resampling individuals with replacement is
# then a multinomial draw on these counts, which makes a paired bootstrap of all
# groups cheap enough to run 500+ times.
collapse_patterns <- function(df) {
  df %>%
    filter(!is.na(cob_3cats)) %>%
    mutate(
      start = pmax(dobyr, coalesce(as.integer(evyrbir), -never)),
      death_exit = case_when(is.na(died) ~ never,
                             is.na(deyrbde) ~ -never,
                             TRUE ~ as.integer(deyrbde)),
      end = pmin(death_exit,
                 coalesce(as.integer(evyreem), never),
                 coalesce(as.integer(census_dereg), never)),
      death_year = coalesce(as.integer(deyrbde), never)
    ) %>%
    count(sex, cob_3cats, dobyr, start, end, death_year, name = "n")
}

# Alive at the start of year t (including those who die in t) and deaths in t,
# by single year of age, sex and country of birth. Same definitions as
# calc_life_table() in create_life_tables.R.
counts_from_patterns <- function(patterns, weight = patterns$n) {
  patterns$w <- weight
  patterns <- patterns[patterns$w > 0, ]
  bind_rows(lapply(study_years, function(y) {
    patterns %>%
      filter(dobyr <= y, y - dobyr <= max_age) %>%
      mutate(age = y - dobyr) %>%
      group_by(age, sex, cob_3cats) %>%
      summarise(N = sum(w * (y >= start & y <= end)),
                D = sum(w * (death_year == y)),
                .groups = "drop") %>%
      mutate(year = y)
  }))
}

add_groups <- function(counts) {
  bind_rows(
    counts %>% mutate(group = "All"),
    counts %>% filter(cob_3cats %in% c(1, 2)) %>% mutate(group = "UK"),
    counts %>% filter(cob_3cats == 1) %>% mutate(group = "EW"),
    counts %>% filter(cob_3cats == 2) %>% mutate(group = "rUK"),
    counts %>% filter(cob_3cats == 3) %>% mutate(group = "NonUK")
  ) %>%
    group_by(group, sex, year, age) %>%
    summarise(N = sum(N), D = sum(D), .groups = "drop")
}

pool_periods <- function(counts, period_fun) {
  counts %>%
    mutate(period = period_fun(year)) %>%
    filter(!is.na(period)) %>%
    group_by(group, sex, period, age) %>%
    summarise(N = sum(N), D = sum(D), .groups = "drop")
}

# ---- Life table --------------------------------------------------------------

# exposure = "original": mx = D / (N + D/2), as in create_life_tables.R
# exposure = "midyear":  mx = D / (N - D/2), i.e. people dying in the year
#                        contribute half a year. N already includes them, so
#                        this is the conventional person-years approximation.
# mx_multiplier scales every death rate (used for the salmon-bias bounds).
life_table <- function(counts, exposure = c("original", "midyear"), mx_multiplier = 1) {
  exposure <- match.arg(exposure)
  counts %>%
    arrange(group, sex, period, age) %>%
    group_by(group, sex, period) %>%
    mutate(
      person_years = if (exposure == "original") N + D / 2 else N - D / 2,
      mx = ifelse(person_years > 0, D / person_years, 0) * mx_multiplier,
      ax = ifelse(age == 0,
                  ifelse(sex == "Male", 0.14929 - 1.99646 * mx, 0.14903 - 2.05527 * mx),
                  0.5),
      qx = pmin(mx / (1 + (1 - ax) * mx), 1),
      lx = 100000 * cumprod(c(1, (1 - qx)[-n()])),
      dx = qx * lx,
      Lx = lx - 0.5 * dx,
      tx = rev(cumsum(rev(Lx))),
      ex = tx / lx
    ) %>%
    ungroup()
}

e_at <- function(lt, at_age = 10) {
  lt %>% filter(age == at_age) %>% select(group, sex, period, ex)
}

# Gaps and contributions computed within one sample (or one bootstrap replicate)
contrasts <- function(e) {
  e %>%
    pivot_wider(names_from = group, values_from = ex) %>%
    mutate(gap_NonUK_vs_UK = NonUK - UK,
           gap_NonUK_vs_EW = NonUK - EW,
           contribution_ref_UK = All - UK,
           contribution_ref_EW = All - EW) %>%
    pivot_longer(-c(sex, period), names_to = "measure", values_to = "value")
}

# ---- Paired bootstrap --------------------------------------------------------

paired_bootstrap <- function(patterns, period_fun, B = n_bootstrap_paired,
                             exposure = "original", at_age = 10, seed = 123) {
  estimate <- function(weight) {
    counts_from_patterns(patterns, weight) %>%
      add_groups() %>%
      pool_periods(period_fun) %>%
      life_table(exposure) %>%
      e_at(at_age) %>%
      contrasts()
  }
  point <- estimate(patterns$n) %>% rename(estimate = value)
  set.seed(seed)
  total <- sum(patterns$n)
  reps <- bind_rows(lapply(seq_len(B), function(b) {
    estimate(as.vector(rmultinom(1, total, patterns$n))) %>% mutate(rep = b)
  }))
  reps %>%
    group_by(sex, period, measure) %>%
    summarise(lower_ci = quantile(value, 0.025, na.rm = TRUE),
              upper_ci = quantile(value, 0.975, na.rm = TRUE),
              bootstrap_mean = mean(value, na.rm = TRUE),
              share_reps_above_zero = mean(value > 0, na.rm = TRUE),
              .groups = "drop") %>%
    left_join(point, by = c("sex", "period", "measure")) %>%
    select(sex, period, measure, estimate, lower_ci, upper_ci,
           bootstrap_mean, share_reps_above_zero)
}

# ---- Salmon-bias bounds ------------------------------------------------------

# Smallest multiplier k on all migrant death rates at which migrant e10 falls to
# the reference group's e10. If exposure is right and only deaths are missed,
# the implied share of migrant deaths that would have to be unrecorded is 1 - 1/k.
salmon_bias_bounds <- function(pooled, exposure = "original", at_age = 10) {
  ref_e <- life_table(pooled, exposure) %>% e_at(at_age)
  migrants <- pooled %>% filter(group == "NonUK")
  cells <- distinct(migrants, sex, period)
  bind_rows(lapply(seq_len(nrow(cells)), function(i) {
    cell <- semi_join(migrants, cells[i, ], by = c("sex", "period"))
    migrant_e <- function(k) e_at(life_table(cell, exposure, mx_multiplier = k), at_age)$ex
    bind_rows(lapply(c("UK", "EW"), function(ref) {
      target <- ref_e$ex[ref_e$group == ref & ref_e$sex == cells$sex[i] &
                           ref_e$period == cells$period[i]]
      k <- if (migrant_e(1) <= target) NA_real_ else
        uniroot(function(k) migrant_e(k) - target, c(1, 50))$root
      tibble(sex = cells$sex[i], period = cells$period[i], reference = ref,
             migrant_e10 = migrant_e(1), reference_e10 = target,
             mortality_multiplier_to_null = k,
             implied_share_deaths_unrecorded = 1 - 1 / k)
    }))
  }))
}

# ---- Age-standardised mortality (ages 10+) -----------------------------------

age_band <- function(age) {
  cut(age, c(seq(10, 80, 10), Inf), right = FALSE,
      labels = c(paste0(seq(10, 70, 10), "-", seq(19, 79, 10)), "80+"))
}

age_standardised_rates <- function(pooled) {
  banded <- pooled %>%
    filter(age >= 10) %>%
    mutate(band = age_band(age)) %>%
    group_by(group, sex, period, band) %>%
    summarise(N = sum(N), D = sum(D), .groups = "drop")
  # Standard: the whole-sample age distribution over the full study, by sex
  standard <- banded %>%
    filter(group == "All") %>%
    group_by(sex, band) %>%
    summarise(std = sum(N), .groups = "drop") %>%
    group_by(sex) %>%
    mutate(std = std / sum(std)) %>%
    ungroup()
  banded %>%
    left_join(standard, by = c("sex", "band")) %>%
    group_by(group, sex, period) %>%
    summarise(crude_rate_per_1000 = 1000 * sum(D) / sum(N),
              standardised_rate_per_1000 = 1000 * sum(std * D / N, na.rm = TRUE),
              .groups = "drop")
}

# ==============================================================================
# Run
# ==============================================================================

patterns <- collapse_patterns(df)
counts <- add_groups(counts_from_patterns(patterns))

# 1 and 2. Paired bootstrap for annual, three-year pooled and decade pooled e10
for (p in list(list("annual", period_annual),
               list("triad", period_triad),
               list("decade", period_decade))) {
  write.csv(paired_bootstrap(patterns, p[[2]]),
            file.path(out_dir, paste0("paired_bootstrap_", p[[1]], ".csv")),
            row.names = FALSE)
}

# Pooled life expectancy at every age by decade (for Figure 3: plot ex itself)
write.csv(
  pool_periods(counts, period_decade) %>% life_table() %>%
    filter(age >= 10, age <= 80) %>% select(group, sex, period, age, ex),
  file.path(out_dir, "pooled_decade_ex_by_age.csv"), row.names = FALSE)

# 3. Emigration timing
emigration_sensitivity <- bind_rows(lapply(c("earliest", "midpoint", "latest"), function(rule) {
  alt <- df
  alt$census_dereg <- assign_census_dereg(df, rule)
  counts_from_patterns(collapse_patterns(alt)) %>%
    add_groups() %>%
    pool_periods(period_decade) %>%
    life_table() %>%
    e_at() %>%
    contrasts() %>%
    mutate(emigration_year_rule = rule)
}))
main_rule <- pool_periods(counts, period_decade) %>% life_table() %>% e_at() %>%
  contrasts() %>% mutate(emigration_year_rule = "random (main analysis)")
write.csv(bind_rows(main_rule, emigration_sensitivity),
          file.path(out_dir, "emigration_timing_sensitivity.csv"), row.names = FALSE)

# 4. Salmon-bias bounds by decade
write.csv(salmon_bias_bounds(pool_periods(counts, period_decade)),
          file.path(out_dir, "salmon_bias_bounds.csv"), row.names = FALSE)

# 5. Cell counts: deaths and exposure by decade and ten-year age band
write.csv(
  pool_periods(counts, period_decade) %>%
    filter(age >= 10) %>%
    mutate(band = age_band(age)) %>%
    group_by(group, sex, period, band) %>%
    summarise(alive_at_start_of_year = sum(N), deaths = sum(D), .groups = "drop") %>%
    mutate(fewer_than_10_deaths = deaths < 10),
  file.path(out_dir, "cell_counts_by_decade_ageband.csv"), row.names = FALSE)

# 6. Age-standardised mortality, three-year periods
write.csv(age_standardised_rates(pool_periods(counts, period_triad)),
          file.path(out_dir, "age_standardised_mortality.csv"), row.names = FALSE)

# 7. Effect of the exposure definition on e10 and on the contrasts
pooled_decade <- pool_periods(counts, period_decade)
write.csv(
  full_join(
    life_table(pooled_decade, "original") %>% e_at() %>% contrasts() %>% rename(original = value),
    life_table(pooled_decade, "midyear") %>% e_at() %>% contrasts() %>% rename(midyear = value),
    by = c("sex", "period", "measure")) %>%
    mutate(difference = midyear - original),
  file.path(out_dir, "exposure_definition_check.csv"), row.names = FALSE)
