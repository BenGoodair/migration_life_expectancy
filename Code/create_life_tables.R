setTimeLimit(elapsed = Inf)

options(timeout = 600)  # Increase timeout (default is 60s)
Sys.setenv(R_DEFAULT_TIMEOUT = 600)  #

# Create the data frame
set.seed(42)
n_bootstrap = 100

df <- data.frame(
  sex = sample(c("Female", "Male"), 5000000, replace = TRUE),
  dobyr = sample(1900:2017, 5000000, replace = TRUE),
  cob_3cats = sample(1:3, 5000000, replace = TRUE)
)


# Calculate 'deyrbde' and remove unrealistic data
df <- df %>%
  mutate(
    # Different life expectancy for women (e.g., mean + 5 years)
    deyrbde = ifelse(
      sex == "Female",
      pmin(2017, round(rnorm(n(), mean = dobyr + 75, sd = 10))),  # Females live longer
      pmin(2017, round(rnorm(n(), mean = dobyr + 70, sd = 10)))   # Males standard life expectancy
    ),
    died = ifelse(is.na(deyrbde) | deyrbde > 2017, NA, 1)
  ) %>%
  filter(is.na(died) | deyrbde >= dobyr)  # Remove unrealistic data

# You'll need evyreem and evyrbir columns too - adding placeholders if not present
# (remove these if you already generate them elsewhere)
df <- df %>%
  mutate(
    evyreem = sample(c(NA, 1960:2017), n(), replace = TRUE, prob = c(0.93, rep(0.07/58, 58))),
    evyrbir = sample(c(NA, 1900:2017), n(), replace = TRUE, prob = c(0.90, rep(0.10/118, 118)))
  )

# Generate census presence indicators
df <- df %>%
  mutate(
    # Helper: was the person alive and in the country at a given census year?
    # Present = born before census, not yet dead, not yet emigrated, not yet arrived if foreign-born
    
    hiscen71 = ifelse(
      dobyr <= 1971 &
        (is.na(deyrbde) | deyrbde >= 1971) &
        (is.na(evyreem) | evyreem > 1971) &
        (is.na(evyrbir) | evyrbir <= 1971),
      "Present at 1971", "Not present at 1971"
    ),
    
    hiscen81 = ifelse(
      dobyr <= 1981 &
        (is.na(deyrbde) | deyrbde >= 1981) &
        (is.na(evyreem) | evyreem > 1981) &
        (is.na(evyrbir) | evyrbir <= 1981),
      "Present at 1981", "Not present at 1981"
    ),
    
    hiscen91 = ifelse(
      dobyr <= 1991 &
        (is.na(deyrbde) | deyrbde >= 1991) &
        (is.na(evyreem) | evyreem > 1991) &
        (is.na(evyrbir) | evyrbir <= 1991),
      "Present at 1991", "Not present at 1991"
    ),
    
    hiscen01 = ifelse(
      dobyr <= 2001 &
        (is.na(deyrbde) | deyrbde >= 2001) &
        (is.na(evyreem) | evyreem > 2001) &
        (is.na(evyrbir) | evyrbir <= 2001),
      "Present at 2001", "Not present at 2001"
    ),
    
    hiscen11 = ifelse(
      dobyr <= 2011 &
        (is.na(deyrbde) | deyrbde >= 2011) &
        (is.na(evyreem) | evyreem > 2011) &
        (is.na(evyrbir) | evyrbir <= 2011),
      "Present at 2011 Census", "Not present at 2011 Census"
    )
  )




df <- df %>% dplyr::mutate(census_dereg = ifelse(
  hiscen71=="Present at 1971"&hiscen81=="Not present at 1981", sample(1971:1980, n(), replace=T),
  ifelse(hiscen81=="Present at 1981" & hiscen91=="Not present at 1991", sample(1981:1990, n(), replace=T),
         ifelse(hiscen91!="Not present at 1991"&hiscen01=="Not present at 2001", sample(1991:2000, n(), replace=T),
                ifelse(hiscen01!="Not present at 2001" & hiscen11=="Not present at 2011 Census", sample(2001:2010, n(), replace=T), NA)))),
  census_dereg = ifelse(is.na(evyreem)&is.na(deyrbde), census_dereg, NA))



calc_life_table <- function(df, year) {
  df %>%
    filter(dobyr <= year) %>%
    mutate(year = year,
           age = year - dobyr,
           alive = ifelse((is.na(died)|deyrbde >= year)&
                            (is.na(evyreem)|evyreem>=year)&
                            (is.na(evyrbir)|evyrbir<=year)&
                            (is.na(census_dereg)|census_dereg>=year), 1, 0),
           dead = ifelse(deyrbde == year, 1, 0)) %>%
    dplyr::filter(age<106)%>%
    group_by(age, sex, year) %>%
    summarise(dead_t = sum(dead, na.rm = TRUE),
              alive_t = sum(alive, na.rm = TRUE), .groups = 'drop')
}


# Calculate life tables for all years
all_years <- lapply(1971:2017, calc_life_table, df = df)
all_df <- bind_rows(all_years)



# Calculate mx
all_df <- all_df %>%
  group_by(year, age, sex) %>%
  mutate(mx = dead_t / (alive_t + dead_t / 2)) %>%
  ungroup() %>%
  mutate(mx = replace(mx, is.nan(mx) | is.infinite(mx), 0))

# Fill missing values by carrying forward
all_df <- all_df %>%
  arrange(sex, year, age) %>%
  group_by(sex, year) %>%
  fill(mx, alive_t, dead_t, .direction = "down") %>%
  ungroup()

# Set ax values (default 0.5 except for infants)
all_df <- all_df %>%
  mutate(ax = ifelse(age == 0, 
                     ifelse(sex == "Male", 0.14929 - 1.99646 * mx, 
                            0.14903 - 2.05527 * mx), 0.5))

gc()


# Calculate qx
all_df <- all_df %>%
  mutate(qx = mx / (1 + (1 - ax) * mx)) %>%
  mutate(qx = replace(qx, is.infinite(qx), NA)) %>%
  fill(qx, .direction = "down")

gc()


# Calculate lx and dx
all_df <- all_df %>%
  group_by(sex, year) %>%
  mutate(lx = 100000,
         dx = qx * lx) %>%
  ungroup()

calc_lx_dx <- function(df) {
  df <- df %>% arrange(year, age)
  # Loop over each year
  for (y in unique(df$year)) {
    # Filter data for the specific year
    year_df <- df %>% filter(year == y)
    # Loop over each age within the year, starting from age 1 since age 0 is the initial value
    for (i in 2:nrow(year_df)) {
      # Update lx and dx based on previous age's values
      year_df$lx[i] <- year_df$lx[i - 1] - year_df$dx[i - 1]
      year_df$dx[i] <- year_df$qx[i] * year_df$lx[i]
    }
    # Assign the modified year_df values back to the main dataframe
    df[df$year == y, ] <- year_df
  }
  return(df)
}

gc()


# Apply to male and female separately
female_df <- calc_lx_dx(all_df %>% filter(sex == "Female"))
male_df <- calc_lx_dx(all_df %>% filter(sex == "Male"))
gc()

# Combine male and female data
all_df <- bind_rows(female_df, male_df)
gc()


all_df <- all_df %>%dplyr::mutate(Lx= lx-.5*dx)


# Calculate tx and ex
all_df <- all_df %>%
  group_by(year, sex) %>%
  mutate(tx = rev(cumsum(rev(Lx))),
         ex = tx / lx) %>%
  ungroup()


####bootsrap####


# Function to calculate life expectancy for a given sample of data
calculate_life_expectancy <- function(df) {
  # Create life table as per original code
  all_years <- lapply(1971:2017, calc_life_table, df = df)
  all_df <- bind_rows(all_years)
  
  # Calculate mx
  all_df <- all_df %>%
    group_by(year, age, sex) %>%
    mutate(mx = dead_t / (alive_t + dead_t / 2)) %>%
    ungroup() %>%
    mutate(mx = replace(mx, is.nan(mx) | is.infinite(mx), 0))
  
  # Fill missing values by carrying forward
  all_df <- all_df %>%
    arrange(sex, year, age) %>%
    group_by(sex, year) %>%
    fill(mx, alive_t, dead_t, .direction = "down") %>%
    ungroup()
  
  # Set ax values (default 0.5 except for infants)
  all_df <- all_df %>%
    mutate(ax = ifelse(age == 0, 
                       ifelse(sex == "Male", 0.14929 - 1.99646 * mx, 
                              0.14903 - 2.05527 * mx), 0.5))
  
  # Calculate qx
  all_df <- all_df %>%
    mutate(qx = mx / (1 + (1 - ax) * mx)) %>%
    mutate(qx = replace(qx, is.infinite(qx), NA)) %>%
    fill(qx, .direction = "down")
  
  # Calculate lx and dx
  all_df <- all_df %>%
    group_by(sex, year) %>%
    mutate(lx = 100000,
           dx = qx * lx) %>%
    ungroup()
  
  # Calculate lx and dx for all ages
  female_df <- calc_lx_dx(all_df %>% filter(sex == "Female"))
  male_df <- calc_lx_dx(all_df %>% filter(sex == "Male"))
  
  # Combine male and female data
  all_df <- bind_rows(female_df, male_df)
  
  
  all_df <- all_df %>%dplyr::mutate(Lx= lx-.5*dx)
  
  
  # Calculate tx and ex
  all_df <- all_df %>%
    group_by(year, sex) %>%
    mutate(tx = rev(cumsum(rev(Lx))),
           ex = tx / lx) %>%
    ungroup()
  
  
  
  # Return life expectancy at age 1 for each year
  life_expectancy_at_birth <- all_df %>%
    filter(age == 10) %>%
    dplyr::select(year, sex, ex)
  
  return(life_expectancy_at_birth)
}

gc()


# Initialize list to store results
bootstrap_results <- list()

# # Perform bootstrap sampling
set.seed(123)
for (i in 1:n_bootstrap) {
  # Sample data with replacement
  boot_sample <- df[sample(1:nrow(df), replace = TRUE), ]
  
  # Calculate life expectancy for the bootstrap sample
  boot_life_expectancy <- calculate_life_expectancy(boot_sample)
  
  # Store result
  bootstrap_results[[i]] <- boot_life_expectancy
  print(i)
}

gc()


# Combine results
bootstrap_df <- bind_rows(bootstrap_results, .id = "bootstrap_sample")

# Calculate confidence intervals (e.g., 2.5% and 97.5% quantiles)
confidence_intervals <- bootstrap_df %>%
  group_by(year, sex) %>%
  summarise(
    lower_ci = quantile(ex, probs = 0.025),
    upper_ci = quantile(ex, probs = 0.975),
    mean_ex = mean(ex),
    .groups = 'drop'
  )



####Just England and Wales####

gc()


engwadf <- df %>% dplyr::filter(cob_3cats==1)

# Calculate life tables for all years
engwa_years <- lapply(1971:2017, calc_life_table, df = engwadf)
engwa_df <- bind_rows(engwa_years)

# Calculate mx
engwa_df <- engwa_df %>%
  group_by(year, age, sex) %>%
  mutate(mx = dead_t / (alive_t + dead_t / 2)) %>%
  ungroup() %>%
  mutate(mx = replace(mx, is.nan(mx) | is.infinite(mx), 0))

# Fill missing values by carrying forward
engwa_df <- engwa_df %>%
  arrange(sex, year, age) %>%
  group_by(sex, year) %>%
  fill(mx, alive_t, dead_t, .direction = "down") %>%
  ungroup()
gc()

# Set ax values (default 0.5 except for infants)
engwa_df <- engwa_df %>%
  mutate(ax = ifelse(age == 0, 
                     ifelse(sex == "Male", 0.14929 - 1.99646 * mx, 
                            0.14903 - 2.05527 * mx), 0.5))

# Calculate qx
engwa_df <- engwa_df %>%
  mutate(qx = mx / (1 + (1 - ax) * mx)) %>%
  mutate(qx = replace(qx, is.infinite(qx), NA)) %>%
  fill(qx, .direction = "down")


gc()

# Calculate lx and dx
engwa_df <- engwa_df %>%
  group_by(sex, year) %>%
  mutate(lx = 100000,
         dx = qx * lx) %>%
  ungroup()


# Apply to male and female separately
female_df <- calc_lx_dx(engwa_df %>% filter(sex == "Female"))
male_df <- calc_lx_dx(engwa_df %>% filter(sex == "Male"))

# Combine male and female data
engwa_df <- bind_rows(female_df, male_df)

gc()
engwa_df <- engwa_df %>%dplyr::mutate(Lx= lx-.5*dx)


# Calculate tx and ex
engwa_df <- engwa_df %>%
  group_by(year, sex) %>%
  mutate(tx = rev(cumsum(rev(Lx))),
         ex = tx / lx) %>%
  ungroup()


gc()



# Initialize list to store results
engwa_bootstrap_results <- list()


# # Perform bootstrap sampling
set.seed(123)
for (i in 1:n_bootstrap) {
  # Sample data with replacement
  boot_sample <- engwadf[sample(1:nrow(engwadf), replace = TRUE), ]
  
  # Calculate life expectancy for the bootstrap sample
  boot_life_expectancy <- calculate_life_expectancy(boot_sample)
  
  # Store result
  engwa_bootstrap_results[[i]] <- boot_life_expectancy
  print(i)
}

# Combine results
engwa_bootstrap_df <- bind_rows(engwa_bootstrap_results, .id = "bootstrap_sample")

# Calculate confidence intervals (e.g., 2.5% and 97.5% quantiles)
engwa_confidence_intervals <- engwa_bootstrap_df %>%
  group_by(year, sex) %>%
  summarise(
    lower_ci = quantile(ex, probs = 0.025),
    upper_ci = quantile(ex, probs = 0.975),
    mean_ex = mean(ex),
    .groups = 'drop'
  )


gc()


####rUK####


rukdf <- df %>% dplyr::filter(cob_3cats==2)

# Calculate life tables for all years
ruk_years <- lapply(1971:2017, calc_life_table, df = rukdf)
ruk_df <- bind_rows(ruk_years)

gc()


# Calculate mx
ruk_df <- ruk_df %>%
  group_by(year, age, sex) %>%
  mutate(mx = dead_t / (alive_t + dead_t / 2)) %>%
  ungroup() %>%
  mutate(mx = replace(mx, is.nan(mx) | is.infinite(mx), 0))

# Fill missing values by carrying forward
ruk_df <- ruk_df %>%
  arrange(sex, year, age) %>%
  group_by(sex, year) %>%
  fill(mx, alive_t, dead_t, .direction = "down") %>%
  ungroup()
gc()

# Set ax values (default 0.5 except for infants)
ruk_df <- ruk_df %>%
  mutate(ax = ifelse(age == 0, 
                     ifelse(sex == "Male", 0.14929 - 1.99646 * mx, 
                            0.14903 - 2.05527 * mx), 0.5))

# Calculate qx
ruk_df <- ruk_df %>%
  mutate(qx = mx / (1 + (1 - ax) * mx)) %>%
  mutate(qx = replace(qx, is.infinite(qx), NA)) %>%
  fill(qx, .direction = "down")

gc()


# Calculate lx and dx
ruk_df <- ruk_df %>%
  group_by(sex, year) %>%
  mutate(lx = 100000,
         dx = qx * lx) %>%
  ungroup()

gc()



# Apply to male and female separately
female_df <- calc_lx_dx(ruk_df %>% filter(sex == "Female"))
male_df <- calc_lx_dx(ruk_df %>% filter(sex == "Male"))

# Combine male and female data
ruk_df <- bind_rows(female_df, male_df)

ruk_df <- ruk_df %>%dplyr::mutate(Lx= lx-.5*dx)


# Calculate tx and ex
ruk_df <- ruk_df %>%
  group_by(year, sex) %>%
  mutate(tx = rev(cumsum(rev(Lx))),
         ex = tx / lx) %>%
  ungroup()



gc()


# Initialize list to store results
ruk_bootstrap_results <- list()


# # Perform bootstrap sampling
set.seed(123)
for (i in 1:n_bootstrap) {
  # Sample data with replacement
  boot_sample <- rukdf[sample(1:nrow(rukdf), replace = TRUE), ]
  
  # Calculate life expectancy for the bootstrap sample
  boot_life_expectancy <- calculate_life_expectancy(boot_sample)
  
  # Store result
  ruk_bootstrap_results[[i]] <- boot_life_expectancy
  print(i)
}

# Combine results
ruk_bootstrap_df <- bind_rows(ruk_bootstrap_results, .id = "bootstrap_sample")
gc()

# Calculate confidence intervals (e.g., 2.5% and 97.5% quantiles)
ruk_confidence_intervals <- ruk_bootstrap_df %>%
  group_by(year, sex) %>%
  summarise(
    lower_ci = quantile(ex, probs = 0.025),
    upper_ci = quantile(ex, probs = 0.975),
    mean_ex = mean(ex),
    .groups = 'drop'
  )


gc()

####Outside UK####

nonukdf <- df %>% dplyr::filter(cob_3cats==3)

# Calculate life tables for all years
nonuk_years <- lapply(1971:2017, calc_life_table, df = nonukdf)
nonuk_df <- bind_rows(nonuk_years)

gc()


# Calculate mx
nonuk_df <- nonuk_df %>%
  group_by(year, age, sex) %>%
  mutate(mx = dead_t / (alive_t + dead_t / 2)) %>%
  ungroup() %>%
  mutate(mx = replace(mx, is.nan(mx) | is.infinite(mx), 0))

gc()


# Fill missing values by carrying forward
nonuk_df <- nonuk_df %>%
  arrange(sex, year, age) %>%
  group_by(sex, year) %>%
  fill(mx, alive_t, dead_t, .direction = "down") %>%
  ungroup()

# Set ax values (default 0.5 except for infants)
nonuk_df <- nonuk_df %>%
  mutate(ax = ifelse(age == 0, 
                     ifelse(sex == "Male", 0.14929 - 1.99646 * mx, 
                            0.14903 - 2.05527 * mx), 0.5))

# Calculate qx
nonuk_df <- nonuk_df %>%
  mutate(qx = mx / (1 + (1 - ax) * mx)) %>%
  mutate(qx = replace(qx, is.infinite(qx), NA)) %>%
  fill(qx, .direction = "down")

gc()


# Calculate lx and dx
nonuk_df <- nonuk_df %>%
  group_by(sex, year) %>%
  mutate(lx = 100000,
         dx = qx * lx) %>%
  ungroup()

gc()

# Apply to male and female separately
female_df <- calc_lx_dx(nonuk_df %>% filter(sex == "Female"))
male_df <- calc_lx_dx(nonuk_df %>% filter(sex == "Male"))

gc()


# Combine male and female data
nonuk_df <- bind_rows(female_df, male_df)
gc()

nonuk_df <- nonuk_df %>%dplyr::mutate(Lx= lx-.5*dx)


# Calculate tx and ex
nonuk_df <- nonuk_df %>%
  group_by(year, sex) %>%
  mutate(tx = rev(cumsum(rev(Lx))),
         ex = tx / lx) %>%
  ungroup()



gc()



# Initialize list to store results
nonuk_bootstrap_results <- list()


# # Perform bootstrap sampling
set.seed(123)
for (i in 1:n_bootstrap) {
  # Sample data with replacement
  boot_sample <- nonukdf[sample(1:nrow(nonukdf), replace = TRUE), ]
  
  # Calculate life expectancy for the bootstrap sample
  boot_life_expectancy <- calculate_life_expectancy(boot_sample)
  
  # Store result
  nonuk_bootstrap_results[[i]] <- boot_life_expectancy
  print(i)
}

# Combine results
nonuk_bootstrap_df <- bind_rows(nonuk_bootstrap_results, .id = "bootstrap_sample")

gc()


# Calculate confidence intervals (e.g., 2.5% and 97.5% quantiles)
nonuk_confidence_intervals <- nonuk_bootstrap_df %>%
  group_by(year, sex) %>%
  summarise(
    lower_ci = quantile(ex, probs = 0.025),
    upper_ci = quantile(ex, probs = 0.975),
    mean_ex = mean(ex),
    .groups = 'drop'
  )

gc()


####AllUK####

allukdf <- df %>% dplyr::filter(cob_3cats==1|cob_3cats==2)

gc()


# Calculate life tables for all years
alluk_years <- lapply(1971:2017, calc_life_table, df = allukdf)
alluk_df <- bind_rows(alluk_years)

gc()


# Calculate mx
alluk_df <- alluk_df %>%
  group_by(year, age, sex) %>%
  mutate(mx = dead_t / (alive_t + dead_t / 2)) %>%
  ungroup() %>%
  mutate(mx = replace(mx, is.nan(mx) | is.infinite(mx), 0))
gc()

# Fill missing values by carrying forward
alluk_df <- alluk_df %>%
  arrange(sex, year, age) %>%
  group_by(sex, year) %>%
  fill(mx, alive_t, dead_t, .direction = "down") %>%
  ungroup()
gc()

# Set ax values (default 0.5 except for infants)
alluk_df <- alluk_df %>%
  mutate(ax = ifelse(age == 0, 
                     ifelse(sex == "Male", 0.14929 - 1.99646 * mx, 
                            0.14903 - 2.05527 * mx), 0.5))

# Calculate qx
alluk_df <- alluk_df %>%
  mutate(qx = mx / (1 + (1 - ax) * mx)) %>%
  mutate(qx = replace(qx, is.infinite(qx), NA)) %>%
  fill(qx, .direction = "down")

gc()


# Calculate lx and dx
alluk_df <- alluk_df %>%
  group_by(sex, year) %>%
  mutate(lx = 100000,
         dx = qx * lx) %>%
  ungroup()


gc()


# Apply to male and female separately
female_df <- calc_lx_dx(alluk_df %>% filter(sex == "Female"))
male_df <- calc_lx_dx(alluk_df %>% filter(sex == "Male"))
gc()

# Combine male and female data
alluk_df <- bind_rows(female_df, male_df)

alluk_df <- alluk_df %>%dplyr::mutate(Lx= lx-.5*dx)


# Calculate tx and ex
alluk_df <- alluk_df %>%
  group_by(year, sex) %>%
  mutate(tx = rev(cumsum(rev(Lx))),
         ex = tx / lx) %>%
  ungroup()

gc()







# Initialize list to store results
alluk_bootstrap_results <- list()


# # Perform bootstrap sampling
set.seed(123)
for (i in 1:n_bootstrap) {
  # Sample data with replacement
  boot_sample <- allukdf[sample(1:nrow(allukdf), replace = TRUE), ]
  
  # Calculate life expectancy for the bootstrap sample
  boot_life_expectancy <- calculate_life_expectancy(boot_sample)
  
  # Store result
  alluk_bootstrap_results[[i]] <- boot_life_expectancy
  print(i)
}

gc()


# Combine results
alluk_bootstrap_df <- bind_rows(alluk_bootstrap_results, .id = "bootstrap_sample")
gc()

# Calculate confidence intervals (e.g., 2.5% and 97.5% quantiles)
alluk_confidence_intervals <- alluk_bootstrap_df %>%
  group_by(year, sex) %>%
  summarise(
    lower_ci = quantile(ex, probs = 0.025),
    upper_ci = quantile(ex, probs = 0.975),
    mean_ex = mean(ex),
    .groups = 'drop'
  )


gc()

####All_removing_NA####

allnonnadf <- df %>% dplyr::filter(!is.na(cob_3cats))

gc()


# Calculate life tables for all years
allnonna_years <- lapply(1971:2017, calc_life_table, df = allnonnadf)
allnonna_df <- bind_rows(allnonna_years)

gc()


# Calculate mx
allnonna_df <- allnonna_df %>%
  group_by(year, age, sex) %>%
  mutate(mx = dead_t / (alive_t + dead_t / 2)) %>%
  ungroup() %>%
  mutate(mx = replace(mx, is.nan(mx) | is.infinite(mx), 0))
gc()

# Fill missing values by carrying forward
allnonna_df <- allnonna_df %>%
  arrange(sex, year, age) %>%
  group_by(sex, year) %>%
  fill(mx, alive_t, dead_t, .direction = "down") %>%
  ungroup()
gc()

# Set ax values (default 0.5 except for infants)
allnonna_df <- allnonna_df %>%
  mutate(ax = ifelse(age == 0, 
                     ifelse(sex == "Male", 0.14929 - 1.99646 * mx, 
                            0.14903 - 2.05527 * mx), 0.5))

# Calculate qx
allnonna_df <- allnonna_df %>%
  mutate(qx = mx / (1 + (1 - ax) * mx)) %>%
  mutate(qx = replace(qx, is.infinite(qx), NA)) %>%
  fill(qx, .direction = "down")

gc()


# Calculate lx and dx
allnonna_df <- allnonna_df %>%
  group_by(sex, year) %>%
  mutate(lx = 100000,
         dx = qx * lx) %>%
  ungroup()


gc()


# Apply to male and female separately
female_df <- calc_lx_dx(allnonna_df %>% filter(sex == "Female"))
male_df <- calc_lx_dx(allnonna_df %>% filter(sex == "Male"))
gc()

# Combine male and female data
allnonna_df <- bind_rows(female_df, male_df)

allnonna_df <- allnonna_df %>%dplyr::mutate(Lx= lx-.5*dx)


# Calculate tx and ex
allnonna_df <- allnonna_df %>%
  group_by(year, sex) %>%
  mutate(tx = rev(cumsum(rev(Lx))),
         ex = tx / lx) %>%
  ungroup()

gc()







# Initialize list to store results
allnonna_bootstrap_results <- list()


# # Perform bootstrap sampling
set.seed(123)
for (i in 1:n_bootstrap) {
  # Sample data with replacement
  boot_sample <- allnonnadf[sample(1:nrow(allnonnadf), replace = TRUE), ]
  
  # Calculate life expectancy for the bootstrap sample
  boot_life_expectancy <- calculate_life_expectancy(boot_sample)
  
  # Store result
  allnonna_bootstrap_results[[i]] <- boot_life_expectancy
  print(i)
}

gc()


# Combine results
allnonna_bootstrap_df <- bind_rows(allnonna_bootstrap_results, .id = "bootstrap_sample")
gc()

# Calculate confidence intervals (e.g., 2.5% and 97.5% quantiles)
allnonna_confidence_intervals <- allnonna_bootstrap_df %>%
  group_by(year, sex) %>%
  summarise(
    lower_ci = quantile(ex, probs = 0.025),
    upper_ci = quantile(ex, probs = 0.975),
    mean_ex = mean(ex),
    .groups = 'drop'
  )


gc()


####Create_master_df####

gc()


all_df_selected <- all_df %>% dplyr::select(year, age, sex, mx, qx, lx, dx, Lx, tx, ex) %>% dplyr::mutate(Country_of_Birth = "All")
engwa_df_selected <- engwa_df %>% dplyr::select(year, age, sex, mx, qx, lx, dx, Lx, tx, ex) %>% dplyr::mutate(Country_of_Birth = "England and Wales")
ruk_df_selected <- ruk_df %>% dplyr::select(year, age, sex, mx, qx, lx, dx, Lx, tx, ex) %>% dplyr::mutate(Country_of_Birth = "Scotland and Northern Ireland")
nonuk_df_selected <- nonuk_df %>% dplyr::select(year, age, sex, mx, qx, lx, dx, Lx, tx, ex) %>% dplyr::mutate(Country_of_Birth = "Outside UK")
alluk_df_selected <- alluk_df %>% dplyr::select(year, age, sex, mx, qx, lx, dx, Lx, tx, ex) %>% dplyr::mutate(Country_of_Birth = "UK")
allnonna_df_selected <- allnonna_df %>% dplyr::select(year, age, sex, mx, qx, lx, dx, Lx, tx, ex) %>% dplyr::mutate(Country_of_Birth = "All (removing missing)")

gc()


all_life_tables <- rbind(all_df_selected ,
                         engwa_df_selected ,
                         ruk_df_selected ,
                         nonuk_df_selected,
                         alluk_df_selected ,
                         allnonna_df_selected)

gc()


all_confidence_intervals <- rbind(confidence_intervals %>%
                                    dplyr::mutate(Country_of_Birth = "All"),
                                  engwa_confidence_intervals %>%
                                    dplyr::mutate(Country_of_Birth = "England and Wales"),
                                  ruk_confidence_intervals %>%
                                    dplyr::mutate(Country_of_Birth = "Scotland and Northern Ireland"),
                                  nonuk_confidence_intervals %>%
                                    dplyr::mutate(Country_of_Birth = "Outside UK"),
                                  alluk_confidence_intervals %>%
                                    dplyr::mutate(Country_of_Birth = "UK"),
                                  allnonna_confidence_intervals %>%
                                    dplyr::mutate(Country_of_Birth = "All (removing missing)"))%>%
  dplyr::mutate(sex = factor(sex, levels = c("Female", "Male")))

gc()

