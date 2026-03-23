df <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Migration_LE_Project/outputs/feb26/descriptives_tab2.csv")
# Libraries
library(dplyr)
library(tidyr)
library(gt)

# Read data
df2 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Migration_LE_Project/outputs/feb26/descriptives_tab1.csv")

# Prepare df2_tab
df2_tab <- df2 %>%
  mutate(cob_3cats = as.integer(cob_3cats),
         pop = alive_t) %>%
  # Map groups: 1 or 2 -> "UK", 3 -> "Non-UK"
  mutate(origin = if_else(cob_3cats == 3, "Non-UK", "UK"),
         origin = factor(origin, levels = c("UK", "Non-UK"))) %>%
  group_by(year, origin) %>%
  summarise(pop = sum(pop, na.rm = TRUE),
            dead_t = sum(dead_t, na.rm = TRUE),
            .groups = "drop")

# Create periods and group variable
desc2 <- df2_tab %>%
  mutate(
    period = cut(
      as.integer(as.character(year)),
      breaks = seq(1970, 2010, by = 10),
      right = FALSE,
      labels = c("1971-1980", "1981-1990", "1991-2000", "2001-2010")
    ),
    group = origin
  )

# Summarise by period and group
summary_data <- desc2 %>%
  group_by(period, group) %>%
  summarise(
    total_population = sum(pop, na.rm = TRUE),
    total_deaths = sum(dead_t, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(mortality_rate = (total_deaths / total_population) * 1000)

# Create the "All" group by summing UK + Non-UK within each period
summary_all <- summary_data %>%
  group_by(period) %>%
  summarise(
    total_population = sum(total_population, na.rm = TRUE),
    total_deaths = sum(total_deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(mortality_rate = (total_deaths / total_population) * 1000,
         group = "All") %>%
  select(period, group, total_population, total_deaths, mortality_rate)

# Combine original groups with the "All" group
summary_data_full <- bind_rows(summary_data, summary_all) %>%
  # Ensure the order of groups (so columns appear in desired order)
  mutate(group = factor(group, levels = c("UK", "Non-UK", "All"))) %>%
  arrange(group, period)

# Pivot wider so each group has its own set of columns (group on top, period as rows)
wide_tbl <- summary_data_full %>%
  pivot_wider(
    id_cols = period,
    names_from = group,
    values_from = c(total_population, total_deaths, mortality_rate),
    names_glue = "{group}_{.value}"
  ) %>%
  # keep periods in the original factor order
  mutate(period = factor(period, levels = c("1971-1980","1981-1990","1991-2000","2001-2010"))) %>%
  arrange(period)

# Build gt table with country spanners across the top and periods down the left
final_table <- wide_tbl %>%
  gt(rowname_col = "period") %>%
  tab_header(
    title = "Table 1. Population and Mortality Statistics by Country of Birth, 1971-2010",
    subtitle = "Ten-year time periods showing population counts, deaths, and mortality rates"
  ) %>%
  tab_spanner(
    label = "UK",
    columns = vars(`UK_total_population`, `UK_total_deaths`, `UK_mortality_rate`)
  ) %>%
  tab_spanner(
    label = "Non-UK",
    columns = vars(`Non-UK_total_population`, `Non-UK_total_deaths`, `Non-UK_mortality_rate`)
  ) %>%
  tab_spanner(
    label = "All",
    columns = vars(`All_total_population`, `All_total_deaths`, `All_mortality_rate`)
  ) %>%
  cols_label(
    # UK
    `UK_total_population` = "Population",
    `UK_total_deaths` = "Deaths",
    `UK_mortality_rate` = "Mort/1,000",
    # Non-UK
    `Non-UK_total_population` = "Population",
    `Non-UK_total_deaths` = "Deaths",
    `Non-UK_mortality_rate` = "Mort/1,000",
    # All
    `All_total_population` = "Population",
    `All_total_deaths` = "Deaths",
    `All_mortality_rate` = "Mort/1,000"
  ) %>%
  fmt_number(
    columns = matches("_total_population$"),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  fmt_number(
    columns = matches("_total_deaths$"),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  fmt_number(
    columns = matches("_mortality_rate$"),
    decimals = 2
  ) %>%
  cols_align(
    align = "left",
    columns = everything()
  ) %>%
  tab_footnote(
    footnote = "Population at risk represents individuals alive at the beginning of each year.",
    locations = cells_column_spanners(spanners = "UK")
  ) %>%
  tab_footnote(
    footnote = "Mortality rate calculated as deaths per 1,000 population at risk.",
    locations = cells_column_spanners(spanners = "All")
  ) %>%
  tab_source_note(
    source_note = "Source: ONS Longitudinal Study, 1971-2010."
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.font.size = 10,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12
  )

# Print to viewer
final_table

# Save as HTML
gtsave(final_table, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Migration_LE_project/outputs/descriptive_table_fin.html")


# --- Required packages ----
required <- c("tidyverse", "matrixStats", "patchwork", "scales")
to_install <- required[!(required %in% installed.packages()[,"Package"])]
if(length(to_install)) install.packages(to_install)
library(tidyverse)
library(matrixStats)
library(patchwork)
library(scales)

# --- Prepare data ----------------------------------------------------------
# Make sure cob_3cats is numeric/integer
df  <- df %>% mutate(cob_3cats = as.integer(cob_3cats))
df2 <- df2 %>% mutate(cob_3cats = as.integer(cob_3cats))

# Create population size variable we will use for counts/weights.
# Here we assume "sample size" = alive_t + dead_t (change if you prefer alive_t only).
df2 <- df2 %>% mutate(pop = alive_t)

# Combine df and df2 for convenience by year & cob_3cats
combined <- df2 %>%
  left_join(df %>% select(year, cob_3cats, median_age), by = c("year","cob_3cats"))

# Map groups: 1 or 2 -> "UK", 3 -> "Non-UK"
combined <- combined %>%
  mutate(origin = if_else(cob_3cats == 3, "Non-UK", "UK")) %>%
  mutate(origin = factor(origin, levels = c("UK","Non-UK")))

# --- Panel (a): number of people born in UK vs outside UK (stacked area) ---
counts_by_year <- combined %>%
  group_by(year, origin) %>%
  summarise(n = sum(pop, na.rm = TRUE), .groups = "drop")

p_a <- ggplot(counts_by_year, aes(x = year, y = n, fill = origin)) +
  geom_area(position = "stack", alpha = 0.95) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("UK" = "#0a3d62", "Non-UK" = "#6c757d"), name = "") +
  labs(x = "Year", y = "Number of people (n)", title = "a) Number born in UK vs outside UK") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0),
        panel.grid.minor = element_blank())+
  coord_cartesian(xlim = c(1971,2011))+
  xlim(1972,2011)

# --- Panel (b): % non-UK (line with points) --------------------------------
pct_nonuk <- counts_by_year %>%
  group_by(year) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  filter(origin == "Non-UK") %>%
  mutate(pct = 100 * n / total)

p_b <- ggplot(pct_nonuk, aes(x = year, y = pct)) +
  geom_line(size = 0.9) +
  geom_point(size = 1.8) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, max(pct_nonuk$pct)*1.15)) +
  labs(x = "Year", y = "% Non-UK", title = "b) Percentage born outside the UK") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        panel.grid.minor = element_blank())+
  coord_cartesian(xlim = c(1971,2011))+
  xlim(1972,2011)

# --- Panel (c): median age of sample by UK vs Non-UK ------------------------
# For UK we want a combined median across cob_3cats 1 and 2.
# We'll compute a weighted median of subgroup medians using pop as weights.
# Weighted median function wrapper:
wmedian <- function(x, w) {
  # matrixStats::weightedMedian is robust; if all weights zero, return NA
  if(sum(w, na.rm = TRUE) == 0) return(NA_real_)
  matrixStats::weightedMedian(x, w = w, na.rm = TRUE)
}

years <- sort(unique(combined$year))

median_by_year <- map_dfr(years, function(yr) {
  tmp <- combined %>% filter(year == yr)
  # UK (combine 1+2)
  uk_rows <- tmp %>% filter(origin == "UK")
  uk_med  <- if(nrow(uk_rows) == 0) NA_real_ else wmedian(uk_rows$median_age, uk_rows$pop)
  uk_pop  <- sum(uk_rows$pop, na.rm = TRUE)
  # Non-UK
  nonuk_rows <- tmp %>% filter(origin == "Non-UK")
  nonuk_med  <- if(nrow(nonuk_rows) == 0) NA_real_ else wmedian(nonuk_rows$median_age, nonuk_rows$pop)
  nonuk_pop  <- sum(nonuk_rows$pop, na.rm = TRUE)
  tibble(year = yr,
         origin = factor(c("UK","Non-UK"), levels = c("UK","Non-UK")),
         median_age = c(uk_med, nonuk_med),
         pop = c(uk_pop, nonuk_pop))
})

p_c <- ggplot(median_by_year, aes(x = year, y = median_age, color = origin)) +
  geom_line(size = 0.9) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c("UK" = "#0a3d62", "Non-UK" = "#6c757d"), name = "") +
  scale_y_continuous(breaks = seq(floor(min(median_by_year$median_age, na.rm = TRUE)),
                                  ceiling(max(median_by_year$median_age, na.rm = TRUE)), by = 1)) +
  labs(x = "Year", y = "Median age (years)", title = "c) Median age by origin") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0),
        panel.grid.minor = element_blank())+
  coord_cartesian(xlim = c(1971,2011))+
  xlim(1972,2011)

# --- Panel (d): mortality rate of the two populations ----------------------
# Mortality rate per 1,000 persons = dead_t / (alive_t + dead_t) * 1000
mort_by_year <- combined %>%
  group_by(year, origin) %>%
  summarise(deaths = sum(dead_t, na.rm = TRUE),
            pop    = sum(pop, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(mort_per_1000 = 1000 * deaths / if_else(pop > 0, pop, NA_real_))

p_d <- ggplot(mort_by_year, aes(x = year, y = mort_per_1000, color = origin)) +
  geom_line(size = 0.9) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c("UK" = "#0a3d62", "Non-UK" = "#6c757d"), name = "") +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(x = "Year", y = "Deaths per 1,000", title = "d) Mortality rate (per 1,000)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0),
        panel.grid.minor = element_blank())+
  coord_cartesian(xlim = c(1971,2011))+
  xlim(1972,2011)

# --- Arrange panels into a single figure (2x2) -----------------------------
top_row    <- p_a + p_b + plot_layout(widths = c(1,1))
bottom_row <- p_c + p_d + plot_layout(widths = c(1,1))
fig <- (p_a + p_b) / (p_c + p_d) + plot_annotation(
  theme = theme(plot.title = element_text(face = "bold"))
)

# Slightly increase spacing and add a classical serif font if desired

# Print to the device
print(fig)

# --- Save high-resolution files for Lancet submission ----------------------
# PDF (vector) and TIFF (300 dpi) recommended. Adjust dimensions as needed.
out_pdf  <- "~/Desktop/figure1_lancet_style.pdf"
out_tiff <- "~/Desktop/figure1_lancet_style.tiff"

ggsave(out_pdf, fig, width = 18, height = 18, units = "cm", device = cairo_pdf)
ggsave(out_tiff, fig, width = 18, height = 18, units = "cm", dpi = 600, compression = "lzw")

message("Saved: ", out_pdf, "\nSaved: ", out_tiff)



#––– PACKAGES –––
library(dplyr)
library(tidyr)
library(flextable)
library(officer)

#––– 1. LOAD DATA –––
our_results_CI <- read.csv(
  "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Migration_LE_project/outputs/feb26/tab1.csv",
  stringsAsFactors = FALSE
)

#––– 2. FILTER FOR BENCHMARK YEARS & GROUPS –––
benchmark_years <- c(1975, 1985, 1995,  2005, 2010)

t_bench <- our_results_CI %>%
  filter(
    year %in% benchmark_years,
    Country.of.birth %in% c("All (removing missing)", "UK", "Outside UK")
  ) %>%
  # make a clean short label for pivoting
  mutate(
    birth = case_when(
      Country.of.birth == "All (removing missing)"  ~ "All",
      Country.of.birth == "UK"           ~ "UK",
      Country.of.birth == "Outside UK"   ~ "Outside_UK"
    )
  ) %>%
  select(year, sex, birth, lower_ci, mean_ex, upper_ci) %>%
  pivot_wider(
    names_from  = birth,
    values_from = c(lower_ci, mean_ex, upper_ci),
    names_glue  = "{.value}_{birth}"
  )

#––– 3. CALCULATE GAPS + CI AND FORMAT ALL ESTIMATES –––
t_bench <- t_bench %>%
  mutate(
    # Raw LE estimates with CI
    `LE (All) [95% CI]` = sprintf("%.2f (%.2f–%.2f)", mean_ex_All, lower_ci_All, upper_ci_All),
    `LE (UK-born) [95% CI]` = sprintf("%.2f (%.2f–%.2f)", mean_ex_UK, lower_ci_UK, upper_ci_UK),
    `LE (Overseas-born) [95% CI]` = sprintf("%.2f (%.2f–%.2f)", mean_ex_Outside_UK, lower_ci_Outside_UK, upper_ci_Outside_UK)
  ) %>%
  select(
    Year = year,
    Sex  = sex,
    `LE (All) [95% CI]`,
    `LE (UK-born) [95% CI]`,
    `LE (Overseas-born) [95% CI]`
  ) %>%
  arrange(Year, Sex)

#––– 4. BUILD & STYLE FLEXTABLE –––
ft <- flextable(t_bench) %>%
  theme_zebra() %>%
  set_header_labels(
    Year                   = "Year",
    Sex                    = "Sex",
    `LE (UK-born)`         = "Life expectancy\nUK-born",
    `LE (Overseas-born)`   = "Life expectancy\nOverseas-born"
  ) %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  autofit()

#––– 5a. SAVE AS HTML –––
save_as_html(ft,
             path = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Migration_LE_project/Data/results_table.html"
)

#––– 5b. SAVE AS WORD DOCX –––
doc <- read_docx() %>%
  body_add_par(
    "Table 1. Life expectancy by place of birth, and UK-Overseas gap, selected years",
    style = "heading 2"
  ) %>%
  body_add_par(
    "Benchmark years: 1980, 1990, 2000, 2010. Source: ONS estimates.",
    style = "Normal"
  ) %>%
  body_add_flextable(ft)

print(doc,
      target = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Migration_LE_project/Data/results_table_fin.docx"
)

message("✔ Table with absolute LE and gaps saved to HTML and Word.")




our_results_CI <- read.csv(
  "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Migration_LE_project/outputs/feb26/tab1.csv",
  stringsAsFactors = FALSE
)


# --- Figure 2: Life Expectancy at Age 10 with 95% CI (Lancet style) --------
# Input: our_results_CI (life expectancy data by year, sex, country of birth)
# ---------------------------------------------------------------------------

# --- Required packages ----
required <- c("tidyverse", "patchwork", "scales", "zoo")
to_install <- required[!(required %in% installed.packages()[,"Package"])]
if(length(to_install)) install.packages(to_install)
library(tidyverse)
library(patchwork)
library(scales)
library(zoo)

# --- Load data -------------------------------------------------------------
our_results_CI <- read.csv(
  "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Migration_LE_project/outputs/feb26/tab1.csv",
  stringsAsFactors = FALSE
)

# --- Filter for UK and Outside UK only -------------------------------------
fig2_data <- our_results_CI %>%
  filter(Country.of.birth %in% c("UK", "Outside UK")) %>%
  mutate(
    origin = factor(Country.of.birth, levels = c("UK", "Outside UK")),
    sex = factor(sex, levels = c("Female", "Male"))
  )

# --- Apply 3-year rolling average ------------------------------------------
fig2_smooth <- fig2_data %>%
  arrange(sex, origin, year) %>%
  group_by(sex, origin) %>%
  mutate(
    mean_ex_smooth = zoo::rollmean(mean_ex, k = 3, fill = NA, align = "center"),
    lower_ci_smooth = zoo::rollmean(lower_ci, k = 3, fill = NA, align = "center"),
    upper_ci_smooth = zoo::rollmean(upper_ci, k = 3, fill = NA, align = "center")
  ) %>%
  ungroup() %>%
  filter(!is.na(mean_ex_smooth))  # Remove NA values from rolling average

# --- Create the plot -------------------------------------------------------
p_fig2 <- ggplot(fig2_smooth, aes(x = year, y = mean_ex_smooth, 
                                  color = origin, fill = origin)) +
  # Confidence interval ribbons
  geom_ribbon(aes(ymin = lower_ci_smooth, ymax = upper_ci_smooth), 
              alpha = 0.2, color = NA) +
  # Lines
  geom_line(size = 0.9) +
  # Facet by sex
  facet_wrap(~sex, ncol = 2) +
  # Color scheme matching Figure 1
  scale_color_manual(
    values = c("UK" = "#0a3d62", "Outside UK" = "#6c757d"),
    name = "Country of Birth"
  ) +
  scale_fill_manual(
    values = c("UK" = "#0a3d62", "Outside UK" = "#6c757d"),
    name = "Country of Birth"
  ) +
  # Axes
  scale_x_continuous(breaks = seq(1970, 2010, by = 10)) +
  scale_y_continuous(breaks = seq(60, 80, by = 2)) +
  labs(
    x = "Year",
    y = "Life Expectancy (ex)",
    title = "Life Expectancy at Age 10 with 95% Confidence Intervals"
  ) +
  # Theme matching Figure 1
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey90", color = NA),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines")
  ) +
  coord_cartesian(xlim = c(1972, 2011))+
  xlim(1972,2011)

# Print to device
print(p_fig2)

# --- Save high-resolution files --------------------------------------------
out_pdf  <- "~/Desktop/figure2_lancet_style.pdf"
out_tiff <- "~/Desktop/figure2_lancet_style.tiff"

ggsave(out_pdf, p_fig2, width = 24, height = 12, units = "cm", device = cairo_pdf)
ggsave(out_tiff, p_fig2, width = 24, height = 12, units = "cm", dpi = 600, compression = "lzw")

message("Saved: ", out_pdf, "\nSaved: ", out_tiff)

# --- Optional: Add data source caption -------------------------------------
p_fig2_with_caption <- p_fig2 +
  labs(caption = "Data source = ONS Longitudinal Study")

print(p_fig2_with_caption)

# Save version with caption
ggsave("~/Desktop/figure2_lancet_style_with_caption.pdf", 
       p_fig2_with_caption, width = 24, height = 12, units = "cm", device = cairo_pdf)



our_results_noCI <- read.csv(
  "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Migration_LE_project/outputs/feb26/age_specific_life_tables.csv",
  stringsAsFactors = FALSE
)%>%
  dplyr::filter(Country_of_Birth=="UK"|Country_of_Birth=="Outside UK",
                age>9,
                age<81)

our_results_noCI <- our_results_noCI %>%
  dplyr::mutate(ex = ex+age,
                decade = ifelse(year>1971&year<1982, "1970s",
                                ifelse(year>1981&year<1992, "1980s",
                                       ifelse(year>1981&year<2002, "1990s",
                                              ifelse(year>1981&year<2012, "2000s",NA)))))%>%
  dplyr::group_by(age, decade, Country_of_Birth)%>%
  dplyr::summarise(ex = mean(ex))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(decade))%>%
  dplyr::mutate(decade = factor(decade, levels = c("1970s", "1980s", "1990s", "2000s")))

ggplot(our_results_noCI, aes(x=age, y = ex, colour = Country_of_Birth, fill =Country_of_Birth))+
  geom_line(size = 0.9) +
  # Facet by sex
  facet_wrap(~decade, ncol = 2) +
  # Color scheme matching Figure 1
  scale_color_manual(
    values = c("UK" = "#0a3d62", "Outside UK" = "#6c757d"),
    name = "Country of Birth"
  ) +
  scale_fill_manual(
    values = c("UK" = "#0a3d62", "Outside UK" = "#6c757d"),
    name = "Country of Birth"
  ) +
  # Axes
  labs(
    x = "Age",
    y = "Life Expectancy (ex plus age)",
    title = "Life Expectancy at different ages"
  ) +
  # Theme matching Figure 1
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey90", color = NA),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines")
  ) 


our_results_noCI <- read.csv(
  "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Migration_LE_project/outputs/feb26/age_specific_life_tables.csv",
  stringsAsFactors = FALSE
)%>%
  dplyr::filter(Country_of_Birth=="All (removing missing)"|Country_of_Birth=="UK",
                age==10)%>%
  tidyr::pivot_wider(id_cols = c("year","sex"), names_from = "Country_of_Birth", values_from = "ex")%>%
  dplyr::mutate(diff = `All (removing missing)`-UK)%>%
  arrange(sex, year) %>%
  group_by(sex) %>%
  mutate(
    diff_smoothed = zoo::rollmean(diff, k = 3, fill = NA, align = "center")
  ) %>%
  ungroup() 

ggplot(our_results_noCI, aes(x=year, y=diff_smoothed))+
  geom_area(fill="#0a3d62")+
  facet_wrap(~sex)+
  xlim(1972,2011)+
  # Axes
  labs(title = "Impact of non-UK born residents on life expectancy at age 10",
               x = "Year", y="Change in life expectancy (years)\n(Total life expectancy minus UK-born life expectancy)",
               caption = "")+
  # Theme matching Figure 1
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey90", color = NA),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines")
  ) 


