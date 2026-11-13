library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(caret)
library(FactoMineR)
library(factoextra)

# ==== 1. Load and inspect ====
# Read the OWID energy dataset and inspect its structure
energy <- read.csv("owid_energy.csv", na.strings = c("", "NA"), strip.white = TRUE)
print(dim(energy))
print(names(energy))
str(energy)
print(summary(energy))

# ==== 2. Filter countries, years and columns ====
# Keep real countries, years 2000 onwards and the 15 study columns
energy_countries <- energy %>% filter(!is.na(iso_code), nchar(iso_code) == 3)
print(dim(energy_countries))

energy_recent <- energy_countries %>% filter(year >= 2000)
print(dim(energy_recent))

energy_sel <- energy_recent %>%
  select(country, iso_code, year, population, gdp,
         total_energy_per_capita_kwh, electricity_generation_twh,
         coal_electricity_share_pct, gas_electricity_share_pct,
         oil_electricity_share_pct, nuclear_electricity_share_pct,
         hydro_electricity_share_pct, wind_electricity_share_pct,
         solar_electricity_share_pct, electricity_carbon_intensity_gco2eq_per_kwh)
print(dim(energy_sel))
print(head(energy_sel))

# ==== 3. Missing values ====
# Count NAs, drop sparse columns and remove incomplete rows
na_counts <- colSums(is.na(energy_sel))
na_pct <- round(na_counts / nrow(energy_sel) * 100, 2)
print(data.frame(column = names(na_counts), na_count = na_counts, na_pct = na_pct, row.names = NULL))
cat("Total NAs:", sum(is.na(energy_sel)), "\n")

print(energy_sel %>% filter(is.na(gdp)) %>% count(year))

cols_to_drop <- names(na_pct)[na_pct > 50]
cat("Columns with more than 50% NAs:", length(cols_to_drop), "\n")
print(cols_to_drop)
energy_keep <- energy_sel[, na_pct <= 50]
print(dim(energy_keep))

energy_clean <- na.omit(energy_keep)
print(data.frame(stage = c("Before na.omit", "After na.omit"),
                 rows = c(nrow(energy_keep), nrow(energy_clean)),
                 total_nas = c(sum(is.na(energy_keep)), sum(is.na(energy_clean)))))

print(data.frame(check = c("Rows >= 3000", "Columns >= 7", "gdp column kept", "gdp NAs remaining",
                           "Rows", "Countries", "First year", "Last year"),
                 result = c(paste(nrow(energy_clean) >= 3000), paste(ncol(energy_clean) >= 7),
                            paste("gdp" %in% names(energy_clean)), sum(is.na(energy_clean$gdp)),
                            nrow(energy_clean), nrow(count(energy_clean, country)),
                            min(energy_clean$year), max(energy_clean$year))))

# ==== 4. Outliers and unusual values ====
# Boxplots, IQR rule, impossible shares and log transforms
share_cols <- c("coal_electricity_share_pct", "gas_electricity_share_pct", "oil_electricity_share_pct",
                "nuclear_electricity_share_pct", "hydro_electricity_share_pct",
                "wind_electricity_share_pct", "solar_electricity_share_pct")
share_long <- melt(energy_clean[, share_cols], measure.vars = share_cols)
p1 <- ggplot(share_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  coord_flip() +
  labs(title = "Distribution of electricity generation shares by source (2000-2022)",
       x = "Electricity source", y = "Share of electricity generation (%)") +
  theme_minimal()
print(p1)

num_cols <- names(energy_clean)[sapply(energy_clean, is.numeric)]
num_cols <- num_cols[num_cols != "year"]
iqr_table <- data.frame(t(sapply(num_cols, function(col) {
  x <- energy_clean[[col]]
  q1 <- unname(quantile(x, 0.25))
  q3 <- unname(quantile(x, 0.75))
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  c(Q1 = q1, Q3 = q3, IQR = iqr, lower = lower, upper = upper, outliers = sum(x < lower | x > upper))
})))
print(round(iqr_table, 2))

gen_upper <- iqr_table["electricity_generation_twh", "upper"]
print(energy_clean %>% filter(electricity_generation_twh > gen_upper) %>%
        count(country) %>% arrange(desc(n)) %>% head(10))

energy_upper <- iqr_table["total_energy_per_capita_kwh", "upper"]
print(energy_clean %>% filter(total_energy_per_capita_kwh > energy_upper) %>%
        count(country) %>% arrange(desc(n)) %>% head(10))

impossible <- energy_clean %>%
  filter(coal_electricity_share_pct < 0 | coal_electricity_share_pct > 100 |
           gas_electricity_share_pct < 0 | gas_electricity_share_pct > 100 |
           oil_electricity_share_pct < 0 | oil_electricity_share_pct > 100 |
           nuclear_electricity_share_pct < 0 | nuclear_electricity_share_pct > 100 |
           hydro_electricity_share_pct < 0 | hydro_electricity_share_pct > 100 |
           wind_electricity_share_pct < 0 | wind_electricity_share_pct > 100 |
           solar_electricity_share_pct < 0 | solar_electricity_share_pct > 100)
cat("Rows with impossible shares (< 0 or > 100):", nrow(impossible), "\n")
energy_clean <- energy_clean %>%
  mutate(across(coal_electricity_share_pct:solar_electricity_share_pct,
                ~ ifelse(.x < 0 | .x > 100, NA, .x)))
cat("NAs after impossible-value check:", sum(is.na(energy_clean)), "\n")

energy_clean <- energy_clean %>%
  mutate(log_population = log(population),
         log_gdp = log(gdp),
         log_electricity_generation_twh = log(electricity_generation_twh),
         log_total_energy_per_capita_kwh = log(total_energy_per_capita_kwh))
print(summary(energy_clean[, c("log_population", "log_gdp", "log_electricity_generation_twh",
                               "log_total_energy_per_capita_kwh")]))

h1 <- ggplot(energy_clean, aes(x = population)) + geom_histogram(bins = 40, fill = "steelblue") +
  labs(title = "Population (raw)", x = "Population (people)", y = "Count (country-years)") + theme_minimal()
h2 <- ggplot(energy_clean, aes(x = log_population)) + geom_histogram(bins = 40, fill = "darkgreen") +
  labs(title = "Population (log)", x = "log(population, people)", y = "Count (country-years)") + theme_minimal()
h3 <- ggplot(energy_clean, aes(x = gdp)) + geom_histogram(bins = 40, fill = "steelblue") +
  labs(title = "GDP (raw)", x = "GDP (international $)", y = "Count (country-years)") + theme_minimal()
h4 <- ggplot(energy_clean, aes(x = log_gdp)) + geom_histogram(bins = 40, fill = "darkgreen") +
  labs(title = "GDP (log)", x = "log(GDP, international $)", y = "Count (country-years)") + theme_minimal()
h5 <- ggplot(energy_clean, aes(x = electricity_generation_twh)) + geom_histogram(bins = 40, fill = "steelblue") +
  labs(title = "Electricity generation (raw)", x = "Electricity generation (TWh)", y = "Count (country-years)") + theme_minimal()
h6 <- ggplot(energy_clean, aes(x = log_electricity_generation_twh)) + geom_histogram(bins = 40, fill = "darkgreen") +
  labs(title = "Electricity generation (log)", x = "log(electricity generation, TWh)", y = "Count (country-years)") + theme_minimal()
h7 <- ggplot(energy_clean, aes(x = total_energy_per_capita_kwh)) + geom_histogram(bins = 40, fill = "steelblue") +
  labs(title = "Energy per capita (raw)", x = "Primary energy per capita (kWh)", y = "Count (country-years)") + theme_minimal()
h8 <- ggplot(energy_clean, aes(x = log_total_energy_per_capita_kwh)) + geom_histogram(bins = 40, fill = "darkgreen") +
  labs(title = "Energy per capita (log)", x = "log(primary energy per capita, kWh)", y = "Count (country-years)") + theme_minimal()
grid.arrange(h1, h2, h3, h4, h5, h6, h7, h8, ncol = 2,
             top = "Before and after natural log transformation (2000-2022)")

# ==== 5. Feature engineering ====
# Create per-capita, log and grouped share features and save cleaned data
energy_clean <- energy_clean %>%
  mutate(gdp_per_capita = gdp / population,
         log_gdp_per_capita = log(gdp_per_capita),
         fossil_share_pct = round(coal_electricity_share_pct + gas_electricity_share_pct +
                                    oil_electricity_share_pct, 2),
         low_carbon_share_pct = round(nuclear_electricity_share_pct + hydro_electricity_share_pct +
                                        wind_electricity_share_pct + solar_electricity_share_pct, 2))
print(head(energy_clean))
str(energy_clean)
print(dim(energy_clean))
write.csv(energy_clean, "energy_cleaned.csv", row.names = FALSE)
