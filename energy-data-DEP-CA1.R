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

# ==== 6. Variation ====
# Summaries and binned counts of the main variables
print(summary(energy_clean[, c("gdp_per_capita", "total_energy_per_capita_kwh", "electricity_generation_twh",
                               "fossil_share_pct", "low_carbon_share_pct",
                               "electricity_carbon_intensity_gco2eq_per_kwh")]))
print(energy_clean %>% count(fossil_share_band = cut_width(fossil_share_pct, width = 10, boundary = 0)))
print(energy_clean %>% count(gdp_per_capita_band = cut_width(gdp_per_capita, width = 10000, boundary = 0)))

# ==== 7. Covariation ====
# Correlation matrix, heatmap and strongest pairs
cor_vars <- c("log_population", "log_gdp_per_capita", "log_total_energy_per_capita_kwh",
              "log_electricity_generation_twh", share_cols,
              "electricity_carbon_intensity_gco2eq_per_kwh", "fossil_share_pct", "low_carbon_share_pct")
cor_labels <- c("log population", "log GDP pc", "log energy pc", "log generation",
                "coal", "gas", "oil", "nuclear", "hydro", "wind", "solar",
                "carbon intensity", "fossil", "low-carbon")
cor_mat <- cor(energy_clean[, cor_vars])
print(round(cor_mat, 2))

cor_long <- melt(cor_mat)
cor_heat <- cor_long %>%
  mutate(Var1 = factor(Var1, levels = cor_vars, labels = cor_labels),
         Var2 = factor(Var2, levels = cor_vars, labels = cor_labels))
p3 <- ggplot(cor_heat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 2.5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation heatmap of energy, wealth and electricity mix variables (2000-2022)",
       x = "Variable", y = "Variable", fill = "Pearson r") +
  theme_minimal()
print(p3)

top_pairs <- cor_long %>%
  filter(as.character(Var1) < as.character(Var2)) %>%
  arrange(desc(abs(value))) %>%
  head(10)
print(top_pairs %>% mutate(value = round(value, 3)))

# ==== 8. Questions ====
# Q1-Q5 on wealth, renewables, coal, carbon intensity and five countries
cat("Q1 correlation (log GDP per capita vs log energy per capita):",
    round(cor(energy_clean$log_gdp_per_capita, energy_clean$log_total_energy_per_capita_kwh), 3), "\n")
p4 <- ggplot(energy_clean, aes(x = log_gdp_per_capita, y = log_total_energy_per_capita_kwh, color = year)) +
  geom_point(alpha = 0.5) +
  labs(title = "Q1: GDP per capita vs primary energy per capita (2000-2022)",
       x = "log(GDP per capita, international $)", y = "log(primary energy per capita, kWh)", color = "Year") +
  theme_minimal()
print(p4)

energy_2022 <- energy_clean %>% filter(year == 2022)
print(energy_2022 %>% arrange(desc(gdp_per_capita)) %>%
        select(country, gdp_per_capita, total_energy_per_capita_kwh) %>% head(10))
print(energy_2022 %>% arrange(gdp_per_capita) %>%
        select(country, gdp_per_capita, total_energy_per_capita_kwh) %>% head(10))

ws_2000 <- energy_clean %>% filter(year == 2000) %>%
  mutate(wind_solar = wind_electricity_share_pct + solar_electricity_share_pct) %>% select(country, wind_solar)
ws_2022 <- energy_2022 %>%
  mutate(wind_solar = wind_electricity_share_pct + solar_electricity_share_pct) %>% select(country, wind_solar)
ws_2000 <- ws_2000 %>% filter(country %in% ws_2022$country) %>% arrange(country)
ws_2022 <- ws_2022 %>% filter(country %in% ws_2000$country) %>% arrange(country)
ws_change <- data.frame(country = ws_2000$country,
                        wind_solar_2000 = round(ws_2000$wind_solar, 2),
                        wind_solar_2022 = round(ws_2022$wind_solar, 2),
                        increase = round(ws_2022$wind_solar - ws_2000$wind_solar, 2))
top15 <- ws_change %>% arrange(desc(increase)) %>% head(15)
print(top15)
top15$country <- factor(top15$country, levels = top15$country[order(top15$increase)])
p5 <- ggplot(top15, aes(x = country, y = increase)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Q2: Top 15 countries by increase in wind + solar share (2000 to 2022)",
       x = "Country", y = "Increase in wind + solar share of electricity (percentage points)") +
  theme_minimal()
print(p5)

income_cuts <- quantile(energy_clean$gdp_per_capita, c(0.33, 0.66))
print(round(income_cuts, 2))
energy_clean <- energy_clean %>%
  mutate(income_group = factor(ifelse(gdp_per_capita <= income_cuts[1], "Low",
                                      ifelse(gdp_per_capita <= income_cuts[2], "Middle", "High")),
                               levels = c("Low", "Middle", "High")))
print(table(energy_clean$income_group))

years <- min(energy_clean$year):max(energy_clean$year)
coal <- energy_clean$coal_electricity_share_pct
coal_by_year <- data.frame(
  year = years,
  Low = sapply(years, function(y) mean(coal[energy_clean$year == y & energy_clean$income_group == "Low"])),
  Middle = sapply(years, function(y) mean(coal[energy_clean$year == y & energy_clean$income_group == "Middle"])),
  High = sapply(years, function(y) mean(coal[energy_clean$year == y & energy_clean$income_group == "High"])),
  Overall = sapply(years, function(y) mean(coal[energy_clean$year == y])))
print(round(coal_by_year, 2))
coal_long <- melt(coal_by_year, id.vars = "year")
p6 <- ggplot(coal_long, aes(x = year, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "Q3: Average coal share of electricity by income group (2000-2022)",
       x = "Year", y = "Average coal share of electricity (%)", color = "Income group") +
  theme_minimal()
print(p6)

cat("Q4 correlation (fossil share vs carbon intensity):",
    round(cor(energy_clean$fossil_share_pct, energy_clean$electricity_carbon_intensity_gco2eq_per_kwh), 3), "\n")
cat("Q4 correlation (low-carbon share vs carbon intensity):",
    round(cor(energy_clean$low_carbon_share_pct, energy_clean$electricity_carbon_intensity_gco2eq_per_kwh), 3), "\n")
p7 <- ggplot(energy_clean, aes(x = low_carbon_share_pct, y = electricity_carbon_intensity_gco2eq_per_kwh,
                               color = income_group)) +
  geom_point(alpha = 0.5) +
  labs(title = "Q4: Low-carbon share vs carbon intensity of electricity (2000-2022)",
       x = "Low-carbon share of electricity (%)", y = "Carbon intensity (gCO2eq per kWh)",
       color = "Income group") +
  theme_minimal()
print(p7)

low_carbon_band <- cut_width(energy_clean$low_carbon_share_pct, width = 20, boundary = 0)
bands <- levels(low_carbon_band)
print(data.frame(low_carbon_band = bands,
                 rows = as.numeric(table(low_carbon_band)),
                 mean_carbon_intensity = round(sapply(bands, function(b)
                   mean(energy_clean$electricity_carbon_intensity_gco2eq_per_kwh[low_carbon_band == b])), 1),
                 row.names = NULL))

focus_countries <- c("Ireland", "United Kingdom", "Germany", "Denmark", "France")
focus <- energy_clean %>% filter(country %in% focus_countries) %>%
  mutate(wind_solar_share_pct = round(wind_electricity_share_pct + solar_electricity_share_pct, 2))
p8a <- ggplot(focus, aes(x = year, y = fossil_share_pct, color = country)) +
  geom_line(linewidth = 1) +
  labs(title = "Fossil share of electricity", x = "Year", y = "Fossil share (%)", color = "Country") +
  theme_minimal()
p8b <- ggplot(focus, aes(x = year, y = wind_solar_share_pct, color = country)) +
  geom_line(linewidth = 1) +
  labs(title = "Wind + solar share of electricity", x = "Year", y = "Wind + solar share (%)", color = "Country") +
  theme_minimal()
grid.arrange(p8a, p8b, ncol = 2, top = "Q5: Electricity transition in five European countries (2000-2022)")

print(focus %>% filter(year == 2000 | year == 2022) %>%
        select(country, year, fossil_share_pct, low_carbon_share_pct, wind_solar_share_pct,
               electricity_carbon_intensity_gco2eq_per_kwh) %>%
        arrange(country, year))

# ==== 9. Unusual patterns ====
# Hydro-dominated, oil-dependent and coal-heavy high income countries
print(energy_clean %>% filter(hydro_electricity_share_pct > 90) %>% count(country) %>% arrange(desc(n)))
print(energy_clean %>% filter(year == 2022) %>% arrange(desc(oil_electricity_share_pct)) %>%
        select(country, oil_electricity_share_pct, income_group) %>% head(10))
print(energy_clean %>% filter(year == 2022, income_group == "High") %>%
        arrange(desc(coal_electricity_share_pct)) %>%
        select(country, coal_electricity_share_pct, gdp_per_capita) %>% head(10))
