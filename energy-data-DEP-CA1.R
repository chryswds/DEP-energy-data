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
