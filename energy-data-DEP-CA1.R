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
