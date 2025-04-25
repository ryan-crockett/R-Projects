rm(list=ls())
library(tidyverse)
PERMID <- 3437712
PERMID <- as.numeric(gsub("//D", "", PERMID))
set.seed(PERMID)

world_data_raw <- read_csv("world_climate_data.csv")
head(world_data_raw$date,20)
library(readr)
library(lubridate)

world_data <- world_data_raw %>%
  mutate(date = case_when(
    grepl("^\\d{4}$", date) ~ ymd(paste0(date, "-01-01")),
    grepl("^\\d{1,2}-\\d{4}$", date) ~ as.Date((paste0("01-", date)), format = "%d-%m-%Y"),
        TRUE ~ mdy(date)
  ))
world_data

##################

seasonality_data <- world_data %>%
  filter(!(is.na(co2) & is.na(methane))) %>%
  mutate(
    co2_seasonality = co2 - co2_deseasonalized,
    methane_seasonality = methane - methane_trend,
    quarter = quarter(date)
  ) %>%
  group_by(quarter) %>%
  summarise(
    co2_seasonality = mean(co2_seasonality, na.rm = TRUE),
    methane_seasonality = mean(methane_seasonality, na.rm = TRUE)
  ) |> arrange(quarter)
seasonality_data

###################### 

co2_data <- world_data %>%
  select(date, co2_deseasonalized) %>%
  filter(!is.na(co2_deseasonalized))

global_temp_data <- world_data %>%
  select(date, global_temp) %>%
  filter(!is.na(global_temp))

library(ggplot2)

co2_data_filtered <- co2_data %>%
  filter(year(date) >= 1950)
global_temp_data_filtered <- global_temp_data %>%
  filter(year(date) >= 1950)

co2_temp_plot1 <- ggplot() +
  geom_line(data = co2_data_filtered, aes(x = date, y = co2_deseasonalized, color = "CO2 Level (ppm)")) +
  geom_line(data = global_temp_data_filtered, aes(x = date, y = global_temp * 400, color = "Global Temperature (C)")) +
  scale_y_continuous(
    name = "CO2 Level (ppm)", 
    sec.axis = sec_axis(~./400, name = "Global Temperature (C)")
  ) +
  scale_color_manual(
    values = c("CO2 Level (ppm)" = "blue", "Global Temperature (C)" = "red")
  ) +
  labs(x = "Year", color = "Measurement") +  # Adding a legend title
  theme_minimal()
co2_temp_plot1

#################### plot 2

co2_data_filtered2 <- arrange(co2_data,date) %>%
  mutate(co2_change = co2_deseasonalized - first(co2_deseasonalized))


co2_temp_plot2 <- ggplot() +
  geom_line(data = co2_data_filtered2, aes(x = date, y = co2_change, color = "Change in CO2 Level (ppm)")) +
  geom_line(data = global_temp_data_filtered, aes(x = date, y = global_temp * 100, color = "Global Temperature (C)")) +
  scale_y_continuous(
    name = "Change in CO2 Level (ppm)", 
    sec.axis = sec_axis(~./100, name = "Global Temperature (C)")
  ) +
  scale_color_manual(
    values = c("Change in CO2 Level (ppm)" = "blue", "Global Temperature (C)" = "red")
  ) +
  labs(x = "Year", color = "Measurement") +  # Adding a legend title
  theme_minimal()

co2_temp_plot2

########################### 

sea_level_data <- world_data %>%
  select(date, sea_level) %>%
  filter(!is.na(sea_level) & !is.na(date))

ocean_heat_data <- world_data %>%
  select(date, ocean_heat) %>%
  filter(!is.na(ocean_heat) & !is.na(date))

sea_level_data_filtered <- sea_level_data %>%
  filter(year(date) >= 1990 & year(date) <= 2020)

ocean_heat_data_filtered <- ocean_heat_data %>%
  filter(year(date) >= 1990 & year(date) <= 2020)

sea_level_ocean_heat_plot <- ggplot() +
  geom_line(data = sea_level_data_filtered, aes(x = date, y = sea_level, color = "Sea Level (mm)")) +
  geom_line(data = ocean_heat_data_filtered, aes(x = date, y = ocean_heat, color = "Ocean Heat (zJ)")) +
  scale_y_continuous(
    name = "Sea Level (mm)", 
    sec.axis = sec_axis(~ . / 1, name = "Ocean Heat (zJ)")
  ) +
  scale_color_manual(
    values = c("Sea Level (mm)" = "blue", "Ocean Heat (zJ)" = "red")
  ) +
  labs(x = "Year", color = "Measurement") +  # Adding a legend title
  theme_minimal()

sea_level_ocean_heat_plot

####################### PART 2

country_climate_data <- read_csv("country_climate_data.csv")

country_data1 <- country_climate_data %>%
  mutate(population = co2 / co2_per_capita,
        gdp_per_capita = gdp / population
  ) %>%
  filter(year >= 1970)
head(country_data1)

################

country_data2 <- country_data1 %>%
  group_by(year) |>
  mutate(
    gdp = ifelse(country == "World", sum(gdp, na.rm = TRUE), gdp),
    gdp_per_capita = ifelse(country == "World", gdp / population, gdp_per_capita)
  ) |>
  ungroup() |>
  filter(any(!is.na(gdp)), .by = country)

head(country_data2)
##################

country_data2_world <- country_data2 %>%
  filter(country == "World")

gdp_co2_world_plot1 <- ggplot(country_data2_world, aes(x = year)) +
  geom_line(aes(y = gdp, color = "GDP"), size = 1) +
  geom_line(aes(y = co2 * 1000, color = "CO2 Emissions"), size = 1) +
  scale_y_continuous(
    name = "GDP",
    trans = "log10",
    sec.axis = sec_axis(~ . / 1000, name = "CO2 Emissions", labels = scales::comma)
  ) +
  scale_color_manual(
    values = c("GDP" = "blue", "CO2 Emissions" = "red")
  ) +
  labs(x = "Year", color = "Measurement") + 
  theme_minimal()

gdp_co2_world_plot1

gdp_co2_world_plot2 <- ggplot(country_data2_world, aes(x = year)) +
  geom_line(aes(y = gdp_per_capita, color = "GDP Per Capita"), size = 1) +
  geom_line(aes(y = co2_per_capita * 1000, color = "CO2 Emissions Per Capita"), size = 1) +
  scale_y_continuous(
    name = "GDP Per Capita",
    trans = "log10",
    sec.axis = sec_axis(~ . / 1000, name = "CO2 Emissions Per Capita", labels = scales::comma)
  ) +
  scale_color_manual(
    values = c("GDP Per Capita" = "blue", "CO2 Emissions Per Capita" = "red")
  ) +
  labs(x = "Year", color = "Measurement") + 
  theme_minimal()
gdp_co2_world_plot2

library(gridExtra)
gdp_co2_world_plots <- grid.arrange(gdp_co2_world_plot1, gdp_co2_world_plot2)
gdp_co2_world_plots

#####################

country_data3 <- country_data2 %>%
  mutate(
    percent_gdp_change = (log(gdp) - lag(log(gdp))),
    percent_co2_change = (log(co2) - lag(log(co2))),
    .by = country
  )
country_data3

selected_countries <- c("United States", "China", "Japan", "Germany", "India", "World")
country_data_selected <- country_data3 %>% filter(country %in% selected_countries)

gdp_co2_change_plot <- ggplot(country_data_selected, aes(x = year)) +
  geom_line(aes(y = percent_gdp_change, color = "Change in GDP (%)")) +
  geom_line(aes(y = percent_co2_change, color = "Change in CO2 Emissions (%)")) +
  scale_y_continuous(
    name = "Change in GDP (%)",
    sec.axis = sec_axis(~ ., name = "Change in CO2 Emissions (%)", labels = scales::percent)
  ) +
  scale_color_manual(
    values = c("Change in GDP (%)" = "blue", "Change in CO2 Emissions (%)" = "red")
  ) +
  labs(x = "Year", color = "Measurement") +  # Adding legend title
  theme_minimal() +
  facet_wrap(~ country, ncol = 3)
gdp_co2_change_plot