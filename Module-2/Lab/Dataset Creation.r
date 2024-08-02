library(tidyverse)
library(lubridate)

# Set seed for reproducibility
set.seed(123)

# Generate synthetic data
n <- 1000  # Number of rows

# Country and year data
countries <- c("USA", "Canada", "Brazil", "UK", "Germany", "France", "India", "China", "Australia", "South Africa")
years <- seq(2000, 2023)

# Generate random data
data <- tibble(
  id = 1:n,
  country = sample(countries, n, replace = TRUE),
  year = sample(years, n, replace = TRUE),
  population = sample(1e6:1.5e9, n, replace = TRUE),
  cases = rpois(n, lambda = 1000),
  gdp_per_capita = rnorm(n, mean = 30000, sd = 15000),
  date = sample(seq(ymd('2000-01-01'), ymd('2023-12-31'), by="day"), n, replace = TRUE),
  temperature = round(rnorm(n, mean = 15, sd = 10), 1),
  region = sample(c("North America", "South America", "Europe", "Asia", "Oceania", "Africa"), n, replace = TRUE)
)

# Create a second dataset for joining examples
additional_data <- tibble(
  country = sample(countries, length(countries), replace = FALSE),
  continent = c("North America", "North America", "South America", "Europe", "Europe", "Europe", "Asia", "Asia", "Oceania", "Africa"),
  avg_life_expectancy = rnorm(length(countries), mean = 75, sd = 5)
)


countriesDs <- read_csv("Datasets/countries.csv")

# Fixing the region column based on the correct mapping
countriesDs <- data %>%
  mutate(region = case_when(
    country == "Brazil" ~ "South America",
    country == "South Africa" ~ "Africa",
    country == "Canada" ~ "North America",
    country == "France" ~ "Europe",
    country == "Germany" ~ "Europe",
    country == "UK" ~ "Europe",
    country == "Australia" ~ "Oceania",
    country == "China" ~ "Asia",
    country == "India" ~ "Asia",
    country == "USA" ~ "North America",
    TRUE ~ region  # Keep the original value if no match is found
  ))

write.csv(countriesDs, "countries.csv", row.names = FALSE) 



### Description of the Dataset
# - **`id`**: Unique identifier for each row.
# - **`country`**: Country name from a predefined list.
# - **`year`**: Random year between 2000 and 2023.
# - **`population`**: Random population size between 1 million and 1.5 billion.
# - **`cases`**: Number of cases where electrical services were interrupted, generated using a Poisson distribution.
# - **`gdp_per_capita`**: GDP per capita, generated using a normal distribution.
# - **`date`**: Random date between 2000 and 2023.
# - **`temperature`**: Temperature in Celsius, generated using a normal distribution.
# - **`region`**: Region of the world, randomly assigned.

### Additional Dataset
# - **`country`**: Matches the country names from the main dataset.
# - **`continent`**: Continent corresponding to each country.
# - **`avg_life_expectancy`**: Average life expectancy, generated using a normal distribution.