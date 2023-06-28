# All the data can be downloaded from 'https://www.gapminder.org/data/' 
# by choosing a specific indicator like life expectancy, population etc.

# Load required libraries
library(tidyverse)      # For data manipulation and visualization
library(gifski)        # For creating GIF animations
library(countrycode)   # For country code conversion
library(gganimate)     # For creating animated plots
library(magick)        # For working with image files
library(svglite)       # For exporting plots to SVG format
library(extrafont)     # For using custom fonts

# Set options
options(scipen = 999)  # Prevent scientific notation in numeric output

# Read 'lex.csv' or the 'life expectancy' dataset into lex data frame
lex <- read.csv('lex.csv', header = TRUE)

# Read 'gdp_pcap.csv' or 'GDP per Capita' dataset into gdp data frame
gdp <- read.csv('gdp_pcap.csv', header = TRUE)

# Pivot lex data frame to long format
lex2 <- lex %>%
  pivot_longer(cols = 2:302, names_to = 'year', values_to = 'lifeExp')

# Remove the 'X' prefix from year column and convert it to numeric
lex2$year <- as.numeric(str_remove(lex2$year, 'X'))

# Iterate through each column in gdp data frame
for (col in names(gdp[, 2:302])) {
  # Convert '27.8k' format to numeric value
  gdp[, col] <- ifelse(grepl("k", gdp[, col]), as.numeric(sub("k", "", gdp[, col])) * 1000, gdp[, col])
}

# Convert columns in gdp data frame to numeric
gdp[, 2:302] <- lapply(gdp[, 2:302], as.numeric)

# Pivot gdp_new data frame to long format
gdp2 <- gdp %>%
  pivot_longer(cols = 2:302, names_to = 'year', values_to = 'gdp_pcap')

# Remove the 'X' prefix from year column and convert it to numeric
gdp2$year <- as.numeric(str_remove(gdp2$year, 'X'))

# Perform a left join between lex2 and gdp2 data frames based on 'year' and 'country' columns
df <- left_join(lex2, gdp2, by = c('year', 'country'), relationship = "one-to-one")

# Read 'pop.csv' or 'Population' dataset into pop data frame
pop <- read.csv('pop.csv', header = TRUE)

# Iterate through each column in pop data frame
for (col in names(pop[, 2:302])) {
  # Convert '27.8k' format to numeric value
  pop[, col] <- ifelse(grepl("k", pop[, col]), as.numeric(sub("k", "", pop[, col])) * 1000, pop[, col])
  # Convert '3.2M' format to numeric value
  pop[, col] <- ifelse(grepl("M", pop[, col]), as.numeric(sub("M", "", pop[, col])) * 1000000, pop[, col])
  # Convert '5.7B' format to numeric value
  pop[, col] <- ifelse(grepl("B", pop[, col]), as.numeric(sub("B", "", pop[, col])) * 1000000000, pop[, col])
}

# Pivot pop data frame to long format
pop2 <- pop %>%
  pivot_longer(cols = 2:302, names_to = 'year', values_to = 'population')

# Remove the 'X' prefix from year column and convert it to numeric
pop2$year <- as.numeric(str_remove(pop2$year, 'X'))

# Perform a left join between df and pop2 data frames based on 'year' and 'country' columns
df2 <- left_join(df, pop2, by = c('year', 'country'), relationship = "one-to-one")

# Assign continent codes to countries
df2$continent <- countrycode(sourcevar = df2$country,
                             origin = "country.name",
                             destination = "continent")

# Convert population column to numeric
df2$population <- as.numeric(df2$population)

# Filter data for years between 1900 and 2023
df3 <- df2 %>% filter(year >= 1900 & year <= 2023)

# Define colors for each country
country_colors <- gapminder::country_colors

# Define colors for each continent
continent_cols <-  c('#0085C7','#F4C300','#000000','#009F3D','#DF0024')

# Assign colors to continents
continent_colors <- c(Oceania = continent_cols[1],
                      Africa = continent_cols[2],
                      Europe = continent_cols[3],
                      Asia = continent_cols[4],
                      Americas = continent_cols[5])

# Create the base ggplot object
p <- ggplot(df3, aes(gdp_pcap, lifeExp, size = population, colour = continent)) +
  geom_point(shape = 19, alpha = 0.7) +
  scale_colour_manual(name = 'Continents', values = continent_colors) +  # Only show legend for continents
  scale_size(name = 'Population',
             range = c(2, 12), 
             breaks = c(1000000, 10000000, 100000000, 1000000000), 
             labels = c('1 M', '10 M', '100 M', '1 B')) +
  scale_x_log10() +
  ylim(c(0,100)) +
  labs(title = 'Unveiling Global Progress: 
       Exploring GDP, Life Expectancy, and Population 
       (1900-2023)',
       subtitle = '
    
    Year: {frame_time}
    
    ', x = '
    
    GDP per Capita ($)', y = 'Life Expectancy (Years)
    
    ') + 
  theme(legend.key = element_rect(fill = 'white'),
        legend.position = "right",  # Position the legend on the right side
        legend.title = element_text(size = 20, family = "Tw Cen MT", hjust = 0.5),
        legend.text = element_text(size = 14),  # Adjust the legend text size
        legend.key.size = unit(1.2, "lines"),  # Adjust the size of legend color keys
        legend.key.height = unit(0.7, "cm"),  # Adjust the height of legend color keys
        legend.spacing = unit(0.2, "lines"),  # Adjust the spacing between legend items
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        text = element_text(family = "Tw Cen MT"),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 22, face = "bold"),
        axis.title = element_text(size = 14),
        plot.margin = margin(1.5, 1.5, 1.5, 1.5, 'cm')) +
  transition_time(as.integer(year)) +
  ease_aes('linear')

# Create the animated plot and save it as a GIF
animate(p, fps = 20, duration = 30, start_pause = 10, end_pause = 30, device = 'svglite', renderer = magick_renderer())
