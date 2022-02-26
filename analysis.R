jail_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library(ggplot2)
library(dplyr)

#my data
my_jail_data <- jail_data %>% select(year, state, urbanicity, county_name, total_pop, black_pop_15to64,
                                     total_jail_pop, black_jail_pop, jail_rated_capacity,
                                     aapi_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop)

my_jail_data <- mutate(my_jail_data, jail_pop_ratio = total_jail_pop/jail_rated_capacity)
my_jail_data <- mutate(my_jail_data, jail_black_pop_ratio = black_jail_pop/jail_rated_capacity)

wa_data <- my_jail_data %>% filter(state == "WA")

# ---------------------------------------------------------------------------------
# Values of Interest

# Value 1: What county in WA had the highest number of Blacks in prison in the latest year?
highest_pop_county <- wa_data %>% filter(year == max(year, na.rm=TRUE)) %>% filter(black_jail_pop == max(black_jail_pop, na.rm=TRUE)) %>% pull(county_name)

# Value 2: What was the ratio of Blacks in prison from the latest year in that county?
ratio_black_jail <- wa_data %>% filter(year == max(year, na.rm=TRUE)) %>% filter(black_jail_pop == max(black_jail_pop, na.rm=TRUE)) %>% pull(jail_black_pop_ratio)

# Value 3: What was the percent Black prison population ratio change difference from 
# the earliest year to the latest year in Washington?
earliest_ratio <- wa_data %>% filter(county_name == highest_pop_county) %>% filter(jail_black_pop_ratio > 0) %>% filter(year == min(year, na.rm=FALSE)) %>% pull(jail_black_pop_ratio)
latest_ratio <- wa_data %>% filter(county_name == highest_pop_county) %>% filter(year == max(year, na.rm=TRUE)) %>% pull(jail_black_pop_ratio)
black_prison_change <- (latest_ratio - earliest_ratio)

# Value 4: What was the ratio of Blacks living in that county in the latest year?
black_county_pop <- wa_data %>% filter(year == max(year, na.rm=TRUE)) %>% filter(county_name == highest_pop_county) %>% pull(black_pop_15to64)
total_county_pop <- wa_data %>% filter(year == max(year, na.rm=TRUE)) %>% filter(county_name == highest_pop_county) %>% pull(total_pop)
county_total_to_black <- (black_county_pop/total_county_pop)

# Value 5: What is the average Black prison population ratio in Washington?
avg_black_pop <- wa_data %>% summarize(avg_black_pop = mean(black_jail_pop, na.rm=TRUE)) %>% pull(avg_black_pop)
avg_total_pop <- wa_data %>% summarize(avg_total_pop = mean(total_jail_pop, na.rm=TRUE)) %>% pull(avg_total_pop)
avg_black_ratio <- (avg_black_pop / avg_total_pop)


# ---------------------------------------------------------------------------------
# Trends Over Time Chart
# Black prison count over the top 5 prison population states (linegraph)

top_5_states <- my_jail_data %>% filter(black_jail_pop > 0) %>% filter(year == max(year)) %>% arrange(-total_jail_pop) %>% top_n(5, total_jail_pop)

top_5_data <- my_jail_data %>% filter(black_jail_pop > 0) %>% 
  filter(state == top_5_states$state[1] |
           state == top_5_states$state[2] |
           state == top_5_states$state[3] |
           state == top_5_states$state[4] |
           state == top_5_states$state[5])

# group by year, sum up pop by the year
top_states_pop <- top_5_data %>% group_by(year, state) %>% summarize(black_jail_pop = sum(black_jail_pop))

trend_over_time <- ggplot(data = top_states_pop) +
  geom_point(mapping = aes(x = year, y = black_jail_pop, color = state)) +
  geom_smooth(mapping = aes(x = year, y = black_jail_pop, color = state), method = "loess", se = FALSE, fullrange = TRUE) +
  labs(title = "Top 5 States' Black Prisoner Count vs Time", x = "Year", y = "Prisoner Population", color = "State")

trend_over_time

# ---------------------------------------------------------------------------------
# Variable Comparison Chart
# Compare all races's prison population with the jail capacity (linegraph?)

race_jail_pop <- wa_data %>% 
  filter(aapi_jail_pop > 0, black_jail_pop > 0, latinx_jail_pop > 0, native_jail_pop > 0,
         white_jail_pop > 0, other_race_jail_pop > 0, jail_rated_capacity > 0) %>% 
  group_by(year) %>% 
  summarize(aapi_prison = sum(aapi_jail_pop), black_prison = sum(black_jail_pop),
            latinx_prison = sum(latinx_jail_pop), native_prison = sum(native_jail_pop),
            white_prison = sum(white_jail_pop), other_race_prison = sum(other_race_jail_pop),
            prison_capacity = sum(jail_rated_capacity))

race_pop_linechart <- ggplot(data = race_jail_pop) +
  geom_smooth(mapping = aes(x = year, y = black_prison, color = "Black"), method = "loess", se = FALSE, fullrange = TRUE) +
  geom_smooth(mapping = aes(x = year, y = latinx_prison, color = "Latinx"), method = "loess", se = FALSE, fullrange = TRUE) +
  geom_smooth(mapping = aes(x = year, y = white_prison, color = "White"), method = "loess", se = FALSE, fullrange = TRUE) +
  geom_smooth(mapping = aes(x = year, y = prison_capacity, color = "Capacity"), method = "loess", se = FALSE, fullrange = TRUE) +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(title = "Prisoner Population vs Jail Capacity in Washington", x ="Year", y = "Population", color = "Race")

race_pop_linechart

# ---------------------------------------------------------------------------------
# IGNORE IGNORE IGNORE IGNORE NOT USING IGNORE THIS
# Compare all races's prison population over the last year (barchart)

latest_aapi_pop <- my_jail_data %>% filter(year == max(year)) %>% filter(aapi_jail_pop > 0) %>% summarize(jail_pop = sum(aapi_jail_pop)) %>% pull (jail_pop)
latest_black_pop <- my_jail_data %>% filter(year == max(year)) %>% filter(black_jail_pop > 0) %>% summarize(jail_pop = sum(black_jail_pop)) %>% pull (jail_pop)
latest_latinx_pop <- my_jail_data %>% filter(year == max(year)) %>% filter(latinx_jail_pop > 0) %>% summarize(jail_pop = sum(latinx_jail_pop)) %>% pull (jail_pop)
latest_native_pop <- my_jail_data %>% filter(year == max(year)) %>% filter(native_jail_pop > 0) %>% summarize(jail_pop = sum(native_jail_pop)) %>% pull (jail_pop)
latest_white_pop <- my_jail_data %>% filter(year == max(year)) %>% filter(white_jail_pop > 0) %>% summarize(jail_pop = sum(white_jail_pop)) %>% pull (jail_pop)
latest_other_pop <- my_jail_data %>% filter(year == max(year)) %>% filter(other_race_jail_pop > 0) %>% summarize(jail_pop = sum(other_race_jail_pop)) %>% pull (jail_pop)

race <- c("AAPI", "Black", "Latinx", "Native", "White", "Other Races")
race_pop <- c(latest_aapi_pop, latest_black_pop, latest_latinx_pop, latest_native_pop, latest_white_pop, latest_other_pop)

latest_pop_of_race <- data.frame(race, race_pop)

race_barchart <- ggplot(data = latest_pop_of_race) +
  geom_col(mapping = aes(x = reorder(race, +race_pop), y = race_pop, fill = race)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(title = "Prisoner Population from the Latest Year by Race", x ="Race", y = "Population", fill = "Race")  

race_barchart  

# ---------------------------------------------------------------------------------
# Map
# Heat map of the US based on Black prisoner population in the latest year
library("mapproj")
library("maps")

state_shape <- map_data("state")
state_abbrevs <- data.frame(state.abb, state.name)

us_jail_pop <- my_jail_data %>% filter(year == max(year)) %>% group_by(state) %>% summarize(black_jail_pop = sum(black_jail_pop, na.rm=TRUE))

us_jail_pop <- left_join(us_jail_pop, state_abbrevs, by=c('state' = 'state.abb'))
us_jail_pop <- us_jail_pop %>% mutate(region = tolower(state.name))
state_shape <- left_join(state_shape, us_jail_pop, by=c('region'))

us_mapplot <- ggplot(state_shape) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop)) +
  coord_map() +
  labs(title = "Black Prisoner Population in Each State by Latest Year", fill = "Population") +
  scale_fill_continuous(low = 'blue', high = 'red', labels = scales::label_number_si())

us_mapplot

















