# We will use from the Johns Hopkins University Center for Systems Science and Engineering created 
# a "https://github.com/RamiKrispin/coronavirus" publicly available data repository

# We will try to visualize COVID-19 data from the first several weeks of the outbreak
# It is part of the Datacamp r project

# loading packages
library('readr')
library('ggplot2')
library('dplyr')

## read confirmed_cases_worldwide.csv data
covid <- read_csv('/Users/huruybelay/Desktop/datascience/datacamp/projects/covid/Visualizing COVID-19/datasets/confirmed_cases_worldwide.csv')
head(covid)

# Confirmed cases throughout the world
ggplot(covid, aes(x = 'date', y = cum_cases)) + 
  geom_line() +
  ylab("Cumulative confirmed cases")

## lets read the other data of covid
covid_china <- read_csv('/Users/huruybelay/Desktop/datascience/datacamp/projects/covid/Visualizing COVID-19/datasets/confirmed_cases_china_vs_world.csv')
head(covid_china)

# China compared to the rest of the world
## Explore the data
glimpse(covid_china)

## drawing line plot
plt_cum_covid_china <- ggplot(covid_china) +
  geom_line(aes(date, cum_cases, color = is_china)) +
  ylab("Cumulative confirmed cases")
plt_cum_covid_china


who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting\nchange"
) %>%
  mutate(date = as.Date(date))
who_events

# Adding a trend line to china
# Using who_events, add vertical dashed lines with an xintercept at date
# and text at date, labeled by event, and at 100000 on the y-axis
plt_cum_covid_china +
  geom_vline(aes(xintercept = date), data = who_events, linetype = "dashed") +
  geom_text(aes(date, label = event), data = who_events, y = 1e5)


# Filter for China, from Feb 15
china_after_feb15 <- covid_china %>%
  filter(is_china == "China", date >= "2020-02-15")

# Using china_after_feb15, draw a line plot cum_cases vs. date
# Add a smooth trend line using linear regression, no error bars
ggplot(china_after_feb15, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases")

# For the rest of the world
# Filter confirmed_cases_china_vs_world for not China
not_china <- covid_china %>%
  filter(is_china == "Not China")

# Using not_china, draw a line plot cum_cases vs. date
# Add a smooth trend line using linear regression, no error bars
plt_not_china_trend_lin <- ggplot(not_china, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases")

# See the result
plt_not_china_trend_lin 


## Adding logaarithmic log
# Modify the plot to use a logarithmic scale on the y-axis
plt_not_china_trend_lin + 
  scale_y_log10()

# Which countries outside of China have been hit hardest?
# Run this to get the data for each country
confirmed_cases_by_country <- read_csv("/Users/huruybelay/Desktop/datascience/datacamp/projects/covid/Visualizing COVID-19/datasets/confirmed_cases_by_country.csv")
glimpse(confirmed_cases_by_country)

# Group by country, summarize to calculate total cases, find the top 7
top_countries_by_total_cases <- confirmed_cases_by_country %>%
  group_by(country) %>%
  summarize(total_cases = max(cum_cases)) %>%
  top_n(7, total_cases)

# See the result
top_countries_by_total_cases

#Plotting hardest hit countries as of Mid-March 2020
# Read in the dataset from datasets/confirmed_cases_top7_outside_china.csv
confirmed_cases_top7_outside_china <- read_csv("/Users/huruybelay/Desktop/datascience/datacamp/projects/covid/Visualizing COVID-19/datasets/confirmed_cases_top7_outside_china.csv")

# Glimpse at the contents of confirmed_cases_top7_outside_china
glimpse(confirmed_cases_top7_outside_china)

# Using confirmed_cases_top7_outside_china, draw a line plot of
# cum_cases vs. date, colored by country
ggplot(confirmed_cases_top7_outside_china, aes(date, cum_cases, color = country)) +
  geom_line() +
  ylab("Cumulative confirmed cases")

