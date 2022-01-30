## Installation of packages
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("skimr")


## Loading the tidyverse and lubridate package
library(tidyverse)
library(skimr)
library(lubridate)

data <- readr::read_csv(file = "data/africa_historic_covid19_data.csv")
data <- read_csv("data/africa_historic_covid19_data.csv")

data

# Simple Exploratory Data Analysis

head(data, n = 10)

tail(data,n = 10)

dim(data)

glimpse(data)


skimr::skim(data)

# Explore the tidyverse package
# Pipes %>%
# dplyr
# data manipulation - dplyr
data %>% select(`Country/Region`, Deaths)

data %>% select(starts_with("D"))

data %>% filter(Confirmed<10000) %>% View()

data %>% filter(between(Confirmed, 1000, 5000)) %>% View()

# lubridate
data_cleaned <- data %>%
  mutate(Date = mdy(Date)) %>%
  mutate(`Country/Region` = as_factor(`Country/Region`))


# grouping
unique(data$`Country/Region`)

data %>%
  group_by(`Country/Region`) %>%
  summarise(Total_Deaths = sum(Deaths)) %>% View()


data_cleaned <- data %>%
  mutate(`Country/Region` = factor(`Country/Region`,
                                   levels = unique(`Country/Region`),
                                   labels = unique(`Country/Region`))) %>%
  mutate(Date = mdy(Date))%>%  # Using lubridate package
  group_by(`Country/Region`) %>%
  summarise(Total_Death = sum(Deaths)) %>%
  ungroup()


## Split a column in multiple columns
data %>%
  separate(Date,c('month','day','year'),'-',remove = FALSE) %>% View()

## wide data
data_wide <- data %>%
  pivot_wider(names_from = `Country/Region`,
              values_from = c(Confirmed, Deaths, Recovered),
              names_sep = ".")


## long data
data_long <- data_wide %>%
  pivot_longer(!Date,
               names_to = c("d_type", "country"),
               names_pattern = "([[:alpha:]]+).([[:alpha:]]+)")

data_long <- data_long %>%
  group_by(d_type) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from = d_type,
              values_from = value)

data_long <- data_long %>% select(!row)



# data visualization - ggplot2
# forcats package
gggplot2_data <- data_cleaned %>%
  mutate(`Country/Region` = fct_reorder(`Country/Region`, Total_Death, .desc = TRUE)) %>%
  mutate(`Country/Region` = fct_lump(`Country/Region`,
                                     n=10, other_level = "Others", w = Total_Death))

g <- gggplot2_data %>%
  ggplot(aes(`Country/Region`, Total_Death, fill=`Country/Region`))+ # millions+
  geom_col()+
  scale_y_continuous(labels = scales::label_number(suffix = " M", scale = 1e-6))+
  labs(title = "Total Number of Deaths Per Country or Region",
       y = "No of Deaths in Millions(M)", x="Country or Region")

g


# interactive visuals
library(plotly)

ggplotly(g, tooltip = c("x","y"))

ggsave("my_viz.png")

ggsave("my_viz.pdf",width = 20, height = 30)






