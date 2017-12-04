library("tidyverse")
raw_journeys <- read_csv("data-raw/journeys.csv")

raw_journeys %>%
  separate(start.location,
           into = c("start.country",
                    "start.city")) %>%
  separate(end.location,
           into = c("end.country",
                    "end.city")) %>%
  write_csv('data/journeys.csv')
