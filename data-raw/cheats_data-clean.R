library("tidyverse")
library("lubridate")

raw_journeys <- read_csv("data-raw/journeys.csv")

raw_journeys <- raw_journeys %>%
  separate(start.location, into = c("start.country", "start.city"), extra = "merge") %>%
  separate(end.location, into = c("end.country", "end.city"), extra = "merge")

raw_journeys %>%
  mutate(date = dmy(date)) %>%
  write_csv("data/cheats_journeys.csv")


