library("tidyverse")
library("leaflet")

read.csv()

journeys <- read_csv("data/cheats_journeys.csv")

leaflet() %>%
  addProviderTiles(providers$Esri.WorldShadedRelief) %>%
  addCircleMarkers(data = journeys,
                   lng = ~start.longitude,
                   lat = ~start.latitude,
                   color = "green") %>%
  addCircleMarkers(data = journeys,
                   lng = ~end.longitude,
                   lat = ~end.latitude,
                   color = "red")

gg_routes <- journeys %>%
  count(start.country, end.country) %>%
  arrange(desc(n)) %>%
  mutate(route = paste(start.country,
                       "->",
                       end.country)) %>%
  mutate(route = fct_reorder(route, n)) %>%
  ggplot(aes(x = route, y = n)) +
  geom_col() + 
  coord_flip()
ggsave("routes-plot.png", gg_routes)
