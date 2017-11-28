library("tidyverse")
library("leaflet")
library("highcharter")
library("sf")
library("statesRcontiguous")
library("lubridate")

journeys <- read_csv("data/cheats_journeys.csv")

## ==== Country -> Country journeys tally ----
country_to_country_counts <- journeys %>%
  count(start.country, end.country) %>%
  mutate(journey = paste(start.country, "->", end.country)) %>%
  arrange(n) %>%
  mutate(journey = as.factor(journey)) %>%
  mutate(journey = fct_reorder(journey, n))

gg_country_to_country_counts <- country_to_country_counts %>%
  ggplot(aes(x = journey, y = n)) + geom_col() +
  coord_flip() +
  xlab("") +
  ylab("Number of journeys") +
  ggtitle("Number of journeys split by start and end country")
gg_country_to_country_counts
# ggsave("gg_country_to_country_counts.png", gg_country_to_country_counts)

journeys %>%
  count(start.country, end.country) %>%
  mutate(journey = paste(start.country, "->", end.country)) %>%
  arrange(desc(n)) %>%
  hchart(type = "bar",
         hcaes(x = journey, y = n)) %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Number of journeys")) %>%
  hc_title(text = "Number of journeys split by start and end country")

## ==== End location chart ----

end_country_tallies <- journeys %>%
  group_by(end.country) %>%
  summarise(total.letters = sum(number.of.letters),
            total.journeys = n()) %>%
  mutate(
    total.letters = total.letters / sum(total.letters),
    total.journeys = total.journeys / sum(total.journeys)
  ) %>%
  arrange(total.letters) %>%
  mutate(end.country = fct_reorder(end.country, total.letters)) %>%
  gather(measure, value,-end.country) 

gg_end_country_tallies <- end_country_tallies %>%
  ggplot(aes(x = end.country,
             y = value,
             fill = measure)) +
  geom_col(position = "dodge") +
  coord_flip() +
  xlab("Final Destination Country") +
  ylab("Percentage") +
  ggtitle("Final destination of letters",
          subtitle = "Ordered by percentage of journeys") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c("#1b9e77", "#7570b3"),
    name = "",
    breaks = c("total.letters", "total.journeys"),
    labels = c("Percentage of Journeys", "Percentage of letters")
  )
gg_end_country_tallies
# ggsave("gg_end_country_tallies.png", gg_end_country_tallies)

journeys %>%
  group_by(end.country) %>%
  summarise(total.letters = sum(number.of.letters),
            total.journeys = n()) %>%
  mutate(
    total.letters = 100 * total.letters / sum(total.letters),
    total.journeys = 100 * total.journeys / sum(total.journeys)
  ) %>%
  arrange(desc(total.letters)) %>%
  mutate(end.country = fct_reorder(end.country, total.letters)) %>%
  gather(measure, value, -end.country)  %>%
  hchart(type = "bar",
         hcaes(x = end.country,
               y = value,
               group = measure)) %>%
  hc_xAxis(title = list(text = "Final Destination Country")) %>%
  hc_yAxis(title = list(text = "Percentage"),
           labels = list(format = '{value}%')) %>%
  hc_title(text = "Final destination of letters") %>%
  hc_subtitle(text = "Ordered by percentage of journeys")

## ==== Calendar heatmap

dated_journeys <- journeys %>%
  select(date, number.of.letters) %>%
  mutate(year = year(date),
         yearmonthf = paste(month(date, label = TRUE), year(date)),
         monthf = month(date, abbr = TRUE, label = TRUE),
         week = week(date),
         monthweek = ceiling(day(journeys$date) / 7),
         weekdayf = wday(date,label = TRUE))

dated_journeys %>%
  mutate(year.chr = as.character(year)) %>%
  group_by(year.chr) %>%
  summarise(observations = n()) %>%
  arrange(desc(observations))

gg_dated_journeys <- dated_journeys %>%
  filter(date >= dmy("01-01-1860") & date <= dmy("01-12-1869")) %>%
  ggplot(aes(monthweek, weekdayf, fill = number.of.letters)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Week of Month",
       y="",
       title = "Number of letters sent per day in the 1860s", 
       fill = "Number of letters")
gg_dated_journeys

## ==== Scattergeo plot ----

journeys %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~ start.longitude,
    lat = ~ start.latitude,
    color = "red",
    radius = 1
  ) %>%
  addCircleMarkers(
    lng = ~ end.longitude,
    lat = ~ end.latitude,
    color = "green",
    radius = 1
  )

## ==== States Choropleth ----

contiguous_usa <- shp_all_us_states %>%
  filter(contiguous.united.states == TRUE)

state_send_locs <- journeys %>%
  filter(start.country == "USA") %>%
  st_as_sf(
    coords = c("start.longitude", "start.latitude"),
    crs = st_crs(contiguous_usa)
  )

contiguous_send_counts <- contiguous_usa %>%
  mutate(send.counts = st_covers(contiguous_usa, state_send_locs) %>%
           lengths())

palette_contiguous_us <-
  colorBin("YlOrBr", domain = contiguous_send_counts$send.counts)

contiguous_send_counts %>%
  leaflet() %>%
  addPolygons(
    fillColor = ~ palette_contiguous_us(send.counts),
    fillOpacity = 1,
    weight = 1,
    color = "#000",
    label = ~ paste0(state.name, " (", send.counts, " journeys)") 
  ) %>%
  addLegend(pal = palette_contiguous_us,
            values = ~send.counts)
