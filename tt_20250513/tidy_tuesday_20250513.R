
library(tidyverse)

vesuvius <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-13/vesuvius.csv')

vesuvius <- vesuvius |>
  mutate(
    type = fct(type),
    area = fct(area)
  )


vesuvius2 <- vesuvius |> 
  filter(year>2012)  |>
  filter(duration_magnitude_md>1) |>
  group_by(year) |> 
  mutate(
    num_earthquakes = n()
  )

vesuvius2 |>
  ggplot(aes(x = year, y = num_earthquakes)) +
  geom_line(linewidth = 1,colour = "red") + 
  geom_point(colour = "red", size = 2)+
  geom_smooth(method = "lm")+
  scale_x_continuous(breaks = seq(2013, 2024, by = 2)) +
  labs(
    title = "The number of earthquakes in Mount Vesuvius is increasing",
    x = "year", 
    y = "earquakes with Md > 1",
    caption = "Data: Italian Istituto Nazionale di Geofisica e Vulcanologia"
  ) +
  theme_bw()+
  theme(panel.grid.minor = element_blank())

ggsave(
  filename = "vesuvius_earquakes.png",
  height = 4.5,
  width = 7,
  bg="white",
  units = "in",
  dpi = 300
)
